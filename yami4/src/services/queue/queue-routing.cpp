// Copyright Maciej Sobczak 2008-2015.
// This file is part of YAMI4.
//
// YAMI4 is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// YAMI4 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

#include "queue-routing.h"
#include "../common/log.h"
#include "../common/utils.h"

#include <yami4-core/raw_buffer_data_source.h>

#include <string>
#include <deque>
#include <map>
#include <memory>
#include <vector>

using namespace queue;

namespace // unnamed
{

// Internal data structures involve:
// 1. map of queues, keyed by queue name
// 2. each queue in the map is composed of actual bounded message queue
// and a queue of clients waiting for messages (waiting client queue)
//
// The message/request flow is defined as:
//
// 1. when a new message arrives for the given queue:
// 1a. if the queue does not exist and the creation policy is "static"
//  then the message is rejected,
//  otherwise ("dynamic") the given queue is created, and:
// 1b. if there are waiting clients,
//  the message is routed to the first waiting client,
//  which is then removed from the waiting client queue,
//  otherwise (if there are no waiting clients) the message is
//  added to the message queue, in which case:
//  - if the queue is already full the message is either rejected
// or some message is dropped, according to the overflow policy
//
// if the message was not rejected, it is confirmed with empty reply
//
// 2. when a new client requests his message:
// 2a. if there is at least one message in the message queue,
//  it is popped from the queue and sent to the requesting client,
//  otherwise (if there are no messages) the client is added
//  to the waiting client queue, in which case:
//  - if the waiting client queue is already full
// the request is rejected
//
// 3. when a new client try-requests (try-get) his message:
// 3a. if there is at least one message in the message queue,
//  it is popped from the queue and sent to the requesting client,
//  otherwise (if there are no messages) the request is
//  immediately rejected
//
// Messages are sent to clients as responses to their original requests.

// Type of a single message (content) queue.
typedef std::vector<char> binary_buffer;
typedef std::deque<binary_buffer *> data_queue;

// Information about a single client waiting on the given queue.
struct waiting_client
{
    std::string target;
    long long requesting_message_id;
};

// Type of queue of waiting clients.
typedef std::deque<waiting_client> client_queue;

// Bundle of content queue and client queue.
struct routing_queue
{
    routing_queue()
        : total_content_size(0)
    {
    }
    
    data_queue message_content_queue;
    std::size_t total_content_size;
    client_queue waiting_clients_queue;
};

// Type of map that manages the complete routing state.
typedef std::map<std::string, routing_queue> routing_map;

std::size_t max_queue_num_of_messages_;
std::size_t max_queue_size_;
creation_policy queue_creation_policy_;

// Note: there is no need to make it protected
// as there is only one thread accessing this structure
routing_map routing_;

std::pair<routing_map::iterator, bool> do_create_queue(const std::string & queue_name)
{
    std::pair<routing_map::iterator, bool> result =
        routing_.insert(std::make_pair(queue_name, routing_queue()));
    if (result.second)
    {
        logger::put(logger::queues,
            std::string("    created queue " + queue_name));
    }

    return result;
}

// helper for finding and dynamic creation (if requested) of queues
void find_queue(const std::string & queue_name,
    routing_map::iterator & out_it,
    bool & out_new_queue_inserted,
    bool allow_creation)
{
    out_new_queue_inserted = false;

    out_it = routing_.find(queue_name);
    if (out_it == routing_.end())
    {
        if (allow_creation && (queue_creation_policy_ == dynamic_creation))
        {
            std::pair<routing_map::iterator, bool> result =
                do_create_queue(queue_name);

            out_it = result.first;
            out_new_queue_inserted = result.second;
        }
    }
}

} // unnamed namespace

void queue::init_routing(std::size_t max_queue_num_of_messages,
    std::size_t max_queue_size,
    creation_policy queue_creation_policy)
{
    max_queue_num_of_messages_ = max_queue_num_of_messages;
    max_queue_size_ = max_queue_size;
    queue_creation_policy_ = queue_creation_policy;
}

void queue::create_queue(const std::string & queue_name)
{
    (void)do_create_queue(queue_name);
}

bool queue::put_to_queue(
    const std::string & queue_name,
    const yami::core::serializable & body,
    bool (*process)(
        const char * target,
        long long incoming_msg_id,
        const yami::core::serializable & body),
    bool & out_no_such_queue)
{
    routing_map::iterator it;
    bool new_queue_inserted;

    find_queue(queue_name, it, new_queue_inserted, true);

    if (it != routing_.end())
    {
        out_no_such_queue = false;

        routing_queue & q = it->second;

        if (new_queue_inserted || q.waiting_clients_queue.empty())
        {
            // there are no clients waiting in this queue,
            // store the message

            std::size_t current_num_of_message = q.message_content_queue.size();
            if (current_num_of_message == max_queue_num_of_messages_)
            {
                logger::put(logger::queues,
                    std::string("    max length of queue reached for ") + queue_name);

                return false;
            }

            // copy the incoming buffer
            // using standard serialization features

            std::size_t new_message_size;
            yami::core::result res = body.get_serialize_buffer_size(new_message_size);
            if (res != yami::core::ok)
            {
                logger::put(logger::queues,
                    std::string("    ") + utils::result_to_string(res));

                return false;
            }

            if ((q.total_content_size + new_message_size) > max_queue_size_)
            {
                logger::put(logger::queues,
                    std::string("    max size of queue reached for ") + queue_name);

                return false;
            }

            std::auto_ptr<binary_buffer> buf(new binary_buffer());
            buf->resize(new_message_size);

            char * buffers[1] = { &(*buf)[0] };
            std::size_t buffer_sizes[1] = { new_message_size };

            res = body.serialize(buffers, buffer_sizes, 1);
            if (res != yami::core::ok)
            {
                logger::put(logger::queues,
                    std::string("    ") + utils::result_to_string(res));

                return false;
            }

            q.message_content_queue.push_back(buf.get());

            buf.release();

            q.total_content_size += new_message_size;

            return true;
        }
        else
        {
            // there is at least one client waiting for the message
            // - send the incoming message to that client and
            //   remove him from the queue of waiting clients

            waiting_client & client = q.waiting_clients_queue.front();

            bool sent_successfully =
                process(client.target.c_str(), client.requesting_message_id, body);

            q.waiting_clients_queue.pop_front();

            // if it was the only waiting client, then
            // now both content queue and waiting client queue are empty
            // - in this case tear down the whole structure

            if (q.waiting_clients_queue.empty())
            {
                routing_.erase(it);

                logger::put(logger::queues,
                    std::string("    deleted queue ") + queue_name);
            }

            return sent_successfully;
        }
    }
    else
    {
        out_no_such_queue = true;

        return true;
    }
}

bool queue::get_from_queue(const std::string & source,
    const std::string & queue_name,
    long long incoming_msg_id,
    bool (*process)(
        const char * target,
        long long incoming_msg_id,
        const yami::core::serializable & body),
    bool block)
{
    routing_map::iterator it;
    bool new_queue_inserted;

    find_queue(queue_name, it, new_queue_inserted, block);

    if (it != routing_.end())
    {
        routing_queue & q = it->second;

        if (new_queue_inserted || q.message_content_queue.empty())
        {
            // there are no messages in this queue

            if (block)
            {
                // add the new client to the waiting clients queue

                waiting_client c;
                c.target = source;
                c.requesting_message_id = incoming_msg_id;

                q.waiting_clients_queue.push_back(c);

                return true;
            }
            else
            {
                return false;
            }
        }
        else
        {
            // there is at least one message in the queue
            // - send this message to the requesting client and
            //   remove it from the message queue
            
            std::auto_ptr<binary_buffer> first_message(q.message_content_queue.front());
            q.message_content_queue.pop_front();

            yami::core::raw_buffer_data_source raw_buffer(
                &(*first_message)[0], first_message->size());

            (void)process(source.c_str(), incoming_msg_id, raw_buffer);

            q.total_content_size -= first_message->size();

            // if it was the only message in the queue, then
            // now both content queue and waiting client queue are empty
            // - in this case tear down the whole structure

            if (q.message_content_queue.empty() &&
                (queue_creation_policy_ == dynamic_creation))
            {
                routing_.erase(it);

                logger::put(logger::queues,
                    std::string("    deleted queue ") + queue_name);
            }

            return true;
        }
    }
    else
    {
        return false;
    }
}

yami::core::result queue::fill_simple_stats(yami::core::parameters & params)
{
    yami::core::result res = params.set_integer("num_of_queues",
        static_cast<int>(routing_.size()));
    if (res != yami::core::ok)
    {
        return res;
    }

    std::size_t num_of_all_waiting_messages = 0;
    std::size_t total_of_all_content_sizes = 0;
    std::size_t num_of_all_waiting_clients = 0;

    routing_map::const_iterator it = routing_.begin();
    routing_map::const_iterator end = routing_.end();
    for ( ; it != end; ++it)
    {
        const routing_queue & q = it->second;

        const std::size_t queue_length = q.message_content_queue.size();

        num_of_all_waiting_messages += queue_length;

        std::size_t total_content_size_for_this_queue = 0;
        for (std::size_t j = 0; j != queue_length; ++j)
        {
            total_content_size_for_this_queue +=
                q.message_content_queue[j]->size();
        }

        total_of_all_content_sizes += total_content_size_for_this_queue;

        num_of_all_waiting_clients += q.waiting_clients_queue.size();
    }
    
    res = params.set_integer("num_of_all_waiting_messages",
        static_cast<int>(num_of_all_waiting_messages));
    if (res != yami::core::ok)
    {
        return res;
    }

    res = params.set_long_long("total_of_all_content_sizes",
        static_cast<long long>(total_of_all_content_sizes));
    if (res != yami::core::ok)
    {
        return res;
    }

    res = params.set_integer("num_of_all_waiting_clients",
        static_cast<int>(num_of_all_waiting_clients));

    return res;
}

yami::core::result queue::fill_detailed_stats(yami::core::parameters & params)
{
    const std::size_t queues_count = routing_.size();

    yami::core::result res = yami::core::ok;

    if (queues_count != 0)
    {
        // create arrays and fill details

        int * num_of_waiting_messages;
        long long * total_content_sizes;
        int * num_of_waiting_clients;

        const char * queue_names_field_name = "queue_names";
        
        res = params.create_string_array(
            queue_names_field_name, queues_count);
        if (res != yami::core::ok)
        {
            return res;
        }

        res = params.create_integer_array("num_of_waiting_messages",
            queues_count, num_of_waiting_messages);
        if (res != yami::core::ok)
        {
            return res;
        }

        res = params.create_long_long_array("total_content_sizes",
            queues_count, total_content_sizes);
        if (res != yami::core::ok)
        {
            return res;
        }

        res = params.create_integer_array("num_of_waiting_clients",
            queues_count, num_of_waiting_clients);
        if (res != yami::core::ok)
        {
            return res;
        }

        routing_map::const_iterator it = routing_.begin();
        routing_map::const_iterator end = routing_.end();
        for (std::size_t i = 0; it != end; ++it, ++i)
        {
            const std::string & queue_name = it->first;
            const routing_queue & q = it->second;

            const std::size_t queue_length = q.message_content_queue.size();

            num_of_waiting_messages[i] = static_cast<int>(queue_length);

            std::size_t total_content_size_for_this_queue = 0;
            for (std::size_t j = 0; j != queue_length; ++j)
            {
                total_content_size_for_this_queue +=
                    q.message_content_queue[j]->size();
            }

            total_content_sizes[i] = total_content_size_for_this_queue;

            num_of_waiting_clients[i] = q.waiting_clients_queue.size();

            res = params.set_string_in_array(
                queue_names_field_name, i, queue_name.c_str());
            if (res != yami::core::ok)
            {
                return res;
            }
        }
    }

    return res;
}
