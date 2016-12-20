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

#include "queue-messaging.h"
#include "queue-routing.h"
#include "../common/log.h"
#include "../common/utils.h"

#include <yami4-core/agent.h>
#include <yami4-core/parameters.h>
#include <yami4-core/raw_buffer_data_source.h>

#include <boost/thread/thread.hpp>

using namespace queue;

namespace // unnamed
{

yami::core::agent agent_;

yami::core::parameters empty_parameters_;

boost::thread * worker_thread_;

const char * const no_such_queue_error_ =
    "There is no such queue.";

const char * const no_data_error_ =
    "There is no such queue or there is no data available.";

bool send_reply(const char * target, long long message_id,
    const yami::core::serializable & body)
{
    yami::core::parameters reply_header;

    logger::put(logger::messages,
        std::string("    sending response to ") + target);

    yami::core::result res = reply_header.set_string("type", "reply");
    if (res != yami::core::ok)
    {
        return false;
    }

    res = reply_header.set_long_long("message_id", message_id);
    if (res != yami::core::ok)
    {
        return false;
    }

    res = agent_.post(target, reply_header, body);

    if (res == yami::core::ok)
    {
        return true;
    }
    else
    {
        logger::put(logger::messages,
            std::string("    send error for ") + target);

        // intentionally swallow errors here:
        // response failures are not handled in any meaningful way,
        // but the put request that triggered the failue can be repeated
        // at the highest level, so that other clients
        // can have their chance to receive the same message
        // (get requests are not repeated as their failures cannot be
        // distinguished from successful response followed by client crash)
        
        // Note:
        // this mechanism does not take into account the possibility
        // of the response to fail in the middle of transmission
        // in such a case the response (and therefore the queued message)
        // will be silently lost;
        // in any way, this situation cannot be distinguished from
        // successful transmission followed by client crash
        
        return false;
    }
}

void confirm(const char * source, long long incoming_msg_id)
{
    yami::core::parameters confirmation_header;

    yami::core::result res = confirmation_header.set_string("type", "reply");
    if (res != yami::core::ok)
    {
        return;
    }

    res = confirmation_header.set_long_long("message_id", incoming_msg_id);
    if (res != yami::core::ok)
    {
        return;
    }

    (void)agent_.post(source, confirmation_header, empty_parameters_);
}

void reject(const char * source, long long incoming_msg_id, const std::string & reason)
{
    logger::put(logger::messages,
        std::string("    rejecting message from ") + source + ", " + reason);

    yami::core::parameters reject_header;

    yami::core::result res = reject_header.set_string("type", "exception");
    if (res != yami::core::ok)
    {
        return;
    }

    res = reject_header.set_long_long("message_id", incoming_msg_id);
    if (res != yami::core::ok)
    {
        return;
    }

    res = reject_header.set_string("reason", reason.c_str());
    if (res != yami::core::ok)
    {
        return;
    }

    (void)agent_.post(source, reject_header, empty_parameters_);
}

void process_put(const char * source, long long incoming_msg_id,
    const std::string & queue_name, const yami::core::serializable & body)
{
    bool first_try = true;
    while (true)
    {
        if (first_try)
        {
            logger::put(logger::messages,
                std::string("put requested for queue ") + queue_name);
        }
        else
        {
            logger::put(logger::messages,
                std::string("put repeated for queue ") + queue_name);
        }

        bool no_such_queue;
        bool completely_processed =
            queue::put_to_queue(queue_name, body, &send_reply, no_such_queue);
        if (completely_processed)
        {
            if (no_such_queue)
            {
                reject(source, incoming_msg_id, no_such_queue_error_);
            }
            else
            {
                confirm(source, incoming_msg_id);
            }

            break;
        }

        first_try = false;
    }
}

yami::core::result process_stats(
    const char * source, long long incoming_msg_id, bool include_details)
{
    yami::core::parameters stats_header;
    yami::core::parameters params;

    yami::core::result res = stats_header.set_string("type", "reply");
    if (res != yami::core::ok)
    {
        return res;
    }

    res = stats_header.set_long_long("message_id", incoming_msg_id);
    if (res != yami::core::ok)
    {
        return res;
    }
    
    res = queue::fill_simple_stats(params);
    if (res != yami::core::ok)
    {
        return res;
    }

    if (include_details)
    {
        res = queue::fill_detailed_stats(params);
        if (res != yami::core::ok)
        {
            return res;
        }
    }

    res = agent_.post(source, stats_header, params);

    return res;
}

extern "C" void incoming_handler(
    void * hint,
    const char * source,
    const char * header_buffers[],
    std::size_t header_buffer_sizes[],
    std::size_t num_of_header_buffers,
    const char * body_buffers[],
    std::size_t body_buffer_sizes[],
    std::size_t num_of_body_buffers)
{
    (void)hint;

    yami::core::parameters header;
    yami::core::result res = header.deserialize(
        header_buffers, header_buffer_sizes, num_of_header_buffers);
    if (res != yami::core::ok)
    {
        // ignore malformed messages

        return;
    }

    std::string msg_type;
    res = utils::get_string(header, "type", msg_type);
    if (res != yami::core::ok)
    {
        return;
    }

    if (msg_type == "message")
    {
        std::string object_name;
        res = utils::get_string(header, "object_name", object_name);
        if (res != yami::core::ok)
        {
            return;
        }

        std::string message_name;
        res = utils::get_string(header, "message_name", message_name);
        if (res != yami::core::ok)
        {
            return;
        }

        long long incoming_msg_id;
        res = header.get_long_long("message_id", incoming_msg_id);
        if (res != yami::core::ok)
        {
            return;
        }

        if (message_name == "put")
        {
            yami::core::raw_buffer_data_source raw_body_source(
                body_buffers, body_buffer_sizes, num_of_body_buffers);

            process_put(
                source, incoming_msg_id, object_name, raw_body_source);
        }
        else if (message_name == "get")
        {
            logger::put(logger::messages,
                std::string("get requested for queue ") + object_name);

            bool success = queue::get_from_queue(
                source, object_name, incoming_msg_id, &send_reply, true);

            if (success == false)
            {
                reject(source, incoming_msg_id, no_such_queue_error_);
            }
        }
        else if (message_name == "try-get")
        {
            logger::put(logger::messages,
                std::string("try-get requested for queue ") + object_name);
            
            bool success = queue::get_from_queue(
                source, object_name, incoming_msg_id, &send_reply, false);

            if (success == false)
            {
                reject(source, incoming_msg_id, no_data_error_);
            }
        }
        else if (object_name == "stats")
        {
            if (message_name == "get")
            {
                res = process_stats(source, incoming_msg_id, false);
            }
            else if (message_name == "get-details")
            {
                res = process_stats(source, incoming_msg_id, true);
            }
        }

        if (res != yami::core::ok)
        {
            const std::string & error_msg = utils::result_to_string(res);

            logger::put(logger::messages, "    error: " + error_msg);

            reject(source, incoming_msg_id, error_msg);
        }
    }
}

void worker()
{
    const std::size_t dummy_unit_timeout = 10 * 1000;

    while (true)
    {
        yami::core::result res = agent_.do_some_work(dummy_unit_timeout);
        if ((res == yami::core::ok) ||
            (res == yami::core::timed_out) ||
            (res == yami::core::io_error))
        {
            // I/O errors can be ignored here as they are normal
            // when sending data to failing targets
            // otherwise the exception from the worker thread
            // indicates something serious

            continue;
        }
        else
        {
            logger::put(logger::main, "error in worker: " +
                utils::result_to_string(res));

            break;
        }
    }
}

} // unnamed namespace

void queue::init_messaging(const std::string & listener)
{
    yami::core::result res = agent_.init(&incoming_handler, NULL);

    if (res != yami::core::ok)
    {
        logger::put(logger::main, "cannot initialize internal agent");

        return;
    }

    const char * resolved_target;

    res = agent_.add_listener(listener.c_str(), NULL, NULL, &resolved_target);
    if (res != yami::core::ok)
    {
        logger::put(logger::main, "cannot set up the listener");

        return;
    }

    logger::put(logger::main, std::string("listening on ") + resolved_target);

    worker_thread_ = new boost::thread(worker);

    logger::put(logger::main, "queue server started");
}
