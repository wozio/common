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

#include "broker-messaging.h"
#include "broker-ids.h"
#include "broker-routing.h"
#include "../common/log.h"
#include "../common/utils.h"

#include <yami4-core/agent.h>
#include <yami4-core/parameters.h>
#include <yami4-core/raw_buffer_data_source.h>

#include <boost/thread/thread.hpp>

using namespace broker;

namespace // unnamed
{

yami::core::agent agent_;

yami::core::parameters empty_parameters_;

long long incoming_counter_ = 0;
long long outgoing_counter_ = 0;

overflow_policy subscription_overflow_policy_;

bool warmed_ = false;

boost::thread * worker_thread_;

void confirm(const char * source, long long incoming_msg_id, logger::module m)
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

    res = agent_.post(source, confirmation_header, empty_parameters_);

    if (res == yami::core::ok)
    {
        logger::put(m, std::string("    confirmed to ") + source);
    }
}

void reject(const char * source, long long incoming_msg_id, const std::string & reason)
{
    logger::put(logger::messages,
        std::string("    rejected message from ") + source);

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

bool forward_to_single_subscriber(
    const std::string & target_object, const std::string & target_location,
    const std::string & tags,
    const yami::core::serializable & body,
    yami::core::message_progress_function progress_handler,
    void * progress_hint)
{
    logger::put(logger::messages, "    routing to " + target_location);

    ++outgoing_counter_;

    yami::core::channel_descriptor cd;
    yami::core::result res;

    yami::core::parameters update_header;

    res = update_header.set_string("type", "message");

    if (target_object == "*")
    {
        // this is a forwarding subscription
        // use original object name (list of targets)
        // and "publish" as the message name

        update_header.set_string("object_name", tags.c_str());
        update_header.set_string("message_name", "publish");

        // also, make sure that the forwarding channel exists

        bool dummy_created_new_channel;
        res = agent_.open(target_location.c_str(), cd, dummy_created_new_channel);
        if (res != yami::core::ok)
        {
            return false;
        }
    }
    else
    {
        // this is a regular subscription

        update_header.set_string("object_name", target_object.c_str());

        const std::string & message_name = "update " + tags;
        update_header.set_string("message_name", message_name.c_str());

        res = agent_.is_open(target_location.c_str(), cd);
        if (res != yami::core::ok)
        {
            return false;
        }
    }

    res = update_header.set_long_long("message_id", broker::next_message_id());
    if (res != yami::core::ok)
    {
        return false;
    }

    res = agent_.post(cd, update_header, body, 0, progress_handler, progress_hint);

    return res == yami::core::ok;
}

yami::core::result process_publish(const char * source, long long incoming_msg_id,
    const std::string & tags, const yami::core::serializable & body,
    bool requires_confirmation)
{
    logger::put(logger::messages, "processing " + tags + " from " + source);

    ++incoming_counter_;

    bool overflow;

    broker::iterate_matching_subscriptions(
        tags, body, &forward_to_single_subscriber, overflow);

    if (requires_confirmation)
    {
        if (overflow &&
            (subscription_overflow_policy_ == broker::reject_message))
        {
            reject(source, incoming_msg_id, "Message queue overflow");
        }
        else
        {
            confirm(source, incoming_msg_id, logger::messages);
        }
    }

    return yami::core::ok;
}

yami::core::result process_subscribe(
    const char * source, long long incoming_msg_id,
    const std::string & tags,
    const yami::core::parameters & params)
{
    std::string destination_object;
    
    yami::core::result res =
        utils::get_string(params, "destination_object", destination_object);
    if (res != yami::core::ok)
    {
        return res;
    }

    broker::subscribe(tags, destination_object, source);

    confirm(source, incoming_msg_id, logger::subscriptions);

    return res;
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

    res = params.set_long_long("total_incoming", incoming_counter_);
    if (res != yami::core::ok)
    {
        return res;
    }

    res = params.set_long_long("total_outgoing", outgoing_counter_);
    if (res != yami::core::ok)
    {
        return res;
    }

    if (include_details)
    {
        res = broker::fill_detailed_stats(params);
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

    bool requires_confirmation = false;

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

        if (message_name == "publish")
        {
            if (warmed_)
            {
                // treat object name as a list of tags
                // this message does not require confirmation

                yami::core::raw_buffer_data_source raw_body_source(
                    body_buffers, body_buffer_sizes, num_of_body_buffers);

                res = process_publish(source, incoming_msg_id,
                    object_name, raw_body_source, requires_confirmation);
            }
        }
        else if (message_name == "publish_confirm")
        {
            if (warmed_)
            {
                // treat object name as a list of tags
                // this message requires confirmation

                requires_confirmation = true;

                yami::core::raw_buffer_data_source raw_body_source(
                    body_buffers, body_buffer_sizes, num_of_body_buffers);

                res = process_publish(source, incoming_msg_id,
                    object_name, raw_body_source, requires_confirmation);
            }
        }
        else if (message_name == "subscribe")
        {
            // treat object name as a list of tags

            requires_confirmation = true;

            yami::core::parameters params;
            res = params.deserialize(
                body_buffers, body_buffer_sizes, num_of_body_buffers);
            if (res == yami::core::ok)
            {
                res = process_subscribe(
                    source, incoming_msg_id, object_name, params);
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

            if (requires_confirmation)
            {
                reject(source, incoming_msg_id, error_msg);
            }
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

void broker::init_messaging(const std::string & listener,
    overflow_policy subscription_overflow_policy)
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

    subscription_overflow_policy_ = subscription_overflow_policy;

    worker_thread_ = new boost::thread(worker);

    logger::put(logger::main, "started in the warm-up mode");
}

void broker::allow_incoming()
{
    warmed_ = true;

    logger::put(logger::main, "fully initialized");
}
