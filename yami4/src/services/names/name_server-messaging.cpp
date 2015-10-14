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

#include "name_server-messaging.h"
#include "name_server-storage.h"
#include "../common/log.h"
#include "../common/utils.h"

#include <yami4-core/agent.h>
#include <yami4-core/parameters.h>
#include <yami4-core/parameter_entry.h>

#include <boost/thread/thread.hpp>

#include <cstdio>

using namespace name_server;

namespace // unnamed
{

yami::core::agent agent_;

yami::core::parameters empty_parameters_;

const char * object_field_name = "object";
const char * location_field_name = "location";

boost::thread * worker_thread_;

void reply(const char * source, long long incoming_msg_id,
    const yami::core::parameters & reply_body)
{
    yami::core::parameters reply_header;

    yami::core::result res = reply_header.set_string("type", "reply");
    if (res != yami::core::ok)
    {
        return;
    }

    res = reply_header.set_long_long("message_id", incoming_msg_id);
    if (res != yami::core::ok)
    {
        return;
    }

    res = agent_.post(source, reply_header, reply_body);

    if (res != yami::core::ok)
    {
        logger::put(logger::messages, "    error: " +
            utils::result_to_string(res));
    }
}

void reject(const char * source, long long incoming_msg_id,
    const std::string & reason)
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

void process_single_bind(const std::string & object_name, const std::string & location)
{
    name_server::store(object_name, location);

    logger::put(logger::messages,
        std::string("    ") + object_name + " bound to " + location);
}

yami::core::result process_bind(const char * source, long long incoming_msg_id,
    const yami::core::parameters & msg_body)
{
    yami::core::result res;

    yami::core::parameter_entry eobj;
    res = msg_body.find(object_field_name, eobj);
    bool object_found = res == yami::core::ok;
    yami::core::parameter_entry eloc;
    res = msg_body.find(location_field_name, eloc);
    bool location_found = res == yami::core::ok;
    if (object_found && location_found)
    {
        yami::core::parameter_type object_entry_type = eobj.type();
        if (object_entry_type == yami::core::string)
        {
            // single call

            logger::put(logger::messages,
                std::string("single bind from ") + source);

            const char * objc;
            std::size_t objlen;

            res = eobj.get_string(objc, objlen);
            if (res == yami::core::ok)
            {
                const char * locc;
                std::size_t loclen;

                res = eloc.get_string(locc, loclen);
                if (res == yami::core::ok)
                {
                    const std::string obj(objc, objlen);
                    const std::string loc(locc, loclen);

                    process_single_bind(obj, loc);

                    reply(source, incoming_msg_id, empty_parameters_);
                }
            }
        }
        else if (object_entry_type == yami::core::string_array)
        {
            // array call

            logger::put(logger::messages,
                std::string("array bind from ") + source);

            std::size_t length;
            res = eobj.get_string_array_length(length);
            for (std::size_t i = 0; (res == yami::core::ok) && (i != length); ++i)
            {
                const char * objc;
                std::size_t objlen;

                res = eobj.get_string_in_array(i, objc, objlen);
                if (res == yami::core::ok)
                {
                    const char * locc;
                    std::size_t loclen;

                    res = eloc.get_string_in_array(i, locc, loclen);
                    if (res == yami::core::ok)
                    {
                        const std::string obj(objc, objlen);
                        const std::string loc(locc, loclen);
                        
                        process_single_bind(obj, loc);
                    }
                }
            }

            reply(source, incoming_msg_id, empty_parameters_);
        }
    }

    if (res == yami::core::unexpected_value)
    {
        reject(source, incoming_msg_id, "Unknown message");
    }
    else if (res != yami::core::ok)
    {
        reject(source, incoming_msg_id, utils::result_to_string(res));
    }

    return res;
}

yami::core::result process_resolve(const char * source, long long incoming_msg_id,
    const yami::core::parameters & msg_body)
{
    yami::core::result res;

    yami::core::parameters reply_body;

    yami::core::parameter_entry eobj;
    res = msg_body.find(object_field_name, eobj);
    bool object_found = res == yami::core::ok;
    if (object_found)
    {
        yami::core::parameter_type object_entry_type = eobj.type();
        if (object_entry_type == yami::core::string)
        {
            // single call

            logger::put(logger::messages,
                std::string("single resolve from ") + source);

            const char * objc;
            std::size_t objlen;

            res = eobj.get_string(objc, objlen);
            if (res == yami::core::ok)
            {
                const std::string obj(objc, objlen);

                const std::string & location = name_server::resolve(obj);
                
                reply_body.set_string(location_field_name, location.c_str());

                reply(source, incoming_msg_id, reply_body);
            }
        }
        else if (object_entry_type == yami::core::string_array)
        {
            // array call

            logger::put(logger::messages,
                std::string("array resolve from ") + source);

            std::size_t length;
            res = eobj.get_string_array_length(length);
            if (res == yami::core::ok)
            {
                res = reply_body.create_string_array(location_field_name, length);
                for (std::size_t i = 0; (res == yami::core::ok) && (i != length); ++i)
                {
                    const char * objc;
                    std::size_t objlen;

                    res = eobj.get_string_in_array(i, objc, objlen);
                    if (res == yami::core::ok)
                    {
                        const std::string obj(objc, objlen);

                        const std::string & location = name_server::resolve(obj);
                
                        res = reply_body.set_string_in_array(
                            location_field_name, i, location.c_str());
                    }
                }
            }

            if (res == yami::core::ok)
            {
                reply(source, incoming_msg_id, reply_body);
            }
        }
    }

    if (res == yami::core::unexpected_value)
    {
        reject(source, incoming_msg_id, "Unknown message");
    }
    else if (res != yami::core::ok)
    {
        reject(source, incoming_msg_id, utils::result_to_string(res));
    }

    return res;
}

yami::core::result process_list(const char * source, long long incoming_msg_id)
{
    logger::put(logger::messages, std::string("list from ") + source);

    const name_server::name_map & mapping = name_server::name_mapping();

    yami::core::parameters reply_body;

    yami::core::result res = reply_body.create_string_array(
        object_field_name, mapping.size());
    if (res == yami::core::ok)
    {
        res = reply_body.create_string_array(
            location_field_name, mapping.size());
        
        name_map::const_iterator it = mapping.begin();
        name_map::const_iterator end = mapping.end();
        std::size_t i = 0;
        for ( ; (res == yami::core::ok) && (it != end); ++it, ++i)
        {
            const std::string & object_name = it->first;
            const std::string & location = it->second;

            res = reply_body.set_string_in_array(
                object_field_name, i, object_name.c_str());
            if (res == yami::core::ok)
            {
                res = reply_body.set_string_in_array(
                    location_field_name, i, location.c_str());
            }
        }
    }

    if (res == yami::core::ok)
    {
        reply(source, incoming_msg_id, reply_body);
    }

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

    yami::core::parameters body;
    res = body.deserialize(
        body_buffers, body_buffer_sizes, num_of_body_buffers);
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

        if (message_name == "bind")
        {
            res = process_bind(source, incoming_msg_id, body);
        }
        else if (message_name == "resolve")
        {
            res = process_resolve(source, incoming_msg_id, body);
        }
        else if (message_name == "list")
        {
            res = process_list(source, incoming_msg_id);
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

void name_server::init_messaging(const std::string & listener)
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

    logger::put(logger::main, "name server started");
}
