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

#include "../agent.h"
#include "../channel_descriptor.h"
#include "../parameters.h"
#include <cassert>
#include <string>

// this is a test for message with the same priorities
// one bigger message is posted then small messsage with the same priority
// is posted after that
// both client and server parts expect the long message to be
// pushed/arrived first

std::string short_content;
std::string long_content;

char hint_for_short_message_progress;
char hint_for_long_message_progress;

std::string server_target; // as seen by client

bool short_message_pushed = false;
bool long_message_pushed = false;
bool short_message_received = false;
bool long_message_received = false;

void message_progress_function(
    void * hint,
    std::size_t sent_bytes,
    std::size_t total_byte_count)
{
    if (hint == &hint_for_short_message_progress)
    {
        if (sent_bytes == total_byte_count && sent_bytes > 0)
        {
            // make sure that the long message is pushed first
            assert(long_message_pushed);
            short_message_pushed = true;
        }
    }
    else if (hint == &hint_for_long_message_progress)
    {
        if (sent_bytes == total_byte_count && sent_bytes > 0)
        {
            // make sure that the long message is pushed first
            assert(short_message_pushed == false);
            long_message_pushed = true;
        }
    }
    else
    {
        assert(false);
    }
}

void incoming_message_dispatch_function(
    void * /* hint */,
    const char * /* source */,
    const char * header_buffers[],
    std::size_t header_buffer_sizes[],
    std::size_t num_of_header_buffers,
    const char * body_buffers[],
    std::size_t body_buffer_sizes[],
    std::size_t num_of_body_buffers)
{
    yami::core::result res;

    yami::core::parameters header;
    yami::core::parameters body;

    res = header.deserialize(
        header_buffers, header_buffer_sizes, num_of_header_buffers);
    assert(res == yami::core::ok);

    res = body.deserialize(
        body_buffers, body_buffer_sizes, num_of_body_buffers);
    assert(res == yami::core::ok);

    assert(header.size() == 1);
    assert(body.size() == 1);

    const char * message_type;
    std::size_t message_type_length;
    res = header.get_string("message-type",
        message_type, message_type_length);
    assert(res == yami::core::ok);

    if (std::string(message_type, message_type_length) == "short")
    {
        // this is a short message

        // the long one was already received
        assert(long_message_received);

        const char * content;
        std::size_t content_length;
        res = body.get_string("content", content, content_length);
        assert(res == yami::core::ok);
        assert(std::string(content, content_length) == short_content);

        short_message_received = true;
    }
    else if (std::string(message_type, message_type_length) == "long")
    {
        // this is a long message

        // the short one was not yet received
        assert(short_message_received == false);

        const char * content;
        std::size_t content_length;
        res = body.get_string("content", content, content_length);
        assert(res == yami::core::ok);
        assert(std::string(content, content_length) == long_content);

        long_message_received = true;
    }
    else
    {
        // nothing else should ever arrive here
        assert(false);
    }
}

int main()
{
    // prepare short and long content
    short_content = "abc";
    for (int i = 0; i != 10000; ++i)
    {
        long_content += "abc";
    }

    // system-assigned port:
    const char * generic_server_target = "tcp://localhost:*";

    // create server agent

    yami::core::agent server_agent;
    yami::core::result res = server_agent.init(
        incoming_message_dispatch_function, NULL);

    // create listener

    const char * resolved_server_target;
    res = server_agent.add_listener(generic_server_target,
        NULL, NULL, &resolved_server_target);
    assert(res == yami::core::ok);

    server_target = resolved_server_target;

    // create client agent

    yami::core::agent client_agent;
    res = client_agent.init(NULL, NULL);
    assert(res == yami::core::ok);

    // open new connection

    yami::core::channel_descriptor cd;
    bool created_new_channel;
    res = client_agent.open(server_target.c_str(), cd, created_new_channel);
    assert(res == yami::core::ok && created_new_channel);

    // prepare the long message header and body
    // and post it first

    yami::core::parameters long_header;
    yami::core::parameters long_body;

    res = long_header.set_string("message-type", "long");
    assert(res == yami::core::ok);

    res = long_body.set_string("content", long_content.c_str());
    assert(res == yami::core::ok);

    res = client_agent.post(cd, long_header, long_body,
        0, // whatever priority
        message_progress_function, &hint_for_long_message_progress);
    assert(res == yami::core::ok);

    // prepare the short message header and body
    // and post it after the first one

    yami::core::parameters short_header;
    yami::core::parameters short_body;

    res = short_header.set_string("message-type", "short");
    assert(res == yami::core::ok);

    res = short_body.set_string("content", short_content.c_str());
    assert(res == yami::core::ok);

    res = client_agent.post(cd, short_header, short_body,
        0, // the same priority
        message_progress_function, &hint_for_short_message_progress);
    assert(res == yami::core::ok);

    // do the work on both client and server side so that they can
    // pass the messages properly
    // (the pending work at the server side related to listener registration
    // or accepting new connection is also performed in this loop)

    while (short_message_received == false ||
        long_message_received == false)
    {
        res = client_agent.do_some_work(1);
        assert(res == yami::core::ok || res == yami::core::timed_out);

        res = server_agent.do_some_work(1);
        assert(res == yami::core::ok || res == yami::core::timed_out);
    }

    assert(short_message_pushed);
    assert(long_message_pushed);
}
