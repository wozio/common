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

// this is a simple messaging test with implicit (stored in the queue)
// close request on the client side
// unnecessary callbacks are ommitted
// otherwise the test is identical to message-test1

char hint_for_message_dispatch;
char hint_for_client_disconnection_hook;
char hint_for_server_disconnection_hook;

std::string server_target; // as seen by client

bool dispatch_called = false;
bool client_disconnection_hook_called = false;
bool server_disconnection_hook_called = false;

yami::core::parameters incoming_header;
yami::core::parameters incoming_body;

void client_disconnection_hook(void * hint, const char * name,
    yami::core::result reason)
{
    assert(hint == &hint_for_client_disconnection_hook);
    assert(name == server_target);
    assert(reason == yami::core::channel_closed);

    client_disconnection_hook_called = true;
}

void server_disconnection_hook(void * hint, const char * /* name */,
    yami::core::result reason)
{
    assert(hint == &hint_for_server_disconnection_hook);
    assert(reason == yami::core::channel_closed);

    server_disconnection_hook_called = true;
}

void incoming_message_dispatch_function(
    void * hint,
    const char * /* source */,
    const char * header_buffers[],
    std::size_t header_buffer_sizes[],
    std::size_t num_of_header_buffers,
    const char * body_buffers[],
    std::size_t body_buffer_sizes[],
    std::size_t num_of_body_buffers)
{
    assert(hint == &hint_for_message_dispatch);

    yami::core::result res = incoming_header.deserialize(
        header_buffers, header_buffer_sizes, num_of_header_buffers);
    assert(res == yami::core::ok);

    res = incoming_body.deserialize(
        body_buffers, body_buffer_sizes, num_of_body_buffers);
    assert(res == yami::core::ok);

    dispatch_called = true;
}

int main()
{
    // system-assigned port:
    const char * generic_server_target = "tcp://localhost:*";

    // create server agent

    yami::core::agent server_agent;
    yami::core::result res = server_agent.init(
        incoming_message_dispatch_function, &hint_for_message_dispatch,
        server_disconnection_hook, &hint_for_server_disconnection_hook);

    // there is nothing to do at the server side for the moment:
    res = server_agent.do_some_work(0); // no timeout
    assert(res == yami::core::timed_out);

    // create listener

    const char * resolved_server_target;
    res = server_agent.add_listener(generic_server_target,
        NULL, NULL, &resolved_server_target);
    assert(res == yami::core::ok);

    server_target = resolved_server_target;

    // the listener has been added and the selector was interrupted
    // so that the worker will report normal outcome
    res = server_agent.do_some_work(1000);
    assert(res == yami::core::ok);

    // create client agent

    yami::core::agent client_agent;
    res = client_agent.init(NULL, NULL,
        client_disconnection_hook, &hint_for_client_disconnection_hook);
    assert(res == yami::core::ok);

    // open new connection

    yami::core::channel_descriptor cd;
    bool created_new_channel;
    res = client_agent.open(server_target.c_str(), cd, created_new_channel);
    assert(res == yami::core::ok && created_new_channel);

    // do the work to accept the connection at the server side

    res = server_agent.do_some_work(1000);
    assert(res == yami::core::ok);

    // prepare the message header and body

    yami::core::parameters header; // empty
    yami::core::parameters body;

    res = body.set_string("greetings", "Hello YAMI4");
    assert(res == yami::core::ok);

    // post the message

    res = client_agent.post(cd, header, body);
    assert(res == yami::core::ok);

    // post also the close request (it will be queued)

    res = client_agent.close(cd);
    assert(res == yami::core::ok);

    // do the work on the client side to actually push the frame

    res = client_agent.do_some_work(0);
    assert(res == yami::core::ok);

    // do the work on the server side to receive the message

    assert(dispatch_called == false);
    while (dispatch_called == false)
    {
        res = server_agent.do_some_work(1000);
        assert(res == yami::core::ok);
    }
    // message has been seen by server-side code

    // verify that the data arrived properly

    assert(incoming_header.size() == 0); // empty header
    assert(incoming_body.size() == 1);
    const char * value;
    std::size_t value_length;
    res = incoming_body.get_string("greetings", value, value_length);
    assert(res == yami::core::ok);
    assert(std::string(value, value_length) == "Hello YAMI4");

    // no explicit close on the client side, "doing work" is necessary
    // to actually process the pending close request
    // (the close request will be taken from the queue
    // and the channel will be closed)

    assert(client_disconnection_hook_called == false);
    res = client_agent.do_some_work(0);
    assert(res == yami::core::ok);
    assert(client_disconnection_hook_called);

    // server needs to "do work" in order to discover closed connection

    assert(server_disconnection_hook_called == false);
    res = server_agent.do_some_work(1000);
    assert(res == yami::core::ok);
    assert(server_disconnection_hook_called); // connection closed (server)
}
