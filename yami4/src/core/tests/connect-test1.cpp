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

// this is a simple connection test with immediate close on the client side

char hint_for_server_connection_hook;
char hint_for_server_disconnection_hook;
char hint_for_client_disconnection_hook;

std::string client_target; // as seen by server
std::string server_target; // as seen by client

bool server_connection_hook_called = false;
bool server_disconnection_hook_called = false;
bool client_disconnection_hook_called = false;

void new_incoming_connection_hook(void * hint, const char * source,
    std::size_t /* index */, std::size_t /* sequence_number*/)
{
    assert(hint == &hint_for_server_connection_hook);

    client_target = source;

    server_connection_hook_called = true;
}

void server_disconnection_hook(void * hint, const char * source,
    yami::core::result reason)
{
    assert(hint == &hint_for_server_disconnection_hook);
    assert(source == client_target);
    assert(reason == yami::core::channel_closed);

    server_disconnection_hook_called = true;
}

void client_disconnection_hook(void * hint, const char * source,
    yami::core::result reason)
{
    assert(hint == &hint_for_client_disconnection_hook);
    assert(source == server_target);
    assert(reason == yami::core::channel_closed);

    client_disconnection_hook_called = true;
}

int main()
{
    // system-assigned port:
    const char * generic_server_target = "tcp://localhost:*";

    // create server agent

    yami::core::agent server_agent;
    yami::core::result res = server_agent.init(
        NULL, NULL, // no dispatch callback
        server_disconnection_hook, &hint_for_server_disconnection_hook);

    // there is nothing to do at the server side:
    res = server_agent.do_some_work(0); // no timeout
    assert(res == yami::core::timed_out);

    // create listener

    const char * resolved_server_target;
    res = server_agent.add_listener(generic_server_target,
        new_incoming_connection_hook, &hint_for_server_connection_hook,
        &resolved_server_target);
    assert(res == yami::core::ok);

    server_target = resolved_server_target;

    // the listener has been added and the selector was interrupted
    // so that the worker will report normal outcome
    res = server_agent.do_some_work(1000);
    assert(res == yami::core::ok);

    // create client agent

    yami::core::agent client_agent;
    res = client_agent.init(
        NULL, NULL, // no dispatch callback
        client_disconnection_hook, &hint_for_client_disconnection_hook);
    assert(res == yami::core::ok);

    // the connection does not exist yet

    yami::core::channel_descriptor cd;
    res = client_agent.is_open(server_target.c_str(), cd);
    assert(res == yami::core::no_such_name);

    // open new connection

    bool created_new_channel;
    res = client_agent.open(server_target.c_str(), cd, created_new_channel);
    assert(res == yami::core::ok && created_new_channel);

    // the new connection should be visible at the client side

    res = client_agent.is_open(server_target.c_str(), cd);
    assert(res == yami::core::ok);

    // the new connection should be visible at the server side

    assert(server_connection_hook_called == false);
    res = server_agent.do_some_work(1000);
    assert(res == yami::core::ok);
    assert(server_connection_hook_called); // connection accepted by server

    // cleanup

    assert(client_disconnection_hook_called == false);
    client_agent.clean();
    assert(client_disconnection_hook_called); // connection closed (client)

    // server needs to "do work" in order to discover closed connection

    assert(server_disconnection_hook_called == false);
    res = server_agent.do_some_work(1000);
    assert(res == yami::core::ok);
    assert(server_disconnection_hook_called); // connection closed (server)
}
