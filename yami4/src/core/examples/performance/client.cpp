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

#include <yami4-core/agent.h>
#include <yami4-core/parameters.h>

#include "../common_utils/string_to_int.h"
#include <current_time.h>

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <iostream>

using namespace yami::core;

bool reply_arrived;

// the dispatch function is used to process the message
// that comes back from the server
// - in this case it is always a reply to the client message
// in more complex system the message headers should be used
// to match related messages
void dispatch_function(
    void * /* hint */,
    const char * /* source */,
    const char * /* header_buffers */ [],
    std::size_t /* header_buffer_sizes */ [],
    std::size_t /* num_of_header_buffers */,
    const char * body_buffers[],
    std::size_t body_buffer_sizes[],
    std::size_t num_of_body_buffers)
{
    // ignore the header, but deserialize the message body

    parameters body;

    result res = body.deserialize(
        body_buffers, body_buffer_sizes, num_of_body_buffers);
    assert(res == ok);

    // extract the value from server

    const char * value;
    std::size_t value_length;

    res = body.get_string("answer", value, value_length);
    assert(res == ok);

    assert(std::strncmp(value, "pong", value_length) == 0);

    reply_arrived = true;
}

void ping_pong(agent & client_agent, channel_descriptor cd)
{
    // send a single message and wait for response

    parameters empty_header;
    parameters body;

    result res = body.set_string("question", "ping");
    assert(res == ok);

    res = client_agent.post(cd, empty_header, body);
    assert(res == ok);

    // process the event loop until the reply is received
    // note: normally the event loop would be processed
    // in a separate thread, but since this program
    // has nothing else to do while waiting for response,
    // the event processing can be performed in the main thread

    const std::size_t time_out = 1000; // 1 second

    reply_arrived = false;
    while (reply_arrived == false)
    {
        res = client_agent.do_some_work(time_out);
        assert(res == ok);
    }

    // at this point the reply has been already processed
    // and validated
}

void run_client(const char * server_address, int iterations)
{
    agent client_agent;

    // create the client agent and open a channel to the given server

    result res = client_agent.init(&dispatch_function, NULL, NULL, NULL);
    assert(res == ok);

    channel_descriptor cd;
    bool created_new;
    res = client_agent.open(server_address, cd, created_new);
    assert(res == ok);

    const long long start_time = examples::clock();

    for (int i = 0; i != iterations; ++i)
    {
        ping_pong(client_agent, cd);
    }

    const long long stop_time = examples::clock();
    const long long duration = stop_time - start_time;

    const double performance = static_cast<double>(iterations) /
        duration * 1000;

    std::cout << "performed " << iterations << " iterations in "
        << duration << "ms -> "
        << performance << " messages per second" << std::endl;

    // the cleanup is automatic in the agent's destructor
}

int main(int argc, char * argv[])
{
    if (argc != 3)
    {
        std::cout << "expecting two parameters: "
            "server destination and number of iterations\n";
        return EXIT_FAILURE;
    }

    const char * server_address = argv[1];

    int iterations;
    if (examples::string_to_int(argv[2], iterations) == false)
    {
        std::cout << "cannot parse the second parameter"
            << std::endl;
    }
    else
    {
        run_client(server_address, iterations);
    }
}
