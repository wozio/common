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

#include <flag.h>
#include <start_thread.h>

#include <cassert>
#include <cstdlib>
#include <iostream>
#include <string>

using namespace yami::core;

examples::flag worker_can_stop;
examples::flag worker_stopped;

// worker thread function that is used to process I/O events
// for the client agent
// this thread should stop working before the agent is closed
extern "C" void worker_thread_function(void * p)
{
    agent * client_agent = static_cast<agent *>(p);

    const std::size_t time_out = 1000; // 1 second

    while (worker_can_stop.get_value() == false)
    {
        result res = client_agent->do_some_work(time_out);
        assert(res == ok || res == timed_out);
    }

    worker_stopped.notify();
}

void run_client(const char * server_address)
{
    agent client_agent;

    // create the client agent
    // and open a channel to the given server

    result res = client_agent.init(NULL, NULL, NULL, NULL);
    assert(res == ok);

    res = client_agent.open(server_address);
    assert(res == ok);

    // start the worker thread that will process I/O events

    examples::start_thread(
        worker_thread_function, &client_agent);

    // read lines of text from standard input
    // and post each one for transmission

    std::string input_line;

    while (std::getline(std::cin, input_line))
    {
        parameters empty_header;
        parameters body;

        // the "content" field name is arbitrary,
        // but needs to be recognized at the server side

        res = body.set_string(
            "content", input_line.c_str());
        assert(res == ok);

        res = client_agent.post(
            server_address, empty_header, body);
        assert(res == ok);
    }

    // stop the worker thread
    // and clean up the client resources

    worker_can_stop.notify();
    worker_stopped.wait();

    // the cleanup is automatic in the agent's destructor
}

int main(int argc, char * argv[])
{
    if (argc != 2)
    {
        std::cout
            << "expecting one parameter: server destination\n";
        return EXIT_FAILURE;
    }

    const char * server_address = argv[1];

    run_client(server_address);
}
