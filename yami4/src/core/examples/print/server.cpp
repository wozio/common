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

#include <cassert>
#include <cstdlib>
#include <iostream>
#include <string>

using namespace yami::core;

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
        body_buffers,
        body_buffer_sizes,
        num_of_body_buffers);
    assert(res == ok);

    // extract the content field
    // and print it on standard output

    const char * line_value;
    std::size_t line_length;

    res = body.get_string(
        "content", line_value, line_length);
    assert(res == ok);

    // the line content does not include the terminating zero,
    // so the proper string value needs to be rebuilt
    // for later use

    const std::string line(line_value, line_length);

    std::cout << line << std::endl;
}

void run_server(const char * server_address)
{
    agent server_agent;

    // create the server agent
    // and activate a listener on the given address

    result res = server_agent.init(
        &dispatch_function, NULL, NULL, NULL);
    assert(res == ok);

    const char * resolved_address;
    res = server_agent.add_listener(
        server_address, NULL, NULL, &resolved_address);
    assert(res == ok);

    std::cout << "The server is listening on "
        << resolved_address << std::endl;

    // run the event loop
    // normally this loop would be executed by a separate
    // thread, but this server has nothing else to do,
    // so it is possible to process the I/O events here:

    std::cout << "waiting for messages..." << std::endl;

    const std::size_t time_out = 1000; // 1 second

    while (true)
    {
        res = server_agent.do_some_work(time_out);
        assert(res == ok || res == timed_out);
    }

    // never reaches here
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

    run_server(server_address);
}
