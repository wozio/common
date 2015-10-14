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
#include <yami4-core/option_names.h>
#include <yami4-core/parameters.h>

#include <cassert>
#include <cstdlib>
#include <iostream>

using namespace yami::core;

void dispatch_function(
    void * hint,
    const char * source,
    const char * /* header_buffers */ [],
    std::size_t /* header_buffer_sizes */ [],
    std::size_t /* num_of_header_buffers */,
    const char * body_buffers[],
    std::size_t body_buffer_sizes[],
    std::size_t num_of_body_buffers)
{
    agent * server_agent = static_cast<agent *>(hint);

    // ignore the header, but deserialize the message body

    parameters body;

    result res = body.deserialize(
        body_buffers,
        body_buffer_sizes,
        num_of_body_buffers);
    assert(res == ok);

    // extract the parameters for calculations

    int a;
    int b;

    res = body.get_integer("a", a);
    assert(res == ok);

    res = body.get_integer("b", b);
    assert(res == ok);

    // prepare the answer with results of four calculations
    // the message header is not used here

    parameters empty_header;
    parameters reply_body;

    res = reply_body.set_integer("sum", a + b);
    assert(res == ok);

    res = reply_body.set_integer("difference", a - b);
    assert(res == ok);

    res = reply_body.set_integer("product", a * b);
    assert(res == ok);

    // if the ration cannot be computed,
    // it is not included in the response
    // the client will interpret that fact properly
    if (b != 0)
    {
        res = reply_body.set_integer("ratio", a / b);
        assert(res == ok);
    }

    res = server_agent->post(
        source, empty_header, reply_body);
    assert(res == ok);

    std::cout << "got message with parameters "
        << a << " and " << b
        << ", response has been sent back"
        << std::endl;
}

void run_server(const char * server_address)
{
    agent server_agent;

    // create the server agent
    // and activate a listener on the given address

    parameters options;
    options.set_string(option_names::ssl_certificate_file, "ssl_server.crt");
    options.set_string(option_names::ssl_private_key_file, "ssl_server.key");

    result res = server_agent.init(
        options,
        &dispatch_function, &server_agent, NULL, NULL);
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
        assert(res == ok || res == timed_out || res == io_error);

        if (res == io_error)
        {
            std::cout << "I/O error" << std::endl;
        }
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
