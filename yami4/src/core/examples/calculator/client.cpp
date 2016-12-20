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

#include <cassert>
#include <cstdlib>
#include <iostream>

using namespace yami::core;

// these variables are used
//  to pass back answers from the server
bool reply_arrived = false;
int sum;
int difference;
int product;
bool ratio_defined;
int ratio;

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
        body_buffers,
        body_buffer_sizes,
        num_of_body_buffers);
    assert(res == ok);

    // extract the values from server

    res = body.get_integer("sum", sum);
    assert(res == ok);

    res = body.get_integer("difference", difference);
    assert(res == ok);

    res = body.get_integer("product", product);
    assert(res == ok);

    res = body.get_integer("ratio", ratio);
    assert(res == ok || res == no_such_name);

    ratio_defined = (res == ok);

    reply_arrived = true;
}

void run_client(const char * server_address, int a, int b)
{
    agent client_agent;

    // create the client agent
    // and open a channel to the given server

    result res = client_agent.init(
        &dispatch_function, NULL, NULL, NULL);
    assert(res == ok);

    res = client_agent.open(server_address);
    assert(res == ok);

    // prepare the parameters for sending to the server

    // the message header is not used here,
    // all the information is passed as message body

    parameters empty_header;
    parameters message_body;

    res = message_body.set_integer("a", a);
    assert(res == ok);

    res = message_body.set_integer("b", b);
    assert(res == ok);

    res = client_agent.post(
        server_address, empty_header, message_body);
    assert(res == ok);

    // run the even loop until the reply is received
    // note: the event loop can be run in a separate thread,
    // but in this case the program has nothing else to do
    // until the reply arrives, so it makes sense to merge
    // the even loop processing in the main thread

    // the timeout is arbitrary
    const std::size_t time_out = 1000; // 1 second

    bool communication_timed_out = false;
    while (communication_timed_out == false &&
        reply_arrived == false)
    {
        result res = client_agent.do_some_work(time_out);
        assert(res == ok || res == timed_out);
        if (res == timed_out)
        {
            communication_timed_out = true;
        }
    }

    if (reply_arrived)
    {
        std::cout << "sum        = " << sum << '\n';
        std::cout << "difference = " << difference << '\n';
        std::cout << "product    = " << product << '\n';

        std::cout << "ratio      = ";
        if (ratio_defined)
        {
            std::cout << ratio;
        }
        else
        {
            std::cout << "<undefined>";
        }

        std::cout << std::endl;
    }
    else
    {
        std::cout
            << "there was no reply, communication timed out"
            << std::endl;
    }

    // the cleanup is automatic in the agent's destructor
}

int main(int argc, char * argv[])
{
    if (argc != 4)
    {
        std::cout << "expecting three parameters: "
            "server destination and two integers\n";
        return EXIT_FAILURE;
    }

    const char * server_address = argv[1];

    int a;
    int b;
    if (examples::string_to_int(argv[2], a) == false ||
        examples::string_to_int(argv[3], b) == false)
    {
        std::cout
            << "cannot parse the second or third parameter"
            << std::endl;
    }
    else
    {
        run_client(server_address, a, b);
    }
}
