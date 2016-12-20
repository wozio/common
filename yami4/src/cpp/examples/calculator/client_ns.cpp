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

#include <yami4-cpp/yami.h>

#include <cstdlib>
#include <iostream>

#include "../common_utils/string_to_int.h"

int main(int argc, char * argv[])
{
    if (argc != 4)
    {
        std::cout << "expecting three parameters: "
            "name server address and two integers\n";
        return EXIT_FAILURE;
    }

    const std::string name_server_address = argv[1];

    int a;
    int b;
    if (examples::string_to_int(argv[2], a) == false ||
        examples::string_to_int(argv[3], b) == false)
    {
        std::cout
            << "cannot parse the second or third parameter"
            << std::endl;
        return EXIT_FAILURE;
    }

    try
    {
        yami::agent client_agent;

        // obtain the address of calculator server

        yami::parameters resolve_params;

        resolve_params.set_string("object", "calculator");

        std::auto_ptr<yami::outgoing_message> ns_query(
            client_agent.send(name_server_address,
                "names", "resolve", resolve_params));
        
        ns_query->wait_for_completion();
        if (ns_query->get_state() != yami::replied)
        {
            std::cout << "error: "
                << ns_query->get_exception_msg() << std::endl;

            return EXIT_FAILURE;
        }

        const yami::parameters & resolve_reply =
            ns_query->get_reply();
        const std::string & calculator_address =
            resolve_reply.get_string("location");

        // send message to the calculator object

        yami::parameters params;
        params.set_integer("a", a);
        params.set_integer("b", b);

        std::auto_ptr<yami::outgoing_message> om(
            client_agent.send(calculator_address,
                "calculator", "calculate", params));

        om->wait_for_completion();
        const yami::message_state state = om->get_state();
        if (state == yami::replied)
        {
            const yami::parameters & reply =
                om->get_reply();

            int sum = reply.get_integer("sum");
            int difference = reply.get_integer("difference");
            int product = reply.get_integer("product");

            int ratio;
            yami::parameter_entry ratio_entry;
            const bool ratio_defined =
                reply.find("ratio", ratio_entry);
            if (ratio_defined)
            {
                ratio = ratio_entry.get_integer();
            }

            std::cout << "sum        = "
                << sum << '\n';
            std::cout << "difference = "
                << difference << '\n';
            std::cout << "product    = "
                << product << '\n';

            std::cout << "ratio      = ";
            if (ratio_defined)
            {
                std::cout << ratio;
            }
            else
            {
                std::cout << "<undefined>";
            }
        }
        else if (state == yami::rejected)
        {
            std::cout << "The message has been rejected: "
                << om->get_exception_msg();
        }
        else
        {
            std::cout << "The message has been abandoned.";
        }

        std::cout << std::endl;
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << std::endl;
    }
}
