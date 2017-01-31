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

int main(int argc, char * argv[])
{
    if (argc != 4)
    {
        std::cout <<
            "expecting 3 parameters:\n"
            "    name server address\n"
            "    object name\n"
            "    object location\n";

        return EXIT_FAILURE;
    }

    const std::string & name_server_address = argv[1];
    const std::string & object_name = argv[2];
    const std::string & object_location = argv[3];

    try
    {
        yami::agent agent;

        yami::parameters params;

        params.set_string("object", object_name);
        params.set_string("location", object_location);
        
        std::auto_ptr<yami::outgoing_message> om(
            agent.send(name_server_address,
                "names", "bind", params));

        om->wait_for_completion();
        const yami::message_state state = om->get_state();
        if (state == yami::rejected)
        {
            std::cout << "bind rejected: "
                << om->get_exception_msg() << '\n';
        }
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << '\n';
    }
}
