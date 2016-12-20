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

void update(yami::incoming_message & message)
{
    const yami::parameters & content =
        message.get_parameters();

    const std::string & text = content.get_string("value");

    std::cout << "got message: " << text << std::endl;
}

int main(int argc, char * argv[])
{
    if (argc != 3)
    {
        std::cout <<
            "expecting 2 parameters:\n"
            "    broker address\n"
            "    list of tags for subscription\n";

        return EXIT_FAILURE;
    }

    const std::string & broker_address = argv[1];
    const std::string & list_of_tags = argv[2];

    try
    {
        yami::agent subscriber_agent;

        // prepare subscription update callback

        const std::string update_object_name = "update_handler";

        subscriber_agent.register_object(
            update_object_name, update);

        // subscribe to the producer

        yami::parameters params;
        params.set_string(
            "destination_object", update_object_name);

        subscriber_agent.send_one_way(broker_address,
            list_of_tags, "subscribe", params);

        std::cout
            << "subscribed, waiting for updates" << std::endl;

        // block forever and receive updates in background
        int dummy;
        std::cin >> dummy;
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << '\n';
    }
}
