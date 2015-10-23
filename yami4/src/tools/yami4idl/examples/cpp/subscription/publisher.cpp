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

#include "subscription.h"
#include <yami4-cpp/yami.h>

#include <cstdlib>
#include <iostream>

#include <pause.h>

int main(int argc, char * argv[])
{
    if (argc != 2)
    {
        std::cout
            << "expecting one parameter: "
            << "publisher destination\n";
        return EXIT_FAILURE;
    }

    const std::string publisher_address = argv[1];

    try
    {
        yami::value_publisher random_value;

        yami::agent publisher_agent;

        const std::string resolved_address =
            publisher_agent.add_listener(publisher_address);

        std::cout << "The publisher is listening on "
            << resolved_address << std::endl;

        publisher_agent.register_value_publisher(
            "random_number", random_value);

        // publish random numbers forever
        while (true)
        {
            subscription::payload p;
            
            const int random = std::rand() % 100;
            p.value = random;

            std::cout
                << "publishing value " << random << std::endl;

            yami::parameters params;
            p.write(params);
            
            random_value.publish(params);

            // pause for 1s
            examples::pause();
        }
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << std::endl;
    }
}
