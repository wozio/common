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

        yami::activity_statistics_monitor stats;
        yami::agent publisher_agent(stats);

        publisher_agent.register_object("stats", stats);

        const std::string resolved_address =
            publisher_agent.add_listener(publisher_address);

        std::cout << "The publisher is listening on "
            << resolved_address << std::endl;

        publisher_agent.register_value_publisher(
            "random_number", random_value);

        // publish random numbers forever
        yami::parameters content;
        while (true)
        {
            const int random = std::rand() % 100;
            content.set_integer("value", random);

            std::cout
                << "publishing value " << random << std::endl;

            random_value.publish(content);

            // pause for 1s
            examples::pause();
        }
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << std::endl;
    }
}
