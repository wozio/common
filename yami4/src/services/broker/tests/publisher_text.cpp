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

#include "../../common/utils.h"

#include <cstdlib>
#include <iostream>

int main(int argc, char * argv[])
{
    if (argc != 5)
    {
        std::cout <<
            "expecting 4 parameters:\n"
            "    broker address\n"
            "    list of tags for publishing\n"
            "    message text\n"
            "    delay\n";

        return EXIT_FAILURE;
    }

    const std::string & broker_address = argv[1];
    const std::string & list_of_tags = argv[2];
    const std::string & text = argv[3];

    const double t = std::strtod(argv[4], NULL);
    const int wait_time = static_cast<int>(t * 1000);

    try
    {
        yami::agent publisher_agent;

        yami::parameters content;

        content.set_string("value", text);

        int counter = 1;
        while (true)
        {
            publisher_agent.send_one_way(
                broker_address, list_of_tags, "publish", content);

            std::cout << "sent " << counter << std::endl;
            ++counter;

            utils::millisleep(wait_time);
        }
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << '\n';
    }
}
