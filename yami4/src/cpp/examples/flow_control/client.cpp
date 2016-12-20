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
    if (argc != 2 && argc != 5)
    {
        std::cout << "need 1 or 4 parameters:\n"
            "   - server address\n"
            "   - outgoing high water mark\n"
            "   - outgoing low water mark\n"
            "   - number of iterations\n"
            "If only server address is given,"
            " the limits will have default values"
            " and the loop will be infinite\n";

        return EXIT_FAILURE;
    }

    const std::string server_address = argv[1];
    int num_of_iterations = -1;

    yami::parameters options;
    if (argc == 5)
    {
        int outgoing_high_water_mark;
        int outgoing_low_water_mark;

        if (examples::string_to_int(argv[2],
                outgoing_high_water_mark) == false ||
            examples::string_to_int(argv[3],
                outgoing_low_water_mark) == false ||
            examples::string_to_int(argv[4],
                num_of_iterations) == false)
        {
            std::cout << "invalid arguments\n";
            return EXIT_FAILURE;
        }

        options.set_integer("outgoing_high_water_mark",
            outgoing_high_water_mark);
        options.set_integer("outgoing_low_water_mark",
            outgoing_low_water_mark);
    }

    try
    {
        yami::agent client_agent(options);

        yami::parameters params;

        int index = 1;
        while (true)
        {
            params.set_integer("index", index);

            client_agent.send_one_way(server_address,
                "object", "message", params);

            std::cout
                << "posted message " << index << std::endl;

            if (num_of_iterations > 0)
            {
                if (index == num_of_iterations)
                {
                    break;
                }
            }

            ++index;
        }
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << std::endl;
    }
}
