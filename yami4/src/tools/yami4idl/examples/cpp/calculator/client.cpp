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

#include "calculator.h"
#include <yami4-cpp/yami.h>

#include <cstdlib>
#include <iostream>

#include "../common_utils/string_to_int.h"

int main(int argc, char * argv[])
{
    if (argc != 4)
    {
        std::cout << "expecting three parameters: "
            "server destination and two integers\n";
        return EXIT_FAILURE;
    }

    const std::string server_address = argv[1];
    
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
        
        calculator::operations my_calculator(
            client_agent, server_address, "calculator");
        
        calculator::operands op;
        op.a = a;
        op.b = b;

        calculator::results res;

        my_calculator.calculate(op, res);

        std::cout << "sum        = " << res.sum << '\n';
        std::cout << "difference = " << res.difference << '\n';
        std::cout << "product    = " << res.product << '\n';

        std::cout << "ratio      = ";
        if (res.ratio_valid)
        {
            std::cout << res.ratio;
        }
        else
        {
            std::cout << "<undefined>";
        }

        std::cout << std::endl;
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << std::endl;
    }
}
