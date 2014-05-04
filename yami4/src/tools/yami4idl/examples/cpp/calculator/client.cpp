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
