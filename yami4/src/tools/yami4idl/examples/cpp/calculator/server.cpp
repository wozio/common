#include "calculator.h"
#include <yami4-cpp/yami.h>

#include <cstdlib>
#include <iostream>

class calculator_impl : public calculator::operations_server
{
public:
    
    virtual void calculate(
        const calculator::operands & op, calculator::results & res)
    {
        res.sum        = op.a + op.b;
        res.difference = op.a - op.b;
        res.product    = op.a * op.b;

        // if the ratio cannot be computed,
        // it is not included in the response
        // the client will interpret that fact properly
        if (op.b != 0)
        {
            res.ratio = op.a / op.b;
            res.ratio_valid = true;
        }

        std::cout << "got message with parameters "
            << op.a << " and " << op.b
            << ", response has been sent back"
            << std::endl;
    }
};

int main(int argc, char * argv[])
{
    if (argc != 2)
    {
        std::cout
            << "expecting one parameter: server destination\n";
        return EXIT_FAILURE;
    }

    const std::string server_address = argv[1];

    try
    {
        yami::agent server_agent;
        
        const std::string resolved_address =
            server_agent.add_listener(server_address);

        std::cout << "The server is listening on "
            << resolved_address << std::endl;

        calculator_impl my_server;

        server_agent.register_object("calculator", my_server);

        // block
        std::string dummy;
        std::cin >> dummy;
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << std::endl;
    }
}
