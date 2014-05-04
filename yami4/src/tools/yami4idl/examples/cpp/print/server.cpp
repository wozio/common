#include "print.h"
#include <yami4-cpp/yami.h>

#include <cstdlib>
#include <iostream>

class printer_impl : public print::printer_server
{
public:

    virtual void print(const print::text & t)
    {
        std::cout << t.content << std::endl;
    }

    virtual void print_synchronously(const print::text & t)
    {
        std::cout << t.content << std::endl;
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

        printer_impl my_server;

        server_agent.register_object("printer", my_server);

        // block
        std::string dummy;
        std::cin >> dummy;
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << std::endl;
    }
}
