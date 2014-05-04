#include "print.h"
#include <yami4-cpp/yami.h>

#include <cstdlib>
#include <iostream>

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
        yami::agent client_agent;
        
        print::printer my_printer(
            client_agent, server_address, "printer");

        // read lines of text from standard input
        // and post each one for transmission

        std::string input_line;

        while (std::getline(std::cin, input_line))
        {
            print::text msg;
            
            msg.content = input_line;

            // message with confirmation      
            my_printer.print_synchronously(msg);
        }
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << std::endl;
    }
}

