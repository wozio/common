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

        // read lines of text from standard input
        // and post each one for transmission

        std::string input_line;

        while (std::getline(std::cin, input_line))
        {
            yami::parameters params;

            // the "content" field name is arbitrary,
            // but needs to be recognized at the server side

            params.set_string("content", input_line);

            client_agent.send_one_way(server_address,
                "printer", "print", params);
        }
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << std::endl;
    }
}
