#include <yami4-cpp/yami.h>

#include <cstdlib>
#include <iostream>

void call(yami::incoming_message & im)
{
    const yami::parameters & params = im.get_parameters();

    // extract the content field
    // and print it on standard output

    std::cout << params.get_string("content") << std::endl;
}

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

        server_agent.register_object("printer", call);

        // block
        std::string dummy;
        std::cin >> dummy;
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << std::endl;
    }
}
