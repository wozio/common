#include <yami4-cpp/yami.h>

#include <cstdlib>
#include <iostream>

void update(yami::incoming_message & message)
{
    const yami::parameters & content =
        message.get_parameters();

    const int value = content.get_integer("value");

    std::cout << "received update: " << value << std::endl;
}

int main(int argc, char * argv[])
{
    if (argc != 2)
    {
        std::cout
            << "expecting parameter: publisher destination\n";
        return EXIT_FAILURE;
    }

    const std::string publisher_address = argv[1];

    try
    {
        yami::agent subscriber_agent;

        // prepare subscription update callback

        const std::string update_object_name =
            "update_handler";

        subscriber_agent.register_object(
            update_object_name, update);

        // subscribe to the producer

        yami::parameters params;
        params.set_string(
            "destination_object", update_object_name);

        subscriber_agent.send_one_way(publisher_address,
            "random_number", "subscribe", params);

        std::cout
            << "subscribed, waiting for updates" << std::endl;

        // block forever and receive updates in background
        int dummy;
        std::cin >> dummy;
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << std::endl;
    }
}
