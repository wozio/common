#include "subscription.h"
#include <yami4-cpp/yami.h>

#include <cstdlib>
#include <iostream>

#include <pause.h>

int main(int argc, char * argv[])
{
    if (argc != 2)
    {
        std::cout
            << "expecting one parameter: "
            << "publisher destination\n";
        return EXIT_FAILURE;
    }

    const std::string publisher_address = argv[1];

    try
    {
        yami::value_publisher random_value;

        yami::agent publisher_agent;

        const std::string resolved_address =
            publisher_agent.add_listener(publisher_address);

        std::cout << "The publisher is listening on "
            << resolved_address << std::endl;

        publisher_agent.register_value_publisher(
            "random_number", random_value);

        // publish random numbers forever
        while (true)
        {
            subscription::payload p;
            
            const int random = std::rand() % 100;
            p.value = random;

            std::cout
                << "publishing value " << random << std::endl;

            yami::parameters params;
            p.write(params);
            
            random_value.publish(params);

            // pause for 1s
            examples::pause();
        }
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << std::endl;
    }
}
