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

        yami::activity_statistics_monitor stats;
        yami::agent publisher_agent(stats);

        publisher_agent.register_object("stats", stats);

        const std::string resolved_address =
            publisher_agent.add_listener(publisher_address);

        std::cout << "The publisher is listening on "
            << resolved_address << std::endl;

        publisher_agent.register_value_publisher(
            "random_number", random_value);

        // publish random numbers forever
        yami::parameters content;
        while (true)
        {
            const int random = std::rand() % 100;
            content.set_integer("value", random);

            std::cout
                << "publishing value " << random << std::endl;

            random_value.publish(content);

            // pause for 1s
            examples::pause();
        }
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << std::endl;
    }
}
