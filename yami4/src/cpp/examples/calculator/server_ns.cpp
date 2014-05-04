#include <yami4-cpp/yami.h>

#include <cstdlib>
#include <iostream>

void call(yami::incoming_message & im)
{
    // extract the parameters for calculations

    const yami::parameters & params = im.get_parameters();

    const int a = params.get_integer("a");
    const int b = params.get_integer("b");

    // prepare the answer with results of four calculations

    yami::parameters reply_params;

    reply_params.set_integer("sum", a + b);
    reply_params.set_integer("difference", a - b);
    reply_params.set_integer("product", a * b);

    // if the ratio cannot be computed,
    // it is not included in the response
    // the client will interpret that fact properly
    if (b != 0)
    {
        reply_params.set_integer("ratio", a / b);
    }

    im.reply(reply_params);

    std::cout << "got message with parameters "
        << a << " and " << b
        << ", response has been sent back"
        << std::endl;
}

int main(int argc, char * argv[])
{
    if (argc != 2)
    {
        std::cout
            << "expecting one parameter: name server address\n";
        return EXIT_FAILURE;
    }

    const std::string name_server_address = argv[1];

    try
    {
        yami::agent server_agent;

        // prepare the server and bind its address
        // to the name server

        const std::string resolved_address =
            server_agent.add_listener("tcp://*:*");

        std::cout << "The server is listening on "
            << resolved_address << std::endl;

        yami::parameters bind_params;
        bind_params.set_string("object", "calculator");
        bind_params.set_string("location", resolved_address);

        std::auto_ptr<yami::outgoing_message> ns_bind(
            server_agent.send(name_server_address,
                "names", "bind", bind_params));

        ns_bind->wait_for_completion();
        if (ns_bind->get_state() != yami::replied)
        {
            std::cout << "error: "
                << ns_bind->get_exception_msg() << std::endl;

            return EXIT_FAILURE;
        }

        std::cout << "Address bound by name server."
            << std::endl;

        server_agent.register_object("calculator", call);

        // block
        std::string dummy;
        std::cin >> dummy;
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << std::endl;
    }
}
