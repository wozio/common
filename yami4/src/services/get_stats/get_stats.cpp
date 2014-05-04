#include <yami4-cpp/yami.h>

#include <cstdlib>
#include <iostream>

int main(int argc, char * argv[])
{
    if (argc != 2 && argc != 3 && argc != 4)
    {
        std::cout << "expecting one or two or three parameters: "
            "server destination, optional statistics monitor object name "
            "and optional reset flag\n";
        return EXIT_FAILURE;
    }

    const std::string server_address = argv[1];
    std::string stats_object_name = "stats";
    if (argc == 3)
    {
        stats_object_name = argv[2];
    }

    bool reset_counters = false;
    if (argc == 4)
    {
        const std::string reset_command = argv[3];
        reset_counters = reset_command == "reset";
    }

    try
    {
        yami::agent client_agent;

        yami::parameters params;
        params.set_boolean("reset", reset_counters);

        std::auto_ptr<yami::outgoing_message> om(
            client_agent.send(server_address,
                stats_object_name, "get", params));

        om->wait_for_completion();
        const yami::message_state state = om->get_state();
        if (state == yami::replied)
        {
            const yami::parameters & reply =
                om->get_reply();

            std::cout << "server uptime: "
                << reply.get_integer("uptime") << " sec\n\n";

            std::cout << "listeners:\n";
            
            const char * listeners_field = "listeners";
            std::size_t listeners_count =
                reply.get_string_array_length(listeners_field);
            for (std::size_t i = 0; i != listeners_count; ++i)
            {
                std::cout << "    "
                    << reply.get_string_in_array(listeners_field, i) << "\n";
            }
            std::cout << '\n';

            std::cout << "registered objects:\n";

            const char * objects_field = "objects";
            std::size_t objects_count =
                reply.get_string_array_length(objects_field);
            for (std::size_t i = 0; i != objects_count; ++i)
            {
                std::cout << "    "
                    << reply.get_string_in_array(objects_field, i) << "\n";
            }
            std::cout << '\n';

            std::cout << "connections:\n";

            const char * conn_names_field = "connection_names";
            std::size_t connections_count =
                reply.get_string_array_length(conn_names_field);
            std::size_t messages_sent_count;
            long long * messages_sent = reply.get_long_long_array(
                "messages_sent", messages_sent_count);
            std::size_t messages_received_count;
            long long * messages_received = reply.get_long_long_array(
                "messages_received", messages_received_count);
            std::size_t bytes_sent_count;
            long long * bytes_sent = reply.get_long_long_array(
                "bytes_sent", bytes_sent_count);
            std::size_t bytes_received_count;
            long long * bytes_received = reply.get_long_long_array(
                "bytes_received", bytes_received_count);

            if (connections_count == messages_sent_count &&
                connections_count == messages_received_count &&
                connections_count == bytes_sent_count &&
                connections_count == bytes_received_count)
            {
                for (std::size_t i = 0; i != connections_count; ++i)
                {
                    std::cout << "    "
                        << reply.get_string_in_array(conn_names_field, i)
                        << "\n";
                    std::cout << "        messages in/out: "
                        << messages_received[i] << "/"
                        << messages_sent[i] << '\n';
                    std::cout << "        bytes    in/out: "
                        << bytes_received[i] << "/"
                        << bytes_sent[i] << '\n';
                }
                std::cout << '\n';
            }
            else
            {
                std::cout << "    Inconsistent server data.\n";
            }

            std::cout << "i/o errors:\n";

            const char * conn_error_names_field = "error_names";
            std::size_t conn_errors_count =
                reply.get_string_array_length(conn_error_names_field);
            std::size_t errors_count;
            long long * connection_errors = reply.get_long_long_array(
                "errors", errors_count);

            if (conn_errors_count == errors_count)
            {
                for (std::size_t i = 0; i != conn_errors_count; ++i)
                {
                    std::cout << "    "
                        << reply.get_string_in_array(
                            conn_error_names_field, i)
                        << ' '
                        << connection_errors[i]
                        << '\n';
                }
            }
            else
            {
                std::cout << "    Inconsistent server data.\n";
            }            
        }
        else if (state == yami::rejected)
        {
            std::cout << "The message has been rejected: "
                << om->get_exception_msg();
        }
        else
        {
            std::cout << "The message has been abandoned.";
        }

        std::cout << std::endl;
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << std::endl;
    }
}
