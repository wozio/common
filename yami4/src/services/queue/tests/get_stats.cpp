// Copyright Maciej Sobczak 2008-2015.
// This file is part of YAMI4.
//
// YAMI4 is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// YAMI4 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

#include <yami4-cpp/yami.h>

#include <cstdlib>
#include <iostream>

int main(int argc, char * argv[])
{
    if (argc != 2)
    {
        std::cout <<
            "expecting 1 parameter:\n"
            "    queue server address\n";

        return EXIT_FAILURE;
    }

    const std::string & queue_server_address = argv[1];

    try
    {
        yami::agent agent;

        std::auto_ptr<yami::outgoing_message> om(
            agent.send(queue_server_address, "stats", "get-details"));

        om->wait_for_completion();
        const yami::message_state state = om->get_state();
        if (state == yami::replied)
        {
            const yami::parameters & reply = om->get_reply();
            
            std::cout << "number of queues: "
                << reply.get_integer("num_of_queues") << '\n';
            
            std::cout << "number of all waiting messages: "
                << reply.get_integer("num_of_all_waiting_messages") << '\n';
            
            std::cout << "total byte size of all queues: "
                << reply.get_long_long("total_of_all_content_sizes") << '\n';
            
            std::cout << "number of all waiting clients: "
                << reply.get_integer("num_of_all_waiting_clients") << '\n';
            
            const char * queue_names_field_name = "queue_names";

            yami::parameter_entry e;
            bool found = reply.find(queue_names_field_name, e);
            if (found)
            {
                std::cout << "queue stats "
                    "(queue name, num of waiting messages, "
                    "total content size, num of waiting clients):\n";

                std::size_t queues_count;
                const int * num_of_waiting_messages = reply.get_integer_array(
                    "num_of_waiting_messages", queues_count);

                const long long * total_content_sizes = reply.get_long_long_array(
                    "total_content_sizes", queues_count);

                const int * num_of_waiting_clients = reply.get_integer_array(
                    "num_of_waiting_clients", queues_count);

                for (std::size_t i = 0; i != queues_count; ++i)
                {
                    std::cout << reply.get_string_in_array(queue_names_field_name, i)
                        << " " << num_of_waiting_messages[i]
                        << " " << total_content_sizes[i]
                        << " " << num_of_waiting_clients[i]
                        << '\n';
                }
            }
            else
            {
                std::cout << "there are no queues\n";
            }
        }
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << '\n';
    }
}
