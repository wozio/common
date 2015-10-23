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
            "expecting 1 parameter: "
            "    broker address\n";

        return EXIT_FAILURE;
    }

    const std::string & broker_address = argv[1];

    try
    {
        yami::agent agent;

        std::auto_ptr<yami::outgoing_message> om(
            agent.send(broker_address, "stats", "get-details"));

        om->wait_for_completion();
        const yami::message_state state = om->get_state();
        if (state == yami::replied)
        {
            const yami::parameters & reply = om->get_reply();
            
            std::cout << "total incoming: " << reply.get_long_long("total_incoming") << '\n';
            std::cout << "total outgoing: " << reply.get_long_long("total_outgoing") << '\n';

            yami::parameter_entry e;
            bool found = reply.find("overflows", e);
            if (found)
            {
                std::cout << "subscriber stats "
                    "(overflow flag, message count, byte count, location):\n";

                std::size_t active_subscription_count;
                const bool * overflows = e.get_boolean_array(
                    active_subscription_count);

                const long long * sent_messages = reply.get_long_long_array(
                    "sent_messages", active_subscription_count);

                const long long * sent_bytes = reply.get_long_long_array(
                    "sent_bytes", active_subscription_count);

                for (std::size_t i = 0; i != active_subscription_count; ++i)
                {
                    if (overflows[i])
                    {
                        std::cout << "# ";
                    }
                    else
                    {
                        std::cout << "  ";
                    }

                    std::cout << sent_messages[i] << ' ' << sent_bytes[i] << ' '
                        << reply.get_string_in_array("locations", i) << '\n';
                }
            }
            else
            {
                std::cout << "there are no subscriptions\n";
            }
        }
    }
    catch (const std::exception & e)
    {
        std::cout << "error: " << e.what() << '\n';
    }
}
