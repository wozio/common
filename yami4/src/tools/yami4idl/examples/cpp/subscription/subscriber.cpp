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

#include "subscription.h"
#include <yami4-cpp/yami.h>

#include <cstdlib>
#include <iostream>

class subscriber_impl : public subscription::subscriber_server
{
public:

    virtual void subscription_update(const subscription::payload & p)
    {
        std::cout << "received update: " << p.value << std::endl;
    }
};

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

        subscriber_impl my_subscriber;
        
        subscriber_agent.register_object(
            update_object_name, my_subscriber);

        // subscribe to the producer

        subscription::publisher my_publisher(
            subscriber_agent, publisher_address,
            "random_number");
        
        subscription::subscription_info s;
        s.destination_object = update_object_name;

        my_publisher.subscribe(s);

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
