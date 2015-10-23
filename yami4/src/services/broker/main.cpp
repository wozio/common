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

#include "broker.h"
#include "broker-configuration.h"
#include "broker-messaging.h"
#include "broker-routing.h"
#include "../common/log.h"
#include "../common/utils.h"

#include <cstdio>

int main(int argc, char * argv[])
{
    try
    {
        bool success = broker::init_config(argc, argv);

        if (success)
        {
            for (logger::module m = logger::main; m != logger::last_module_;
                 m = static_cast<logger::module>(m + 1))
            {
                logger::enable(m, broker::log_enabled(m));
            }

            logger::put(logger::main, "initialized configuration settings");

            broker::init_routing(
                broker::max_subscriptions(),
                broker::max_client_queue(),
                broker::subscription_overflow_policy());

            // install all forward channels

            int forward_index = 1;
            while (true)
            {
                const std::string & target = broker::forward_target(forward_index);
                const std::string & filter = broker::forward_filter(forward_index);

                if (target.empty())
                {
                    break;
                }

                broker::set_forwarding(filter, target);

                ++forward_index;
            }

            broker::init_messaging(broker::listener(),
                broker::subscription_overflow_policy());

            utils::millisleep(broker::warmup_time());

            broker::allow_incoming();
        }
        else
        {
            logger::put(logger::main, "configuration not initialized properly");
        }

        while (true)
        {
            utils::millisleep(10000); // arbitary delay
        }
    }
    catch (const std::exception & e)
    {
        logger::put(logger::main, "unknown exception in main task:");
        logger::put(logger::main, e.what());
    }
}
