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

#include "queue.h"
#include "queue-configuration.h"
#include "queue-messaging.h"
#include "queue-routing.h"
#include "../common/log.h"
#include "../common/utils.h"

int main(int argc, char * argv[])
{
    try
    {
        bool success = queue::init_config(argc, argv);

        if (success)
        {
            for (logger::module m = logger::main; m != logger::last_module_;
                 m = static_cast<logger::module>(m + 1))
            {
                logger::enable(m, queue::log_enabled(m));
            }

            logger::put(logger::main, "initialized configuration settings");

            queue::init_routing(
                queue::max_queue_num_of_messages(),
                queue::max_queue_size(),
                queue::queue_creation_policy());

            // install all predefined queues

            int queue_index = 1;
            while (true)
            {
                const std::string & queue_name = queue::initial_queue_name(queue_index);

                if (queue_name.empty())
                {
                    break;
                }

                queue::create_queue(queue_name);

                ++queue_index;
            }

            queue::init_messaging(queue::listener());
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
