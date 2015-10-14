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

#include "queue-configuration.h"
#include "../common/properties.h"
#include "../common/utils.h"

#include <cstdio>

using namespace queue;

namespace // unnamed
{

const char * default_config_file_name = "yami4queue.cfg";

} // unnamed namespace

bool queue::init_config(int argc, char * argv[])
{
    bool result = false;

    try
    {
        if (argc != 1)
        {
            properties::load_properties(argv[1], argc, argv);
        }
        else
        {
            properties::load_properties(default_config_file_name, argc, argv);
        }

        result = true;
    }
    catch (...)
    {
        // error while loading config file
    }

    return result;
}

std::string queue::listener()
{
    return properties::get("listener", "tcp://*:*");
}

std::size_t queue::max_waiting_clients()
{
    const std::string & s = properties::get("max_waiting_clients", "10");

    int d;
    const int res = std::sscanf(s.c_str(), "%d", &d);

    if ((res == 1) && (d >= 0))
    {
        return static_cast<std::size_t>(d);
    }
    else
    {
        return 100;
    }
}

std::size_t queue::max_queue_num_of_messages()
{
    const std::string & s = properties::get("max_queue_num_of_messages", "10");

    int d;
    const int res = std::sscanf(s.c_str(), "%d", &d);

    if ((res == 1) && (d >= 0))
    {
        return static_cast<std::size_t>(d);
    }
    else
    {
        return 100;
    }
}

std::size_t queue::max_queue_size()
{
    const std::string & s = properties::get("max_queue_size", "1000000");

    int d;
    const int res = std::sscanf(s.c_str(), "%d", &d);

    if ((res == 1) && (d >= 0))
    {
        return static_cast<std::size_t>(d);
    }
    else
    {
        return 100;
    }
}

creation_policy queue::queue_creation_policy()
{
    const std::string & s = properties::get("queue_creation_policy", "dynamic");

    if (s == "static")
    {
        return static_creation;
    }
    else // if (s == "dynamic")
    {
        return dynamic_creation;
    }
}

bool queue::log_enabled(logger::module m)
{
    const std::string & key = std::string("log.") + logger::module_name(m);
    const std::string & value = properties::get(key, "false");

    return value == "true";
}

std::string queue::initial_queue_name(std::size_t index)
{
    const std::string & key = std::string("queue.") + utils::size_to_string(index);

    return properties::get(key);
}
