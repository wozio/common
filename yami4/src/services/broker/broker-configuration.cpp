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

#include "broker-configuration.h"
#include "../common/properties.h"
#include "../common/utils.h"

#include <cstdio>

using namespace broker;

namespace // unnamed
{

const char * default_config_file_name = "yami4broker.cfg";

} // unnamed namespace

bool broker::init_config(int argc, char * argv[])
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

std::string broker::listener()
{
    return properties::get("listener", "tcp://*:*");
}

std::size_t broker::warmup_time()
{
    const std::string & s = properties::get("warmup", "0");

    int d;
    const int res = std::sscanf(s.c_str(), "%d", &d);

    if ((res == 1) && (d >= 0))
    {
        return static_cast<std::size_t>(d);
    }
    else
    {
        return 0;
    }
}

std::size_t broker::max_subscriptions()
{
    const std::string & s = properties::get("max_subscriptions", "100");

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

std::size_t broker::max_client_queue()
{
    const std::string & s = properties::get("max_client_queue", "10");

    int d;
    const int res = std::sscanf(s.c_str(), "%d", &d);

    if ((res == 1) && (d >= 0))
    {
        return static_cast<std::size_t>(d);
    }
    else
    {
        return 10;
    }
}

overflow_policy broker::subscription_overflow_policy()
{
    const std::string & s = properties::get("overflow_policy", "reject_message");

    if (s == "drop_update")
    {
        return drop_update;
    }
    else if (s == "unsubscribe")
    {
        return unsubscribe;
    }
    else
    {
        return reject_message;
    }
}

bool broker::log_enabled(logger::module m)
{
    const std::string & key = std::string("log.") + logger::module_name(m);
    const std::string & value = properties::get(key, "false");

    return value == "true";
}

std::string broker::forward_target(std::size_t index)
{
    const std::string & key = std::string("forward.") +
        utils::size_to_string(index) + ".target";

    return properties::get(key);
}

std::string broker::forward_filter(std::size_t index)
{
    const std::string & key = std::string("forward.") +
        utils::size_to_string(index) + ".filter";

    return properties::get(key);
}
