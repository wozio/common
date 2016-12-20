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

#include "name_server-configuration.h"
#include "../common/properties.h"

#include <cstdio>

using namespace name_server;

namespace // unnamed
{

const char * default_config_file_name = "yami4names.cfg";

} // unnamed namespace

bool name_server::init_config(int argc, char * argv[])
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

std::string name_server::listener()
{
    return properties::get("listener", "tcp://*:*");
}

std::string name_server::data_directory()
{
    return properties::get("data-directory", "data");
}

bool name_server::log_enabled(logger::module m)
{
    const std::string & key = std::string("log.") + logger::module_name(m);
    const std::string & value = properties::get(key, "false");

    return value == "true";
}
