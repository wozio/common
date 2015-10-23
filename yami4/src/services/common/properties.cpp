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

#include "properties.h"

#include <boost/algorithm/string.hpp>
#include <fstream>
#include <map>
#include <string>

using namespace properties;

namespace // unnamed
{

typedef std::map<std::string, std::string> properties_map;
properties_map props_;

void process_entry(const std::string & e)
{
    if (e.empty() || (e[0] == '#'))
    {
        return;
    }
        
    std::string::size_type equals_pos = e.find('=');
    if (equals_pos != std::string::npos)
    {
        std::string name = e.substr(0, equals_pos);
        std::string value = e.substr(equals_pos + 1);
            
        boost::algorithm::trim(name);
        boost::algorithm::to_lower(name);
        boost::algorithm::trim(value);
            
        if ((name.empty() == false) && (value.empty() == false))
        {
            props_[name] = value;
        }
    }
}

} // unnamed namespace

void properties::load_properties(const char * config_file_name, int argc, char * argv[])
{
    std::ifstream prop_file(config_file_name);
    
    std::string line;
    while (std::getline(prop_file, line))
    {
        process_entry(line);
    }

    // check also command line arguments that start with -D
    for (int i = 1; i < argc; ++i)
    {
        const std::string & e = argv[i];

        if (e.find("-D") != std::string::npos)
        {
            process_entry(e.substr(2));
        }
    }
}

void properties::clear()
{
    props_.clear();
}

bool properties::is_defined(const std::string & name)
{
    return props_.find(name) != props_.end();
}

std::string properties::get(const std::string & name, const std::string & default_value)
{
    std::string n(name);
    boost::algorithm::to_lower(n);
    properties_map::iterator it = props_.find(n);
    if (it != props_.end())
    {
        return it->second;
    }
    else
    {
        return default_value;
    }
}
