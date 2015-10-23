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

#include "name_utils.h"

#include "idl.h"

#include <cctype>

using namespace name_utils;

std::string name_utils::package_to_file_name(const std::string & package_name)
{
    std::string file_name;

    for (std::size_t i = 0; i != package_name.size(); ++i)
    {
        char c = package_name[i];

        c = std::tolower(c);
        if (c == '.')
        {
            c = '-';
        }

        file_name += c;
    }

    file_name += ".ydl";

    return file_name;
}

void name_utils::verify_name_is_simple(const std::string & name)
{
    std::string::size_type dot = name.find('.');
    if (dot != std::string::npos)
    {
        throw idl::invalid_input_error(
            "'" + name + "' - this must be a simple name");
    }
}

bool name_utils::name_is_qualified(const std::string & name)
{
    std::string::size_type dot = name.find('.');
    return dot != std::string::npos;
}

std::string name_utils::first_component(const std::string & s, bool leave_if_single)
{
    std::string::size_type first_dot = s.find('.');
    if (first_dot != std::string::npos)
    {
        return s.substr(0, first_dot);
    }

    if (leave_if_single)
    {
        return s;
    }
    else
    {
        return "";
    }
}

std::string name_utils::trim_first_component(const std::string & s, bool leave_if_single)
{
    std::string::size_type first_dot = s.find('.');
    if (first_dot != std::string::npos)
    {
        return s.substr(first_dot + 1);
    }

    if (leave_if_single)
    {
        return s;
    }
    else
    {
        return "";
    }
}

std::string name_utils::last_component(const std::string & s, bool leave_if_single)
{
    std::string::size_type last_dot = s.rfind('.');
    if (last_dot != std::string::npos)
    {
        return s.substr(last_dot + 1);
    }

    if (leave_if_single)
    {
        return s;
    }
    else
    {
        return "";
    }
}

std::string name_utils::trim_last_component(const std::string & s, bool leave_if_single)
{
    std::string::size_type last_dot = s.rfind('.');
    if (last_dot != std::string::npos)
    {
        return s.substr(0, last_dot);
    }

    if (leave_if_single)
    {
        return s;
    }
    else
    {
        return "";
    }
}
