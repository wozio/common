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

#ifndef YAMI4IDL_IDL_H
#define YAMI4IDL_IDL_H

#include <stdexcept>
#include <list>
#include <string>

namespace idl
{

class invalid_input_error : public std::runtime_error
{
public:
    invalid_input_error(const std::string & msg)
        : std::runtime_error(msg)
    {
    }
};

class unsupported_error : public std::runtime_error
{
public:
    unsupported_error()
        : std::runtime_error("the requested feature is not supported")
    {
    }
};

enum input_mode { full, import };

enum casing_mode { default_casing, ident, lower_case, camel_case };

casing_mode string_to_casing(const std::string & s);

typedef std::list<std::string> name_list;

} // namespace idl

#endif // YAMI4IDL_IDL_H
