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

#ifndef YAMI4SERVICES_UTILS_H
#define YAMI4SERVICES_UTILS_H

#include <yami4-core/core.h>
#include <yami4-core/parameters.h>

#include <string>

namespace utils
{

// Sleeps the given number of milliseconds.
void millisleep(int delay);

// Converts size_t to string
std::string size_to_string(std::size_t v);

// Helper for extracting string fields.
yami::core::result get_string(
    const yami::core::parameters & params, const char * field_name,
    std::string & out_result);

// Converts result codes into messages.
std::string result_to_string(yami::core::result res);

} // namespace logger

#endif // YAMI4SERVICES_UTILS_H
