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

#ifndef YAMICPP_STRING_TO_INT_H_INCLUDED
#define YAMICPP_STRING_TO_INT_H_INCLUDED

#include <cstdlib>

namespace examples
{

// helper function for convering string to int
bool string_to_int(const char * str, int & value)
{
    bool result;
    char * endptr;
    const long tmp = std::strtol(str, &endptr, 10);
    if (endptr != str && *endptr == '\0')
    {
        value = static_cast<int>(tmp);
        result = true;
    }
    else
    {
        result = false;
    }

    return result;
}

} // namespace examples

#endif // YAMICPP_STRING_TO_INT_H_INCLUDED
