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

#include "utils.h"

#include <cstdio>

#ifdef _WIN32
#include <Windows.h>
#else // _WIN32
#include <time.h>
#endif // _WIN32

using namespace utils;

void utils::millisleep(int delay)
{
#ifdef _WIN32
    Sleep(delay);
#else // _WIN32
    timespec tv;

    tv.tv_sec = delay / 1000;
    tv.tv_nsec = (delay % 1000) * 1000000;

    nanosleep(&tv, NULL);
#endif // _WIN32
}

std::string utils::size_to_string(std::size_t v)
{
    char buf[20];

    std::sprintf(buf, "%llu", static_cast<unsigned long long>(v));

    return buf;
}

yami::core::result utils::get_string(
    const yami::core::parameters & params, const char * field_name,
    std::string & out_result)
{
    const char * value;
    std::size_t value_len;

    yami::core::result res =
        params.get_string(field_name, value, value_len);
    if (res == yami::core::ok)
    {
        out_result.assign(value, value_len);
    }

    return res;
}

std::string utils::result_to_string(yami::core::result res)
{
    switch (res)
    {
    case yami::core::no_such_name:
        return "No such name.";
    case yami::core::bad_type:
        return "Bad type.";
    case yami::core::no_such_index:
        return "No such index.";
    case yami::core::no_memory:
        return "Not enough memory.";
    case yami::core::nesting_too_deep:
        return "Nesting of parameters is too deep.";
    case yami::core::not_enough_space:
        return 
            "Not enough space or not enough data in the buffer.";
    case yami::core::no_entries:
        return "No entries found.";
    case yami::core::unexpected_value:
        return 
            "The value that was given or received is incorrect.";
    case yami::core::bad_protocol:
        return "The given protocol is not supported.";
    case yami::core::io_error:
        return "I/O error.";
    case yami::core::timed_out:
        return "Operation timed out.";
    case yami::core::channel_closed:
        return "The channel was closed.";
    case yami::core::bad_state:
        return "The given object is in the wrong state.";
    default:
        // operation completed successfully
        return "";
    }
}
