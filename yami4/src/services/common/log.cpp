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

#include "log.h"

#include <ctime>
#include <cstdio>

using namespace logger;

namespace // unnamed
{

bool activity_flags_[5] = { true, true, true, true, true };

char format_buf_[20];

const char * current_time_formatted()
{
    std::time_t time = std::time(NULL);
    std::tm * t = std::localtime(&time);

    std::sprintf(format_buf_, "%d-%02d-%02d %02d:%02d:%02d",
        t->tm_year + 1900,
        t->tm_mon + 1,
        t->tm_mday,
        t->tm_hour,
        t->tm_min,
        t->tm_sec);

    return format_buf_;
}

} // unnamed namespace

const char * logger::module_name(module m)
{
    switch (m)
    {
    case main:
        return "MAIN";
    case subscriptions:
        return "SUBSCRIPTIONS";
    case queues:
        return "QUEUES";
    case store:
        return "STORE";
    case messages:
        return "MESSAGES";
    default:
        return "";
    }
}

void logger::put(module m, const std::string & msg)
{
    if (activity_flags_[m])
    {
        std::printf("%s [%s] %s\n", current_time_formatted(), module_name(m), msg.c_str());
    }
}

void logger::enable(module m, bool active)
{
    activity_flags_[m] = active;
}
