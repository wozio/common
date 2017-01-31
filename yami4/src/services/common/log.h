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

#ifndef YAMI4SERVICES_LOG_H
#define YAMI4SERVICES_LOG_H

#include <string>

namespace logger
{

enum module { main, subscriptions, queues, store, messages, last_module_ };

// Returns string representation of the module name.
const char * module_name(module m);

// Writes message associated with the given module to the log file.
void put(module m, const std::string & msg);

// Enables or disables output for the given module.
// By default all modules are enabled.
void enable(module m, bool active);

} // namespace logger

#endif // YAMI4SERVICES_LOG_H
