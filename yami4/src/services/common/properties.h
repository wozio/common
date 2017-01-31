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

#ifndef YAMI4SERVICES_PROPERTIES_H
#define YAMI4SERVICES_PROPERTIES_H

#include <string>

namespace properties
{

// Loads the properties from a given file and from command line options.
// The new mappings (name=value) will be applied onto the existing ones.
void load_properties(const char * config_file_name, int argc, char * argv[]);

// Clears the properties that were read from file(s).
// Does not clear the properties set by command-line options.
void clear();

// Returns True is the given property is defined either in
// the regular property set or in the properties defined by
// command-line options.
bool is_defined(const std::string & name);

// Returns the value for the given name, or Default if no
// such name is found.
// During the lookup, the command-line properties override those
// read from file(s).
std::string get(const std::string & name, const std::string & default_value = "");

} // namespace properties

#endif // YAMI4SERVICES_PROPERTIES_H
