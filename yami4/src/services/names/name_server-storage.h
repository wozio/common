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

#ifndef YAMI4NAMES_STORAGE_H
#define YAMI4NAMES_STORAGE_H

#include "../common/log.h"

#include <map>
#include <string>

namespace name_server
{

typedef std::map<std::string, std::string> name_map;

// Initializes the storage module.
void init_storage(const std::string & data_directory);

// Stores new binding.
void store(const std::string & object_name, const std::string & location);

// Resolves a given name. Returns empty string if name is not found.
const std::string & resolve(const std::string & object_name);

// Returns the access to the local cache.
const name_map & name_mapping();

} // namespace name_server

#endif // YAMI4NAMES_STORAGE_H
