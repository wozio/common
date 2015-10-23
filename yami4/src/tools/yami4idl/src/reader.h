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

#ifndef YAMI4IDL_READER_H
#define YAMI4IDL_READER_H

#include "idl.h"

namespace reader
{

void add_import_directory(const std::string & dir);

void read_file(const std::string & file_name);

void import_package(const std::string & package_name);

std::string last_location();

} // namespace reader

#endif // YAMI4IDL_READER_H
