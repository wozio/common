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

#include "idl.h"

using namespace idl;

casing_mode idl::string_to_casing(const std::string & s)
{
    if (s == "ident")
    {
        return ident;
    }
    if (s == "lower_case")
    {
        return lower_case;
    }
    if (s == "camel_case")
    {
        return camel_case;
    }
    else
    {
        return default_casing;
    }
}
