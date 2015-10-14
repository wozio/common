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

#ifndef YAMI4IDL_STRUCTURES_H
#define YAMI4IDL_STRUCTURES_H

#include "idl.h"

#include <map>

namespace structures
{

void collect_import(const std::string & imported_package_name, bool is_explicit);

void create_package(const std::string & new_package_name);

void create_type(const std::string & package_name,
    const std::string & new_type_name);

void create_field(const std::string & package_name,
    const std::string & enclosing_type_name,
    const std::string & new_field_name,
    const std::string & field_type_name,
    bool optional);

void create_interface(const std::string & package_name,
    const std::string & new_interface_name);

void create_message(const std::string & package_name,
    const std::string & enclosing_interface_name,
    const std::string & new_message_name,
    bool oneway,
    const std::string & in_param_name,
    const std::string & in_param_type,
    const std::string & out_param_name,
    const std::string & out_param_type);

void verify_param_type_is_user_defined(const std::string & package_name,
    const std::string & type_name);

void finish_package(const std::string & package_name);

void set_mode(idl::input_mode mode);

// shared definitions

extern const std::string k_boolean;
extern const std::string k_integer;
extern const std::string k_long_long;
extern const std::string k_float;
extern const std::string k_string;
extern const std::string k_binary;
extern const std::string k_boolean_array;
extern const std::string k_integer_array;
extern const std::string k_long_long_array;
extern const std::string k_float_array;
extern const std::string k_string_array;
extern const std::string k_binary_array;

struct field_definition
{
    std::string type_name;
    bool optional;
};

struct message_definition
{
    bool oneway;
    std::string in_param_name; // or "" if no such param
    std::string in_param_type;
    std::string out_param_name;
    std::string out_param_type;
};

// {field_name -> field_definition}
typedef std::map<std::string, field_definition> field_map;

struct type_fields
{
    field_map definitions;
    idl::name_list ordered_names;
};

// {message_name -> message_definition}
typedef std::map<std::string, message_definition> message_map;

struct interface_messages
{
    message_map definitions;
    idl::name_list ordered_names;
};

// this container keeps ordered field definitions for all types
// {type_name -> type_fields}
typedef std::map<std::string, type_fields> type_map;

// this container keeps ordered message definitions for all interfaces
// {interface_name -> interface_messages}
typedef std::map<std::string, interface_messages> interface_map;

struct package_definitions
{
    idl::input_mode created_as;

    idl::name_list explicit_imports;
    idl::name_list implicit_imports;

    type_map type_definitions;
    idl::name_list ordered_type_names;

    interface_map interface_definitions;
    idl::name_list ordered_interface_names;
};

// this container keeps ordered type definitions for all packages
// {package_name -> package_definitions}
typedef std::map<std::string, package_definitions> package_map;

// this list includes only packages that were parsed and processed
// in the Full mode, as opposed to imported packages
// these packages are used as sources for code generation
extern idl::name_list fully_defined_packages;

// main data structure with complete and incomplete definitions
// for all visited packages
extern package_map all_packages;

} // namespace structures

#endif // YAMI4IDL_STRUCTURES_H
