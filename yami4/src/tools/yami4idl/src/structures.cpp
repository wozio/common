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

#include "structures.h"
#include "name_utils.h"

using namespace structures;

namespace // unnamed
{

idl::input_mode processing_mode_ = idl::full;

// these are names of imported packages that are collected
// before the actual package name is known
idl::name_list pending_explicit_imports_;
idl::name_list pending_implicit_imports_;

bool type_is_primitive(const std::string & type_name)
{
    return (type_name == k_boolean) ||
        (type_name == k_integer) ||
        (type_name == k_long_long) ||
        (type_name == k_float) ||
        (type_name == k_string) ||
        (type_name == k_binary) ||
        (type_name == k_boolean_array) ||
        (type_name == k_integer_array) ||
        (type_name == k_long_long_array) ||
        (type_name == k_float_array) ||
        (type_name == k_string_array) ||
        (type_name == k_binary_array);
}

void verify_type_is_not_primitive(const std::string & type_name)
{
    if (type_is_primitive(type_name))
    {
        throw idl::invalid_input_error(
            "'" + type_name + "' - basic types cannot be redefined");
    }
}

bool type_is_known_in_package(const std::string & package_name,
    bool check_hierarchy, const std::string & type_name)
{
    if (package_name.empty())
    {
        return false;
    }

    package_map::iterator it = all_packages.find(package_name);
    if (it != all_packages.end())
    {
        package_definitions & pkg = it->second;

        type_map::iterator tit = pkg.type_definitions.find(type_name);

        if (tit != pkg.type_definitions.end())
        {
            return true;
        }
    }

    // if the name is not known in the given package,
    // then try in the parent package(s)

    if (check_hierarchy)
    {
        return type_is_known_in_package(
            name_utils::trim_last_component(package_name),
            check_hierarchy, type_name);
    }
    else
    {
        return false;
    }
}

bool type_is_user_defined(const std::string & package_name,
    const std::string & type_name)
{
    if (name_utils::name_is_qualified(type_name))
    {
        // qualified name lookup

        const std::string & explicit_package_name =
            name_utils::trim_last_component(type_name);
        const std::string & simple_type_name =
            name_utils::last_component(type_name);

        return type_is_known_in_package(
            explicit_package_name, false, simple_type_name);
    }
    else
    {
        // this is a simple name - search for it in the same package
        // or in parent packages

        return type_is_known_in_package(
            package_name, true, type_name);
    }
}

void verify_field_type_is_known(const std::string & package_name,
    const std::string & field_type_name)
{
    if ((type_is_primitive(field_type_name) == false) &&
        (type_is_user_defined(package_name, field_type_name) == false))
    {
        throw idl::invalid_input_error(
            "'" + field_type_name + "' - unknown field type name");
    }
}

} // unnamed namespace

const std::string structures::k_boolean = "Boolean";
const std::string structures::k_integer = "Integer";
const std::string structures::k_long_long = "Long_Long";
const std::string structures::k_float = "Float";
const std::string structures::k_string = "String";
const std::string structures::k_binary = "Binary";
const std::string structures::k_boolean_array = "Boolean_Array";
const std::string structures::k_integer_array = "Integer_Array";
const std::string structures::k_long_long_array = "Long_Long_Array";
const std::string structures::k_float_array = "Float_Array";
const std::string structures::k_string_array = "String_Array";
const std::string structures::k_binary_array = "Binary_Array";

idl::name_list structures::fully_defined_packages;

package_map structures::all_packages;

void structures::collect_import(
    const std::string & imported_package_name, bool is_explicit)
{
    if (is_explicit)
    {
        pending_explicit_imports_.push_back(imported_package_name);
    }
    else
    {
        pending_implicit_imports_.push_back(imported_package_name);
    }
}

void structures::create_package(const std::string & new_package_name)
{
    package_map::iterator it = all_packages.find(new_package_name);
    if (it != all_packages.end())
    {
        // such package was already created - check for mode conflicts:
        // - it is OK to import the already defined or already imported package
        // - it is OK to fully define the already imported package
        // - it is an error to attempt to redefine the already defined package

        package_definitions & pkg = it->second;

        if (pkg.created_as == idl::full)
        {
            if (processing_mode_ == idl::full)
            {
                throw idl::invalid_input_error(
                    "'" + new_package_name + "' - packages cannot be redefined");
            }
            else
            {
                // do not touch the existing package if it was created as full
                return;
            }
        }

        // reconstruct the existing package from scratch
        pkg.created_as = processing_mode_;
        pkg.explicit_imports = pending_explicit_imports_;
        pkg.implicit_imports = pending_implicit_imports_;
        pkg.type_definitions.clear();
        pkg.ordered_type_names.clear();
        pkg.interface_definitions.clear();
        pkg.ordered_interface_names.clear();
    }
    else
    {
        package_definitions pkg;
        pkg.created_as = processing_mode_;
        pkg.explicit_imports = pending_explicit_imports_;
        pkg.implicit_imports = pending_implicit_imports_;

        all_packages[new_package_name] = pkg;
    }

    pending_explicit_imports_.clear();
    pending_implicit_imports_.clear();
}

void structures::create_type(const std::string & package_name,
    const std::string & new_type_name)
{
    verify_type_is_not_primitive(new_type_name);

    // the package exists already

    package_map::iterator it = all_packages.find(package_name);
    package_definitions & pkg = it->second;

    type_map::iterator tit = pkg.type_definitions.find(new_type_name);
    if (tit == pkg.type_definitions.end())
    {
        pkg.type_definitions[new_type_name] = type_fields();

        pkg.ordered_type_names.push_back(new_type_name);
    }
    else
    {
        throw idl::invalid_input_error(
            "'" + new_type_name + "' - types cannot be redefined");
    }
}

void structures::create_field(const std::string & package_name,
    const std::string & enclosing_type_name,
    const std::string & new_field_name,
    const std::string & field_type_name,
    bool optional)
{
    verify_field_type_is_known(package_name, field_type_name);

    // the package exists already

    package_map::iterator it = all_packages.find(package_name);
    package_definitions & pkg = it->second;

    // the enclosing type exists, too

    type_map::iterator tit = pkg.type_definitions.find(enclosing_type_name);
    type_fields & fields = tit->second;

    field_map::iterator mit = fields.definitions.find(new_field_name);
    if (mit == fields.definitions.end())
    {
        field_definition fd;
        fd.type_name = field_type_name;
        fd.optional = optional;

        fields.definitions[new_field_name] = fd;

        fields.ordered_names.push_back(new_field_name);
    }
    else
    {
        throw idl::invalid_input_error(
            "'" + new_field_name +
            "' - duplicate field name, fields cannot be redefined");
    }
}

void structures::create_interface(const std::string & package_name,
    const std::string & new_interface_name)
{
    if (processing_mode_ == idl::import)
    {
        // no need to populate interfaces in imported packages
        return;
    }

    // the package exists already (enforced by grammar)

    package_map::iterator it = all_packages.find(package_name);
    package_definitions & pkg = it->second;

    interface_map::iterator iit = pkg.interface_definitions.find(new_interface_name);
    if (iit == pkg.interface_definitions.end())
    {
        pkg.interface_definitions[new_interface_name] = interface_messages();

        pkg.ordered_interface_names.push_back(new_interface_name);
    }
    else
    {
        throw idl::invalid_input_error(
            "'" + new_interface_name + "' - interfaces cannot be redefined");
    }
}

void structures::create_message(const std::string & package_name,
    const std::string & enclosing_interface_name,
    const std::string & new_message_name,
    bool oneway,
    const std::string & in_param_name,
    const std::string & in_param_type,
    const std::string & out_param_name,
    const std::string & out_param_type)
{
    if (processing_mode_ == idl::import)
    {
        // no need to populate interfaces in imported packages
        return;
    }

    // the package and interface exist already (enforced by grammar)
    // and parameter types were already verified as well

    package_map::iterator it = all_packages.find(package_name);
    package_definitions & pkg = it->second;

    interface_map::iterator iit =
        pkg.interface_definitions.find(enclosing_interface_name);
    interface_messages & messages = iit->second;

    message_map::iterator mit = messages.definitions.find(new_message_name);
    if (mit == messages.definitions.end())
    {
        message_definition md;
        md.oneway = oneway;
        md.in_param_name = in_param_name;
        md.in_param_type = in_param_type;
        md.out_param_name = out_param_name;
        md.out_param_type = out_param_type;

        messages.definitions[new_message_name] = md;

        messages.ordered_names.push_back(new_message_name);
    }
    else
    {
        throw idl::invalid_input_error(
            "'" + new_message_name +
            "' - duplicate message name, messages cannot be redefined " +
            "within the same interface");
    }
}

void structures::verify_param_type_is_user_defined(const std::string & package_name,
    const std::string & type_name)
{
    if (type_is_user_defined(package_name, type_name) == false)
    {
        throw idl::invalid_input_error(
            "'" + type_name + "' - unknown type name");
    }
}

void structures::finish_package(const std::string & package_name)
{
    if (processing_mode_ == idl::full)
    {
        fully_defined_packages.push_back(package_name);
    }
}

void structures::set_mode(idl::input_mode mode)
{
    processing_mode_ = mode;
}
