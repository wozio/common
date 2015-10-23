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

#include "parser.h"

#include "idl.h"
#include "name_utils.h"
#include "reader.h"
#include "structures.h"

using namespace parser;

namespace // unnamed
{

idl::input_mode processing_mode_ = idl::full;

enum processor_state_type
{
    top_level,                     // expecting import or package
    import_package_name,           // name
    import_semicolon,              // ;
    package_name,                  // name
    package_is,                    // is
    type_name,                     // name
    type_is,                       // is
    type_fields,                   // name, end
    type_field_colon,              // :
    type_field_type,               // optional, field type name
    type_field_end,                // ;
    type_end_name,                 // name
    type_semicolon,                // ;
    interface_name,                // name
    interface_is,                  // is
    interface_messages,            // oneway, message, end
    interface_message_name,        // name
    interface_message_signature,   // (, ;
    interface_message_param_name,  // name
    interface_message_param_colon, // :
    interface_message_param_mode,  // in, out
    interface_message_param_type,  // type name
    interface_message_param_end,   // ), ;
    interface_message_semicolon,   // ;
    interface_end_name,            // name
    interface_semicolon,           // ;
    package_end_name,              // name
    package_semicolon,             // ;
    finish                         // nothing is accepted in this state
};

const std::string k_import = "import";
const std::string k_package = "package";
const std::string k_type = "type";
const std::string k_interface = "interface";
const std::string k_end = "end";
const std::string k_is = "is";
const std::string k_in = "in";
const std::string k_out = "out";
const std::string k_message = "message";
const std::string k_oneway = "oneway";
const std::string k_optional = "optional";
const std::string k_colon = ":";
const std::string k_semicolon = ";";
const std::string k_open = "(";
const std::string k_close = ")";

processor_state_type processor_state_ = top_level;
bool in_package_ = false;
std::string import_pkg_name_;
std::string current_package_name_;
std::string current_type_name_;
std::string current_field_name_;
std::string current_field_type_;
bool current_field_is_optional_;
bool current_message_is_oneway_;
std::string current_interface_name_;
std::string current_message_name_;
std::string param_name_;
std::string param_mode_;
std::string in_param_name_;
std::string in_param_type_;
std::string out_param_name_;
std::string out_param_type_;

void ensure_proper_name(const std::string & token)
{
    const char first = token[0];
    const char last = token[token.size() - 1];

    if ((isalpha(first) == false) || (isalpha(last) == false))
    {
        throw idl::invalid_input_error(
            "name should not begin or end with underscore");
    }
}

void mark_parent_package_as_imported(const std::string & package_name)
{
    const std::string & parent_package_name =
        name_utils::trim_last_component(package_name);

    if (parent_package_name.empty())
    {
        return;
    }

    structures::collect_import(parent_package_name, false);

    mark_parent_package_as_imported(parent_package_name);
}

void import_parent_package(const std::string & package_name)
{
    const std::string & parent_package_name =
        name_utils::trim_last_component(package_name);

    if (parent_package_name.empty())
    {
        return;
    }

    reader::import_package(parent_package_name);

    import_parent_package(parent_package_name);
}

} // unnamed namespace

void parser::process_token(const std::string & token)
{
    switch (processor_state_)
    {
    case top_level:

        if (token == k_import)
        {
            if (in_package_ == false)
            {
                processor_state_ = import_package_name;
            }
            else
            {
                throw idl::invalid_input_error(
                    "imports are not allowed inside package");
            }
        }
        else if (token == k_package)
        {
            if (in_package_ == false)
            {
                processor_state_ = package_name;
            }
            else
            {
                throw idl::invalid_input_error("packages cannot be nested");
            }
        }
        else if (token == k_type)
        {
            if (in_package_ == false)
            {
                throw idl::invalid_input_error(
                    "cannot define types outside of package");
            }

            processor_state_ = type_name;
        }
        else if (token == k_interface)
        {
            if (in_package_ == false)
            {
                throw idl::invalid_input_error(
                    "cannot define interfaces outside of package");
            }

            processor_state_ = interface_name;
        }
        else if (token == k_end)
        {
            if (in_package_ == false)
            {
                throw idl::invalid_input_error(
                    "nothing to 'end' here (there is no package)");
            }

            processor_state_ = package_end_name;
        }
        else
        {
            throw idl::invalid_input_error("invalid token");
        }

        break;

    case import_package_name:

        ensure_proper_name(token);

        import_pkg_name_ = token;

        processor_state_ = import_semicolon;

        break;

    case import_semicolon:

        if (processing_mode_ == idl::full)
        {
            reset();
            reader::import_package(import_pkg_name_);
            structures::collect_import(import_pkg_name_, true);
            reset();
        }

        processor_state_ = top_level;

        break;

    case package_name:

        // mark parent packages as implicitly imported

        mark_parent_package_as_imported(token);

        structures::create_package(token);

        // really import all parent packages if the package is process in full mode

        if (processing_mode_ == idl::full)
        {
            import_parent_package(token);
        }

        current_package_name_ = token;

        processor_state_ = package_is;

        break;

    case package_is:

        if (token == k_is)
        {
            in_package_ = true;
            processor_state_ = top_level;
        }
        else
        {
            throw idl::invalid_input_error("'is' expected after package name");
        }

        break;

    case type_name:

        name_utils::verify_name_is_simple(token);

        current_type_name_ = token;

        structures::create_type (current_package_name_, token);

        processor_state_ = type_is;

        break;

    case type_is:

        if (token == k_is)
        {
            processor_state_ = type_fields;
        }
        else
        {
            throw idl::invalid_input_error("'is' expected after type name");
        }

        break;

    case type_fields:

        if (token == k_end)
        {
            processor_state_ = type_end_name;
        }
        else
        {
            name_utils::verify_name_is_simple(token);

            current_field_name_ = token;

            processor_state_ = type_field_colon;
        }

        break;

    case type_field_colon:

        if (token == k_colon)
        {
            current_field_is_optional_ = false;
            processor_state_ = type_field_type;
        }
        else
        {
            throw idl::invalid_input_error("':' expected after field name");
        }

        break;

    case type_field_type:

        if (token == k_optional)
        {
            current_field_is_optional_ = true;
        }
        else
        {
            current_field_type_ = token;
            processor_state_ = type_field_end;
        }

        break;

    case type_field_end:

        if (token == k_semicolon)
        {
            structures::create_field(
                current_package_name_,
                current_type_name_,
                current_field_name_,
                current_field_type_,
                current_field_is_optional_);

            processor_state_ = type_fields;
        }

        break;

    case type_end_name:

        if (token == current_type_name_)
        {
            processor_state_ = type_semicolon;
        }
        else
        {
            throw idl::invalid_input_error(
                "'" + current_type_name_ + "' expected here");
        }

        break;

    case type_semicolon:

        if (token == k_semicolon)
        {
            processor_state_ = top_level;
        }
        else
        {
            throw idl::invalid_input_error(
                "';' expected here");
        }

        break;

    case interface_name:

        name_utils::verify_name_is_simple(token);

        current_interface_name_ = token;

        structures::create_interface(current_package_name_, token);

        processor_state_ = interface_is;

        break;

    case interface_is:

        if (token == k_is)
        {
            current_message_is_oneway_ = false;
            processor_state_ = interface_messages;
        }
        else
        {
            throw idl::invalid_input_error(
                "'is' expected after interface name");
        }

        break;

    case interface_messages:

        if (token == k_oneway)
        {
            current_message_is_oneway_ = true;
        }
        else if (token == k_message)
        {
            processor_state_ = interface_message_name;
        }
        else if (token == k_end)
        {
            processor_state_ = interface_end_name;
        }
        else
        {
            throw idl::invalid_input_error(
                "'oneway' or 'message' or 'end' expected here");
        }

        break;

    case interface_message_name:

        name_utils::verify_name_is_simple(token);

        current_message_name_ = token;

        in_param_name_.clear();
        out_param_name_.clear();

        processor_state_ = interface_message_signature;

        break;

    case interface_message_signature:

        if (token == k_open)
        {
            processor_state_ = interface_message_param_name;
        }
        else if (token == k_semicolon)
        {
            structures::create_message(
                current_package_name_,
                current_interface_name_,
                current_message_name_,
                current_message_is_oneway_,
                in_param_name_,
                in_param_type_,
                out_param_name_,
                out_param_type_);

            current_message_is_oneway_ = false;
            processor_state_ = interface_messages;
        }
        else
        {
            throw idl::invalid_input_error(
                "'(' for the message signature or ';' expected here");
        }

        break;

    case interface_message_param_name:

        name_utils::verify_name_is_simple(token);

        param_name_ = token;

        processor_state_ = interface_message_param_colon;

        break;

    case interface_message_param_colon:

        if (token == k_colon)
        {
            processor_state_ = interface_message_param_mode;
        }
        else
        {
            throw idl::invalid_input_error("':' expected here.");
        }

        break;

    case interface_message_param_mode:

        if (token == k_in)
        {
            if (in_param_name_.empty() == false)
            {
                throw idl::invalid_input_error(
                    "parameter of 'in' mode was already specified "
                    "for this message");
            }
            else
            {
                param_mode_ = token;
            }
        }
        else if (token == k_out)
        {
            if (current_message_is_oneway_)
            {
                throw idl::invalid_input_error(
                    "oneway messages cannot have out parameters");
            }
            else if (out_param_name_.empty() == false)
            {
                throw idl::invalid_input_error(
                    "parameter of 'out' mode was already specified "
                    "for this message");
            }
            else
            {
                param_mode_ = token;
            }
        }
        else
        {
            throw idl::invalid_input_error(
                "'in' or 'out' parameter mode expected here");
        }

        processor_state_ = interface_message_param_type;

        break;

    case interface_message_param_type:

        if (processing_mode_ == idl::full)
        {
            structures::verify_param_type_is_user_defined(
                current_package_name_, token);
        }

        if (param_mode_ == k_in)
        {
            in_param_name_ = param_name_;
            in_param_type_ = token;
        }
        else // mode == out
        {
            out_param_name_ = param_name_;
            out_param_type_ = token;
        }

        processor_state_ = interface_message_param_end;

        break;

    case interface_message_param_end:

        if (token == k_semicolon)
        {
            processor_state_ = interface_message_param_name;
        }
        else if (token == k_close)
        {
            processor_state_ = interface_message_semicolon;
        }
        else
        {
            throw idl::invalid_input_error(
                "';' or ')' expected after parameter specification");
        }

        break;

    case interface_message_semicolon:

        if (token == k_semicolon)
        {
            structures::create_message(
                current_package_name_,
                current_interface_name_,
                current_message_name_,
                current_message_is_oneway_,
                in_param_name_,
                in_param_type_,
                out_param_name_,
                out_param_type_);

            current_message_is_oneway_ = false;
            processor_state_ = interface_messages;
        }
        else
        {
            throw idl::invalid_input_error(
                "';' expected at the end of message definition");
        }

        break;

    case interface_end_name:

        if (token == current_interface_name_)
        {
            processor_state_ = type_semicolon;
        }
        else
        {
            throw idl::invalid_input_error(
                "'" + current_interface_name_ + "' expected here.");
        }

        break;

    case interface_semicolon:

        if (token == k_semicolon)
        {
            processor_state_ = top_level;
        }
        else
        {
            throw idl::invalid_input_error(
                "';' expected after interface definition");
        }

        break;

    case package_end_name:

        if (token == current_package_name_)
        {
            processor_state_ = package_semicolon;
        }
        else
        {
            throw idl::invalid_input_error(
                "'" + current_package_name_ + "' expected here");
        }

        break;

    case package_semicolon:

        if (token == k_semicolon)
        {
            processor_state_ = finish;
            in_package_ = false;

            structures::finish_package(current_package_name_);
        }
        else
        {
            throw idl::invalid_input_error(
                "';' expected after package definition");
        }

        break;

    case finish:

        throw idl::invalid_input_error(
            "no text is expected after package definition");

        break;
    }
}

bool parser::finished()
{
    return processor_state_ == finish;
}

void parser::reset()
{
    processor_state_ = top_level;
    in_package_ = false;
}

void parser::set_mode(idl::input_mode mode)
{
    processing_mode_ = mode;
}
