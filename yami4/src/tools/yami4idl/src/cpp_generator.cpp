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

#include "cpp_generator.h"
#include "name_utils.h"
#include "structures.h"

#include <boost/algorithm/string/case_conv.hpp>
#include <boost/filesystem.hpp>

#include <cctype>
#include <cstdio>

using namespace structures;
using namespace structures::cpp_generator;

namespace // unnamed
{

idl::casing_mode casing_;

enum mode_type {client, server};

std::string base_file_name(
    const std::string & output_dir, const std::string & package_name)
{
    std::string base_name;
    for (std::size_t i = 0; i != package_name.size(); ++i)
    {
        char c = package_name[i];

        if (c == '.')
        {
            c = '-';
        }
        else if (isupper(c))
        {
            c = tolower(c);
        }

        base_name += c;
    }

    boost::filesystem::path p =
        boost::filesystem::path(output_dir) / boost::filesystem::path(base_name);

    return p.string();
}

std::string header_file_name(
    const std::string & output_dir, const std::string & package_name)
{
    return base_file_name(output_dir, package_name) + ".h";
}

std::string impl_file_name(
    const std::string & output_dir, const std::string & package_name)
{
    return base_file_name(output_dir, package_name) + ".cpp";
}

std::string cpp_name(const std::string & idl_name)
{
    std::string cpp;
    bool new_word = true;

    for (std::size_t i = 0; i != idl_name.size(); ++i)
    {
        char c = idl_name[i];

        if (c != '.')
        {
            switch (casing_)
            {
            case idl::ident:
                cpp += c;
                break;
            case idl::lower_case:
            case idl::default_casing:
                cpp += std::tolower(c);
                break;
            case idl::camel_case:
                if (c == '_')
                {
                    new_word = true;
                }
                else
                {
                    if (new_word)
                    {
                        cpp += std::toupper(c);
                    }
                    else
                    {
                        cpp += std::tolower(c);
                    }

                    new_word = false;
                }

                break;
            }
        }
        else
        {
            cpp += "::";
            new_word = true;
        }
    }

    return cpp;
}

std::string cpp_field_valid_name(const std::string & idl_name)
{
    return cpp_name(idl_name + "_Valid");
}

std::string yami4_field_name(const std::string & idl_name)
{
    // "communication" field names are always lower_case with underscore

    std::string s(idl_name);
    boost::algorithm::to_lower(s);

    return s;
}

std::string yami4_operation_name(const std::string & idl_name)
{
    // "communication" messages names are always lower_case with underscore

    std::string s(idl_name);
    boost::algorithm::to_lower(s);

    return s;
}

std::string cpp_type(const std::string & idl_type)
{
    if (idl_type == k_boolean)
    {
        return "bool";
    }
    else if (idl_type == k_integer)
    {
        return "int";
    }
    else if (idl_type == k_long_long)
    {
        return "long long";
    }
    else if (idl_type == k_float)
    {
        return "double";
    }
    else if (idl_type == k_string)
    {
        return "std::string";
    }
    else if (idl_type == k_binary)
    {
        return "std::vector<char>";
    }
    else if (idl_type == k_boolean_array)
    {
        return "std::vector<bool>";
    }
    else if (idl_type == k_integer_array)
    {
        return "std::vector<int>";
    }
    else if (idl_type == k_long_long_array)
    {
        return "std::vector<long long>";
    }
    else if (idl_type == k_float_array)
    {
        return "std::vector<double>";
    }
    else if (idl_type == k_string_array)
    {
        return "std::vector<std::string>";
    }
    else if (idl_type == k_binary_array)
    {
        return "std::vector<std::vector<char> >";
    }
    else
    {
        // user-defined type
         
        return cpp_name (idl_type);
    }
}

std::string header_guard(const std::string & package_name)
{
    std::string pkg(package_name);
    boost::algorithm::to_upper(pkg);

    std::string guard = "YAMI4_IDL_" + pkg + "_H_INCLUDED";

    for (std::size_t i = 0; i != guard.size(); ++i)
    {
        char c = guard[i];
        if (c == '.')
        {
            guard[i] = '_';
        }
    }

    return guard;
}

void put_operation_signature(std::FILE * file, const message_definition & md)
{
    std::fprintf(file, "(");

    if (md.in_param_name.empty() == false)
    {
        std::fprintf(file, "const %s & %s",
            cpp_name(md.in_param_type).c_str(),
            cpp_name(md.in_param_name).c_str());

        if (md.out_param_name.empty() == false)
        {
            std::fprintf(file, ", ");
        }
    }

    if (md.out_param_name.empty() == false)
    {
        std::fprintf(file, "%s & %s",
            cpp_name(md.out_param_type).c_str(),
            cpp_name(md.out_param_name).c_str());
    }

    std::fprintf(file, ")");
}

void write_header_interface(std::FILE * file,
    const interface_messages & im, mode_type mode)
{
    idl::name_list::const_iterator mit = im.ordered_names.begin();
    idl::name_list::const_iterator mend = im.ordered_names.end();
    for ( ; mit != mend; ++mit)
    {
        const std::string & message_name = *mit;

        message_map::const_iterator mdit = im.definitions.find(message_name);

        const message_definition & md = mdit->second;

        if (mode == server)
        {
            std::fprintf(file, "    virtual ");
        }
        else
        {
            std::fprintf(file, "    ");
        }
        
        std::fprintf(file, "void %s", cpp_name(message_name).c_str());

        put_operation_signature(file, md);

        if (mode == server)
        {
            std::fprintf(file, " = 0;\n");
        }
        else
        {
            std::fprintf(file, ";\n");
        }
    }
}

void wrap_in_namespaces(std::FILE * file, const std::string & package_name,
    const package_definitions & pkg)
{
    // strip external package name and turn it into namespace

    const std::string & outer_package =
        name_utils::first_component(package_name, true);
    const std::string & cpp_outer_package = cpp_name(outer_package);

    const std::string & inner_packages =
        name_utils::trim_first_component(package_name, false);

    std::fprintf(file, "namespace %s\n", cpp_outer_package.c_str());
    std::fprintf(file, "{\n");
    std::fprintf(file, "\n");

    if (inner_packages.empty() == false)
    {
        wrap_in_namespaces(file, inner_packages, pkg);
    }
    else
    {
        idl::name_list::const_iterator tit = pkg.ordered_type_names.begin();
        idl::name_list::const_iterator tend = pkg.ordered_type_names.end();
        for ( ; tit != tend; ++tit)
        {
            const std::string & type_name = *tit;
            const std::string & cpp_type_name = cpp_name(type_name);

            std::fprintf(file, "struct %s\n", cpp_type_name.c_str());
            std::fprintf(file, "{\n");
            std::fprintf(file, "    %s();\n", cpp_type_name.c_str());
            std::fprintf(file, "\n");
            std::fprintf(file, "    void write(yami::parameters & params) const;\n");
            std::fprintf(file, "    void read(const yami::parameters & params);\n");
            std::fprintf(file, "\n");

            type_map::const_iterator tfit = pkg.type_definitions.find(type_name);

            const type_fields & tf = tfit->second;

            idl::name_list::const_iterator fit = tf.ordered_names.begin();
            idl::name_list::const_iterator fend = tf.ordered_names.end();
            for ( ; fit != fend; ++fit)
            {
                const std::string & field_name = *fit;

                field_map::const_iterator fdit = tf.definitions.find(field_name);

                const field_definition & fd = fdit->second;

                if (fd.optional)
                {
                    std::fprintf(file, "    bool %s;\n",
                        cpp_field_valid_name(field_name).c_str());
                }

                std::fprintf(file, "    %s %s;\n",
                    cpp_type(fd.type_name).c_str(),
                    cpp_name(field_name).c_str());
            }

            std::fprintf(file, "};\n");
            std::fprintf(file, "\n");
        }

        idl::name_list::const_iterator iit = pkg.ordered_interface_names.begin();
        idl::name_list::const_iterator iend = pkg.ordered_interface_names.end();
        for ( ; iit != iend; ++iit)
        {
            const std::string & interface_name = *iit;
            const std::string & cpp_interface_name = cpp_name(interface_name);

            std::fprintf(file, "class %s\n", cpp_interface_name.c_str());
            std::fprintf(file, "{\n");
            std::fprintf(file, "public:\n");
            std::fprintf(file, "\n");
            std::fprintf(file, "    %s(yami::agent & client_agent,\n",
                cpp_interface_name.c_str());
            std::fprintf(file, "        const std::string & server_location, const std::string & object_name,\n");
            std::fprintf(file, "        int timeout = 0);\n");
            std::fprintf(file, "\n");

            interface_map::const_iterator imit =
                pkg.interface_definitions.find(interface_name);
            const interface_messages & im = imit->second;

            write_header_interface(file, im, client);

            std::fprintf(file, "\n");
            std::fprintf(file, "private:\n");
            std::fprintf(file, "\n");
            std::fprintf(file, "    yami::agent & agent_;\n");
            std::fprintf(file, "    const std::string server_location_;\n");
            std::fprintf(file, "    const std::string object_name_;\n");
            std::fprintf(file, "    const std::size_t timeout_;\n");
            std::fprintf(file, "};\n");
            std::fprintf(file, "\n");
            std::fprintf(file, "class %s\n",
                cpp_name(interface_name + "_Server").c_str());
            std::fprintf(file, "{\n");
            std::fprintf(file, "public:\n");
            std::fprintf(file, "\n");
            std::fprintf(file, "    virtual ~%s() {}\n", cpp_name(interface_name + "_Server").c_str());
            std::fprintf(file, "\n");

            write_header_interface(file, im, server);
            
            std::fprintf(file, "\n");
            std::fprintf(file, "    void operator()(yami::incoming_message & im_);\n");
            std::fprintf(file, "};\n");
            std::fprintf(file, "\n");
        }
    }

    std::fprintf(file, "} // namespace %s\n", cpp_outer_package.c_str());
    std::fprintf(file, "\n");
}

void write_header(std::FILE * file,
    const std::string & output_dir, const std::string & package_name)
{
    const std::string & guard = header_guard(package_name);

    std::fprintf(file, "//\n");
    std::fprintf(file, "// C++ type definitions for package %s.\n", package_name.c_str());
    std::fprintf(file, "// This file was generated automatically by yami4idl.\n");
    std::fprintf(file, "//\n");
    std::fprintf(file, "\n");
    std::fprintf(file, "#ifndef %s\n", guard.c_str());
    std::fprintf(file, "#define %s\n", guard.c_str());
    std::fprintf(file, "\n");
    std::fprintf(file, "#include <yami4-cpp/parameters.h>\n");
    std::fprintf(file, "#include <string>\n");
    std::fprintf(file, "#include <vector>\n");
    std::fprintf(file, "\n");
    std::fprintf(file, "namespace yami\n");
    std::fprintf(file, "{\n");
    std::fprintf(file, "    class agent;\n");
    std::fprintf(file, "    class incoming_message;\n");
    std::fprintf(file, "}\n");
    std::fprintf(file, "\n");

    package_map::const_iterator it = all_packages.find(package_name);
    const package_definitions & pkg = it->second;

    // process explicit imports

    bool some_imports_added = false;

    idl::name_list::const_iterator imit = pkg.explicit_imports.begin();
    idl::name_list::const_iterator imend = pkg.explicit_imports.end();

    for ( ; imit != imend; ++imit)
    {
        const std::string & import_name = *imit;

        std::fprintf(file, "#include \"%s\"\n",
            header_file_name(output_dir, import_name).c_str());

        some_imports_added = true;
    }

    // process implicit imports

    imit = pkg.implicit_imports.begin();
    imend = pkg.implicit_imports.end();

    for ( ; imit != imend; ++imit)
    {
        const std::string & import_name = *imit;

        std::fprintf(file, "#include \"%s\"\n",
            header_file_name(output_dir, import_name).c_str());

        some_imports_added = true;
    }

    if (some_imports_added)
    {
        std::fprintf(file, "\n");
    }

    wrap_in_namespaces(file, package_name, pkg);

    std::fprintf(file, "#endif // %s\n", guard.c_str());
}

void write_implementation_type_cleaner(std::FILE * file, const type_fields & tf)
{
    idl::name_list::const_iterator fit = tf.ordered_names.begin();
    idl::name_list::const_iterator fend = tf.ordered_names.end();
    for ( ; fit != fend; ++fit)
    {
        const std::string & field_name = *fit;

        field_map::const_iterator fdit = tf.definitions.find(field_name);

        const field_definition & fd = fdit->second;

        if (fd.optional)
        {
            std::fprintf(file, "    %s = false;\n",
                cpp_field_valid_name(field_name).c_str());
        }
    }
}

void write_implementation_type_writer(std::FILE * file, const type_fields & tf)
{
    idl::name_list::const_iterator fit = tf.ordered_names.begin();
    idl::name_list::const_iterator fend = tf.ordered_names.end();
    for ( ; fit != fend; ++fit)
    {
        const std::string & field_name = *fit;

        field_map::const_iterator fdit = tf.definitions.find(field_name);

        const field_definition & fd = fdit->second;

        const std::string & type_name = fd.type_name;

        if (fd.optional)
        {
            std::fprintf(file, "    if (%s)\n",
                cpp_field_valid_name(field_name).c_str());
            std::fprintf(file, "    {\n");
        }

        if (type_name == k_boolean)
        {
            std::fprintf(file, "    params.set_boolean(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                cpp_name(field_name).c_str());
        }
        else if (type_name == k_integer)
        {
            std::fprintf(file, "    params.set_integer(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                cpp_name(field_name).c_str());
        }
        else if (type_name == k_long_long)
        {
            std::fprintf(file, "    params.set_long_long(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                cpp_name(field_name).c_str());
        }
        else if (type_name == k_float)
        {
            std::fprintf(file, "    params.set_double_float(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                cpp_name(field_name).c_str());
        }
        else if (type_name == k_string)
        {
            std::fprintf(file, "    params.set_string_shallow(\"%s\",\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        %s.c_str(), %s.size());\n",
                cpp_name(field_name).c_str(),
                cpp_name(field_name).c_str());
        }
        else if (type_name == k_binary)
        {
            std::fprintf(file, "    params.set_binary_shallow(\"%s\",\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        &%s[0], %s.size());\n",
                cpp_name(field_name).c_str(),
                cpp_name(field_name).c_str());
        }
        else if (type_name == k_boolean_array)
        {
            std::fprintf(file, "    {\n");
            std::fprintf(file, "        std::size_t size_ = %s.size();\n",
                cpp_name(field_name).c_str());
            std::fprintf(file, "        bool * tmp_ = new bool[size_];\n");
            std::fprintf(file, "        for (std::size_t i_ = 0; i_ != size_; ++i_)\n");
            std::fprintf(file, "        {\n");
            std::fprintf(file, "            tmp_[i_] = %s[i_];\n",
                cpp_name(field_name).c_str());
            std::fprintf(file, "        }\n");
            std::fprintf(file, "        params.set_boolean_array(\"%s\", tmp_, size_);\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        delete [] tmp_;\n");
            std::fprintf(file, "    }\n");
        }
        else if (type_name == k_integer_array)
        {
            std::fprintf(file, "    params.set_integer_array_shallow(\"%s\",\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        &%s[0], %s.size());\n",
                cpp_name(field_name).c_str(),
                cpp_name(field_name).c_str());
        }
        else if (type_name == k_long_long_array)
        {
            std::fprintf(file, "    params.set_long_long_array_shallow(\"%s\",\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        &%s[0], %s.size());\n",
                cpp_name(field_name).c_str(),
                cpp_name(field_name).c_str());
        }
        else if (type_name == k_float_array)
        {
            std::fprintf(file, "    params.set_double_float_array_shallow(\"%s\",\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        &%s[0], %s.size());\n",
                cpp_name(field_name).c_str(),
                cpp_name(field_name).c_str());
        }
        else if (type_name == k_string_array)
        {
            std::fprintf(file, "    {\n");
            std::fprintf(file, "        std::size_t size_ = %s.size();\n",
                cpp_name(field_name).c_str());
            std::fprintf(file, "        params.create_string_array(\"%s\", size_);\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        for (std::size_t i_ = 0; i_ != size_; ++i_)\n");
            std::fprintf(file, "        {\n");
            std::fprintf(file, "            params.set_string_in_array(\"%s\", i_, %s[i_]);\n",
                yami4_field_name(field_name).c_str(),
                cpp_name(field_name).c_str());
            std::fprintf(file, "        }\n");
            std::fprintf(file, "    }\n");
        }
        else if (type_name == k_binary_array)
        {
            std::fprintf(file, "    {\n");
            std::fprintf(file, "        std::size_t size_ = %s.size();\n",
                cpp_name(field_name).c_str());
            std::fprintf(file, "        params.create_binary_array(\"%s\", size_);\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        for (std::size_t i_ = 0; i_ != size_; ++i_)\n");
            std::fprintf(file, "        {\n");
            std::fprintf(file, "            params.set_binary_in_array(\n");
            std::fprintf(file, "                \"%s\", i_, &%s[i_][0], %s[i_].size());\n",
                yami4_field_name(field_name).c_str(),
                cpp_name(field_name).c_str(),
                cpp_name(field_name).c_str());
            std::fprintf(file, "        }\n");
            std::fprintf(file, "    }\n");
        }
        else
        {
            // user-defined type
                        
            const std::string & nested = cpp_name(field_name) + "_nested";
            
            std::fprintf(file, "    yami::parameters %s(params.create_nested_parameters(\"%s\"));\n",
                nested.c_str(), yami4_field_name(field_name).c_str());
                           
            std::fprintf(file, "    %s.write(%s);\n",
                cpp_name(field_name).c_str(), nested.c_str());
        }

        if (fd.optional)
        {
            std::fprintf(file, "    }\n");
        }
    }
}

void write_implementation_type_reader(std::FILE * file, const type_fields & tf)
{
    bool param_entry_already_defined = false;

    idl::name_list::const_iterator fit = tf.ordered_names.begin();
    idl::name_list::const_iterator fend = tf.ordered_names.end();
    for ( ; fit != fend; ++fit)
    {
        const std::string & field_name = *fit;

        field_map::const_iterator fdit = tf.definitions.find(field_name);

        const field_definition & fd = fdit->second;

        const std::string & type_name = fd.type_name;

        if (fd.optional)
        {
            if (param_entry_already_defined == false)
            {
                std::fprintf(file, "    yami::parameter_entry e_;\n");

                param_entry_already_defined = true;
            }
            
            std::fprintf(file, "    %s = params.find(\"%s\", e_);\n",
                cpp_field_valid_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());

            std::fprintf(file, "    if (%s)\n",
                cpp_field_valid_name(field_name).c_str());

            std::fprintf(file, "    {\n");
        }

        if (type_name == k_boolean)
        {
            std::fprintf(file, "    %s = params.get_boolean(\"%s\");\n",
                cpp_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_integer)
        {
            std::fprintf(file, "    %s = params.get_integer(\"%s\");\n",
                cpp_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_long_long)
        {
            std::fprintf(file, "    %s = params.get_long_long(\"%s\");\n",
                cpp_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_float)
        {
            std::fprintf(file, "    %s = params.get_double_float(\"%s\");\n",
                cpp_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_string)
        {
            std::fprintf(file, "    %s = params.get_string(\"%s\");\n",
                cpp_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_binary)
        {
            std::fprintf(file, "    {\n");
            std::fprintf(file, "        std::size_t size_;\n");
            std::fprintf(file, "        const char * buf_ = reinterpret_cast<const char *>(\n");
            std::fprintf(file, "            params.get_binary(\"%s\", size_));\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        %s.assign(buf_, buf_ + size_);\n",
                cpp_name(field_name).c_str());
            std::fprintf(file, "    }\n");
        }
        else if (type_name == k_boolean_array)
        {
            std::fprintf(file, "    {\n");
            std::fprintf(file, "        std::size_t size_;\n");
            std::fprintf(file, "        const bool * buf_ = params.get_boolean_array(\n");
            std::fprintf(file, "            \"%s\", size_);\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        %s.assign(buf_, buf_ + size_);\n",
                cpp_name(field_name).c_str());
            std::fprintf(file, "    }\n");
        }
        else if (type_name == k_integer_array)
        {
            std::fprintf(file, "    {\n");
            std::fprintf(file, "        std::size_t size_;\n");
            std::fprintf(file, "        const int * buf_ = params.get_integer_array(\n");
            std::fprintf(file, "            \"%s\", size_);\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        %s.assign(buf_, buf_ + size_);\n",
                cpp_name(field_name).c_str());
            std::fprintf(file, "    }\n");
        }
        else if (type_name == k_long_long_array)
        {
            std::fprintf(file, "    {\n");
            std::fprintf(file, "        std::size_t size_;\n");
            std::fprintf(file, "        const long long * buf_ = params.get_long_long_array(\n");
            std::fprintf(file, "            \"%s\", size_);\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        %s.assign(buf_, buf_ + size_);\n",
                cpp_name(field_name).c_str());
            std::fprintf(file, "    }\n");
        }
        else if (type_name == k_float_array)
        {
            std::fprintf(file, "    {\n");
            std::fprintf(file, "        std::size_t size_;\n");
            std::fprintf(file, "        const double * buf_ = params.get_double_float_array(\n");
            std::fprintf(file, "            \"%s\", size_);\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        %s.assign(buf_, buf_ + size_);\n",
                cpp_name(field_name).c_str());
            std::fprintf(file, "    }\n");
        }
        else if (type_name == k_string_array)
        {
            std::fprintf(file, "    {\n");
            std::fprintf(file, "        std::size_t size_ = params.get_string_array_length(\"%s\");\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        %s.resize(size_);\n",
                cpp_name(field_name).c_str());
            std::fprintf(file, "        for (std::size_t i_ = 0; i_ != size_; ++i_)\n");
            std::fprintf(file, "        {\n");
            std::fprintf(file, "            %s[i_] = params.get_string_in_array(\"%s\", i_);\n",
                cpp_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        }\n");
            std::fprintf(file, "    }\n");
        }
        else if (type_name == k_binary_array)
        {
            std::fprintf(file, "    {\n");
            std::fprintf(file, "        std::size_t size_ = params.get_binary_array_length(\"%s\");\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        %s.resize(size_);\n",
                cpp_name(field_name).c_str());
            std::fprintf(file, "        for (std::size_t i_ = 0; i_ != size_; ++i_)\n");
            std::fprintf(file, "        {\n");
            std::fprintf(file, "            std::size_t bufSize_;\n");
            std::fprintf(file, "            const char * buf_ = reinterpret_cast<const char *>(\n");
            std::fprintf(file, "                params.get_binary_in_array(\"%s\", i_, bufSize_));\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "            %s[i_].assign(buf_, buf_ + bufSize_);\n",
                cpp_name(field_name).c_str());
            std::fprintf(file, "        }\n");
            std::fprintf(file, "    }\n");
        }
        else
        {
            // user-defined type
                        
            const std::string & nested = cpp_name(field_name) + "_nested";
            
            std::fprintf(file, "    yami::parameters %s(params.get_nested_parameters(\"%s\"));\n",
                nested.c_str(), yami4_field_name(field_name).c_str());
                           
            std::fprintf(file, "    %s.read(%s);\n",
                cpp_name(field_name).c_str(), nested.c_str());
        }

        if (fd.optional)
        {
            std::fprintf(file, "    }\n");
        }
    }
}

void write_implementation_messages(std::FILE * file,
    const std::string & interface_name,
    const interface_messages & im, mode_type mode)
{
    idl::name_list::const_iterator mit = im.ordered_names.begin();
    idl::name_list::const_iterator mend = im.ordered_names.end();
    for ( ; mit != mend; ++mit)
    {
        const std::string & message_name = *mit;

        message_map::const_iterator mdit = im.definitions.find(message_name);

        const message_definition & md = mdit->second;

        if (mode == client)
        {
            std::fprintf(file, "void %s::%s",
                cpp_name(interface_name).c_str(),
                cpp_name(message_name).c_str());
                        
            put_operation_signature(file, md);

            std::fprintf(file, "\n");

            std::fprintf(file, "{\n");
            
            if (md.in_param_name.empty() == false)
            {
                std::fprintf(file, "    yami::parameters %s_;\n",
                    cpp_name(md.in_param_name).c_str());
                std::fprintf(file, "    %s.write(%s_);\n",
                    cpp_name(md.in_param_name).c_str(),
                    cpp_name(md.in_param_name).c_str());
            }

            if (md.oneway)
            {
                if (md.in_param_name.empty() == false)
                {
                    std::fprintf(file, "    agent_.send_one_way(server_location_, object_name_, \"%s\", %s_);\n",
                        yami4_operation_name(message_name).c_str(),
                        cpp_name(md.in_param_name).c_str());
                }
                else
                {
                    std::fprintf(file, "    agent_.send_one_way(server_location_, object_name_, \"%s\");\n",
                        yami4_operation_name(message_name).c_str());
                }
            }
            else
            {
                if (md.in_param_name.empty() == false)
                {
                    std::fprintf(file, "    std::auto_ptr<yami::outgoing_message> om_(\n");
                    std::fprintf(file, "        agent_.send(server_location_, object_name_, \"%s\", %s_));\n",
                        yami4_operation_name(message_name).c_str(),
                        cpp_name(md.in_param_name).c_str());
                }
                else
                {
                    std::fprintf(file, "    std::auto_ptr<yami::outgoing_message> om_(\n");
                    std::fprintf(file, "        agent_.send(server_location_, object_name_, \"%s\"));\n",
                        yami4_operation_name(message_name).c_str());
                }

                std::fprintf(file, "\n");
                std::fprintf(file, "    if (timeout_ != 0)\n");
                std::fprintf(file, "    {\n");
                std::fprintf(file, "        bool on_time_ = om_->wait_for_completion(timeout_);\n");
                std::fprintf(file, "        if (on_time_ == false)\n");
                std::fprintf(file, "        {\n");
                std::fprintf(file, "            throw yami::yami_runtime_error(\"Operation timed out.\");\n");
                std::fprintf(file, "        }\n");
                std::fprintf(file, "    }\n");
                std::fprintf(file, "    else\n");
                std::fprintf(file, "    {\n");
                std::fprintf(file, "        om_->wait_for_completion();\n");
                std::fprintf(file, "    }\n");
                std::fprintf(file, "\n");
                std::fprintf(file, "    const yami::message_state state_ = om_->get_state();\n");
                std::fprintf(file, "    switch (state_)\n");
                std::fprintf(file, "    {\n");
                std::fprintf(file, "    case yami::replied:\n");

                if (md.out_param_name.empty() == false)
                {
                    std::fprintf(file, "        %s.read(om_->get_reply());\n",
                        cpp_name(md.out_param_name).c_str());
                }

                std::fprintf(file, "        break;\n");
                std::fprintf(file, "    case yami::abandoned:\n");
                std::fprintf(file, "        throw yami::yami_runtime_error(\n");
                std::fprintf(file, "            \"Operation was abandoned due to communication errors.\");\n");
                std::fprintf(file, "    case yami::rejected:\n");
                std::fprintf(file, "        throw yami::yami_runtime_error(\n");
                std::fprintf(file, "            \"Operation was rejected: \" + om_->get_exception_msg());\n");
                std::fprintf(file, "\n");
                std::fprintf(file, "    // these are for completeness:\n");
                std::fprintf(file, "    case yami::posted:\n");
                std::fprintf(file, "    case yami::transmitted:\n");
                std::fprintf(file, "        break;\n");
                std::fprintf(file, "    }\n");
            }

            std::fprintf(file, "}\n");
            std::fprintf(file, "\n");
        }
        else // server
        {
            std::fprintf(file, "    if (msg_name_ == \"%s\")\n",
                yami4_operation_name(message_name).c_str());
            std::fprintf(file, "    {\n");
                        
            if (md.in_param_name.empty() == false)
            {
                std::fprintf(file, "        %s %s;\n",
                    cpp_name(md.in_param_type).c_str(),
                    cpp_name(md.in_param_name).c_str());
                std::fprintf(file, "        %s.read(im_.get_parameters());\n",
                    cpp_name(md.in_param_name).c_str());
            }

            if (md.out_param_name.empty() == false)
            {
                std::fprintf(file, "        %s %s;\n",
                    cpp_name(md.out_param_type).c_str(),
                    cpp_name(md.out_param_name).c_str());
            }

            if ((md.in_param_name.empty() == false) ||
                (md.out_param_name.empty() == false))
            {
                std::fprintf(file, "\n");
            }
                        
            std::fprintf(file, "        %s(", cpp_name(message_name).c_str());

            if (md.in_param_name.empty() == false)
            {
                std::fprintf(file, "%s", cpp_name(md.in_param_name).c_str());
                if (md.out_param_name.empty() == false)
                {
                    std::fprintf(file, ", ");
                }
            }
                        
            if (md.out_param_name.empty() == false)
            {
                std::fprintf(file, "%s", cpp_name(md.out_param_name).c_str());
            }
                        
            std::fprintf(file, ");\n");
    
            if (md.oneway == false)
            {
                std::fprintf(file, "\n");
                           
                if (md.out_param_name.empty() == false)
                {
                    std::fprintf(file, "        yami::parameters %s_;\n",
                        cpp_name(md.out_param_name).c_str());
                    std::fprintf(file, "        %s.write(%s_);\n",
                        cpp_name(md.out_param_name).c_str(),
                        cpp_name(md.out_param_name).c_str());
                    std::fprintf(file, "        im_.reply(%s_);\n",
                        cpp_name(md.out_param_name).c_str());
                }
                else
                {
                    std::fprintf(file, "        im_.reply();\n");
                }
            }
                        
            std::fprintf(file, "    }\n");
            std::fprintf(file, "    else\n");
        }
    }
}

void write_implementation(std::FILE * file, const std::string & package_name)
{
    const std::string & cpp_package_name = cpp_name(package_name);

    std::fprintf(file, "//\n");
    std::fprintf(file, "// C++ implementations for package %s.\n",
        package_name.c_str());
    std::fprintf(file, "// This file was generated automatically by yami4idl.\n");
    std::fprintf(file, "//\n");
    std::fprintf(file, "\n");
    std::fprintf(file, "#include \"%s\"\n", header_file_name("", package_name).c_str());
    std::fprintf(file, "\n");
    std::fprintf(file, "#include <yami4-cpp/agent.h>\n");
    std::fprintf(file, "#include <yami4-cpp/errors.h>\n");
    std::fprintf(file, "#include <yami4-cpp/incoming_message.h>\n");
    std::fprintf(file, "#include <yami4-cpp/outgoing_message.h>\n");
    std::fprintf(file, "\n");
    std::fprintf(file, "using namespace %s;\n", cpp_package_name.c_str());
    std::fprintf(file, "\n");

    package_map::const_iterator it = all_packages.find(package_name);
    const package_definitions & pkg = it->second;

    idl::name_list::const_iterator tit = pkg.ordered_type_names.begin();
    idl::name_list::const_iterator tend = pkg.ordered_type_names.end();
    for ( ; tit != tend; ++tit)
    {
        const std::string & type_name = *tit;
        const std::string & cpp_type_name = cpp_name(type_name);

        type_map::const_iterator tfit = pkg.type_definitions.find(type_name);

        const type_fields & tf = tfit->second;

        std::fprintf(file, "%s::%s()\n",
            cpp_type_name.c_str(), cpp_type_name.c_str());
        std::fprintf(file, "{\n");

        write_implementation_type_cleaner(file, tf);

        std::fprintf(file, "}\n");
        std::fprintf(file, "\n");
        std::fprintf(file, "void %s::write(yami::parameters & params) const\n",
            cpp_type_name.c_str());
        std::fprintf(file, "{\n");

        write_implementation_type_writer(file, tf);

        std::fprintf(file, "}\n");
        std::fprintf(file, "\n");
        std::fprintf(file, "void %s::read(const yami::parameters & params)\n",
            cpp_type_name.c_str());
        std::fprintf(file, "{\n");

        write_implementation_type_reader(file, tf);

        std::fprintf(file, "}\n");
        std::fprintf(file, "\n");
    }

    idl::name_list::const_iterator iit = pkg.ordered_interface_names.begin();
    idl::name_list::const_iterator iend = pkg.ordered_interface_names.end();
    for ( ; iit != iend; ++iit)
    {
        const std::string & interface_name = *iit;
        const std::string & cpp_interface_name = cpp_name(interface_name);

        interface_map::const_iterator imit =
            pkg.interface_definitions.find(interface_name);
        const interface_messages & im = imit->second;

        std::fprintf(file, "%s::%s(yami::agent & client_agent,\n",
            cpp_interface_name.c_str(), cpp_interface_name.c_str());
        std::fprintf(file, "    const std::string & server_location, const std::string & object_name,\n");
        std::fprintf(file, "    int timeout)\n");
        std::fprintf(file, "    : agent_(client_agent),\n");
        std::fprintf(file, "    server_location_(server_location),\n");
        std::fprintf(file, "    object_name_(object_name),\n");
        std::fprintf(file, "    timeout_(timeout)\n");
        std::fprintf(file, "{\n");
        std::fprintf(file, "}\n");
        std::fprintf(file, "\n");

        write_implementation_messages(file, interface_name, im, client);

        std::fprintf(file, "void %s::operator()(yami::incoming_message & im_)\n",
            cpp_name(interface_name + "_Server").c_str());
        std::fprintf(file, "{\n");
        std::fprintf(file, "    const std::string & msg_name_ = im_.get_message_name();\n");
        std::fprintf(file, "\n");

        write_implementation_messages(file, interface_name, im, server);

        std::fprintf(file, "    {\n");
        std::fprintf(file, "        throw yami::yami_runtime_error(\"Unknown operation name.\");\n");
        std::fprintf(file, "    }\n");
        std::fprintf(file, "}\n");
        std::fprintf(file, "\n");
    }
}

void process_fully_defined_package(
    const std::string & output_dir, const std::string & package_name)
{
    const std::string & h_file_name = header_file_name(output_dir, package_name);

    std::FILE * file = std::fopen(h_file_name.c_str(), "w");
    write_header(file, output_dir, package_name);
    std::fclose(file);

    const std::string & i_file_name = impl_file_name(output_dir, package_name);

    file = std::fopen(i_file_name.c_str(), "w");
    write_implementation(file, package_name);
    std::fclose(file);
}

} // unnamed namespace

void structures::cpp_generator::generate(const std::string & output_dir,
    idl::casing_mode casing_style)
{
    casing_ = casing_style;

    idl::name_list::const_iterator pit = structures::fully_defined_packages.begin();
    idl::name_list::const_iterator pend = structures::fully_defined_packages.end();
    for ( ; pit != pend; ++pit)
    {
        const std::string & package_name = *pit;

        process_fully_defined_package(output_dir, package_name);
    }
}
