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

#include "java_generator.h"
#include "name_utils.h"
#include "structures.h"

#include <boost/algorithm/string/case_conv.hpp>
#include <boost/filesystem.hpp>

#include <cctype>
#include <cstdio>

using namespace structures;
using namespace structures::java_generator;

namespace // unnamed
{

idl::casing_mode casing_;

enum mode_type {client, server};

std::string java_name(const std::string & idl_name, bool className)
{
    std::string java;
    bool new_word = className;

    for (std::size_t i = 0; i != idl_name.size(); ++i)
    {
        char c = idl_name[i];

        if (c != '.')
        {
            switch (casing_)
            {
            case idl::ident:
                java += c;
                break;
            case idl::lower_case:
                java += std::tolower(c);
                break;
            case idl::camel_case:
            case idl::default_casing:
                if (c == '_')
                {
                    new_word = true;
                }
                else
                {
                    if (new_word)
                    {
                        java += std::toupper(c);
                    }
                    else
                    {
                        java += std::tolower(c);
                    }

                    new_word = false;
                }

                break;
            }
        }
        else
        {
            java += '.';
            new_word = true;
        }
    }

    return java;
}

std::string java_package_name(const std::string & idl_name)
{
    std::string java;

    for (std::size_t i = 0; i != idl_name.size(); ++i)
    {
        char c = idl_name[i];

        if (c != '.')
        {
            // skip underscores

            if (c != '_')
            {
                java += std::tolower(c);
            }
        }
        else
        {
            java += '.';
        }
    }

    return java;
}

std::string java_class_name(const std::string & idl_name)
{
    const std::string & package_name = name_utils::trim_last_component(idl_name, false);
    const std::string & plain_class_name = name_utils::last_component(idl_name, true);

    if (package_name.empty() == false)
    {
        return java_package_name(package_name) + '.' + java_name(plain_class_name, true);
    }
    else
    {
        return java_name(plain_class_name, true);
    }
}

std::string name_to_path(const std::string & from_dir, const std::string & name)
{
    const std::string & first = name_utils::first_component(name, true);
    const std::string & tail = name_utils::trim_first_component(name, false);

    boost::filesystem::path p =
        boost::filesystem::path(from_dir) / boost::filesystem::path(first);

    if (tail.empty())
    {
        return p.string();
    }
    else
    {
        return name_to_path(p.string(), tail);
    }
}

std::string file_name(const std::string & output_dir,
    const std::string & package_name, const std::string & class_name)
{
    const std::string & j_package_name = java_package_name(package_name);
    const std::string & j_class_name = java_class_name(class_name);

    boost::filesystem::path p =
        boost::filesystem::path(name_to_path(output_dir, j_package_name)) /
        boost::filesystem::path(j_class_name);

    return p.string() + ".java";
}

void make_directory_for_package(const std::string & output_dir,
    const std::string & package_name, bool & directory_created)
{
    const std::string & dir_name = name_to_path(output_dir, java_package_name(package_name));

    directory_created = false;

    try
    {
        boost::filesystem::path p = boost::filesystem::path(dir_name);

        boost::filesystem::create_directories(p);

        directory_created = true;
    }
    catch (...)
    {
        std::printf("cannot create directory %s for package %s, skipping these files\n",
            dir_name.c_str(), package_name.c_str());
    }
}

std::string java_member_name(const std::string & idl_name)
{
    // member names never have package prefixes

    return java_name(idl_name, false);
}

std::string java_field_valid_name(const std::string & idl_name)
{
    return java_member_name(idl_name + "_Valid");
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

std::string java_type(const std::string & idl_type)
{
    if (idl_type == k_boolean)
    {
        return "boolean";
    }
    else if (idl_type == k_integer)
    {
        return "int";
    }
    else if (idl_type == k_long_long)
    {
        return "long";
    }
    else if (idl_type == k_float)
    {
        return "double";
    }
    else if (idl_type == k_string)
    {
        return "String";
    }
    else if (idl_type == k_binary)
    {
        return "byte[]";
    }
    else if (idl_type == k_boolean_array)
    {
        return "boolean[]";
    }
    else if (idl_type == k_integer_array)
    {
        return "int[]";
    }
    else if (idl_type == k_long_long_array)
    {
        return "long[]";
    }
    else if (idl_type == k_float_array)
    {
        return "double[]";
    }
    else if (idl_type == k_string_array)
    {
        return "String[]";
    }
    else if (idl_type == k_binary_array)
    {
        return "byte[][]";
    }
    else
    {
        // user-defined type
         
        return java_class_name (idl_type);
    }
}

void put_operation_signature(std::FILE * file, const message_definition & md)
{
    std::fprintf(file, "(");

    if (md.in_param_name.empty() == false)
    {
        std::fprintf(file, "%s %s",
            java_class_name(md.in_param_type).c_str(),
            java_member_name(md.in_param_name).c_str());

        if (md.out_param_name.empty() == false)
        {
            std::fprintf(file, ", ");
        }
    }

    if (md.out_param_name.empty() == false)
    {
        std::fprintf(file, "%s %s",
            java_class_name(md.out_param_type).c_str(),
            java_member_name(md.out_param_name).c_str());
    }

    std::fprintf(file, ")");
}

void write_file_head(std::FILE * file,
    const std::string & package_name, const package_definitions & pkg)
{
    std::fprintf(file, "//\n");
    std::fprintf(file, "// Java package and class definitions for package %s.\n", package_name.c_str());
    std::fprintf(file, "// This file was generated automatically by yami4idl.\n");
    std::fprintf(file, "//\n");
    std::fprintf(file, "\n");
    std::fprintf(file, "package %s;\n", java_package_name(package_name).c_str());
    std::fprintf(file, "\n");

    // process implicit imports

    idl::name_list::const_iterator imit = pkg.implicit_imports.begin();
    idl::name_list::const_iterator imend = pkg.implicit_imports.end();

    for ( ; imit != imend; ++imit)
    {
        const std::string & import_name = *imit;

        std::fprintf(file, "import %s.*;\n", java_package_name(import_name).c_str());
    }

    std::fprintf(file, "\n");
    std::fprintf(file, "import com.inspirel.yami.Agent;\n");
    std::fprintf(file, "import com.inspirel.yami.IncomingMessage;\n");
    std::fprintf(file, "import com.inspirel.yami.IncomingMessageCallback;\n");
    std::fprintf(file, "import com.inspirel.yami.NoSuchNameException;\n");
    std::fprintf(file, "import com.inspirel.yami.OutgoingMessage;\n");
    std::fprintf(file, "import com.inspirel.yami.Parameters;\n");
    std::fprintf(file, "import com.inspirel.yami.YAMIIOException;\n");
    std::fprintf(file, "\n");
    std::fprintf(file, "import java.util.concurrent.TimeoutException;\n");
    std::fprintf(file, "\n");
}

void write_field_declarations(std::FILE * file, const type_fields & tf)
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
            std::fprintf(file, "    public boolean %s;\n", java_field_valid_name(field_name).c_str());
        }

        std::fprintf(file, "    public %s %s;\n",
            java_type(type_name).c_str(), java_member_name(field_name).c_str());
    }
}

void write_field_cleaners(std::FILE * file, const type_fields & tf)
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
            std::fprintf(file, "        %s = false;\n", java_field_valid_name(field_name).c_str());
        }
    }
}

void write_field_writers(std::FILE * file, const type_fields & tf)
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
            std::fprintf(file, "        if (%s) {\n", java_field_valid_name(field_name).c_str());
        }

        if (type_name == k_boolean)
        {
            std::fprintf(file, "        params.setBoolean(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                java_member_name(field_name).c_str());
        }
        else if (type_name == k_integer)
        {
            std::fprintf(file, "        params.setInteger(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                java_member_name(field_name).c_str());
        }
        else if (type_name == k_long_long)
        {
            std::fprintf(file, "        params.setLong(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                java_member_name(field_name).c_str());
        }
        else if (type_name == k_float)
        {
            std::fprintf(file, "        params.setDouble(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                java_member_name(field_name).c_str());
        }
        else if (type_name == k_string)
        {
            std::fprintf(file, "        params.setString(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                java_member_name(field_name).c_str());
        }
        else if (type_name == k_binary)
        {
            std::fprintf(file, "        params.setBinary(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                java_member_name(field_name).c_str());
        }
        else if (type_name == k_boolean_array)
        {
            std::fprintf(file, "        params.setBooleanArray(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                java_member_name(field_name).c_str());
        }
        else if (type_name == k_integer_array)
        {
            std::fprintf(file, "        params.setIntegerArray(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                java_member_name(field_name).c_str());
        }
        else if (type_name == k_long_long_array)
        {
            std::fprintf(file, "        params.setLongArray(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                java_member_name(field_name).c_str());
        }
        else if (type_name == k_float_array)
        {
            std::fprintf(file, "        params.setDoubleArray(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                java_member_name(field_name).c_str());
        }
        else if (type_name == k_string_array)
        {
            std::fprintf(file, "        params.setStringArray(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                java_member_name(field_name).c_str());
        }
        else if (type_name == k_binary_array)
        {
            std::fprintf(file, "        params.setBinaryArray(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(),
                java_member_name(field_name).c_str());
        }
        else
        {
            // user-defined type

            const std::string & nested = java_member_name(field_name) + "Nested";

            std::fprintf(file, "        Parameters %s = new Parameters();\n", nested.c_str());
            std::fprintf(file, "        %s.write(%s);\n",
                java_member_name(field_name).c_str(), nested.c_str());
                              
            std::fprintf(file, "        params.setNestedParameters(\"%s\", %s);\n",
                yami4_field_name(field_name).c_str(), nested.c_str());
        }

        if (fd.optional)
        {
            std::fprintf(file, "        }\n");
        }
    }
}

void write_field_readers(std::FILE * file, const type_fields & tf)
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
                std::fprintf(file, "        Parameters.Entry e;\n");

                param_entry_already_defined = true;
            }

            std::fprintf(file, "        e = params.find(\"%s\");\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "        %s = e != null;\n",
                java_field_valid_name(field_name).c_str());
            std::fprintf(file, "        if (%s) {\n", java_field_valid_name(field_name).c_str());
        }

        if (type_name == k_boolean)
        {
            std::fprintf(file, "        %s = params.getBoolean(\"%s\");\n",
                java_member_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_integer)
        {
            std::fprintf(file, "        %s = params.getInteger(\"%s\");\n",
                java_member_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_long_long)
        {
            std::fprintf(file, "        %s = params.getLong(\"%s\");\n",
                java_member_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_float)
        {
            std::fprintf(file, "        %s = params.getDouble(\"%s\");\n",
                java_member_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_string)
        {
            std::fprintf(file, "        %s = params.getString(\"%s\");\n",
                java_member_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_binary)
        {
            std::fprintf(file, "        %s = params.getBinary(\"%s\");\n",
                java_member_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_boolean_array)
        {
            std::fprintf(file, "        %s = params.getBooleanArray(\"%s\");\n",
                java_member_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_integer_array)
        {
            std::fprintf(file, "        %s = params.getIntegerArray(\"%s\");\n",
                java_member_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_long_long_array)
        {
            std::fprintf(file, "        %s = params.getLongArray(\"%s\");\n",
                java_member_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_float_array)
        {
            std::fprintf(file, "        %s = params.getDoubleArray(\"%s\");\n",
                java_member_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_string_array)
        {
            std::fprintf(file, "        %s = params.getStringArray(\"%s\");\n",
                java_member_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_binary_array)
        {
            std::fprintf(file, "        %s = params.getBinaryArray(\"%s\");\n",
                java_member_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else
        {
            // user-defined type

            const std::string & nested = java_member_name(field_name) + "Nested";

            std::fprintf(file, "        Parameters %s = params.getNestedParameters(\"%s\");\n",
                nested.c_str(), yami4_field_name(field_name).c_str());

            std::fprintf(file, "        %s.read(%s);\n",
                java_member_name(field_name).c_str(), nested.c_str());
        }

        if (fd.optional)
        {
            std::fprintf(file, "        }\n");
        }
    }
}

void write_type(const std::string & output_dir,
    const std::string & package_name, const package_definitions & pkg,
    const std::string & type_name)
{
    const std::string & f_name = file_name(output_dir, package_name, type_name);

    std::FILE * file = std::fopen(f_name.c_str(), "w");

    write_file_head(file, package_name, pkg);

    type_map::const_iterator tfit = pkg.type_definitions.find(type_name);

    const type_fields & tf = tfit->second;

    std::fprintf(file, "public class %s {\n", java_class_name(type_name).c_str());
    std::fprintf(file, "\n");

    write_field_declarations(file, tf);

    std::fprintf(file, "\n");
    std::fprintf(file, "    public %s() {\n", java_class_name(type_name).c_str());

    write_field_cleaners(file, tf);

    std::fprintf(file, "    }\n");

    std::fprintf(file, "\n");
    std::fprintf(file, "    public void write(Parameters params) {\n");

    write_field_writers(file, tf);

    std::fprintf(file, "    }\n");

    std::fprintf(file, "\n");
    std::fprintf(file, "    public void read(Parameters params) {\n");

    write_field_readers(file, tf);

    std::fprintf(file, "    }\n");
    std::fprintf(file, "}\n");

    std::fclose(file);
}

void write_messages(std::FILE * file, const interface_messages & im, mode_type mode)
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
            std::fprintf(file, "    public abstract ");
        }
        else
        {
            std::fprintf(file, "    public ");
        }

        std::fprintf(file, "void %s", java_member_name(message_name).c_str());

        put_operation_signature(file, md);

        std::fprintf(file, "\n");
        std::fprintf(file, "        throws Exception");

        if (mode == server)
        {
            std::fprintf(file, ";\n");
            std::fprintf(file, "\n");
        }
        else
        {
            std::fprintf(file, " {\n");
            std::fprintf(file, "\n");

            if (md.in_param_name.empty() == false)
            {
                std::fprintf(file, "        Parameters %sY4 = new Parameters();\n",
                    java_member_name(md.in_param_name).c_str());
                std::fprintf(file, "        %s.write(%sY4);\n",
                    java_member_name(md.in_param_name).c_str(),
                    java_member_name(md.in_param_name).c_str());
                std::fprintf(file, "\n");
            }

            if (md.oneway)
            {
                if (md.in_param_name.empty() == false)
                {
                    std::fprintf(file, "        agent.sendOneWay(serverLocation, objectName,\n");
                    std::fprintf(file, "            \"%s\", %sY4);\n",
                        yami4_operation_name(message_name).c_str(),
                        java_member_name(md.in_param_name).c_str());
                }
                else
                {
                    std::fprintf(file, "        agent.sendOneWay(serverLocation, objectName,\n");
                    std::fprintf(file, "            \"%s\", null);\n",
                        yami4_operation_name(message_name).c_str());
                }
            }
            else
            {
                if (md.in_param_name.empty() == false)
                {
                    std::fprintf(file, "        OutgoingMessage msg = agent.send(serverLocation, objectName,\n");
                    std::fprintf(file, "            \"%s\", %sY4);\n",
                        yami4_operation_name(message_name).c_str(),
                        java_member_name(md.in_param_name).c_str());
                }
                else
                {
                    std::fprintf(file, "        OutgoingMessage msg = agent.send(serverLocation, objectName,\n");
                    std::fprintf(file, "            \"%s\", null);\n",
                        yami4_operation_name(message_name).c_str());
                }

                std::fprintf(file, "\n");
                std::fprintf(file, "        if (timeout != 0L) {\n");
                std::fprintf(file, "            boolean onTime = msg.waitForCompletion(timeout);\n");
                std::fprintf(file, "            if (onTime == false) {\n");
                std::fprintf(file, "                throw new TimeoutException(\"Operation timed out.\");\n");
                std::fprintf(file, "            }\n");
                std::fprintf(file, "        } else {\n");
                std::fprintf(file, "            msg.waitForCompletion();\n");
                std::fprintf(file, "        }\n");
                std::fprintf(file, "\n");
                std::fprintf(file, "        OutgoingMessage.MessageState state = msg.getState();\n");
                std::fprintf(file, "        switch (state) {\n");
                std::fprintf(file, "        case REPLIED:\n");

                if (md.out_param_name.empty() == false)
                {
                    std::fprintf(file, "            %s.read(msg.getReply());\n",
                        java_member_name(md.out_param_name).c_str());
                }

                std::fprintf(file, "            msg.close();\n");
                std::fprintf(file, "            break;\n");
                std::fprintf(file, "        case ABANDONED:\n");
                std::fprintf(file, "            msg.close();\n");
                std::fprintf(file, "            throw new YAMIIOException(\n");
                std::fprintf(file, "                \"Operation was abandoned due to communication errors.\");\n");
                std::fprintf(file, "        case REJECTED:\n");
                std::fprintf(file, "            msg.close();\n");
                std::fprintf(file, "            throw new YAMIIOException(\n");
                std::fprintf(file, "                \"Operation was rejected: \" + msg.getExceptionMsg());\n");
                std::fprintf(file, "        }\n");
            }

            std::fprintf(file, "    }\n");
            std::fprintf(file, "\n");
        }
    }
}

void write_message_dispatchers(std::FILE * file, const interface_messages & im)
{
    idl::name_list::const_iterator mit = im.ordered_names.begin();
    idl::name_list::const_iterator mend = im.ordered_names.end();
    for ( ; mit != mend; ++mit)
    {
        const std::string & message_name = *mit;

        message_map::const_iterator mdit = im.definitions.find(message_name);

        const message_definition & md = mdit->second;

        std::fprintf(file, "if (msgName.equals(\"%s\")) {\n",
            yami4_operation_name(message_name).c_str());

        if (md.in_param_name.empty() == false)
        {
            std::fprintf(file, "            %s %s = new %s();\n",
                java_class_name(md.in_param_type).c_str(),
                java_member_name(md.in_param_name).c_str(),
                java_class_name(md.in_param_type).c_str());
            std::fprintf(file, "            %s.read(msg.getParameters());\n",
                java_member_name(md.in_param_name).c_str());
            std::fprintf(file, "\n");
        }
                        
        if (md.out_param_name.empty() == false)
        {
            std::fprintf(file, "            %s %s = new %s();\n",
                java_class_name(md.out_param_type).c_str(),
                java_member_name(md.out_param_name).c_str(),
                java_class_name(md.out_param_type).c_str());
        }

        if ((md.in_param_name.empty() == false) ||
            (md.out_param_name.empty() == false))
        {
            std::fprintf(file, "\n");
        }
                        
        std::fprintf(file, "            %s(", java_member_name(message_name).c_str());

        if (md.in_param_name.empty() == false)
        {
            std::fprintf(file, "%s", java_member_name(md.in_param_name).c_str());
            if (md.out_param_name.empty() == false)
            {
                std::fprintf(file, ", ");
            }
        }                        

        if (md.out_param_name.empty() == false)
        {
            std::fprintf(file, "%s", java_member_name(md.out_param_name).c_str());
        }              
          
        std::fprintf(file, ");\n");
    
        if (md.oneway == false)
        {
            std::fprintf(file, "\n");
                          
            if (md.out_param_name.empty() == false)
            {
                std::fprintf(file, "            Parameters %sY4 = new Parameters();\n",
                    java_member_name(md.out_param_name).c_str());
                std::fprintf(file, "            %s.write(%sY4);\n",
                    java_member_name(md.out_param_name).c_str(),
                    java_member_name(md.out_param_name).c_str());
                std::fprintf(file, "            msg.reply(%sY4);\n",
                    java_member_name(md.out_param_name).c_str());
            }
            else
            {
                std::fprintf(file, "            msg.reply(null);\n");
            }
        }
                        
        std::fprintf(file, "        } else ");
    }
}

void write_interface(const std::string & output_dir,
    const std::string & package_name, const package_definitions & pkg,
    const std::string & interface_name)
{
    // generate client code

    std::string f_name = file_name(output_dir, package_name, interface_name);

    std::FILE * file = std::fopen(f_name.c_str(), "w");

    write_file_head(file, package_name, pkg);

    std::fprintf(file, "public class %s {\n", java_class_name(interface_name).c_str());
    std::fprintf(file, "\n");
    std::fprintf(file, "    private Agent agent;\n");
    std::fprintf(file, "    private String serverLocation;\n");
    std::fprintf(file, "    private String objectName;\n");
    std::fprintf(file, "    private long timeout;\n");
    std::fprintf(file, "\n");
    std::fprintf(file, "    public %s(\n", java_class_name(interface_name).c_str());
    std::fprintf(file, "        Agent agent, String serverLocation, String objectName, long timeout) {\n");
    std::fprintf(file, "\n");
    std::fprintf(file, "        this.agent = agent;\n");
    std::fprintf(file, "        this.serverLocation = serverLocation;\n");
    std::fprintf(file, "        this.objectName = objectName;\n");
    std::fprintf(file, "        this.timeout = timeout;\n");
    std::fprintf(file, "    }\n");
    std::fprintf(file, "\n");
    std::fprintf(file, "    public %s(\n", java_class_name(interface_name).c_str());
    std::fprintf(file, "        Agent agent, String serverLocation, String objectName) {\n");
    std::fprintf(file, "\n");
    std::fprintf(file, "        this(agent, serverLocation, objectName, 0L);\n");
    std::fprintf(file, "    }\n");
    std::fprintf(file, "\n");

    interface_map::const_iterator imit =
        pkg.interface_definitions.find(interface_name);
    const interface_messages & im = imit->second;

    write_messages(file, im, client);

    std::fprintf(file, "\n");
    std::fprintf(file, "}\n");

    std::fclose(file);

    // generate server code

    f_name = file_name(output_dir, package_name, interface_name + "_Server");

    file = std::fopen(f_name.c_str(), "w");

    write_file_head(file, package_name, pkg);

    std::fprintf(file, "public abstract class %s\n",
        java_class_name(interface_name + "_Server").c_str());
    std::fprintf(file, "    implements IncomingMessageCallback {\n");
    std::fprintf(file, "\n");
    std::fprintf(file, "    public %s() {\n",
        java_class_name(interface_name + "_Server").c_str());
    std::fprintf(file, "    }\n");
    std::fprintf(file, "\n");
            
    write_messages(file, im, server);
            
    std::fprintf(file, "    public void call(IncomingMessage msg) throws Exception {\n");
    std::fprintf(file, "        String msgName = msg.getMessageName();\n");
    std::fprintf(file, "\n");
    std::fprintf(file, "        ");

    write_message_dispatchers(file, im);

    std::fprintf(file, "{\n");
    std::fprintf(file, "            throw new NoSuchNameException(\"Unknown operation name.\");\n");
    std::fprintf(file, "        }\n");
    std::fprintf(file, "    }\n");
    std::fprintf(file, "}\n");

    std::fclose(file);
}

void write_files(const std::string & output_dir, const std::string & package_name)
{
    package_map::const_iterator it = all_packages.find(package_name);
    const package_definitions & pkg = it->second;

    idl::name_list::const_iterator tit = pkg.ordered_type_names.begin();
    idl::name_list::const_iterator tend = pkg.ordered_type_names.end();
    for ( ; tit != tend; ++tit)
    {
        const std::string & type_name = *tit;

        write_type(output_dir, package_name, pkg, type_name);
    }

    idl::name_list::const_iterator iit = pkg.ordered_interface_names.begin();
    idl::name_list::const_iterator iend = pkg.ordered_interface_names.end();
    for ( ; iit != iend; ++iit)
    {
        const std::string & interface_name = *iit;

        write_interface(output_dir, package_name, pkg, interface_name);
    }
}

void process_fully_defined_package(
    const std::string & output_dir, const std::string & package_name)
{
    bool directory_created;

    make_directory_for_package(output_dir, package_name, directory_created);
    if (directory_created)
    {
        write_files(output_dir, package_name);
    }
}

} // unnamed namespace

void structures::java_generator::generate(const std::string & output_dir, idl::casing_mode casing_style)
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
