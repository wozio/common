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

#include "ada_generator.h"
#include "name_utils.h"
#include "structures.h"

#include <boost/algorithm/string/case_conv.hpp>
#include <boost/filesystem.hpp>

#include <cctype>
#include <cstdio>

using namespace structures;
using namespace structures::ada_generator;

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

std::string spec_file_name(
    const std::string & output_dir, const std::string & package_name)
{
    return base_file_name(output_dir, package_name) + ".ads";
}

std::string body_file_name(
    const std::string & output_dir, const std::string & package_name)
{
    return base_file_name(output_dir, package_name) + ".adb";
}

std::string ada_name(const std::string & idl_name)
{
    return idl_name;
}

std::string ada_package_prefix(const std::string & idl_name)
{
    const std::string & package_name =
        name_utils::trim_last_component(idl_name);

    if (package_name.empty() == false)
    {
        return package_name + ".";
    }
    else
    {
        return "";
    }
}

std::string ada_field_valid_name(const std::string & idl_name)
{
    return ada_name(idl_name + "_Valid");
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

std::string ada_type(const std::string & idl_type)
{
    if (idl_type == k_boolean)
    {
        return "Boolean";
    }
    else if (idl_type == k_integer)
    {
        return "YAMI.Parameters.YAMI_Integer";
    }
    else if (idl_type == k_long_long)
    {
        return "YAMI.Parameters.YAMI_Long_Long_Integer";
    }
    else if (idl_type == k_float)
    {
        return "YAMI.Parameters.YAMI_Long_Float";
    }
    else if (idl_type == k_string)
    {
        return "Unbounded_String";
    }
    else if (idl_type == k_binary)
    {
        return "YAMI.Parameters.Binary_Holder";
    }
    else if (idl_type == k_boolean_array)
    {
        return "YAMI.Parameters.Boolean_Array_Holder";
    }
    else if (idl_type == k_integer_array)
    {
        return "YAMI.Parameters.YAMI_Integer_Array_Holder";
    }
    else if (idl_type == k_long_long_array)
    {
        return "YAMI.Parameters.YAMI_Long_Long_Integer_Array_Holder";
    }
    else if (idl_type == k_float_array)
    {
        return "YAMI.Parameters.YAMI_Long_Float_Array_Holder";
    }
    else if (idl_type == k_string_array)
    {
        return "YAMI.Parameters.String_Array";
    }
    else if (idl_type == k_binary_array)
    {
        return "YAMI.Parameters.Binary_Array";
    }
    else
    {
        // user-defined type
         
        return ada_name (idl_type);
    }
}

void finish_operation_signature(std::FILE * file, const message_definition & md)
{
    if (md.in_param_name.empty() == false)
    {
        std::fprintf(file, ";\n");
         
        std::fprintf(file, "      %s : in %s",
            ada_name(md.in_param_name).c_str(),
            ada_name(md.in_param_type).c_str());
                        
        if (md.out_param_name.empty() == false)
        {
            std::fprintf(file, "; ");
        }
    }
                     
    if (md.out_param_name.empty() == false)
    {
        if (md.in_param_name.empty())
        {
            std::fprintf(file, ";\n");
            std::fprintf(file, "      ");
        }
         
        std::fprintf(file, "%s : out %s",
            ada_name(md.out_param_name).c_str(),
            ada_name(md.out_param_type).c_str());
    }
      
    std::fprintf(file, ")");
}

void write_spec_types(std::FILE * file, const package_definitions & pkg)
{
    idl::name_list::const_iterator tit = pkg.ordered_type_names.begin();
    idl::name_list::const_iterator tend = pkg.ordered_type_names.end();
    for ( ; tit != tend; ++tit)
    {
        const std::string & type_name = *tit;

        std::fprintf(file, "   type %s is record\n",
            ada_name(type_name).c_str());

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
                std::fprintf(file, "      %s : Boolean := False;\n",
                    ada_field_valid_name(field_name).c_str());
            }

            std::fprintf(file, "      %s : %s;\n",
                ada_name(field_name).c_str(),
                ada_type(fd.type_name).c_str());
        }

        std::fprintf(file, "   end record;\n");
        std::fprintf(file, "\n");
        std::fprintf(file, "   procedure Write (V_Y4 : in %s;\n",
            ada_name(type_name).c_str());
        std::fprintf(file, "      P_Y4 : in out YAMI.Parameters.Parameters_Collection);\n");
        std::fprintf(file, "   procedure Read (V_Y4 : out %s;\n",
            ada_name(type_name).c_str());
        std::fprintf(file, "      P_Y4 : in YAMI.Parameters.Parameters_Collection);\n");
        std::fprintf(file, "\n");
    }
}

void write_spec_single_interface(std::FILE * file,
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

        if (mode == server)
        {
            std::fprintf(file, "   procedure %s (Server_Y4 : in out %s",
                ada_name(message_name).c_str(),
                ada_name(interface_name + "_Server").c_str());
        }
        else
        {
            std::fprintf(file, "   procedure %s (Client_Y4 : in out %s",
                ada_name(message_name).c_str(),
                ada_name(interface_name).c_str());
        }

        finish_operation_signature(file, md);
        
        if (mode == server)
        {
            std::fprintf(file, " is abstract;\n");
        }
        else
        {
            std::fprintf(file, ";\n");
        }
    }
}

void write_spec_interfaces(std::FILE * file, const package_definitions & pkg)
{
    idl::name_list::const_iterator iit = pkg.ordered_interface_names.begin();
    idl::name_list::const_iterator iend = pkg.ordered_interface_names.end();
    for ( ; iit != iend; ++iit)
    {
        const std::string & interface_name = *iit;

        std::fprintf(file, "   type %s is tagged record\n",
            ada_name(interface_name).c_str());
        std::fprintf(file, "      Agent : YAMI.Agents.Agent_Access;\n");
        std::fprintf(file, "      Server_Location : Unbounded_String;\n");
        std::fprintf(file, "      Object_Name : Unbounded_String;\n");
        std::fprintf(file, "      Timeout : Duration;\n");
        std::fprintf(file, "   end record;\n");
        std::fprintf(file, "\n");
        std::fprintf(file, "   procedure Initialize_%s\n",
            ada_name(interface_name).c_str());
        std::fprintf(file, "     (Client_Y4 : out %s;\n",
            ada_name(interface_name).c_str());
        std::fprintf(file, "      Agent : in out YAMI.Agents.Agent;\n");
        std::fprintf(file, "      Server_Location : in String;\n");
        std::fprintf(file, "      Object_Name : in String;\n");
        std::fprintf(file, "      Timeout : in Duration := 0.0);\n");
        std::fprintf(file, "\n");

        interface_map::const_iterator imit =
            pkg.interface_definitions.find(interface_name);
        const interface_messages & im = imit->second;

        write_spec_single_interface(file, interface_name, im, client);
            
        std::fprintf(file, "\n");
        std::fprintf(file, "   type %s is abstract\n",
            ada_name(interface_name + "_Server").c_str());
        std::fprintf(file, "      new YAMI.Incoming_Messages.Message_Handler with null record;\n");
        std::fprintf(file, "\n");
            
        write_spec_single_interface(file, interface_name, im, server);
            
        std::fprintf(file, "\n");
        std::fprintf(file, "   overriding procedure Call (Server_Y4 : in out %s;\n",
            ada_name(interface_name + "_Server").c_str());
        std::fprintf(file, "      Message_Y4 : in out YAMI.Incoming_Messages.Incoming_Message'Class);\n");
        std::fprintf(file, "\n");
    }
}

void write_spec(std::FILE * file, const std::string & package_name)
{
    std::fprintf(file, "--\n");
    std::fprintf(file, "--  Ada specifications for package %s.\n", package_name.c_str());
    std::fprintf(file, "--  This file was generated automatically by yami4idl.\n");
    std::fprintf(file, "--\n");
    std::fprintf(file, "\n");
    std::fprintf(file, "with YAMI.Agents;\n");
    std::fprintf(file, "with YAMI.Incoming_Messages;\n");
    std::fprintf(file, "with YAMI.Outgoing_Messages;\n");
    std::fprintf(file, "with YAMI.Parameters;\n");
    std::fprintf(file, "\n");
    std::fprintf(file, "with Ada.Strings.Unbounded;\n");
    std::fprintf(file, "use Ada.Strings.Unbounded;\n");
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

        std::fprintf(file, "with %s;\n", ada_name(import_name).c_str());

        some_imports_added = true;
    }

    if (some_imports_added)
    {
        std::fprintf(file, "\n");
    }

    std::fprintf(file, "package %s is\n", package_name.c_str());
    std::fprintf(file, "\n");

    write_spec_types(file, pkg);

    write_spec_interfaces(file, pkg);

    std::fprintf(file, "end %s;\n", package_name.c_str());
}

void write_body_type_writer(std::FILE * file, const type_fields & tf)
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
            std::fprintf(file, "      if V_Y4.%s then\n", ada_field_valid_name(field_name).c_str());
        }

        if (type_name == k_boolean)
        {
            std::fprintf(file, "      P_Y4.Set_Boolean (\"%s\", V_Y4.%s);\n",
                yami4_field_name(field_name).c_str(),
                ada_name(field_name).c_str());
        }
        else if (type_name == k_integer)
        {
            std::fprintf(file, "      P_Y4.Set_Integer (\"%s\", V_Y4.%s);\n",
                yami4_field_name(field_name).c_str(),
                ada_name(field_name).c_str());
        }
        else if (type_name == k_long_long)
        {
            std::fprintf(file, "      P_Y4.Set_Long_Long (\"%s\", V_Y4.%s);\n",
                yami4_field_name(field_name).c_str(),
                ada_name(field_name).c_str());
        }
        else if (type_name == k_float)
        {
            std::fprintf(file, "      P_Y4.Set_Double_Float (\"%s\", V_Y4.%s);\n",
                yami4_field_name(field_name).c_str(),
                ada_name(field_name).c_str());
        }
        else if (type_name == k_string)
        {
            std::fprintf(file, "      P_Y4.Set_String (\"%s\",\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "         To_String (V_Y4.%s));\n",
                ada_name(field_name).c_str());
        }
        else if (type_name == k_binary)
        {
            std::fprintf(file, "      declare\n");
            std::fprintf(file, "         procedure Process (A : in Ada.Streams.Stream_Element_Array) is\n");
            std::fprintf(file, "         begin\n");
            std::fprintf(file, "            P_Y4.Set_Binary (\"%s\", A);\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "         end Process;\n");
            std::fprintf(file, "      begin\n");
            std::fprintf(file, "         V_Y4.%s.Query_Element (Process'Access);\n",
                ada_name(field_name).c_str());
            std::fprintf(file, "      end;\n");
        }
        else if (type_name == k_boolean_array)
        {
            std::fprintf(file, "      declare\n");
            std::fprintf(file, "         procedure Process (A : in YAMI.Parameters.Boolean_Array) is\n");
            std::fprintf(file, "         begin\n");
            std::fprintf(file, "            P_Y4.Set_Booleans (\"%s\", A);\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "         end Process;\n");
            std::fprintf(file, "      begin\n");
            std::fprintf(file, "         V_Y4.%s.Query_Element (Process'Access);\n",
                ada_name(field_name).c_str());
            std::fprintf(file, "      end;\n");
        }
        else if (type_name == k_integer_array)
        {
            std::fprintf(file, "      declare\n");
            std::fprintf(file, "         procedure Process (A : in YAMI.Parameters.YAMI_Integer_Array) is\n");
            std::fprintf(file, "         begin\n");
            std::fprintf(file, "            P_Y4.Set_Integers (\"%s\", A);\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "         end Process;\n");
            std::fprintf(file, "      begin\n");
            std::fprintf(file, "         V_Y4.%s.Query_Element (Process'Access);\n",
                ada_name(field_name).c_str());
            std::fprintf(file, "      end;\n");
        }
        else if (type_name == k_long_long_array)
        {
            std::fprintf(file, "      declare\n");
            std::fprintf(file, "         procedure Process (A : in YAMI.Parameters.YAMI_Long_Long_Integer_Array) is\n");
            std::fprintf(file, "         begin\n");
            std::fprintf(file, "            P_Y4.Set_Long_Long_Integers (\"%s\", A);\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "         end Process;\n");
            std::fprintf(file, "      begin\n");
            std::fprintf(file, "         V_Y4.%s.Query_Element (Process'Access);\n",
                ada_name(field_name).c_str());
            std::fprintf(file, "      end;\n");
        }
        else if (type_name == k_float_array)
        {
            std::fprintf(file, "      declare\n");
            std::fprintf(file, "         procedure Process (A : in YAMI.Parameters.YAMI_Long_Float_Array) is\n");
            std::fprintf(file, "         begin\n");
            std::fprintf(file, "            P_Y4.Set_Long_Floats (\"%s\", A);\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "         end Process;\n");
            std::fprintf(file, "      begin\n");
            std::fprintf(file, "         V_Y4.%s.Query_Element (Process'Access);\n",
                ada_name(field_name).c_str());
            std::fprintf(file, "      end;\n");
        }
        else if (type_name == k_string_array)
        {
            std::fprintf(file, "      P_Y4.Set_Strings (\"%s\", V_Y4.%s));\n",
                yami4_field_name(field_name).c_str(),
                ada_name(field_name).c_str());
        }
        else if (type_name == k_binary_array)
        {
            std::fprintf(file, "      P_Y4.Set_Binaries (\"%s\", V_Y4.%s));\n",
                yami4_field_name(field_name).c_str(),
                ada_name(field_name).c_str());
        }
        else
        {
            // user-defined type
                        
            const std::string & nested = ada_name(field_name) + "_Nested_Y4";

            std::fprintf(file, "      declare\n");
            std::fprintf(file, "         %s : YAMI.Parameters.Parameters_Collection :=\n",
                nested.c_str());
            std::fprintf(file, "            YAMI.Parameters.Parameters_Collection.Create_Nested_Parameters\n");
            std::fprintf(file, "              (\"%s\"));\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "      begin\n");
            std::fprintf(file, "         %sWrite (V_Y4.%s, %s);\n",
                ada_package_prefix(type_name).c_str(),
                ada_name(field_name).c_str(),
                nested.c_str());
            std::fprintf(file, "      end;\n");
        }

        if (fd.optional)
        {
            std::fprintf(file, "      end if;\n");
        }
    }
}

void write_body_type_reader(std::FILE * file, const type_fields & tf)
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
            std::fprintf(file, "      declare\n");
            std::fprintf(file, "         E_Y4 : YAMI.Parameters.Parameter_Entry;\n");
            std::fprintf(file, "      begin\n");
                           
            std::fprintf(file, "      P_Y4.Find (\"%s\", E_Y4, V_Y4.%s);\n",
                yami4_field_name(field_name).c_str(),
                ada_field_valid_name(field_name).c_str());
            std::fprintf(file, "      if V_Y4.%s then",
                ada_field_valid_name(field_name).c_str());
        }

        if (type_name == k_boolean)
        {
            std::fprintf(file, "      V_Y4.%s := P_Y4.Get_Boolean (\"%s\");\n",
                ada_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_integer)
        {
            std::fprintf(file, "      V_Y4.%s := P_Y4.Get_Integer (\"%s\");\n",
                ada_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_long_long)
        {
            std::fprintf(file, "      V_Y4.%s := P_Y4.Get_Long_Long (\"%s\");\n",
                ada_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_float)
        {
            std::fprintf(file, "      V_Y4.%s := P_Y4.Get_Double_Float (\"%s\");\n",
                ada_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_string)
        {
            std::fprintf(file, "      V_Y4.%s := To_Unbounded_String (P_Y4.Get_String (\"%s\"));\n",
                ada_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_binary)
        {
            std::fprintf(file, "      V_Y4.%s.Replace_Element (P_Y4.Get_Binary (\"%s\"));\n",
                ada_name(field_name).c_str(),
                yami4_field_name(field_name).c_str());
        }
        else if (type_name == k_boolean_array)
        {
            std::fprintf(file, "      P_Y4.Get_Booleans (\"%s\", V_Y4.%s);\n",
                yami4_field_name(field_name).c_str(),
                ada_name(field_name).c_str());
        }
        else if (type_name == k_integer_array)
        {
            std::fprintf(file, "      P_Y4.Get_Integers (\"%s\", V_Y4.%s);\n",
                yami4_field_name(field_name).c_str(),
                ada_name(field_name).c_str());
        }
        else if (type_name == k_long_long_array)
        {
            std::fprintf(file, "      P_Y4.Get_Long_Long_Integers (\"%s\", V_Y4.%s);\n",
                yami4_field_name(field_name).c_str(),
                ada_name(field_name).c_str());
        }
        else if (type_name == k_float_array)
        {
            std::fprintf(file, "      P_Y4.Get_Long_Floats (\"%s\", V_Y4.%s);\n",
                yami4_field_name(field_name).c_str(),
                ada_name(field_name).c_str());
        }
        else if (type_name == k_string_array)
        {
            std::fprintf(file, "      P_Y4.Get_Strings (\"%s\", V_Y4.%s);\n",
                yami4_field_name(field_name).c_str(),
                ada_name(field_name).c_str());
        }
        else if (type_name == k_binary_array)
        {
            std::fprintf(file, "      P_Y4.Get_Binaries (\"%s\", V_Y4.%s);\n",
                yami4_field_name(field_name).c_str(),
                ada_name(field_name).c_str());
        }
        else
        {
            // user-defined type
                        
            const std::string & nested = ada_name(field_name) + "_Nested_Y4";

            std::fprintf(file, "      declare\n");
            std::fprintf(file, "         %s : YAMI.Parameters.Parameters_Collection :=\n", nested.c_str());
            std::fprintf(file, "            Params.Get_Nested_Parameters (\"%s\");\n",
                yami4_field_name(field_name).c_str());
            std::fprintf(file, "      begin\n");
            std::fprintf(file, "         %sRead (V_Y4.%s, %s);\n",
                ada_package_prefix(type_name).c_str(),
                ada_name(field_name).c_str(),
                nested.c_str());
            std::fprintf(file, "      end;\n");
        }

        if (fd.optional)
        {
            std::fprintf(file, "      end if;\n");
            std::fprintf(file, "      end;\n");
        }
    }
}

void write_body_types(std::FILE * file, const package_definitions & pkg)
{
    idl::name_list::const_iterator tit = pkg.ordered_type_names.begin();
    idl::name_list::const_iterator tend = pkg.ordered_type_names.end();
    for ( ; tit != tend; ++tit)
    {
        const std::string & type_name = *tit;

        std::fprintf(file, "   procedure Write (V_Y4 : in %s;\n", ada_name(type_name).c_str());
        std::fprintf(file, "      P_Y4 : in out YAMI.Parameters.Parameters_Collection) is\n");
        std::fprintf(file, "   begin\n");

        type_map::const_iterator tfit = pkg.type_definitions.find(type_name);

        const type_fields & tf = tfit->second;

        write_body_type_writer(file, tf);

        std::fprintf(file, "   end Write;\n");
        std::fprintf(file, "\n");
        std::fprintf(file, "   procedure Read (V_Y4 : out %s;\n", ada_name(type_name).c_str());
        std::fprintf(file, "      P_Y4 : in YAMI.Parameters.Parameters_Collection) is\n");
        std::fprintf(file, "   begin\n");

        write_body_type_reader(file, tf);

        std::fprintf(file, "   end Read;\n");
        std::fprintf(file, "\n");
    }
}

void write_body_messages(std::FILE * file,
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
            std::fprintf(file, "   procedure %s (Client_Y4 : in out %s",
                ada_name(message_name).c_str(),
                ada_name(interface_name).c_str());

            finish_operation_signature(file, md);
                        
            std::fprintf(file, " is\n");

            if (md.in_param_name.empty() == false)
            {
                std::fprintf(file, "      %s_Y4 : YAMI.Parameters.Parameters_Collection :=\n",
                    ada_name(md.in_param_name).c_str());
                std::fprintf(file, "         YAMI.Parameters.Make_Parameters;\n");
            }
                        
            if (md.oneway)
            {
                std::fprintf(file, "   begin\n");

                if (md.in_param_name.empty() == false)
                {
                    std::fprintf(file, "      %sWrite (%s, %s_Y4);\n",
                        ada_package_prefix(md.in_param_type).c_str(),
                        ada_name(md.in_param_name).c_str(),
                        ada_name(md.in_param_name).c_str());
                    std::fprintf(file, "\n");
                    std::fprintf(file, "      Client_Y4.Agent.All.Send_One_Way "
                        "(To_String (Client_Y4.Server_Location),\n");
                    std::fprintf(file, "         To_String (Client_Y4.Object_Name), \"%s\",\n",
                        yami4_operation_name(message_name).c_str());
                    std::fprintf(file, "         %s_Y4);\n",
                        ada_name(md.in_param_name).c_str());
                }
                else
                {
                    std::fprintf(file, "      Client_Y4.Agent.All.Send_One_Way "
                        "(To_String (Client_Y4.Server_Location),\n");
                    std::fprintf(file, "         To_String (Client_Y4.Object_Name), \"%s\");\n",
                        yami4_operation_name(message_name).c_str());
                }
            }
            else // two-way message
            {
                std::fprintf(file, "      Msg_Y4 : aliased YAMI.Outgoing_Messages.Outgoing_Message;\n");
                std::fprintf(file, "   begin\n");
                           
                if (md.in_param_name.empty() == false)
                {
                    std::fprintf(file, "      %sWrite (%s, %s_Y4);\n",
                        ada_package_prefix(md.in_param_type).c_str(),
                        ada_name(md.in_param_name).c_str(),
                        ada_name(md.in_param_name).c_str());
                    std::fprintf(file, "\n");
                    std::fprintf(file, "      Client_Y4.Agent.All.Send (To_String "
                        "(Client_Y4.Server_Location),\n");
                    std::fprintf(file, "         To_String (Client_Y4.Object_Name), \"%s\",\n",
                        yami4_operation_name(message_name).c_str());
                    std::fprintf(file, "         %s_Y4, Msg_Y4'Unchecked_Access);\n",
                        ada_name(md.in_param_name).c_str());
                }
                else
                {
                    std::fprintf(file, "      Client_Y4.Agent.All.Send (To_String "
                        "(Client_Y4.Server_Location),\n");
                    std::fprintf(file, "         To_String (Client_Y4.Object_Name), \"%s\",",
                        yami4_operation_name(message_name).c_str());
                    std::fprintf(file, " Msg_Y4'Unchecked_Access);\n");
                }
                        
                std::fprintf(file, "\n");
                std::fprintf(file, "      if Client_Y4.Timeout /= 0.0 then\n");
                std::fprintf(file, "         select\n");
                std::fprintf(file, "            Msg_Y4.Wait_For_Completion;\n");
                std::fprintf(file, "         or\n");
                std::fprintf(file, "            delay Client_Y4.Timeout;\n");
                std::fprintf(file, "            raise YAMI.Runtime_Error with \"Operation timed out.\";\n");
                std::fprintf(file, "         end select;\n");
                std::fprintf(file, "      else\n");
                std::fprintf(file, "         Msg_Y4.Wait_For_Completion;\n");
                std::fprintf(file, "      end if;\n");
                std::fprintf(file, "\n");
                std::fprintf(file, "      case Msg_Y4.State is\n");
                std::fprintf(file, "         when YAMI.Outgoing_Messages.Replied =>\n");
                    
                if (md.out_param_name.empty() == false)
                {
                    std::fprintf(file, "            declare\n");
                    std::fprintf(file, "               procedure Process_Y4\n");
                    std::fprintf(file, "                 (Content_Y4 : "
                        "in out YAMI.Parameters.Parameters_Collection) is\n");
                    std::fprintf(file, "               begin\n");
                    std::fprintf(file, "                  %sRead (%s, Content_Y4);\n",
                        ada_package_prefix(md.out_param_type).c_str(),
                        ada_name(md.out_param_name).c_str());
                    std::fprintf(file, "               end Process_Y4;\n");
                    std::fprintf(file, "            begin\n");
                    std::fprintf(file, "               Msg_Y4.Process_Reply_Content (Process_Y4'Access);\n");
                    std::fprintf(file, "            end;\n");
                }
                else
                {
                    std::fprintf(file, "            null;\n");
                }
                        
                std::fprintf(file, "         when YAMI.Outgoing_Messages.Abandoned =>\n");
                std::fprintf(file, "            raise YAMI.Runtime_Error with\n");
                std::fprintf(file, "               \"Operation was abandoned due to communication errors.\";\n");
                std::fprintf(file, "         when YAMI.Outgoing_Messages.Rejected =>\n");
                std::fprintf(file, "            raise YAMI.Runtime_Error with\n");
                std::fprintf(file, "               \"Operation was rejected: \" & Msg_Y4.Exception_Message;\n");
                std::fprintf(file, "         when others =>\n");
                std::fprintf(file, "            null;\n");
                std::fprintf(file, "      end case;\n");
            }

            std::fprintf(file, "   end %s;\n", ada_name(message_name).c_str());
            std::fprintf(file, "\n");
        }
        else // server
        {
            std::fprintf(file, "if Message_Name_Y4 = \"%s\" then\n",
                yami4_operation_name(message_name).c_str());
            std::fprintf(file, "         declare\n");
                        
            if (md.in_param_name.empty() == false)
            {
                std::fprintf(file, "            %s : %s;\n",
                    ada_name(md.in_param_name).c_str(),
                    ada_name(md.in_param_type).c_str());
                std::fprintf(file, "            procedure Process_Y4\n");
                std::fprintf(file, "              (Content_Y4 : in out YAMI.Parameters.Parameters_Collection) is\n");
                std::fprintf(file, "            begin\n");
                std::fprintf(file, "               %sRead (%s, Content_Y4);\n",
                    ada_package_prefix(md.in_param_type).c_str(),
                    ada_name(md.in_param_name).c_str());
                std::fprintf(file, "            end Process_Y4;\n");
            }

            if (md.out_param_name.empty() == false)
            {
                std::fprintf(file, "            %s : %s;\n",
                    ada_name(md.out_param_name).c_str(),
                    ada_name(md.out_param_type).c_str());
                std::fprintf(file, "            %s_Y4 : YAMI.Parameters.Parameters_Collection :=\n",
                    ada_name(md.out_param_name).c_str());
                std::fprintf(file, "               YAMI.Parameters.Make_Parameters;\n");
            }

            std::fprintf(file, "         begin\n");
                        
            if (md.in_param_name.empty() == false)
            {
                std::fprintf(file, "            Message_Y4.Process_Content (Process_Y4'Access);\n");
            }

            std::fprintf(file, "            %s'Class (Server_Y4).%s",
                ada_name(interface_name + "_Server").c_str(),
                ada_name(message_name).c_str());

            if ((md.in_param_name.empty() == false) || (md.out_param_name.empty() == false))
            {
                std::fprintf(file, " (");
            }
                        
            if (md.in_param_name.empty() == false)
            {
                std::fprintf(file, "%s", ada_name(md.in_param_name).c_str());
                if (md.out_param_name.empty() == false)
                {
                    std::fprintf(file, ", ");
                }
            }
                        
            if (md.out_param_name.empty() == false)
            {
                std::fprintf(file, "%s", ada_name(md.out_param_name).c_str());
            }
                        
            if ((md.in_param_name.empty() == false) || (md.out_param_name.empty() == false))
            {
                std::fprintf(file, ")");
            }
                        
            std::fprintf(file, ";\n");
    
            if (md.oneway == false)
            {
                if (md.out_param_name.empty() == false)
                {
                    std::fprintf(file, "            %sWrite (%s, %s_Y4);\n",
                        ada_package_prefix(md.out_param_type).c_str(),
                        ada_name(md.out_param_name).c_str(),
                        ada_name(md.out_param_name).c_str());
                    std::fprintf(file, "            Message_Y4.Reply (%s_Y4);\n",
                        ada_name(md.out_param_name).c_str());
                }
                else
                {
                    std::fprintf(file, "            Message_Y4.Reply;\n");
                }
            }
                        
            std::fprintf(file, "         end;\n");
            std::fprintf(file, "      els");
        }
    }
}

void write_body_interfaces(std::FILE * file, const package_definitions & pkg)
{
    idl::name_list::const_iterator iit = pkg.ordered_interface_names.begin();
    idl::name_list::const_iterator iend = pkg.ordered_interface_names.end();
    for ( ; iit != iend; ++iit)
    {
        const std::string & interface_name = *iit;

        interface_map::const_iterator imit =
            pkg.interface_definitions.find(interface_name);
        const interface_messages & im = imit->second;

        std::fprintf(file, "   procedure Initialize_%s\n", ada_name(interface_name).c_str());
        std::fprintf(file, "     (Client_Y4 : out %s;\n", ada_name(interface_name).c_str());
        std::fprintf(file, "      Agent : in out YAMI.Agents.Agent;\n");
        std::fprintf(file, "      Server_Location : in String;\n");
        std::fprintf(file, "      Object_Name : in String;\n");
        std::fprintf(file, "      Timeout : in Duration := 0.0) is\n");
        std::fprintf(file, "   begin\n");
        std::fprintf(file, "      Client_Y4.Agent := Agent'Unchecked_Access;\n");
        std::fprintf(file, "      Client_Y4.Server_Location := To_Unbounded_String (Server_Location);\n");
        std::fprintf(file, "      Client_Y4.Object_Name := To_Unbounded_String (Object_Name);\n");
        std::fprintf(file, "      Client_Y4.Timeout := Timeout;\n");
        std::fprintf(file, "   end Initialize_%s;\n", ada_name(interface_name).c_str());
        std::fprintf(file, "\n");

        write_body_messages(file, interface_name, im, client);

        std::fprintf(file, "   procedure Call (Server_Y4 : in out %s;\n",
            ada_name(interface_name + "_Server").c_str());
        std::fprintf(file, "      Message_Y4 : in out YAMI.Incoming_Messages.Incoming_Message'Class) is\n");
        std::fprintf(file, "      Message_Name_Y4 : constant String := Message_Y4.Message_Name;\n");
        std::fprintf(file, "   begin\n");
        std::fprintf(file, "      ");

        write_body_messages(file, interface_name, im, server);

        std::fprintf(file, "e\n");
        std::fprintf(file, "         raise YAMI.Runtime_Error with \"Unknown operation name.\";\n");
        std::fprintf(file, "      end if;\n");
        std::fprintf(file, "   end Call;\n");
        std::fprintf(file, "\n");
    }
}

void write_body(std::FILE * file, const std::string & package_name)
{
    std::fprintf(file, "--\n");
    std::fprintf(file, "--  Ada implementations for package %s.\n", package_name.c_str());
    std::fprintf(file, "--  This file was generated automatically by yami4idl.\n");
    std::fprintf(file, "--\n");
    std::fprintf(file, "\n");

    package_map::const_iterator it = all_packages.find(package_name);
    const package_definitions & pkg = it->second;
    
    std::fprintf(file, "with Ada.Strings.Unbounded;\n");
    std::fprintf(file, "use Ada.Strings.Unbounded;\n");
    std::fprintf(file, "\n");
    
    std::fprintf(file, "package body %s is\n", ada_name(package_name).c_str());
    std::fprintf(file, "\n");

    write_body_types(file, pkg);

    write_body_interfaces(file, pkg);

    std::fprintf(file, "end %s;\n", package_name.c_str());
}

void process_fully_defined_package(
    const std::string & output_dir, const std::string & package_name)
{
    const std::string & h_file_name = spec_file_name(output_dir, package_name);

    std::FILE * file = std::fopen(h_file_name.c_str(), "w");
    write_spec(file, package_name);
    std::fclose(file);

    const std::string & i_file_name = body_file_name(output_dir, package_name);

    file = std::fopen(i_file_name.c_str(), "w");
    write_body(file, package_name);
    std::fclose(file);
}

} // unnamed namespace

void structures::ada_generator::generate(const std::string & output_dir,
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
