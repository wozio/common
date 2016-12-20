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

#include "idl_generator.h"
#include "structures.h"

#include <boost/filesystem.hpp>

#include <cctype>
#include <cstdio>

using namespace structures;
using namespace structures::idl_generator;

namespace // unnamed
{

std::string make_file_name(
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
        else if (std::isupper(c))
        {
            c = std::tolower(c);
        }

        base_name += c;
    }

    base_name += ".ydl";

    boost::filesystem::path p =
        boost::filesystem::path(output_dir) / boost::filesystem::path(base_name);

    return p.string();
}

void process_fully_defined_package(
    const std::string & output_dir, const std::string & package_name)
{
    const std::string & file_name = make_file_name(output_dir, package_name);

    std::FILE * file = std::fopen(file_name.c_str(), "w");

    std::fprintf(file, "--\n");
    std::fprintf(file, "--  IDL definitions for package %s.\n", package_name.c_str());
    std::fprintf(file, "--  This file was generated automatically by yami4idl.\n");
    std::fprintf(file, "--\n");

    package_map::iterator it = all_packages.find(package_name);
    const package_definitions & pkg = it->second;

    idl::name_list::const_iterator impit = pkg.explicit_imports.begin();
    idl::name_list::const_iterator impend = pkg.explicit_imports.end();

    for ( ; impit != impend; ++impit)
    {
        const std::string & import_name = *impit;
        std::fprintf(file, "import %s;\n", import_name.c_str());
    }

    if (pkg.explicit_imports.empty())
    {
        std::fprintf(file, "\n");
    }

    std::fprintf(file, "package %s is\n", package_name.c_str());
    std::fprintf(file, "\n");

    idl::name_list::const_iterator tit = pkg.ordered_type_names.begin();
    idl::name_list::const_iterator tend = pkg.ordered_type_names.end();
    for ( ; tit != tend; ++tit)
    {
        const std::string & type_name = *tit;

        std::fprintf(file, "   type %s is\n", type_name.c_str());

        type_map::const_iterator tfit = pkg.type_definitions.find(type_name);
        const type_fields & tf = tfit->second;

        idl::name_list::const_iterator fit = tf.ordered_names.begin();
        idl::name_list::const_iterator fend = tf.ordered_names.end();
        for ( ; fit != fend; ++fit)
        {
            const std::string & field_name = *fit;

            std::fprintf(file, "      %s : ", field_name.c_str());

            field_map::const_iterator fdit = tf.definitions.find(field_name);
            const field_definition & fd = fdit->second;

            if (fd.optional)
            {
                std::fprintf(file, "optional ");
            }

            std::fprintf(file, "%s;\n", fd.type_name.c_str());
        }

        std::fprintf(file, "   end %s;\n", type_name.c_str());
        std::fprintf(file, "\n");
    }

    idl::name_list::const_iterator iit = pkg.ordered_interface_names.begin();
    idl::name_list::const_iterator iend = pkg.ordered_interface_names.end();
    for ( ; iit != iend; ++iit)
    {
        const std::string & interface_name = *iit;

        std::fprintf(file, "   interface %s is\n", interface_name.c_str());

        interface_map::const_iterator imit =
            pkg.interface_definitions.find(interface_name);
        const interface_messages & im = imit->second;

        idl::name_list::const_iterator mit = im.ordered_names.begin();
        idl::name_list::const_iterator mend = im.ordered_names.end();
        for ( ; mit != mend; ++mit)
        {
            const std::string & message_name = *mit;

            message_map::const_iterator mdit = im.definitions.find(message_name);
            const message_definition & md = mdit->second;

            if (md.oneway)
            {
                std::fprintf(file, "      oneway message ");
            }
            else
            {
                std::fprintf(file, "      message ");
            }

            std::fprintf(file, "%s", message_name.c_str());

            if ((md.in_param_name.empty() == false) ||
                (md.out_param_name.empty() == false))
            {
                std::fprintf(file, " (");

                if (md.in_param_name.empty() == false)
                {
                    std::fprintf(file, "%s : in %s",
                        md.in_param_name.c_str(), md.in_param_type.c_str());

                    if (md.out_param_name.empty() == false)
                    {
                        std::fprintf(file, "; ");
                    }
                }

                if (md.out_param_name.empty() == false)
                {
                    std::fprintf(file, "%s : out %s",
                        md.out_param_name.c_str(), md.out_param_type.c_str());
                }

                std::fprintf(file, ")");
            }

            std::fprintf(file, ";\n");
        }

        std::fprintf(file, "   end %s;\n", interface_name.c_str());
        std::fprintf(file, "\n");
    }

    std::fprintf(file, "end %s;\n", package_name.c_str());

    std::fclose(file);
}

} // unnamed namespace

void structures::idl_generator::generate(const std::string & output_dir)
{
    idl::name_list::const_iterator pit = structures::fully_defined_packages.begin();
    idl::name_list::const_iterator pend = structures::fully_defined_packages.end();
    for ( ; pit != pend; ++pit)
    {
        const std::string & package_name = *pit;

        process_fully_defined_package(output_dir, package_name);
    }
}
