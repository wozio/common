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
#include "cpp_generator.h"
#include "idl.h"
#include "idl_generator.h"
#include "java_generator.h"
#include "parser.h"
#include "reader.h"

#include <boost/filesystem.hpp>

#include <cstdlib>
#include <cstdio>
#include <string>
#include <vector>

namespace // unnamed
{

bool process_all_files(const idl::name_list & filenames)
{
    bool result = true;

    idl::name_list::const_iterator it = filenames.begin();
    idl::name_list::const_iterator end = filenames.end();
    for ( ; it != end; ++it)
    {
        const std::string & file_name = *it;

        try
        {
            parser::reset();
            reader::read_file(file_name);
        }
        catch (const idl::invalid_input_error & e)
        {
            std::printf("%s%s\n", reader::last_location().c_str(), e.what());

            result = false;
        }
    }

    return result;
}

} // unnamed namespace

int main(int argc, char * argv[])
{
    if (argc == 1)
    {
        std::puts("no input files");
        return EXIT_FAILURE;
    }

    // collect command-line options

    idl::name_list file_names;

    std::string output_dir;
    bool next_is_output_dir = false;

    idl::casing_mode casing_style = idl::default_casing;
    bool next_is_casing = false;

    bool generate_ada = false;
    bool generate_cpp = false;
    bool generate_idl = false;
    bool generate_java = false;

    for (int i = 1; i != argc; ++i)
    {
        std::string arg(argv[i]);

        if (next_is_output_dir)
        {
            boost::filesystem::path p(arg);

            if (boost::filesystem::exists(p) && boost::filesystem::is_directory(p))
            {
                output_dir = arg;
                next_is_output_dir = false;
            }
            else
            {
                std::printf("output directory '%s' does not exist\n", arg.c_str());
                return EXIT_FAILURE;
            }
        }
        else if (next_is_casing)
        {
            casing_style = idl::string_to_casing(argv[i]);
            next_is_casing = false;
        }
        else if (arg == "-help" || arg == "--help")
        {
            std::puts("usage: yami4idl [options] [file1.ydl file2.ydl ...]\n"
                "options:\n"
                "    --help             : print this help\n"
                "\n"
                "    --ada              : generate Ada code (default casing is ident)\n"
                "    --cpp              : generate C++ code (default casing is lower_case)\n"
                "    --idl              : generate IDL code (beware overwriting source files)\n"
                "    --java             : generate Java code (default casing is camel_case\n"
                "\n"
                "    --casing           : ident, lower_case, camel_case or default\n"
                "\n"
                "    -I<dir>            : search <dir> for imported packages\n"
                "\n"
                "    --output-dir <dir> : directory where new files will be created\n");
        }
        else if (arg == "-ada" || arg == "--ada")
        {
            generate_ada = true;
        }
        else if (arg == "-cpp" || arg == "--cpp")
        {
            generate_cpp = true;
        }
        else if (arg == "-idl" || arg == "--idl")
        {
            generate_idl = true;
        }
        else if (arg == "-java" || arg == "--java")
        {
            generate_java = true;
        }
        else if (arg == "-casing" || arg == "--casing")
        {
            next_is_casing = true;
        }
        else if (arg.substr(0, 2) == "-I")
        {
            reader::add_import_directory(arg.substr(2));
        }
        else if (arg == "-output-dir" || arg == "--output-dir")
        {
            next_is_output_dir = true;
        }
        else
        {
            file_names.push_back(arg);
        }
    }

    if (next_is_output_dir)
    {
        std::puts("missing output directory for -I option");
        return EXIT_FAILURE;
    }

    if (next_is_casing)
    {
        std::puts("missing casing name for --casing option");
        return EXIT_FAILURE;
    }

    bool ok = process_all_files(file_names);
    if (ok)
    {
        // execute requested actions

        if (generate_ada)
        {
            structures::ada_generator::generate(output_dir, casing_style);
        }

        if (generate_cpp)
        {
            structures::cpp_generator::generate(output_dir, casing_style);
        }

        if (generate_idl)
        {
            structures::idl_generator::generate(output_dir);
        }

        if (generate_java)
        {
            structures::java_generator::generate(output_dir, casing_style);
        }
    }
}
