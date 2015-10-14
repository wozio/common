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

#include "reader.h"
#include "idl.h"
#include "name_utils.h"
#include "parser.h"
#include "structures.h"
#include "tokenizer.h"

#include <boost/filesystem.hpp>

#include <cstdio>

using namespace reader;

namespace // unnamed
{

std::size_t line_;
std::size_t column_;
std::string current_file_name_;

idl::name_list import_dirs_;

std::string resolved_file_name(const std::string & file_name)
{
    boost::filesystem::path file_path(file_name);

    if (boost::filesystem::exists(file_path))
    {
        return file_name;
    }

    idl::name_list::iterator it = import_dirs_.begin();
    idl::name_list::iterator end = import_dirs_.end();
    for ( ; it != end; ++it)
    {
        boost::filesystem::path dir_path(*it);

        boost::filesystem::path candidate_path = dir_path / file_path;

        if (boost::filesystem::exists(candidate_path))
        {
            return candidate_path.string();
        }
    }

    return "";
}

std::string size_to_string(std::size_t i)
{
    const std::size_t max_len = 20;
    char buf[max_len];

    std::sprintf(buf, "%u", static_cast<unsigned int>(i));

    return buf;
}

} // unnamed namespace

void reader::add_import_directory(const std::string & dir)
{
    boost::filesystem::path p(dir);

    if (boost::filesystem::exists(p) && boost::filesystem::is_directory(p))
    {
        import_dirs_.push_back(dir);
    }
    else
    {
        std::printf("'%s' is not a directory\n", dir.c_str());
    }
}

void reader::read_file(const std::string & file_name)
{
    const std::string & res_file_name = resolved_file_name(file_name);

    if (res_file_name.empty())
    {
        std::printf("cannot find file '%s'\n", file_name.c_str());
        return;
    }

    current_file_name_ = file_name;
    line_ = 1;
    column_ = 1;

    std::FILE * file = std::fopen(res_file_name.c_str(), "r");
    if (file == NULL)
    {
        std::printf("error opening file '%s'\n", res_file_name.c_str());
        return;
    }

    char c = static_cast<char>(std::fgetc(file));
    while (std::feof(file) == 0)
    {
        if (c != '\r')
        {
            tokenizer::tokenize(c);
        }

        if (c == '\n')
        {
            ++line_;
            column_ = 1;
        }
        else
        {
            ++column_;
        }

        c = static_cast<char>(std::fgetc(file));
    }

    // ensure tokens from distinct files are not concatenated
    tokenizer::tokenize('\n');

    std::fclose(file);

    if (parser::finished() == false)
    {
        std::printf("file '%s' is not complete\n", file_name.c_str());
    }
}

void reader::import_package(const std::string & package_name)
{
    std::size_t tmp_line = line_;
    std::size_t tmp_column = column_;
    std::string tmp_current_file_name = current_file_name_;

    tokenizer::reset();
    parser::reset();
    parser::set_mode(idl::import);
    structures::set_mode(idl::import);

    read_file(name_utils::package_to_file_name(package_name));

    parser::set_mode(idl::full);
    structures::set_mode(idl::full);

    line_ = tmp_line;
    column_ = tmp_column;
    current_file_name_ = tmp_current_file_name;
}

std::string reader::last_location()
{
    return current_file_name_ + ":" + size_to_string(line_) +
        ":" + size_to_string(tokenizer::last_accumulated_length()) + ": ";
}
