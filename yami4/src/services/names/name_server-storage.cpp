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

#include "name_server-storage.h"

#include "../common/log.h"
#include "../common/utils.h"

#include <boost/filesystem.hpp>
#include <cstdio>

using namespace name_server;

namespace // unnamed
{

std::string data_directory_;

name_map map_;

const std::string empty_string;

const std::size_t max_binding_size = 200;

}

void name_server::init_storage(const std::string & data_directory)
{
    data_directory_ = data_directory;

    std::size_t num_of_entries = 0;

    boost::filesystem::path dir_path(data_directory);

    if (boost::filesystem::exists(dir_path) &&
        boost::filesystem::is_directory(dir_path))
    {
        boost::filesystem::directory_iterator it(dir_path);
        boost::filesystem::directory_iterator end;

        for ( ; it != end; ++it)
        {
            boost::filesystem::path entry_path(it->path());

            if (boost::filesystem::is_regular_file(entry_path))
            {
                std::FILE * file = std::fopen(entry_path.string().c_str(), "r");

                char buf[max_binding_size];
                std::fgets(buf, max_binding_size, file);
                for (std::size_t i = 0; i != max_binding_size; ++i)
                {
                    if (buf[i] == '\n')
                    {
                        buf[i] = '\0';
                        break;
                    }
                }

                map_[entry_path.filename().string()] = buf;
                
                std::fclose(file);

                ++num_of_entries;
            }
        }
    }

    logger::put(logger::store, "initialized persistent store at " + data_directory);

    logger::put(logger::store, "recovered " +
        utils::size_to_string(num_of_entries) + " bindings from store");
}

void name_server::store(const std::string & object_name, const std::string & location)
{
    boost::filesystem::path dir_path(data_directory_);
    boost::filesystem::path file_path(dir_path);
    file_path /= object_name;

    std::FILE * file = std::fopen(file_path.string().c_str(), "w");

    std::fputs(location.c_str(), file);
    std::fputs("\n", file);

    std::fclose(file);

    map_[object_name] = location;
}

const std::string & name_server::resolve(const std::string & object_name)
{
    name_map::const_iterator it = map_.find(object_name);
    if (it != map_.end())
    {
        return it->second;
    }
    else
    {
        return empty_string;
    }
}

const name_map & name_server::name_mapping()
{
    return map_;
}
