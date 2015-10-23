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

#ifndef YAMICORE_PARAMETERS_DETAILS_H_INCLUDED
#define YAMICORE_PARAMETERS_DETAILS_H_INCLUDED

#include "core.h"
#include "details-fwd.h"
#include "parameter_type.h"
#include <cstddef>

namespace yami
{

namespace core
{
class parameters;
} // namespace core

namespace details
{

const std::size_t short_name_optimization_threshold = 16;
const std::size_t initial_number_of_entries = 4;
const std::size_t max_nesting_level = 5;

struct entry_name
{
    std::size_t name_length;
    union packed
    {
        const char * long_value;
        char short_value[short_name_optimization_threshold];
    } buffer;

    core::result set(const char * value, std::size_t length,
        allocator & alloc);

    const char * value() const;

    void clear(allocator & alloc);

    bool equals(const char * value, std::size_t length) const;

    // for unit tests
    void dump(details::dump_sink & sink) const;
};

struct string_array_element
{
    const char * value;
    std::size_t length;

    void clear(allocator & alloc);
};

struct binary_array_element
{
    const void * value;
    std::size_t length;

    void clear(allocator & alloc);
};

struct entry
{
    entry_name name;

    core::parameter_type type;

    union packed
    {
        bool b;
        int i;
        long long L;
        double d;
        struct string_data
        {
            const char * value;
            std::size_t length;
            bool own;
        } str;
        struct binary_data
        {
            const void * value;
            std::size_t length;
            bool own;
        } bin;
        struct boolean_array_data
        {
            bool * values;
            std::size_t length;
            bool own;
        } ba;
        struct integer_array_data
        {
            int * values;
            std::size_t length;
            bool own;
        } ia;
        struct long_long_array_data
        {
            long long * values;
            std::size_t length;
            bool own;
        } La;
        struct double_float_array_data
        {
            double * values;
            std::size_t length;
            bool own;
        } da;
        struct string_array_data
        {
            string_array_element * values;
            std::size_t length;
            bool own;
        } sa;
        struct binary_array_data
        {
            binary_array_element * values;
            std::size_t length;
            bool own;
        } bina;
        core::parameters * nested;
        struct nested_array_data
        {
            core::parameters * values;
            std::size_t length;
            bool own;
        } nesteda;
    } item;

    core::result set_name(const char * value, std::size_t length,
        allocator & alloc)
    {
        return name.set(value, length, alloc);
    }

    void clear_name(allocator & alloc)
    {
        name.clear(alloc);
    }

    bool name_equals(const char * value, std::size_t length) const
    {
        return name.equals(value, length);
    }

    void clear_item(allocator & alloc);
};

void get_serialize_buffer_size(
    const core::parameters & params,
    std::size_t & size); // starts with 0 (for root) and gets updated

core::result serialize_entry(
    const details::entry & e,
	char * * buffers,
    const std::size_t * buffer_sizes,
    std::size_t num_of_buffers,
    std::size_t & current_buffer, char * & buffer_position);

core::result serialize(
    const core::parameters & params,
	char * * buffers,
    const std::size_t * buffer_sizes,
    std::size_t num_of_buffers,
    std::size_t & current_buffer, char * & buffer_position);

core::result deserialize_entry(
    core::parameters & params,
	const char * * buffers,
    const std::size_t * buffer_sizes,
    std::size_t num_of_buffers,
    std::size_t & current_buffer, const char * & buffer_position);

core::result deserialize(
    core::parameters & params,
	const char * * buffers,
    const std::size_t * buffer_sizes,
    std::size_t num_of_buffers,
    std::size_t & current_buffer, const char * & buffer_position);

int type_code(core::parameter_type t);

core::result get_type_from_code(int code, core::parameter_type & type);

// find the entry with given name
// returns the index or num_of_entries if not found
std::size_t find_entry(const entry * data, std::size_t num_of_entries,
    const char * name, std::size_t name_length);

// find an appropriate place for a new entry
// by either locating an unused entry
// or by extending the set
core::result find_empty_entry(entry * & data, std::size_t & num_of_entries,
    std::size_t & index, allocator & alloc);

// finds an existing entry with the given name and clears its item
// or creates a new entry with this name already set
core::result prepare_for_set(entry * & data, std::size_t & num_of_entries,
    const char * name, std::size_t name_length,
    std::size_t & index, allocator & alloc);

// finds the next used entry or returns num_of_entries if nothing was found
std::size_t find_next_used(const entry * data, std::size_t num_of_entries,
    std::size_t current_index);

core::result do_set_string(const char * name, std::size_t name_length,
    const char * value, std::size_t value_length,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc, bool own);

core::result do_set_binary(const char * name, std::size_t name_length,
    const void * value, std::size_t value_length,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc, bool own);

core::result do_set_boolean_array(const char * name, std::size_t name_length,
    const bool * values, std::size_t array_length,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc, bool own);

core::result do_set_integer_array(const char * name, std::size_t name_length,
    const int * values, std::size_t array_length,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc, bool own);

core::result do_set_long_long_array(
    const char * name, std::size_t name_length,
    const long long * values, std::size_t array_length,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc, bool own);

core::result do_set_double_float_array(
    const char * name, std::size_t name_length,
    const double * values, std::size_t array_length,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc, bool own);

core::result do_create_string_array(
    const char * name, std::size_t name_length,
    std::size_t array_length, std::size_t & index,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc);

core::result do_set_string_in_array(
    std::size_t item_index, std::size_t array_index,
    const char * value, std::size_t value_length,
    entry * data, allocator & alloc);

core::result do_place_string_in_array(
    std::size_t item_index, std::size_t array_index,
    const char * value, std::size_t value_length,
    entry * data, allocator & alloc);

core::result do_create_binary_array(
    const char * name, std::size_t name_length,
    std::size_t array_length, std::size_t & index,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc);

core::result do_set_binary_in_array(
    std::size_t item_index, std::size_t array_index,
    const void * value, std::size_t value_length,
    entry * data, allocator & alloc);

core::result do_place_binary_in_array(
    std::size_t item_index, std::size_t array_index,
    const void * value, std::size_t value_length,
    entry * data, allocator & alloc);

core::result do_create_nested_array(
    const char * name, std::size_t name_length,
    std::size_t array_length, std::size_t & index,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc);

core::result do_access_nested_in_array(
    std::size_t item_index, std::size_t array_index,
    core::parameters * & nested,
    entry * data);

} // namespace details

} // namespace yami

#endif // YAMICORE_PARAMETERS_DETAILS_H_INCLUDED
