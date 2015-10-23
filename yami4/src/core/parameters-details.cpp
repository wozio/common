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

#include "parameters-details.h"
#include "allocator.h"
#include "fatal_errors.h"
#include "parameters.h"
#include "serialization.h"
#include <cstring>
#include <new>

using namespace yami;
using namespace yami::details;

int details::type_code(core::parameter_type t)
{
    int res = 0;  // dummy initialization to please the compiler
    switch (t)
    {
    case core::boolean:                 res = 1;  break;
    case core::integer:                 res = 2;  break;
    case core::long_long:               res = 3;  break;
    case core::double_float:            res = 4;  break;
    case core::string:                  res = 5;  break;
    case core::binary:                  res = 6;  break;
    case core::boolean_array:           res = 7;  break;
    case core::integer_array:           res = 8;  break;
    case core::long_long_array:         res = 9;  break;
    case core::double_float_array:      res = 10; break;
    case core::string_array:            res = 11; break;
    case core::binary_array:            res = 12; break;
    case core::nested_parameters:       res = 13; break;
    case core::nested_parameters_array: res = 14; break;
    default:
        fatal_failure(__FILE__, __LINE__);
    }

    return res;
}

core::result details::get_type_from_code(
    int code, core::parameter_type & type)
{
    core::result res = core::ok;
    switch (code)
    {
    case 1:  type = core::boolean;                 break;
    case 2:  type = core::integer;                 break;
    case 3:  type = core::long_long;               break;
    case 4:  type = core::double_float;            break;
    case 5:  type = core::string;                  break;
    case 6:  type = core::binary;                  break;
    case 7:  type = core::boolean_array;           break;
    case 8:  type = core::integer_array;           break;
    case 9:  type = core::long_long_array;         break;
    case 10: type = core::double_float_array;      break;
    case 11: type = core::string_array;            break;
    case 12: type = core::binary_array;            break;
    case 13: type = core::nested_parameters;       break;
    case 14: type = core::nested_parameters_array; break;
    default:
        res = core::unexpected_value;
    }

    return res;
}

core::result entry_name::set(const char * name, std::size_t length,
    allocator & alloc)
{
    core::result res;
    name_length = length;
    if (length <= short_name_optimization_threshold)
    {
        std::memcpy(buffer.short_value, name, length);
        res = core::ok;
    }
    else
    {
        char * allocated =
            static_cast<char *>(alloc.allocate(length));
        if (allocated != NULL)
        {
            std::memcpy(allocated, name, length);
            buffer.long_value = allocated;
            res = core::ok;
        }
        else
        {
            res = core::no_memory;
        }
    }

    return res;
}

const char * entry_name::value() const
{
    const char * res;
    if (name_length <= short_name_optimization_threshold)
    {
        res = buffer.short_value;
    }
    else
    {
        res = buffer.long_value;
    }

    return res;
}

void entry_name::clear(allocator & alloc)
{
    if (name_length > short_name_optimization_threshold)
    {
        alloc.deallocate(buffer.long_value);
    }

    name_length = 0;
}

bool entry_name::equals(const char * str, std::size_t length) const
{
    bool res = false;

    if (length == name_length)
    {
        if (name_length <= short_name_optimization_threshold)
        {
            res = std::memcmp(buffer.short_value, str, length) == 0;
        }
        else
        {
            res = std::memcmp(buffer.long_value, str, length) == 0;
        }
    }

    return res;
}

void entry_name::dump(dump_sink & sink) const
{
    if (name_length <= short_name_optimization_threshold)
    {
        sink.dump(buffer.short_value, name_length);
    }
    else
    {
        sink.dump(buffer.long_value, name_length);
    }
}

void string_array_element::clear(allocator & alloc)
{
    if (value != NULL)
    {
        alloc.deallocate(value);
        value = NULL;
        length = 0;
    }
}

void binary_array_element::clear(allocator & alloc)
{
    if (value != NULL)
    {
        alloc.deallocate(value);
        value = NULL;
        length = 0;
    }
}

void entry::clear_item(allocator & alloc)
{
    if (type == core::string && item.str.own)
    {
        alloc.deallocate(item.str.value);
    }
    else if (type == core::binary && item.bin.own)
    {
        alloc.deallocate(item.bin.value);
    }
    else if (type == core::boolean_array && item.ba.own)
    {
        alloc.deallocate(item.ba.values);
    }
    else if (type == core::integer_array && item.ia.own)
    {
        alloc.deallocate(item.ia.values);
    }
    else if (type == core::long_long_array && item.La.own)
    {
        alloc.deallocate(item.La.values);
    }
    else if (type == core::double_float_array && item.da.own)
    {
        alloc.deallocate(item.da.values);
    }
    else if (type == core::string_array && item.sa.own)
    {
        for (std::size_t i = 0; i != item.sa.length; ++i)
        {
            item.sa.values[i].clear(alloc);
        }

        alloc.deallocate(item.sa.values);
    }
    else if (type == core::binary_array && item.bina.own)
    {
        for (std::size_t i = 0; i != item.bina.length; ++i)
        {
            item.bina.values[i].clear(alloc);
        }

        alloc.deallocate(item.bina.values);
    }
    else if (type == core::nested_parameters)
    {
        // deep clear

        item.nested->clear();

        alloc.deallocate(item.nested);
    }
    else if (type == core::nested_parameters_array)
    {
        for (std::size_t i = 0; i != item.nesteda.length; ++i)
        {
            // deep clear
            item.nesteda.values[i].clear();
        }

        alloc.deallocate(item.nesteda.values);
    }
    
    type = core::unused;
}

void details::get_serialize_buffer_size(
    const core::parameters & params,
    std::size_t & size) // starts with 0 (for root) and gets updated
{
    // number of entries
    size += 4;

    for (size_t i = 0; i != params.num_of_entries_; ++i)
    {
        const entry & e = params.data_[i];
        
        if (e.type != core::unused)
        {
            // name length and the name itself:
            size += 4;
            size += round_up_4(e.name.name_length);

            // type
            size += 4;

            // value
            switch (e.type)
            {
            case core::boolean:
            case core::integer:
                size += 4;
                break;
            case core::long_long:
                size += 8;
                break;
            case core::double_float:
                size += 8;
                break;
            case core::string:
                // length and value
                size += 4;
                size += round_up_4(e.item.str.length);
                break;
            case core::binary:
                // length and value
                size += 4;
                size += round_up_4(e.item.bin.length);
                break;
            case core::boolean_array:
                // length and packed values
                size += 4;
                size += round_up_4((e.item.ba.length + 7) / 8);
                break;
            case core::integer_array:
                // length and values (each value 4 bytes)
                size += 4;
                size += round_up_4(e.item.ia.length * 4);
                break;
            case core::long_long_array:
                // length and values (each value 8 bytes)
                size += 4;
                size += round_up_4(e.item.La.length * 8);
                break;
            case core::double_float_array:
                // length and values (each value 8 bytes)
                size += 4;
                size += round_up_4(e.item.da.length * 8);
                break;
            case core::string_array:
                // length and then each entry as a string
                size += 4;
                for (std::size_t j = 0; j != e.item.sa.length; ++j)
                {
                    // length of array entry and its value
                    size += 4;
                    size += round_up_4(e.item.sa.values[j].length);
                }
                break;
            case core::binary_array:
                // length and then each entry separately
                size += 4;
                for (std::size_t j = 0; j != e.item.bina.length; ++j)
                {
                    // length of array entry and its value
                    size += 4;
                    size += round_up_4(e.item.bina.values[j].length);
                }
                break;
            case core::nested_parameters:
                get_serialize_buffer_size(*e.item.nested, size);
                break;
            case core::nested_parameters_array:
                // length and then each nested object separately
                size += 4;
                for (std::size_t j = 0; j != e.item.nesteda.length; ++j)
                {
                    get_serialize_buffer_size(e.item.nesteda.values[j], size);
                }
                break;
            default:
                fatal_failure(__FILE__, __LINE__);
            }
        }
    }
}

// helper for serializing individual entries
core::result details::serialize_entry(
    const details::entry & e,
	char * * buffers,
    const std::size_t * buffer_sizes,
    std::size_t num_of_buffers,
    std::size_t & current_buffer, char * & buffer_position)
{
    // name length and the name itself:
    core::result res = put_string(buffers, buffer_sizes, num_of_buffers,
        current_buffer, buffer_position,
        e.name.value(), e.name.name_length);
    if (res == core::ok)
    {
        // type
        res = put_integer(buffers, buffer_sizes, num_of_buffers,
            current_buffer, buffer_position,
            type_code(e.type));
        if (res == core::ok)
        {
            // value
            switch (e.type)
            {
            case core::boolean:
                res = put_integer(
                    buffers, buffer_sizes, num_of_buffers,
                    current_buffer, buffer_position,
                    static_cast<int>(e.item.b));
                break;
            case core::integer:
                res = put_integer(
                    buffers, buffer_sizes, num_of_buffers,
                    current_buffer, buffer_position,
                    e.item.i);
                break;
            case core::long_long:
                res = put_long_long(
                    buffers, buffer_sizes, num_of_buffers,
                    current_buffer, buffer_position,
                    e.item.L);
                break;
            case core::double_float:
                res = put_double_float(
                    buffers, buffer_sizes, num_of_buffers,
                    current_buffer, buffer_position,
                    e.item.d);
                break;
            case core::string:
                res = put_string(
                    buffers, buffer_sizes, num_of_buffers,
                    current_buffer, buffer_position,
                    e.item.str.value, e.item.str.length);
                break;
            case core::binary:
                res = put_string(
                    buffers, buffer_sizes, num_of_buffers,
                    current_buffer, buffer_position,
                    reinterpret_cast<const char *>(e.item.bin.value),
                    e.item.bin.length);
                break;
            case core::boolean_array:
                res = put_boolean_array(
                    buffers, buffer_sizes, num_of_buffers,
                    current_buffer, buffer_position,
                    e.item.ba.values, e.item.ba.length);
                break;
            case core::integer_array:
                res = put_integer_array(
                    buffers, buffer_sizes, num_of_buffers,
                    current_buffer, buffer_position,
                    e.item.ia.values, e.item.ia.length);
                break;
            case core::long_long_array:
                res = put_long_long_array(
                    buffers, buffer_sizes, num_of_buffers,
                    current_buffer, buffer_position,
                    e.item.La.values, e.item.La.length);
                break;
            case core::double_float_array:
                res = put_double_float_array(
                    buffers, buffer_sizes, num_of_buffers,
                    current_buffer, buffer_position,
                    e.item.da.values, e.item.da.length);
                break;
            case core::string_array:
                {
                    const std::size_t array_length = e.item.sa.length;
                    const string_array_element * values =
                        e.item.sa.values;
                    res = put_integer(
                        buffers, buffer_sizes, num_of_buffers,
                        current_buffer, buffer_position,
                        static_cast<int>(array_length));
                    for (std::size_t i = 0;
                         res == core::ok && i != array_length; ++i)
                    {
                        res = put_string(
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position,
                            values[i].value, values[i].length);
                    }
                }
                break;
            case core::binary_array:
                {
                    const std::size_t array_length = e.item.bina.length;
                    const binary_array_element * values =
                        e.item.bina.values;
                    res = put_integer(
                        buffers, buffer_sizes, num_of_buffers,
                        current_buffer, buffer_position,
                        static_cast<int>(array_length));
                    for (std::size_t i = 0;
                         res == core::ok && i != array_length; ++i)
                    {
                        res = put_string(
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position,
                            reinterpret_cast<const char *>(
                                values[i].value),
                            values[i].length);
                    }
                }
                break;
            case core::nested_parameters:
                // recursively:
                res = serialize(*e.item.nested,
                    buffers, buffer_sizes, num_of_buffers,
                    current_buffer, buffer_position);
                break;
            case core::nested_parameters_array:
                {
                    const std::size_t array_length = e.item.nesteda.length;
                    res = put_integer(
                        buffers, buffer_sizes, num_of_buffers,
                        current_buffer, buffer_position,
                        static_cast<int>(array_length));
                    for (std::size_t i = 0;
                         res == core::ok && i != array_length; ++i)
                    {
                        const core::parameters & nested =
                            e.item.nesteda.values[i];
                        res = serialize(nested,
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position);
                    }
                }
                break;
            default:
                fatal_failure(__FILE__, __LINE__);
            }
        }
    }
    
    return res;
}

core::result details::serialize(
    const core::parameters & params,
	char * * buffers,
    const std::size_t * buffer_sizes,
    std::size_t num_of_buffers,
    std::size_t & current_buffer, char * & buffer_position)
{
    // number of non-empty entries:
    core::result res = put_integer(
        buffers, buffer_sizes, num_of_buffers,
        current_buffer, buffer_position,
        (int)params.size());
    
    for (std::size_t i = 0; i != params.num_of_entries_ &&
    	res == core::ok; ++i)
    {
        const details::entry & e = params.data_[i];
        
        if (e.type != core::unused)
        {
            res = serialize_entry(e,
                buffers, buffer_sizes, num_of_buffers, current_buffer, buffer_position);
        }
    }
    
    return res;
}

// helper for deserializing individual entries
core::result details::deserialize_entry(
    core::parameters & params,
	const char * * buffers,
    const std::size_t * buffer_sizes,
    std::size_t num_of_buffers,
    std::size_t & current_buffer, const char * & buffer_position)
{
    // name length and the name itself:
    const char * name;
    std::size_t name_length;
    core::result res = get_string(
        buffers, buffer_sizes, num_of_buffers,
        current_buffer, buffer_position,
        name, name_length, params.allocator_);
    if (res == core::ok)
    {
        // type
        int type_code;
        res = get_integer(
            buffers, buffer_sizes, num_of_buffers,
            current_buffer, buffer_position,
            type_code);
        if (res == core::ok)
        {
            core::parameter_type type;
            res = get_type_from_code(type_code, type);
            if (res == core::ok)
            {
                // value
                switch (type)
                {
                case core::boolean:
                    {
                        int raw_value;
                        res = get_integer(
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position,
                            raw_value);
                        if (res == core::ok)
                        {
                            bool value = raw_value != 0;
                            res = params.set_boolean(
                                name, name_length, value);
                        }
                    }
                    break;
                case core::integer:
                    {
                        int value;
                        res = get_integer(
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position,
                            value);
                        if (res == core::ok)
                        {
                            res = params.set_integer(
                                name, name_length, value);
                        }
                    }
                    break;
                case core::long_long:
                    {
                        long long value;
                        res = get_long_long(
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position,
                            value);
                        if (res == core::ok)
                        {
                            res = params.set_long_long(
                                name, name_length, value);
                        }
                    }
                    break;
                case core::double_float:
                    {
                        double value;
                        res = get_double_float(
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position,
                            value);
                        if (res == core::ok)
                        {
                            res = params.set_double_float(
                                name, name_length, value);
                        }
                    }
                    break;
                case core::string:
                    {
                        const char * value;
                        std::size_t value_length;
                        res = get_string(
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position,
                            value, value_length, params.allocator_);
                        if (res == core::ok)
                        {
                            res = do_set_string(
                                name, name_length,
                                value, value_length,
                                params.data_,
                                params.num_of_entries_,
                                params.allocator_, true);
                        }
                    }
                    break;
                case core::binary:
                    {
                        const char * value;
                        std::size_t value_length;
                        res = get_string(
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position,
                            value, value_length, params.allocator_);
                        if (res == core::ok)
                        {
                            res = do_set_binary(
                                name, name_length,
                                value, value_length,
                                params.data_,
                                params.num_of_entries_,
                                params.allocator_, true);
                        }
                    }
                    break;
                case core::boolean_array:
                    {
                        bool * values;
                        std::size_t values_length;
                        res = get_boolean_array(
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position,
                            values, values_length, params.allocator_);
                        if (res == core::ok)
                        {
                            res = do_set_boolean_array(
                                name, name_length,
                                values, values_length,
                                params.data_,
                                params.num_of_entries_,
                                params.allocator_, true);
                        }
                    }
                    break;
                case core::integer_array:
                    {
                        int * values;
                        std::size_t values_length;
                        res = get_integer_array(
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position,
                            values, values_length, params.allocator_);
                        if (res == core::ok)
                        {
                            res = do_set_integer_array(
                                name, name_length,
                                values, values_length,
                                params.data_,
                                params.num_of_entries_,
                                params.allocator_, true);
                        }
                    }
                    break;
                case core::long_long_array:
                    {
                        long long * values;
                        std::size_t values_length;
                        res = get_long_long_array(
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position,
                            values, values_length, params.allocator_);
                        if (res == core::ok)
                        {
                            res = do_set_long_long_array(
                                name, name_length,
                                values, values_length,
                                params.data_,
                                params.num_of_entries_,
                                params.allocator_, true);
                        }
                    }
                    break;
                case core::double_float_array:
                    {
                        double * values;
                        std::size_t values_length;
                        res = get_double_float_array(
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position,
                            values, values_length, params.allocator_);
                        if (res == core::ok)
                        {
                            res = do_set_double_float_array(
                                name, name_length,
                                values, values_length,
                                params.data_,
                                params.num_of_entries_,
                                params.allocator_, true);
                        }
                    }
                    break;
                case core::string_array:
                    {
                        int tmp;
                        res = get_integer(
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position,
                            tmp);
                        if (res == core::ok)
                        {
                            const std::size_t array_length =
                                static_cast<std::size_t>(tmp);
                            std::size_t item_index;
                            res = do_create_string_array(
                                name, name_length,
                                array_length, item_index,
                                params.data_,
                                params.num_of_entries_,
                                params.allocator_);
                            for (std::size_t i = 0;
                                 res == core::ok && i != array_length;
                                 ++i)
                            {
                                const char * value;
                                std::size_t value_length;
                                res = get_string(
                                    buffers, buffer_sizes,
                                    num_of_buffers,
                                    current_buffer, buffer_position,
                                    value, value_length,
                                    params.allocator_);
                                if (res == core::ok)
                                {
                                    res = do_place_string_in_array(
                                        item_index, i,
                                        value, value_length,
                                        params.data_,
                                        params.allocator_);
                                    if (res != core::ok)
                                    {
                                        params.allocator_.deallocate(value);
                                    }
                                }
                            }
                        }
                    }
                    break;
                case core::binary_array:
                    {
                        int tmp;
                        res = get_integer(
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position,
                            tmp);
                        if (res == core::ok)
                        {
                            const std::size_t array_length =
                                static_cast<std::size_t>(tmp);
                            std::size_t item_index;
                            res = do_create_binary_array(
                                name, name_length,
                                array_length, item_index,
                                params.data_,
                                params.num_of_entries_,
                                params.allocator_);
                            for (std::size_t i = 0;
                                 res == core::ok && i != array_length;
                                 ++i)
                            {
                                const char * value;
                                std::size_t value_length;
                                res = get_string(
                                    buffers, buffer_sizes,
                                    num_of_buffers,
                                    current_buffer, buffer_position,
                                    value, value_length,
                                    params.allocator_);
                                if (res == core::ok)
                                {
                                    res = do_place_binary_in_array(
                                        item_index, i,
                                        value, value_length,
                                        params.data_,
                                        params.allocator_);
                                    if (res != core::ok)
                                    {
                                        params.allocator_.deallocate(value);
                                    }
                                }
                            }
                        }
                    }
                    break;
                case core::nested_parameters:
                    {
                        core::parameters * nested;
                        res = params.create_nested_parameters(
                            name, name_length, nested);
                        if (res == core::ok)
                        {
                            res = deserialize(*nested,
                                buffers, buffer_sizes, num_of_buffers,
                                current_buffer, buffer_position);
                        }
                    }
                    break;
                case core::nested_parameters_array:
                    {
                        int tmp;
                        res = get_integer(
                            buffers, buffer_sizes, num_of_buffers,
                            current_buffer, buffer_position,
                            tmp);
                        if (res == core::ok)
                        {
                            const std::size_t array_length =
                                static_cast<std::size_t>(tmp);
                            std::size_t item_index;
                            res = do_create_nested_array(
                                name, name_length,
                                array_length, item_index,
                                params.data_,
                                params.num_of_entries_,
                                params.allocator_);
                            for (std::size_t i = 0;
                                 res == core::ok && i != array_length;
                                 ++i)
                            {
                                core::parameters * nested;
                                res = do_access_nested_in_array(
                                    item_index, i,
                                    nested,
                                    params.data_);
                                if (res == core::ok)
                                {
                                    res = deserialize(*nested,
                                        buffers, buffer_sizes, num_of_buffers,
                                        current_buffer, buffer_position);
                                }
                            }
                        }
                    }
                    break;
                default:
                    fatal_failure(__FILE__, __LINE__);
                }
            }
        }

        // optimization opportunity:
        // reuse the name buffer between iterations
        // or even fuse it directly into the entry
        params.allocator_.deallocate(name);
    }

	return res;
}

core::result details::deserialize(
    core::parameters & params,
	const char * * buffers,
    const std::size_t * buffer_sizes,
    std::size_t num_of_buffers,
    std::size_t & current_buffer, const char * & buffer_position)
{
    // read number of entries for current object
    int raw_size;
    core::result res = get_integer(
        buffers, buffer_sizes, num_of_buffers,
        current_buffer, buffer_position,
        raw_size);
    
    if (res == core::ok)
    {
        for (int i = 0; i != raw_size && res == core::ok; ++i)
        {
            res = deserialize_entry(params,
                buffers, buffer_sizes, num_of_buffers,
                current_buffer, buffer_position);
        }
    }
    
    return res;
}

std::size_t details::find_entry(
    const entry * data, std::size_t num_of_entries,
    const char * name, std::size_t name_length)
{
    std::size_t index = num_of_entries;

    if (data != NULL)
    {
        for (std::size_t i = 0; i != num_of_entries; ++i)
        {
            if (data[i].type != core::unused &&
                data[i].name_equals(name, name_length))
            {
                index = i;
                break;
            }
        }
    }

    return index;
}

core::result details::find_empty_entry(
    entry * & data, std::size_t & num_of_entries,
    std::size_t & index, allocator & alloc)
{
    // note: initialization here is not needed
    // (the logic later on guarantees that it will be always set)
    // but exists to please less smart compilers

    core::result res = core::ok;

    if (data == NULL)
    {
        // no data entries, this will be the first one

        data = static_cast<entry*>(
            alloc.allocate(sizeof(entry) * initial_number_of_entries));
        if (data != NULL)
        {
            num_of_entries = initial_number_of_entries;
            for (std::size_t i = 0; i != num_of_entries; ++i)
            {
                data[i].type = core::unused;
            }
            index = 0;
            res = core::ok;
        }
        else
        {
            res = core::no_memory;
        }
    }
    else
    {
        // some entries already exist,
        // try to find an empty one

        for (std::size_t i = 0; i != num_of_entries; ++i)
        {
            if (data[i].type == core::unused)
            {
                index = i;
                res = core::ok;
                break;
            }
        }

        if (index == num_of_entries)
        {
            // no empty entry found, expand the set

            const std::size_t new_capacity = 2 * num_of_entries;
            entry * new_data = static_cast<entry *>(
                alloc.allocate(sizeof(entry) * new_capacity));
            if (new_data != NULL)
            {
                std::memcpy(new_data, data, sizeof(entry) * num_of_entries);
                alloc.deallocate(data);
                data = new_data;

                for (std::size_t i = num_of_entries; i != new_capacity; ++i)
                {
                    data[i].type = core::unused;
                }
                index = num_of_entries;
                num_of_entries = new_capacity;

                res = core::ok;
            }
            else
            {
                res = core::no_memory;
            }
        }
    }

    return res;
}

core::result details::prepare_for_set(
    entry * & data, std::size_t & num_of_entries,
    const char * name, std::size_t name_length,
    std::size_t & index, allocator & alloc)
{
    core::result res;

    index = find_entry(data, num_of_entries, name, name_length);
    if (index != num_of_entries)
    {
        // existing entry found

        entry & e = data[index];

        if (e.type != core::nested_parameters)
        {
            // simple entries and arrays

            e.clear_item(alloc);
        }
        else
        {
            // nested entry needs to be deeply destroyed

            e.item.nested->clear();
            alloc.deallocate(e.item.nested);
        }

        res = core::ok;
    }
    else
    {
        res = find_empty_entry(data, num_of_entries, index, alloc);
        if (res == core::ok)
        {
            res = data[index].set_name(name, name_length, alloc);
        }
    }

    return res;
}

std::size_t details::find_next_used(
    const entry * data, std::size_t num_of_entries,
    std::size_t current_index)
{
    std::size_t res = num_of_entries;
    for (std::size_t i = current_index + 1; i != num_of_entries; ++i)
    {
        if (data[i].type != core::unused)
        {
            res = i;
            break;
        }
    }

    return res;
}

core::result details::do_set_string(
    const char * name, std::size_t name_length,
    const char * value, std::size_t value_length,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc, bool own)
{
    std::size_t index;
    const core::result res = prepare_for_set(data, num_of_entries,
        name, name_length, index, alloc);

    if (res == core::ok)
    {
        entry * e = data + index;

        e->type = core::string;
        e->item.str.value = value;
        e->item.str.length = value_length;
        e->item.str.own = own;
    }

    return res;
}

core::result details::do_set_binary(
    const char * name, std::size_t name_length,
    const void * value, std::size_t value_length,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc, bool own)
{
    std::size_t index;
    const core::result res = prepare_for_set(data, num_of_entries,
        name, name_length, index, alloc);

    if (res == core::ok)
    {
        entry * e = data + index;

        e->type = core::binary;
        e->item.bin.value = value;
        e->item.bin.length = value_length;
        e->item.bin.own = own;
    }

    return res;
}

core::result details::do_set_boolean_array(
    const char * name, std::size_t name_length,
    const bool * values, std::size_t array_length,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc, bool own)
{
    std::size_t index;
    const core::result res = prepare_for_set(data, num_of_entries,
        name, name_length, index, alloc);

    if (res == core::ok)
    {
        entry * e = data + index;

        e->type = core::boolean_array;
        e->item.ba.values = const_cast<bool *>(values);
        e->item.ba.length = array_length;
        e->item.ba.own = own;
    }

    return res;
}

core::result details::do_set_integer_array(
    const char * name, std::size_t name_length,
    const int * values, std::size_t array_length,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc, bool own)
{
    std::size_t index;
    const core::result res = prepare_for_set(data, num_of_entries,
        name, name_length, index, alloc);

    if (res == core::ok)
    {
        entry * e = data + index;

        e->type = core::integer_array;
        e->item.ia.values = const_cast<int *>(values);
        e->item.ia.length = array_length;
        e->item.ia.own = own;
    }

    return res;
}

core::result details::do_set_long_long_array(
    const char * name, std::size_t name_length,
    const long long * values, std::size_t array_length,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc, bool own)
{
    std::size_t index;
    const core::result res = prepare_for_set(data, num_of_entries,
        name, name_length, index, alloc);

    if (res == core::ok)
    {
        entry * e = data + index;

        e->type = core::long_long_array;
        e->item.La.values = const_cast<long long *>(values);
        e->item.La.length = array_length;
        e->item.La.own = own;
    }

    return res;
}

core::result details::do_set_double_float_array(
    const char * name, std::size_t name_length,
    const double * values, std::size_t array_length,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc, bool own)
{
    std::size_t index;
    const core::result res = prepare_for_set(data, num_of_entries,
        name, name_length, index, alloc);

    if (res == core::ok)
    {
        entry * e = data + index;

        e->type = core::double_float_array;
        e->item.da.values = const_cast<double *>(values);
        e->item.da.length = array_length;
        e->item.da.own = own;
    }

    return res;
}

core::result details::do_create_string_array(
    const char * name, std::size_t name_length,
    std::size_t array_length, std::size_t & index,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc)
{
    core::result res;

    const std::size_t raw_length =
        array_length * sizeof(string_array_element);
    string_array_element * new_array =
        static_cast<string_array_element *>(
            alloc.allocate(raw_length));
    if (new_array != NULL)
    {
        for (std::size_t i = 0; i != array_length; ++i)
        {
            new_array[i].value = NULL;
            new_array[i].length = 0;
        }

        res = prepare_for_set(data, num_of_entries,
            name, name_length, index, alloc);

        if (res == core::ok)
        {
            entry * e = data + index;

            e->type = core::string_array;
            e->item.sa.values = new_array;
            e->item.sa.length = array_length;
            e->item.sa.own = true;
        }
        else
        {
            alloc.deallocate(new_array);
        }
    }
    else
    {
        res = core::no_memory;
    }

    return res;
}

core::result details::do_create_binary_array(
    const char * name, std::size_t name_length,
    std::size_t array_length, std::size_t & index,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc)
{
    core::result res;

    const std::size_t raw_length =
        array_length * sizeof(binary_array_element);
    binary_array_element * new_array =
        static_cast<binary_array_element *>(
            alloc.allocate(raw_length));
    if (new_array != NULL)
    {
        for (std::size_t i = 0; i != array_length; ++i)
        {
            new_array[i].value = NULL;
            new_array[i].length = 0;
        }

        res = prepare_for_set(data, num_of_entries,
            name, name_length, index, alloc);

        if (res == core::ok)
        {
            entry * e = data + index;

            e->type = core::binary_array;
            e->item.bina.values = new_array;
            e->item.bina.length = array_length;
            e->item.bina.own = true;
        }
        else
        {
            alloc.deallocate(new_array);
        }
    }
    else
    {
        res = core::no_memory;
    }

    return res;
}

core::result details::do_create_nested_array(
    const char * name, std::size_t name_length,
    std::size_t array_length, std::size_t & index,
    entry * & data, std::size_t & num_of_entries,
    allocator & alloc)
{
    core::result res;

    const std::size_t raw_length =
        array_length * sizeof(core::parameters);
    core::parameters * new_array =
        static_cast<core::parameters *>(
            alloc.allocate(raw_length));
    if (new_array != NULL)
    {
        for (std::size_t i = 0; i != array_length; ++i)
        {
            new (&new_array[i]) core::parameters(alloc, true);
        }

        res = prepare_for_set(data, num_of_entries,
            name, name_length, index, alloc);

        if (res == core::ok)
        {
            entry * e = data + index;

            e->type = core::nested_parameters_array;
            e->item.nesteda.values = new_array;
            e->item.nesteda.length = array_length;
            e->item.nesteda.own = true;
        }
        else
        {
            for (std::size_t i = 0; i != array_length; ++i)
            {
                new_array[i].clear();
            }

            alloc.deallocate(new_array);
        }
    }
    else
    {
        res = core::no_memory;
    }

    return res;
}

core::result details::do_set_string_in_array(
    std::size_t item_index, std::size_t array_index,
    const char * value, std::size_t value_length,
    entry * data, allocator & alloc)
{
    core::result res;

    char * new_buffer =
        static_cast<char *>(alloc.allocate(value_length));
    if (new_buffer != NULL)
    {
        std::memcpy(new_buffer, value, value_length);

        res = do_place_string_in_array(
            item_index, array_index,
            new_buffer, value_length,
            data, alloc);

        if (res != core::ok)
        {
            alloc.deallocate(new_buffer);
        }
    }
    else
    {
        res = core::no_memory;
    }

    return res;
}

core::result details::do_set_binary_in_array(
    std::size_t item_index, std::size_t array_index,
    const void * value, std::size_t value_length,
    entry * data, allocator & alloc)
{
    core::result res;

    void * new_buffer = alloc.allocate(value_length);
    if (new_buffer != NULL)
    {
        std::memcpy(new_buffer, value, value_length);

        res = do_place_binary_in_array(
            item_index, array_index,
            new_buffer, value_length,
            data, alloc);

        if (res != core::ok)
        {
            alloc.deallocate(new_buffer);
        }
    }
    else
    {
        res = core::no_memory;
    }

    return res;
}

core::result details::do_place_string_in_array(
    std::size_t item_index, std::size_t array_index,
    const char * value, std::size_t value_length,
    entry * data, allocator & alloc)
{
    core::result res;

    const entry * e = data + item_index;
    if (e->type == core::string_array)
    {
        if (array_index < e->item.sa.length)
        {
            e->item.sa.values[array_index].clear(alloc);
            e->item.sa.values[array_index].value = value;
            e->item.sa.values[array_index].length = value_length;
            res = core::ok;
        }
        else
        {
            res = core::no_such_index;
        }
    }
    else
    {
        res = core::bad_type;
    }

    return res;
}

core::result details::do_place_binary_in_array(
    std::size_t item_index, std::size_t array_index,
    const void * value, std::size_t value_length,
    entry * data, allocator & alloc)
{
    core::result res;

    const entry * e = data + item_index;
    if (e->type == core::binary_array)
    {
        if (array_index < e->item.bina.length)
        {
            e->item.bina.values[array_index].clear(alloc);
            e->item.bina.values[array_index].value = value;
            e->item.bina.values[array_index].length = value_length;
            res = core::ok;
        }
        else
        {
            res = core::no_such_index;
        }
    }
    else
    {
        res = core::bad_type;
    }

    return res;
}

core::result details::do_access_nested_in_array(
    std::size_t item_index, std::size_t array_index,
    core::parameters * & nested,
    entry * data)
{
    core::result res;

    const entry * e = data + item_index;
    if (e->type == core::nested_parameters_array)
    {
        if (array_index < e->item.nesteda.length)
        {
            nested = &e->item.nesteda.values[array_index];
            res = core::ok;
        }
        else
        {
            res = core::no_such_index;
        }
    }
    else
    {
        res = core::bad_type;
    }

    return res;
}
