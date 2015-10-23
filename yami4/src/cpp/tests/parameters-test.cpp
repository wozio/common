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

#include "../yami.h"
#include <cassert>
#include <cstring>
#include <sstream>
#include <vector>

void test1()
{
    yami::parameters params;

    assert(params.size() == 0);

    std::ostringstream ss;
    params.dump(ss);
    assert(ss.str().empty());

    try
    {
        params.remove("no such entry");
        assert(false);
    }
    catch (const yami::yami_logic_error & e)
    {
        assert(e.what() == std::string("No such name."));
    }

    yami::parameters::iterator it = params.begin();
    assert(it == params.end());

    yami::parameter_entry e;
    bool found = params.find("no such entry", e);
    assert(found == false);

    std::size_t serialized_size = params.serialize_buffer_size();
    assert(serialized_size == 4); // only number of entries

    std::vector<char> buf(serialized_size);
    char * buffers[] = {&buf[0]};
    std::size_t sizes[] = {serialized_size};
    params.serialize(buffers, sizes, 1);
    const char expected[] = {0, 0, 0, 0}; // only num of entries
    assert(std::memcmp(&buf[0], expected, serialized_size) == 0);

    yami::parameters params2;
    const char * cbuffers[] = {&buf[0]};
    params2.deserialize(cbuffers, sizes, 1);
    assert(params2.size() == 0);

    params2.clear();
    assert(params2.size() == 0);
}

void check_serialization(const yami::parameters & params,
    const char * expected, std::size_t serialized_size)
{
    {
        // test for serialization into continuous buffer

        std::vector<char> buf(serialized_size);
        char * buffers[] = {&buf[0]};
        std::size_t sizes[] = {serialized_size};
        params.serialize(buffers, sizes, 1);
        assert(std::memcmp(&buf[0], expected, serialized_size) == 0);

        // verify deserialization gives the same result

        yami::parameters params2;
        const char * cbuffers[] = {&buf[0]};
        params2.deserialize(cbuffers, sizes, 1);

        std::size_t serialized_size2 = params2.serialize_buffer_size();
        assert(serialized_size2 == serialized_size);

        std::vector<char> buf2(serialized_size2);
        char * buffers2[] = {&buf2[0]};
        std::size_t sizes2[] = {serialized_size2};
        params.serialize(buffers2, sizes2, 1);
        assert(std::memcmp(&buf[0], &buf2[0], serialized_size) == 0);
    }
    {
        // test for serialization into two buffers
        // for different size relations

        for (std::size_t first_size = 4;
             first_size != serialized_size; first_size += 4)
        {
            const std::size_t second_size = serialized_size - first_size;
            std::vector<char> buf1(first_size);
            std::vector<char> buf2(second_size);
            char * buffers[] = {&buf1[0], &buf2[0]};
            std::size_t sizes[] = {first_size, second_size};
            params.serialize(buffers, sizes, 2);
            assert(std::memcmp(&buf1[0], expected, first_size) == 0);
            assert(std::memcmp(&buf2[0], expected + first_size,
                    second_size) == 0);
        }
    }
}

// test for boolean
void test2()
{
    yami::parameters params;

    params.set_boolean("name", true);

    assert(params.size() == 1);

    yami::parameter_type type = params.type("name");
    assert(type == yami::boolean);

    bool value = params.get_boolean("name");
    assert(value);

    std::ostringstream ss;
    params.dump(ss);
    assert(ss.str() ==
        "entry 0:\n"
        "name: name\n"
        "boolean: 1\n"
        "entry 1:\n"
        "unused\n"
        "entry 2:\n"
        "unused\n"
        "entry 3:\n"
        "unused\n");

    params.set_boolean("name", false);

    assert(params.size() == 1);

    value = params.get_boolean("name");
    assert(value == false);

    try
    {
        (void) params.get_integer("name");
        assert(false);
    }
    catch (const yami::yami_logic_error & e)
    {
        assert(e.what() == std::string("Bad type."));
    }

    yami::parameters::iterator it = params.begin();
    yami::parameter_entry e = *it;
    assert(e.type() == yami::boolean);
    assert(e.name() == "name");
    value = e.get_boolean();
    assert(value == false);
    try
    {
        (void) e.get_integer();
        assert(false);
    }
    catch (const yami::yami_logic_error & ex)
    {
        assert(ex.what() == std::string("Bad type."));
    }

    ++it;
    assert(it == params.end());

    std::size_t serialized_size = params.serialize_buffer_size();
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 4 // value
    );

    bool array[] = { true, false, true };
    params.set_boolean_array(
        "nameA", array, sizeof(array) / sizeof(bool));

    assert(params.size() == 2);

    type = params.type("nameA");
    assert(type == yami::boolean_array);

    std::size_t array_length;
    bool * values = params.get_boolean_array("nameA", array_length);
    assert(array_length == 3);
    assert(values[0] == true);
    assert(values[1] == false);
    assert(values[2] == true);
    values[2] = false;

    it = params.begin();
    e = *it;
    assert(e.type() == yami::boolean);
    assert(e.name() == "name");
    ++it;
    assert(it != params.end());
    e = *it;
    assert(e.type() == yami::boolean_array);
    assert(e.name() == "nameA");
    values = e.get_boolean_array(array_length);
    assert(array_length == 3);
    assert(values[0] == true);
    assert(values[1] == false);
    assert(values[2] == false);

    ++it;
    assert(it == params.end());

    ss.str("");
    params.dump(ss);
    assert(ss.str() ==
        "entry 0:\n"
        "name: name\n"
        "boolean: 0\n"
        "entry 1:\n"
        "name: nameA\n"
        "boolean array: 1 0 0\n"
        "entry 2:\n"
        "unused\n"
        "entry 3:\n"
        "unused\n");

    serialized_size = params.serialize_buffer_size();
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 4 // value
        + 4 // length of 2nd entry name
        + 8 // "nameA"
        + 4 // type
        + 4 // length of array
        + 4 // values for bool array
    );

    const char expected[] =
        {
            2, 0, 0, 0,         // num of entries
            4, 0, 0, 0,         // length of "name"
            'n', 'a', 'm', 'e',
            1, 0, 0, 0,         // type code for boolean
            0, 0, 0, 0,         // false
            5, 0, 0, 0,         // length of "nameA"
            'n', 'a', 'm', 'e',
            'A', 0, 0, 0,
            7, 0, 0, 0,         // type code for boolean array
            3, 0, 0, 0,         // length of array
            0x1, 0, 0, 0        // packed array (100 -> 0x1)
        };

    check_serialization(params, expected, serialized_size);

    bool found = params.find("name", e);
    assert(found);
    assert(e.type() == yami::boolean);
    assert(e.name() == "name");

    found = params.find("nameA", e);
    assert(found);
    assert(e.type() == yami::boolean_array);
    assert(e.name() == "nameA");

    found = params.find("no such name", e);
    assert(found == false);

    params.remove("name");

    assert(params.size() == 1);

    it = params.begin();
    e = *it;
    assert(e.type() == yami::boolean_array);
    assert(e.name() == "nameA");

    ++it;
    assert(it == params.end());

    params.clear();
    assert(params.size() == 0);
}

// test for integer
void test3()
{
    yami::parameters params;

    params.set_integer("name", 123);

    assert(params.size() == 1);

    yami::parameter_type type = params.type("name");
    assert(type == yami::integer);

    int value = params.get_integer("name");
    assert(value == 123);

    std::size_t serialized_size = params.serialize_buffer_size();
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 4 // value
    );

    int array[] = { 10, 20, 30 };
    params.set_integer_array(
        "nameA", array, sizeof(array) / sizeof(int));

    assert(params.size() == 2);

    type = params.type("nameA");
    assert(type == yami::integer_array);

    std::size_t array_length;
    int * values = params.get_integer_array("nameA", array_length);
    assert(array_length == 3);
    assert(values[0] == 10);
    assert(values[1] == 20);
    assert(values[2] == 30);
    values[2] = 31;

    yami::parameters::iterator it = params.begin();
    yami::parameter_entry e = *it;
    assert(e.type() == yami::integer);
    assert(e.name() == "name");
    value = e.get_integer();
    assert(value == 123);
    ++it;
    assert(it != params.end());
    e = *it;
    assert(e.name() == "nameA");
    values = e.get_integer_array(array_length);
    assert(array_length == 3);
    assert(values[0] == 10);
    assert(values[1] == 20);
    assert(values[2] == 31);
    ++it;
    assert(it == params.end());

    std::ostringstream ss;
    params.dump(ss);
    assert(ss.str() ==
        "entry 0:\n"
        "name: name\n"
        "integer: 123\n"
        "entry 1:\n"
        "name: nameA\n"
        "integer array: 10 20 31\n"
        "entry 2:\n"
        "unused\n"
        "entry 3:\n"
        "unused\n");

    serialized_size = params.serialize_buffer_size();
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 4 // value
        + 4 // length of 2nd entry name
        + 8 // "nameA"
        + 4 // type
        + 4 // length of array
        + 12 // values for int array
    );

    const char expected[] =
        {
            2, 0, 0, 0,         // num of entries
            4, 0, 0, 0,         // length of "name"
            'n', 'a', 'm', 'e',
            2, 0, 0, 0,         // type code for integer
            123, 0, 0, 0,       // 123
            5, 0, 0, 0,         // length of "nameA"
            'n', 'a', 'm', 'e',
            'A', 0, 0, 0,
            8, 0, 0, 0,         // type code for integer array
            3, 0, 0, 0,         // length of array
            10, 0, 0, 0,        // array values
            20, 0, 0, 0,        // array values
            31, 0, 0, 0         // array values
        };

    check_serialization(params, expected, serialized_size);

    it = params.begin();
    e = *it;
    assert(e.name() == "name");
    params.remove(it);
    assert(params.size() == 1);
}

// test for long long
void test4()
{
    yami::parameters params;

    params.set_long_long("name", 1234567890LL);

    assert(params.size() == 1);

    yami::parameter_type type = params.type("name");
    assert(type == yami::long_long);

    long long value = params.get_long_long("name");
    assert(value == 1234567890LL);

    std::size_t serialized_size = params.serialize_buffer_size();
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 8 // value
    );

    long long array[] = { 100LL, 200LL, 300LL };
    params.set_long_long_array(
        "nameA", array, sizeof(array) / sizeof(long long));

    assert(params.size() == 2);

    type = params.type("nameA");
    assert(type == yami::long_long_array);

    std::size_t array_length;
    long long * values = params.get_long_long_array("nameA", array_length);
    assert(array_length == 3);
    assert(values[0] == 100LL);
    assert(values[1] == 200LL);
    assert(values[2] == 300LL);
    values[2] = 310LL;

    yami::parameters::iterator it = params.begin();
    yami::parameter_entry e = *it;
    assert(e.type() == yami::long_long);
    value = e.get_long_long();
    assert(value == 1234567890LL);
    ++it;
    assert(it != params.end());
    e = *it;
    assert(e.type() == yami::long_long_array);
    values = e.get_long_long_array(array_length);
    assert(array_length == 3);
    assert(values[0] == 100LL);
    assert(values[1] == 200LL);
    assert(values[2] == 310LL);
    ++it;
    assert(it == params.end());

    std::ostringstream ss;
    params.dump(ss);
    assert(ss.str() ==
        "entry 0:\n"
        "name: name\n"
        "long_long: 1234567890\n"
        "entry 1:\n"
        "name: nameA\n"
        "long_long array: 100 200 310\n"
        "entry 2:\n"
        "unused\n"
        "entry 3:\n"
        "unused\n");

    serialized_size = params.serialize_buffer_size();
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 8 // value
        + 4 // length of 2nd entry name
        + 8 // "nameA"
        + 4 // type
        + 4 // length of array
        + 24 // values for long long array
    );

    const char expected[] =
        {
            2, 0, 0, 0,         // num of entries
            4, 0, 0, 0,         // length of "name"
            'n', 'a', 'm', 'e',
            3, 0, 0, 0,         // type code for long long

            static_cast<char>(210), 2,
            static_cast<char>(150), 73, // 1234567890

            0, 0, 0, 0,
            5, 0, 0, 0,         // length of "nameA"
            'n', 'a', 'm', 'e',
            'A', 0, 0, 0,
            9, 0, 0, 0,         // type code for long long array
            3, 0, 0, 0,         // length of array
            100, 0, 0, 0,       // array values
            0, 0, 0, 0,
            static_cast<char>(200), 0, 0, 0, // array values
            0, 0, 0, 0,
            54, 1, 0, 0,        // array values
            0, 0, 0, 0
        };

    check_serialization(params, expected, serialized_size);
}

// test for double
void test5()
{
    yami::parameters params;

    params.set_double_float("name", 3.125);

    assert(params.size() == 1);

    yami::parameter_type type = params.type("name");
    assert(type == yami::double_float);

    double value = params.get_double_float("name");
    assert(value == 3.125);

    std::size_t serialized_size = params.serialize_buffer_size();
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 8 // value
    );

    double array[] = { 1.875, 2.875, 3.875 };
    params.set_double_float_array(
        "nameA", array, sizeof(array) / sizeof(double));

    assert(params.size() == 2);

    type = params.type("nameA");
    assert(type == yami::double_float_array);

    std::size_t array_length;
    double * values = params.get_double_float_array("nameA", array_length);
    assert(array_length == 3);
    assert(values[0] == 1.875);
    assert(values[1] == 2.875);
    assert(values[2] == 3.875);
    values[2] = 3.625;

    yami::parameters::iterator it = params.begin();
    yami::parameter_entry e = *it;
    assert(e.type() == yami::double_float);
    value = e.get_double_float();
    assert(value == 3.125);
    ++it;
    assert(it != params.end());
    e = *it;
    assert(e.type() == yami::double_float_array);
    values = e.get_double_float_array(array_length);
    assert(array_length == 3);
    assert(values[0] == 1.875);
    assert(values[1] == 2.875);
    assert(values[2] == 3.625);
    ++it;
    assert(it == params.end());

    std::ostringstream ss;
    params.dump(ss);
    assert(ss.str() ==
        "entry 0:\n"
        "name: name\n"
        "double: 3.125\n"
        "entry 1:\n"
        "name: nameA\n"
        "double array: 1.875 2.875 3.625\n"
        "entry 2:\n"
        "unused\n"
        "entry 3:\n"
        "unused\n");

    serialized_size = params.serialize_buffer_size();
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 8 // value
        + 4 // length of 2nd entry name
        + 8 // "nameA"
        + 4 // type
        + 4 // length of array
        + 24 // values for long long array
    );

    const char expected[] =
        {
            2, 0, 0, 0,         // num of entries
            4, 0, 0, 0,         // length of "name"
            'n', 'a', 'm', 'e',
            4, 0, 0, 0,         // type code for double
            0, 0, 0, 0,
            0, 0, 9, 64,        // 3.125
            5, 0, 0, 0,         // length of "nameA"
            'n', 'a', 'm', 'e',
            'A', 0, 0, 0,
            10, 0, 0, 0,        // type code for double array
            3, 0, 0, 0,         // length of array
            0, 0, 0, 0,         // array values
            0, 0, -2, 63,
            0, 0, 0, 0,         // array values
            0, 0, 7, 64,
            0, 0, 0, 0,         // array values
            0, 0, 13, 64
        };

    check_serialization(params, expected, serialized_size);
}

// test for strings
void test6()
{
    yami::parameters params;

    const char * source_value =
        "Kolorowe kredki w pudeleczku nosze,\n"
        "kolorowe kredki, bardzo lubie je!";

    params.set_string("some rather longer name", source_value);

    assert(params.size() == 1);

    yami::parameter_type type = params.type("some rather longer name");
    assert(type == yami::string);

    assert(params.get_string("some rather longer name") == source_value);
    std::size_t value_length;
    const char * value = params.get_string("some rather longer name",
        value_length);
    assert(value != source_value);
    assert(std::string(value, value_length) == source_value);

    params.set_string_shallow("other name",
        source_value, std::strlen(source_value));

    assert(params.size() == 2);

    value = params.get_string("other name", value_length);
    assert(value == source_value);

    std::size_t serialized_size = params.serialize_buffer_size();
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 24 // "some rather longer name"
        + 4 // type
        + 4 // length
        + 72 // "Kolorowe kredki..."
        + 4 // length of 2nd entry name
        + 12 // "other name"
        + 4 // type
        + 4 // length
        + 72 // "Kolorowe kredki..."
    );

    const char * array[] = {"Kazio", "Krzysio", "Rysio", "Zbysio"};
    params.create_string_array(
        "nameA", sizeof(array) / sizeof(const char *));

    type = params.type("nameA");
    assert(type == yami::string_array);

    assert(params.get_string_in_array("nameA", 0).empty());
    assert(params.get_string_in_array("nameA", 3).empty());
    try
    {
        (void) params.get_string_in_array("nameA", 4);
        assert(false);
    }
    catch (const yami::yami_logic_error & e)
    {
        assert(e.what() == std::string("No such index."));
    }

    params.set_string_in_array("nameA", 0, array[0]);
    params.set_string_in_array("nameA", 1, array[1]);
    params.set_string_in_array("nameA", 2, array[2]);
    params.set_string_in_array("nameA", 3, array[3]);

    assert(params.get_string_in_array("nameA", 0) == array[0]);
    assert(params.get_string_in_array("nameA", 3) == array[3]);
    
    yami::parameters::iterator it = params.begin();
    yami::parameter_entry e = *it;
    assert(e.type() == yami::string);
    assert(e.get_string() == source_value);
    ++it;
    assert(it != params.end());
    e = *it;
    assert(e.type() == yami::string);
    assert(e.get_string() == source_value);
    ++it;
    assert(it != params.end());
    e = *it;
    assert(e.type() == yami::string_array);
    assert(e.get_string_in_array(1) == array[1]);
    ++it;
    assert(it == params.end());

    std::ostringstream ss;
    params.dump(ss);
    assert(ss.str() ==
        "entry 0:\n"
        "name: some rather longer name\n"
        "string: Kolorowe kredki w pudeleczku nosze,\n"
        "kolorowe kredki, bardzo lubie je!\n"
        "entry 1:\n"
        "name: other name\n"
        "string: Kolorowe kredki w pudeleczku nosze,\n"
        "kolorowe kredki, bardzo lubie je!\n"
        "entry 2:\n"
        "name: nameA\n"
        "string array: Kazio Krzysio Rysio Zbysio\n"
        "entry 3:\n"
        "unused\n");

    serialized_size = params.serialize_buffer_size();
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 24 // "some rather longer name"
        + 4 // type
        + 4 // length
        + 72 // "Kolorowe kredki..."
        + 4 // length of 2nd entry name
        + 12 // "other name"
        + 4 // type
        + 4 // length
        + 72 // "Kolorowe kredki..."
        + 4 // length of 3rd entry name
        + 8 // "nameA"
        + 4 // type
        + 4 // array length
        + 4 // length of 1st array element
        + 8 // "Kazio"
        + 4 // length of 2nd array element
        + 8 // "Krzysio"
        + 4 // length of 3rd array element
        + 8 // "Rysio"
        + 4 // length of 4th array element
        + 8 // "Zbysio"
    );

    const char expected[] =
        {
            3, 0, 0, 0,         // num of entries
            23, 0, 0, 0,        // length of "some rather longer name"
            's', 'o', 'm', 'e',
            ' ', 'r', 'a', 't',
            'h', 'e', 'r', ' ',
            'l', 'o', 'n', 'g',
            'e', 'r', ' ', 'n',
            'a', 'm', 'e', 0,
            5, 0, 0, 0,         // type code for string
            69, 0, 0, 0,        // length
            'K', 'o', 'l', 'o',
            'r', 'o', 'w', 'e',
            ' ', 'k', 'r', 'e',
            'd', 'k', 'i', ' ',
            'w', ' ', 'p', 'u',
            'd', 'e', 'l', 'e',
            'c', 'z', 'k', 'u',
            ' ', 'n', 'o', 's',
            'z', 'e', ',', '\n',
            'k', 'o', 'l', 'o',
            'r', 'o', 'w', 'e',
            ' ', 'k', 'r', 'e',
            'd', 'k', 'i', ',',
            ' ', 'b', 'a', 'r',
            'd', 'z', 'o', ' ',
            'l', 'u', 'b', 'i',
            'e', ' ', 'j', 'e',
            '!', 0, 0, 0,
            10, 0, 0, 0,        // length of "other name"
            'o', 't', 'h', 'e',
            'r', ' ', 'n', 'a',
            'm', 'e', 0, 0,
            5, 0, 0, 0,         // type code for string
            69, 0, 0, 0,        // length
            'K', 'o', 'l', 'o',
            'r', 'o', 'w', 'e',
            ' ', 'k', 'r', 'e',
            'd', 'k', 'i', ' ',
            'w', ' ', 'p', 'u',
            'd', 'e', 'l', 'e',
            'c', 'z', 'k', 'u',
            ' ', 'n', 'o', 's',
            'z', 'e', ',', '\n',
            'k', 'o', 'l', 'o',
            'r', 'o', 'w', 'e',
            ' ', 'k', 'r', 'e',
            'd', 'k', 'i', ',',
            ' ', 'b', 'a', 'r',
            'd', 'z', 'o', ' ',
            'l', 'u', 'b', 'i',
            'e', ' ', 'j', 'e',
            '!', 0, 0, 0,
            5, 0, 0, 0,         // length of "nameA"
            'n', 'a', 'm', 'e',
            'A', 0, 0, 0,
            11, 0, 0, 0,        // type code for string array
            4, 0, 0, 0,         // length of array
            5, 0, 0, 0,         // length of first entry
            'K', 'a', 'z', 'i',
            'o', 0, 0, 0,
            7, 0, 0, 0,         // length of second entry
            'K', 'r', 'z', 'y',
            's', 'i', 'o', 0,
            5, 0, 0, 0,         // length of third entry
            'R', 'y', 's', 'i',
            'o', 0, 0, 0,
            6, 0, 0, 0,         // length of fourth entry
            'Z', 'b', 'y', 's',
            'i', 'o', 0, 0
        };

    check_serialization(params, expected, serialized_size);
}

// test for binary
void test7()
{
    yami::parameters params;

    const void * source_value = "abc\0d";
    const std::size_t source_length = 5;

    params.set_binary("some rather longer name",
        source_value, source_length);

    assert(params.size() == 1);

    yami::parameter_type type = params.type("some rather longer name");
    assert(type == yami::binary);

    std::size_t value_length;
    const void * value = params.get_binary("some rather longer name",
        value_length);
    assert(value != source_value);
    assert(value_length == source_length);
    assert(std::memcmp(value, source_value, value_length) == 0);

    params.set_binary_shallow("other name",
        source_value, source_length);

    assert(params.size() == 2);

    value = params.get_binary("other name", value_length);
    assert(value == source_value);

    std::size_t serialized_size = params.serialize_buffer_size();
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 24 // "some rather longer name"
        + 4 // type
        + 4 // length
        + 8 // source_value
        + 4 // length of 2nd entry name
        + 12 // "other name"
        + 4 // type
        + 4 // length
        + 8 // source_value
    );

    const void * array[] = {"abc", "klm", "xyz"};
    params.create_binary_array(
        "nameA", sizeof(array) / sizeof(const void *));

    type = params.type("nameA");
    assert(type == yami::binary_array);

    value = params.get_binary_in_array("nameA", 0, value_length);
    assert(value == NULL);
    assert(value_length == 0);
    value = params.get_binary_in_array("nameA", 2, value_length);
    assert(value == NULL);
    assert(value_length == 0);
    try
    {
        (void) params.get_binary_in_array("nameA", 3, value_length);
        assert(false);
    }
    catch (const yami::yami_logic_error & e)
    {
        assert(e.what() == std::string("No such index."));
    }

    params.set_binary_in_array("nameA", 0, array[0], 3);
    params.set_binary_in_array("nameA", 1, array[1], 2);
    params.set_binary_in_array("nameA", 2, array[2], 3);

    value = params.get_binary_in_array("nameA", 0, value_length);
    assert(value_length == 3);
    assert(std::memcmp(value, "abc", 3) == 0);
    value = params.get_binary_in_array("nameA", 1, value_length);
    assert(value_length == 2);
    assert(std::memcmp(value, "klm", 2) == 0);
    
    yami::parameters::iterator it = params.begin();
    yami::parameter_entry e = *it;
    assert(e.type() == yami::binary);
    value = e.get_binary(value_length);
    assert(value_length == source_length);
    assert(std::memcmp(value, source_value, value_length) == 0);
    ++it;
    assert(it != params.end());
    e = *it;
    assert(e.type() == yami::binary);
    value = e.get_binary(value_length);
    assert(value == source_value);
    ++it;
    assert(it != params.end());
    e = *it;
    assert(e.type() == yami::binary_array);
    value = e.get_binary_in_array(1, value_length);
    assert(value_length == 2);
    assert(std::memcmp(value, "klm", 2) == 0);
    ++it;
    assert(it == params.end());

    std::ostringstream ss;
    params.dump(ss);
    assert(ss.str() ==
        "entry 0:\n"
        "name: some rather longer name\n"
        "binary of length 5\n"
        "entry 1:\n"
        "name: other name\n"
        "binary of length 5\n"
        "entry 2:\n"
        "name: nameA\n"
        "binary array of length 3\n"
        "entry 3:\n"
        "unused\n");

    serialized_size = params.serialize_buffer_size();
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 24 // "some rather longer name"
        + 4 // type
        + 4 // length
        + 8 // source_value
        + 4 // length of 2nd entry name
        + 12 // "other name"
        + 4 // type
        + 4 // length
        + 8 // source_value
        + 4 // length of 3rd entry name
        + 8 // "nameA"
        + 4 // type
        + 4 // array length
        + 4 // length of 1st array element
        + 4 // "abc"
        + 4 // length of 2nd array element
        + 4 // "kl"
        + 4 // length of 3rd array element
        + 4 // "xyz"
    );

    const char expected[] =
        {
            3, 0, 0, 0,         // num of entries
            23, 0, 0, 0,        // length of "some rather longer name"
            's', 'o', 'm', 'e',
            ' ', 'r', 'a', 't',
            'h', 'e', 'r', ' ',
            'l', 'o', 'n', 'g',
            'e', 'r', ' ', 'n',
            'a', 'm', 'e', 0,
            6, 0, 0, 0,         // type code for binary
            5, 0, 0, 0,         // length
            'a', 'b', 'c', '\0',
            'd', 0, 0, 0,
            10, 0, 0, 0,        // length of "other name"
            'o', 't', 'h', 'e',
            'r', ' ', 'n', 'a',
            'm', 'e', 0, 0,
            6, 0, 0, 0,         // type code for binary
            5, 0, 0, 0,         // length
            'a', 'b', 'c', '\0',
            'd', 0, 0, 0,
            5, 0, 0, 0,         // length of "nameA"
            'n', 'a', 'm', 'e',
            'A', 0, 0, 0,
            12, 0, 0, 0,        // type code for binary array
            3, 0, 0, 0,         // length of array
            3, 0, 0, 0,         // length of first entry
            'a', 'b', 'c', 0,
            2, 0, 0, 0,         // length of second entry
            'k', 'l', 0, 0,
            3, 0, 0, 0,         // length of third entry
            'x', 'y', 'z', 0
        };

    check_serialization(params, expected, serialized_size);
}

// test for nesting
void test8()
{
    yami::parameters params;

    params.set_integer("name", 123);

    assert(params.size() == 1);

    yami::parameters nested(
        params.create_nested_parameters("nested"));

    assert(params.size() == 2);

    nested.set_integer("internal", 456);

    assert(nested.size() == 1);

    nested.set_double_float("internal2", 3.125);

    assert(params.size() == 2);
    assert(nested.size() == 2);

    yami::parameters nested2(
        nested.create_nested_parameters("more nested"));

    assert(params.size() == 2);
    assert(nested.size() == 3);
    
    nested2.set_integer("more internal", 789);

    assert(params.size() == 2);
    assert(nested.size() == 3);
    assert(nested2.size() == 1);

    yami::parameter_type type = params.type("name");
    assert(type == yami::integer);
    type = params.type("nested");
    assert(type == yami::nested_parameters);
    type = nested.type("internal");
    assert(type == yami::integer);
    type = nested.type("internal2");
    assert(type == yami::double_float);
    type = nested.type("more nested");
    assert(type == yami::nested_parameters);
    type = nested2.type("more internal");
    assert(type == yami::integer);
    try
    {
        (void) nested.type("blabla");
        assert(false);
    }
    catch (const yami::yami_logic_error & e)
    {
        assert(e.what() == std::string("No such name."));
    }

    yami::parameters::iterator it = params.begin();
    yami::parameter_entry e = *it;
    assert(e.type() == yami::integer);
    ++it;
    assert(it != params.end());
    e = *it;
    assert(e.type() == yami::nested_parameters);
    {
        yami::parameters nst(e.get_nested_parameters());
        assert(nst.size() == 3);
    }
    ++it;
    assert(it == params.end());

    std::ostringstream ss;
    params.dump(ss);
    assert(ss.str() ==
        "entry 0:\n"
        "name: name\n"
        "integer: 123\n"
        "entry 1:\n"
        "name: nested\n"
        "nested parameters:\n"
        "  entry 0:\n"
        "  name: internal\n"
        "  integer: 456\n"
        "  entry 1:\n"
        "  name: internal2\n"
        "  double: 3.125\n"
        "  entry 2:\n"
        "  name: more nested\n"
        "  nested parameters:\n"
        "    entry 0:\n"
        "    name: more internal\n"
        "    integer: 789\n"
        "    entry 1:\n"
        "    unused\n"
        "    entry 2:\n"
        "    unused\n"
        "    entry 3:\n"
        "    unused\n"
        "  entry 3:\n"
        "  unused\n"
        "entry 2:\n"
        "unused\n"
        "entry 3:\n"
        "unused\n");

    std::size_t serialized_size = params.serialize_buffer_size();
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 4 // value
        + 4 // length of 2nd entry name
        + 8 // "nested"
        + 4 // type
        /*  */ + 4 // number of params in nested
        /*  */ + 4 // length of 1st entry name
        /*  */ + 8 // "internal"
        /*  */ + 4 // type
        /*  */ + 4 // value
        /*  */ + 4 // length of 2nd entry name
        /*  */ + 12 // "internal2"
        /*  */ + 4 // type
        /*  */ + 8 // value
        /*  */ + 4 // length of 3rd entry name
        /*  */ + 12 // "more nested"
        /*  */ + 4 // type
        /*         */ + 4 // number of params in more nested
        /*         */ + 4 // length of 1st entry name
        /*         */ + 16 // "more internal"
        /*         */ + 4 // type
        /*         */ + 4 // value
    );

    const char expected[] =
        {
            2, 0, 0, 0,         // num of entries
            4, 0, 0, 0,         // length of "name"
            'n', 'a', 'm', 'e',
            2, 0, 0, 0,         // type code for integer
            123, 0, 0, 0,       // value
            6, 0, 0, 0,         // length of "nested"
            'n', 'e', 's', 't',
            'e', 'd', 0, 0,
            13, 0, 0, 0,        // type code for nested
            3, 0, 0, 0,         // num of entries in nested
            8, 0, 0, 0,         // length of "internal"
            'i', 'n', 't', 'e',
            'r', 'n', 'a', 'l',
            2, 0, 0, 0,         // type code for integer
            -56, 1, 0, 0,       // value
            9, 0, 0, 0,         // length of "internal2"
            'i', 'n', 't', 'e',
            'r', 'n', 'a', 'l',
            '2', 0, 0, 0,
            4, 0, 0, 0,         // type code for double
            0, 0, 0, 0,
            0, 0, 9, 64,        // 3.125
            11, 0, 0, 0,        // length of "more nested"
            'm', 'o', 'r', 'e',
            ' ', 'n', 'e', 's',
            't', 'e', 'd', 0,
            13, 0, 0, 0,        // type code for nested
            1, 0, 0, 0,         // num of entries in more nested
            13, 0, 0, 0,        // length of "more internal"
            'm', 'o', 'r', 'e',
            ' ', 'i', 'n', 't',
            'e', 'r', 'n', 'a',
            'l', 0, 0, 0,
            2, 0, 0, 0,         // type code for integer
            21, 3, 0, 0         // value
        };

    check_serialization(params, expected, serialized_size);
}

// test for array of nested parameters
void test8a()
{
    yami::parameters params;

    params.create_nested_array("name", 3);
    assert(params.size() == 1);

    params.set_integer("i", 7);
    assert(params.size() == 2);

    std::size_t array_length = params.get_nested_array_length("name");
    assert(array_length == 3);

    // first nested

    yami::parameters nested1(
        params.get_nested_in_array("name", 0));

    nested1.set_integer("x", 10);
    assert(nested1.size() == 1);

    // second nested

    yami::parameters nested2(
        params.get_nested_in_array("name", 1));

    nested2.set_integer("x", 20);
    nested2.set_integer("y", 21);
    assert(nested2.size() == 2);

    // third nested

    yami::parameters nested3(
        params.get_nested_in_array("name", 2));

    nested3.set_integer("x", 30);
    nested3.set_integer("y", 31);
    nested3.set_integer("z", 32);
    assert(nested3.size() == 3);

    // no more nested

    try
    {
        yami::parameters nested4(
            params.get_nested_in_array("name", 3));
        assert(false);
    }
    catch (const yami::yami_logic_error & e)
    {
        assert(e.what() == std::string("No such index."));
    }

    yami::parameter_type type = params.type("name");
    assert(type == yami::nested_parameters_array);

    yami::parameters::iterator it = params.begin();
    yami::parameter_entry e = *it;
    assert(e.type() == yami::nested_parameters_array);
    ++it;
    assert(it != params.end());
    e = *it;
    assert(e.type() == yami::integer);
    ++it;
    assert(it == params.end());

    std::ostringstream ss;
    params.dump(ss);
    assert(ss.str() ==
        "entry 0:\n"
        "name: name\n"
        "nested parameters array of length 3:\n"
        "  nested at index 0:\n"
        "    entry 0:\n"
        "    name: x\n"
        "    integer: 10\n"
        "    entry 1:\n"
        "    unused\n"
        "    entry 2:\n"
        "    unused\n"
        "    entry 3:\n"
        "    unused\n"
        "  nested at index 1:\n"
        "    entry 0:\n"
        "    name: x\n"
        "    integer: 20\n"
        "    entry 1:\n"
        "    name: y\n"
        "    integer: 21\n"
        "    entry 2:\n"
        "    unused\n"
        "    entry 3:\n"
        "    unused\n"
        "  nested at index 2:\n"
        "    entry 0:\n"
        "    name: x\n"
        "    integer: 30\n"
        "    entry 1:\n"
        "    name: y\n"
        "    integer: 31\n"
        "    entry 2:\n"
        "    name: z\n"
        "    integer: 32\n"
        "    entry 3:\n"
        "    unused\n"
        "entry 1:\n"
        "name: i\n"
        "integer: 7\n"
        "entry 2:\n"
        "unused\n"
        "entry 3:\n"
        "unused\n");

    std::size_t serialized_size = params.serialize_buffer_size();
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 4 // array length
        
        // first nested:

        + 4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "x"
        + 4 // type
        + 4 // value

        // second nested:

        + 4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "x"
        + 4 // type
        + 4 // value
        + 4 // length of 2nd entry name
        + 4 // "y"
        + 4 // type
        + 4 // value

        // third nested:

        + 4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "x"
        + 4 // type
        + 4 // value
        + 4 // length of 2nd entry name
        + 4 // "y"
        + 4 // type
        + 4 // value
        + 4 // length of 3rd entry name
        + 4 // "z"
        + 4 // type
        + 4 // value

        + 4 // length of 2nd entry name
        + 4 // "i"
        + 4 // type
        + 4 // value
    );

    const char expected[] =
        {
            2, 0, 0, 0,         // num of entries
            4, 0, 0, 0,         // length of "name"
            'n', 'a', 'm', 'e',
            14, 0, 0, 0,        // type code for nested params array
            3, 0, 0, 0,         // array length

            // first nested

            1, 0, 0, 0,         // num of entries
            1, 0, 0, 0,         // length of "x"
            'x', 0, 0, 0,
            2, 0, 0, 0,         // type code for integer
            10, 0, 0, 0,        // value

            // second nested

            2, 0, 0, 0,         // num of entries
            1, 0, 0, 0,         // length of "x"
            'x', 0, 0, 0,
            2, 0, 0, 0,         // type code for integer
            20, 0, 0, 0,        // value
            1, 0, 0, 0,         // length of "y"
            'y', 0, 0, 0,
            2, 0, 0, 0,         // type code for integer
            21, 0, 0, 0,        // value

            // third nested

            3, 0, 0, 0,         // num of entries
            1, 0, 0, 0,         // length of "x"
            'x', 0, 0, 0,
            2, 0, 0, 0,         // type code for integer
            30, 0, 0, 0,        // value
            1, 0, 0, 0,         // length of "y"
            'y', 0, 0, 0,
            2, 0, 0, 0,         // type code for integer
            31, 0, 0, 0,        // value
            1, 0, 0, 0,         // length of "z"
            'z', 0, 0, 0,
            2, 0, 0, 0,         // type code for integer
            32, 0, 0, 0,        // value

            1, 0, 0, 0,         // length of "i"
            'i', 0, 0, 0,
            2, 0, 0, 0,         // type code for integer
            7, 0, 0, 0          // value
        };

    check_serialization(params, expected, serialized_size);
}

// test9() for locking removed

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();
    test8();
    test8a();
//  test9();
}
