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

#include "../parameters.h"
#include "../parameter_iterator.h"
#include "../raw_buffer_data_source.h"
#include <sstream>
#include <cstring>
#include <cassert>
#include <vector>

class ostringstream_dump : public yami::details::dump_sink
{
public:
    virtual void indent(std::size_t spaces)
    {
        os_ << std::string(spaces, ' ');
    }

    virtual void dump(std::size_t v)
    {
        os_ << v;
    }

    virtual void dump(bool v)
    {
        os_ << v;
    }

    virtual void dump(int v)
    {
        os_ << v;
    }

    virtual void dump(long long v)
    {
        os_ << v;
    }

    virtual void dump(double v)
    {
        os_ << v;
    }

    virtual void dump(const char * str)
    {
        os_ << str;
    }

    virtual void dump(const char * str, std::size_t str_len)
    {
        os_ << std::string(str, str_len);
    }

    void clear() { os_.str(""); }
    std::string str() const { return os_.str(); }

private:
    std::ostringstream os_;
};

void test1()
{
    yami::core::parameters params;

    assert(params.size() == 0);

    ostringstream_dump ss;
    params.dump(ss);
    assert(ss.str().empty());

    yami::core::result res = params.remove("no such entry");
    assert(res == yami::core::no_such_name);

    yami::core::parameter_iterator it;
    res = params.get_iterator(it);
    assert(res == yami::core::no_entries);

    yami::core::parameter_entry e;
    res = params.find("no such entry", e);
    assert(res == yami::core::no_such_name);

    std::size_t serialized_size;
    res = params.get_serialize_buffer_size(serialized_size);
    assert(res == yami::core::ok);
    assert(serialized_size == 4); // only number of entries

    std::vector<char> buf(serialized_size);
    char * buffers[] = {&buf[0]};
    std::size_t sizes[] = {serialized_size};
    res = params.serialize(buffers, sizes, 1);
    assert(res == yami::core::ok);
    const char expected[] = {0, 0, 0, 0}; // only num of entries
    assert(std::memcmp(&buf[0], expected, serialized_size) == 0);

    yami::core::parameters params2;
    const char * cbuffers[] = {&buf[0]};
    res = params2.deserialize(cbuffers, sizes, 1);
    assert(res == yami::core::ok);
    assert(params2.size() == 0);
}

void check_serialization(const yami::core::parameters & params,
    const char * expected, std::size_t serialized_size)
{
    {
        // test for serialization into continuous buffer

        std::vector<char> buf(serialized_size);
        char * buffers[] = {&buf[0]};
        std::size_t sizes[] = {serialized_size};
        yami::core::result res = params.serialize(buffers, sizes, 1);
        assert(res == yami::core::ok);
        assert(std::memcmp(&buf[0], expected, serialized_size) == 0);

        // verify deserialization gives the same result

        yami::core::parameters params2;
        const char * cbuffers[] = {&buf[0]};
        res = params2.deserialize(cbuffers, sizes, 1);
        assert(res == yami::core::ok);

        std::size_t serialized_size2;
        res = params2.get_serialize_buffer_size(serialized_size2);
        assert(res == yami::core::ok);
        assert(serialized_size2 == serialized_size);

        std::vector<char> buf2(serialized_size2);
        char * buffers2[] = {&buf2[0]};
        std::size_t sizes2[] = {serialized_size2};
        res = params2.serialize(buffers2, sizes2, 1);
        assert(res == yami::core::ok);
        assert(std::memcmp(&buf[0], &buf2[0], serialized_size) == 0);

        // additional sanity check

        ostringstream_dump ss;
        params.dump(ss);
        ostringstream_dump ss2;
        params2.dump(ss2);
        assert(ss.str() == ss2.str());
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
            yami::core::result res = params.serialize(buffers, sizes, 2);
            assert(res == yami::core::ok);
            assert(std::memcmp(&buf1[0], expected, first_size) == 0);
            assert(std::memcmp(&buf2[0], expected + first_size,
                    second_size) == 0);
        }
    }
}

// test for boolean
void test2()
{
    yami::core::parameters params;

    yami::core::result res = params.set_boolean("name", true);
    assert(res == yami::core::ok);

    assert(params.size() == 1);

    yami::core::parameter_type type;
    res = params.get_type("name", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::boolean);

    bool value = false;
    res = params.get_boolean("name", value);
    assert(res == yami::core::ok);
    assert(value);

    ostringstream_dump ss;
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

    res = params.set_boolean("name", false);
    assert(res == yami::core::ok);

    assert(params.size() == 1);

    res = params.get_boolean("name", value);
    assert(res == yami::core::ok);
    assert(value == false);

    int i;
    res = params.get_integer("name", i);
    assert(res == yami::core::bad_type);

    const char * name;
    std::size_t name_length;
    yami::core::parameter_iterator it;
    res = params.get_iterator(it);
    assert(res == yami::core::ok);
    yami::core::parameter_entry e = it.current();
    assert(e.type() == yami::core::boolean);
    e.get_name(name, name_length);
    assert(res == yami::core::ok);
    assert(std::string(name, name_length) == "name");
    value = true;
    res = e.get_boolean(value);
    assert(res == yami::core::ok);
    assert(value == false);
    res = e.get_integer(i);
    assert(res == yami::core::bad_type);
    assert(it.has_next() == false);

    std::size_t serialized_size;
    res = params.get_serialize_buffer_size(serialized_size);
    assert(res == yami::core::ok);
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 4 // value
    );

    bool array[] = { true, false, true };
    res = params.set_boolean_array(
        "nameA", array, sizeof(array) / sizeof(bool));
    assert(res == yami::core::ok);

    assert(params.size() == 2);

    res = params.get_type("nameA", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::boolean_array);

    bool * values;
    std::size_t array_length;
    res = params.get_boolean_array("nameA", values, array_length);
    assert(array_length == 3);
    assert(values[0] == true);
    assert(values[1] == false);
    assert(values[2] == true);
    values[2] = false;

    res = params.get_iterator(it);
    assert(res == yami::core::ok);
    e = it.current();
    assert(e.type() == yami::core::boolean);
    e.get_name(name, name_length);
    assert(std::string(name, name_length) == "name");
    assert(it.has_next());
    it.move_next();
    e = it.current();
    assert(e.type() == yami::core::boolean_array);
    e.get_name(name, name_length);
    assert(std::string(name, name_length) == "nameA");
    res = e.get_boolean_array(values, array_length);
    assert(res == yami::core::ok);
    assert(array_length == 3);
    assert(values[0] == true);
    assert(values[1] == false);
    assert(values[2] == false);
    assert(it.has_next() == false);

    ss.clear();
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

    res = params.get_serialize_buffer_size(serialized_size);
    assert(res == yami::core::ok);
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

    res = params.find("name", e);
    assert(res == yami::core::ok);
    assert(e.type() == yami::core::boolean);
    e.get_name(name, name_length);
    assert(std::string(name, name_length) == "name");

    res = params.find("nameA", e);
    assert(res == yami::core::ok);
    assert(e.type() == yami::core::boolean_array);
    e.get_name(name, name_length);
    assert(std::string(name, name_length) == "nameA");

    res = params.find("no such name", e);
    assert(res == yami::core::no_such_name);

    res = params.remove("name");
    assert(res == yami::core::ok);

    assert(params.size() == 1);

    res = params.get_iterator(it);
    assert(res == yami::core::ok);
    e = it.current();
    assert(e.type() == yami::core::boolean_array);
    e.get_name(name, name_length);
    assert(std::string(name, name_length) == "nameA");
    assert(it.has_next() == false);
}

// test for integer
void test3()
{
    yami::core::parameters params;

    yami::core::result res = params.set_integer("name", 123);
    assert(res == yami::core::ok);

    assert(params.size() == 1);

    yami::core::parameter_type type;
    res = params.get_type("name", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::integer);

    int value = 0;
    res = params.get_integer("name", value);
    assert(res == yami::core::ok);
    assert(value == 123);

    std::size_t serialized_size;
    res = params.get_serialize_buffer_size(serialized_size);
    assert(res == yami::core::ok);
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 4 // value
    );

    int array[] = { 10, 20, 30 };
    res = params.set_integer_array(
        "nameA", array, sizeof(array) / sizeof(int));
    assert(res == yami::core::ok);

    assert(params.size() == 2);

    res = params.get_type("nameA", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::integer_array);

    int * values;
    std::size_t array_length;
    res = params.get_integer_array("nameA", values, array_length);
    assert(array_length == 3);
    assert(values[0] == 10);
    assert(values[1] == 20);
    assert(values[2] == 30);
    values[2] = 31;

    yami::core::parameter_iterator it;
    res = params.get_iterator(it);
    assert(res == yami::core::ok);
    yami::core::parameter_entry e = it.current();
    assert(e.type() == yami::core::integer);
    const char * name;
    std::size_t name_length;
    e.get_name(name, name_length);
    assert(std::string(name, name_length) == "name");
    value = 0;
    res = e.get_integer(value);
    assert(res == yami::core::ok);
    assert(value == 123);
    assert(it.has_next());
    it.move_next();
    e = it.current();
    e.get_name(name, name_length);
    assert(std::string(name, name_length) == "nameA");
    res = e.get_integer_array(values, array_length);
    assert(res == yami::core::ok);
    assert(array_length == 3);
    assert(values[0] == 10);
    assert(values[1] == 20);
    assert(values[2] == 31);
    assert(it.has_next() == false);

    ostringstream_dump ss;
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

    res = params.get_serialize_buffer_size(serialized_size);
    assert(res == yami::core::ok);
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

    res = params.get_iterator(it);
    assert(res == yami::core::ok);
    e = it.current();
    e.get_name(name, name_length);
    assert(std::string(name, name_length) == "name");
    it.remove();
    assert(params.size() == 1);
}

// test for long long
void test4()
{
    yami::core::parameters params;

    yami::core::result res = params.set_long_long("name", 1234567890LL);
    assert(res == yami::core::ok);

    assert(params.size() == 1);

    yami::core::parameter_type type;
    res = params.get_type("name", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::long_long);

    long long value = 0LL;
    res = params.get_long_long("name", value);
    assert(res == yami::core::ok);
    assert(value == 1234567890LL);

    std::size_t serialized_size;
    res = params.get_serialize_buffer_size(serialized_size);
    assert(res == yami::core::ok);
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 8 // value
    );

    long long array[] = { 100LL, 200LL, 300LL };
    res = params.set_long_long_array(
        "nameA", array, sizeof(array) / sizeof(long long));
    assert(res == yami::core::ok);

    assert(params.size() == 2);

    res = params.get_type("nameA", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::long_long_array);

    long long * values;
    std::size_t array_length;
    res = params.get_long_long_array("nameA", values, array_length);
    assert(array_length == 3);
    assert(values[0] == 100LL);
    assert(values[1] == 200LL);
    assert(values[2] == 300LL);
    values[2] = 310LL;

    yami::core::parameter_iterator it;
    res = params.get_iterator(it);
    assert(res == yami::core::ok);
    yami::core::parameter_entry e = it.current();
    assert(e.type() == yami::core::long_long);
    value = 0LL;
    res = e.get_long_long(value);
    assert(res == yami::core::ok);
    assert(value == 1234567890LL);
    assert(it.has_next());
    it.move_next();
    e = it.current();
    assert(e.type() == yami::core::long_long_array);
    res = e.get_long_long_array(values, array_length);
    assert(res == yami::core::ok);
    assert(array_length == 3);
    assert(values[0] == 100LL);
    assert(values[1] == 200LL);
    assert(values[2] == 310LL);
    assert(it.has_next() == false);

    ostringstream_dump ss;
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

    res = params.get_serialize_buffer_size(serialized_size);
    assert(res == yami::core::ok);
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
    yami::core::parameters params;

    yami::core::result res = params.set_double_float("name", 3.125);
    assert(res == yami::core::ok);

    assert(params.size() == 1);

    yami::core::parameter_type type;
    res = params.get_type("name", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::double_float);

    double value = 0;
    res = params.get_double_float("name", value);
    assert(res == yami::core::ok);
    assert(value == 3.125);

    std::size_t serialized_size;
    res = params.get_serialize_buffer_size(serialized_size);
    assert(res == yami::core::ok);
    assert(serialized_size ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 8 // value
    );

    double array[] = { 1.875, 2.875, 3.875 };
    res = params.set_double_float_array(
        "nameA", array, sizeof(array) / sizeof(double));
    assert(res == yami::core::ok);

    assert(params.size() == 2);

    res = params.get_type("nameA", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::double_float_array);

    double * values;
    std::size_t array_length;
    res = params.get_double_float_array("nameA", values, array_length);
    assert(array_length == 3);
    assert(values[0] == 1.875);
    assert(values[1] == 2.875);
    assert(values[2] == 3.875);
    values[2] = 3.625;

    yami::core::parameter_iterator it;
    res = params.get_iterator(it);
    assert(res == yami::core::ok);
    yami::core::parameter_entry e = it.current();
    assert(e.type() == yami::core::double_float);
    value = 0.0;
    res = e.get_double_float(value);
    assert(res == yami::core::ok);
    assert(value == 3.125);
    assert(it.has_next());
    it.move_next();
    e = it.current();
    assert(e.type() == yami::core::double_float_array);
    res = e.get_double_float_array(values, array_length);
    assert(res == yami::core::ok);
    assert(array_length == 3);
    assert(values[0] == 1.875);
    assert(values[1] == 2.875);
    assert(values[2] == 3.625);
    assert(it.has_next() == false);

    ostringstream_dump ss;
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

    res = params.get_serialize_buffer_size(serialized_size);
    assert(res == yami::core::ok);
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
            0, 0, static_cast<char>(-2), 63,
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
    yami::core::parameters params;

    const char * source_value =
        "Kolorowe kredki w pudeleczku nosze,\n"
        "kolorowe kredki, bardzo lubie je!";

    yami::core::result res =
        params.set_string("some rather longer name", source_value);

    assert(res == yami::core::ok);

    assert(params.size() == 1);

    yami::core::parameter_type type;
    res = params.get_type("some rather longer name", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::string);

    const char * value;
    std::size_t value_length;
    res = params.get_string("some rather longer name", value, value_length);
    assert(res == yami::core::ok);
    assert(value != source_value);
    assert(std::string(value, value_length) == source_value);

    res = params.set_string_shallow("other name", source_value);

    assert(res == yami::core::ok);

    assert(params.size() == 2);

    res = params.get_string("other name", value, value_length);
    assert(res == yami::core::ok);
    assert(value == source_value);

    std::size_t serialized_size;
    res = params.get_serialize_buffer_size(serialized_size);
    assert(res == yami::core::ok);
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
    res = params.create_string_array(
        "nameA", sizeof(array) / sizeof(const char *));
    assert(res == yami::core::ok);

    res = params.get_type("nameA", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::string_array);

    res = params.get_string_in_array("nameA", 0, value, value_length);
    assert(res == yami::core::ok);
    assert(value == NULL);
    assert(value_length == 0);
    res = params.get_string_in_array("nameA", 3, value, value_length);
    assert(res == yami::core::ok);
    assert(value == NULL);
    assert(value_length == 0);
    res = params.get_string_in_array("nameA", 4, value, value_length);
    assert(res == yami::core::no_such_index);

    res = params.set_string_in_array("nameA", 0, array[0]);
    assert(res == yami::core::ok);
    res = params.set_string_in_array("nameA", 1, array[1]);
    assert(res == yami::core::ok);
    res = params.set_string_in_array("nameA", 2, array[2]);
    assert(res == yami::core::ok);
    res = params.set_string_in_array("nameA", 3, array[3]);
    assert(res == yami::core::ok);

    res = params.get_string_in_array("nameA", 0, value, value_length);
    assert(res == yami::core::ok);
    assert(std::string(value, value_length) == array[0]);
    assert(value_length == std::strlen(array[0]));
    res = params.get_string_in_array("nameA", 3, value, value_length);
    assert(res == yami::core::ok);
    assert(std::string(value, value_length) == array[3]);
    assert(value_length == std::strlen(array[3]));
    
    yami::core::parameter_iterator it;
    res = params.get_iterator(it);
    assert(res == yami::core::ok);
    yami::core::parameter_entry e = it.current();
    assert(e.type() == yami::core::string);
    res = e.get_string(value, value_length);
    assert(res == yami::core::ok);
    assert(std::string(value, value_length) == source_value);
    assert(it.has_next());
    it.move_next();
    e = it.current();
    assert(e.type() == yami::core::string);
    res = e.get_string(value, value_length);
    assert(res == yami::core::ok);
    assert(value == source_value);
    assert(it.has_next());
    it.move_next();
    e = it.current();
    assert(e.type() == yami::core::string_array);
    res = e.get_string_in_array(1, value, value_length);
    assert(res == yami::core::ok);
    assert(std::string(value, value_length) == array[1]);
    assert(value_length == std::strlen(array[1]));
    assert(it.has_next() == false);

    ostringstream_dump ss;
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

    res = params.get_serialize_buffer_size(serialized_size);
    assert(res == yami::core::ok);
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
    yami::core::parameters params;

    const void * source_value = "abc\0d";
    const std::size_t source_length = 5;

    yami::core::result res =
        params.set_binary("some rather longer name",
            source_value, source_length);

    assert(res == yami::core::ok);

    assert(params.size() == 1);

    yami::core::parameter_type type;
    res = params.get_type("some rather longer name", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::binary);

    const void * value;
    std::size_t value_length;
    res = params.get_binary("some rather longer name", value, value_length);
    assert(res == yami::core::ok);
    assert(value != source_value);
    assert(value_length == source_length);
    assert(std::memcmp(value, source_value, value_length) == 0);

    res = params.set_binary_shallow("other name",
        source_value, source_length);

    assert(res == yami::core::ok);

    assert(params.size() == 2);

    res = params.get_binary("other name", value, value_length);
    assert(res == yami::core::ok);
    assert(value == source_value);

    std::size_t serialized_size;
    res = params.get_serialize_buffer_size(serialized_size);
    assert(res == yami::core::ok);
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
    res = params.create_binary_array(
        "nameA", sizeof(array) / sizeof(const void *));
    assert(res == yami::core::ok);

    res = params.get_type("nameA", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::binary_array);

    res = params.get_binary_in_array("nameA", 0, value, value_length);
    assert(res == yami::core::ok);
    assert(value == NULL);
    assert(value_length == 0);
    res = params.get_binary_in_array("nameA", 2, value, value_length);
    assert(res == yami::core::ok);
    assert(value == NULL);
    assert(value_length == 0);
    res = params.get_binary_in_array("nameA", 3, value, value_length);
    assert(res == yami::core::no_such_index);

    res = params.set_binary_in_array("nameA", 0, array[0], 3);
    assert(res == yami::core::ok);
    res = params.set_binary_in_array("nameA", 1, array[1], 2);
    assert(res == yami::core::ok);
    res = params.set_binary_in_array("nameA", 2, array[2], 3);
    assert(res == yami::core::ok);

    res = params.get_binary_in_array("nameA", 0, value, value_length);
    assert(res == yami::core::ok);
    assert(value_length == 3);
    assert(std::memcmp(value, "abc", 3) == 0);
    res = params.get_binary_in_array("nameA", 1, value, value_length);
    assert(res == yami::core::ok);
    assert(value_length == 2);
    assert(std::memcmp(value, "klm", 2) == 0);
    
    yami::core::parameter_iterator it;
    res = params.get_iterator(it);
    assert(res == yami::core::ok);
    yami::core::parameter_entry e = it.current();
    assert(e.type() == yami::core::binary);
    res = e.get_binary(value, value_length);
    assert(res == yami::core::ok);
    assert(value_length == source_length);
    assert(std::memcmp(value, source_value, value_length) == 0);
    assert(it.has_next());
    it.move_next();
    e = it.current();
    assert(e.type() == yami::core::binary);
    res = e.get_binary(value, value_length);
    assert(res == yami::core::ok);
    assert(value == source_value);
    assert(it.has_next());
    it.move_next();
    e = it.current();
    assert(e.type() == yami::core::binary_array);
    res = e.get_binary_in_array(1, value, value_length);
    assert(res == yami::core::ok);
    assert(value_length == 2);
    assert(std::memcmp(value, "klm", 2) == 0);
    assert(it.has_next() == false);

    ostringstream_dump ss;
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

    res = params.get_serialize_buffer_size(serialized_size);
    assert(res == yami::core::ok);
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
            12, 0, 0, 0,        // type code for string array
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
    yami::core::parameters params;

    yami::core::result res = params.set_integer("name", 123);
    assert(res == yami::core::ok);

    assert(params.size() == 1);

    yami::core::parameters * nested;
    res = params.create_nested_parameters("nested", nested);
    assert(res == yami::core::ok);

    assert(params.size() == 2);

    res = nested->set_integer("internal", 456);
    assert(res == yami::core::ok);

    assert(nested->size() == 1);

    res = nested->set_double_float("internal2", 3.125);
    assert(res == yami::core::ok);

    assert(params.size() == 2);
    assert(nested->size() == 2);

    yami::core::parameters * tmp;
    res = params.get_nested_parameters("nested", tmp);
    assert(res == yami::core::ok);
    assert(tmp == nested);

    yami::core::parameters * nested2;
    res = nested->create_nested_parameters("more nested", nested2);
    assert(res == yami::core::ok);

    assert(params.size() == 2);
    assert(nested->size() == 3);
    
    res = nested2->set_integer("more internal", 789);
    assert(res == yami::core::ok);

    assert(params.size() == 2);
    assert(nested->size() == 3);
    assert(nested2->size() == 1);

    yami::core::parameter_type type;
    res = params.get_type("name", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::integer);
    res = params.get_type("nested", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::nested_parameters);
    res = nested->get_type("internal", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::integer);
    res = nested->get_type("internal2", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::double_float);
    res = nested->get_type("more nested", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::nested_parameters);
    res = nested2->get_type("more internal", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::integer);
    res = nested->get_type("blabla", type);
    assert(res == yami::core::no_such_name);

    yami::core::parameter_iterator it;
    res = params.get_iterator(it);
    assert(res == yami::core::ok);
    yami::core::parameter_entry e = it.current();
    assert(e.type() == yami::core::integer);
    assert(it.has_next());
    it.move_next();
    e = it.current();
    assert(e.type() == yami::core::nested_parameters);
    res = e.get_nested_parameters(nested);
    assert(res == yami::core::ok);
    assert(nested->size() == 3);
    assert(it.has_next() == false);

    ostringstream_dump ss;
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

    std::size_t serialized_size;
    res = params.get_serialize_buffer_size(serialized_size);
    assert(res == yami::core::ok);
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
            8, 0, 0, 0,         // length o "internal"
            'i', 'n', 't', 'e',
            'r', 'n', 'a', 'l',
            2, 0, 0, 0,         // type code for integer
            static_cast<char>(-56), 1, 0, 0, // value
            9, 0, 0, 0,         // length o "internal2"
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
    yami::core::parameters params;

    yami::core::result res = params.create_nested_array("name", 3);
    assert(res == yami::core::ok);
    assert(params.size() == 1);

    res = params.set_integer("i", 7);
    assert(res == yami::core::ok);
    assert(params.size() == 2);

    std::size_t array_length;
    res = params.get_nested_array_length("name", array_length);
    assert(array_length == 3);

    yami::core::parameters * nested;

    // first nested

    res = params.get_nested_in_array("name", 0, nested);
    assert(res == yami::core::ok);

    res = nested->set_integer("x", 10);
    assert(res == yami::core::ok);

    assert(nested->size() == 1);

    // second nested

    res = params.get_nested_in_array("name", 1, nested);
    assert(res == yami::core::ok);

    res = nested->set_integer("x", 20);
    assert(res == yami::core::ok);
    res = nested->set_integer("y", 21);
    assert(res == yami::core::ok);

    assert(nested->size() == 2);

    // third nested

    res = params.get_nested_in_array("name", 2, nested);
    assert(res == yami::core::ok);

    res = nested->set_integer("x", 30);
    assert(res == yami::core::ok);
    res = nested->set_integer("y", 31);
    assert(res == yami::core::ok);
    res = nested->set_integer("z", 32);
    assert(res == yami::core::ok);

    assert(nested->size() == 3);

    // no more nested

    res = params.get_nested_in_array("name", 3, nested);
    assert(res == yami::core::no_such_index);

    yami::core::parameter_type type;
    res = params.get_type("name", type);
    assert(res == yami::core::ok);
    assert(type == yami::core::nested_parameters_array);

    yami::core::parameter_iterator it;
    res = params.get_iterator(it);
    assert(res == yami::core::ok);
    yami::core::parameter_entry e = it.current();
    assert(e.type() == yami::core::nested_parameters_array);
    assert(it.has_next());
    it.move_next();
    e = it.current();
    assert(e.type() == yami::core::integer);
    assert(it.has_next() == false);

    ostringstream_dump ss;
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

    std::size_t serialized_size;
    res = params.get_serialize_buffer_size(serialized_size);
    assert(res == yami::core::ok);
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

// test for raw buffer serialization
void test10()
{
    // single buffer -> single buffer
    {
        const char src_buf[] = "abcdefghijk";

        const char * src_buffers[1];
        src_buffers[0] = src_buf;
        std::size_t src_buffer_sizes[1];
        src_buffer_sizes[0] = sizeof(src_buf);
        std::size_t num_of_src_buffers = 1;

        yami::core::raw_buffer_data_source rb(
            src_buffers, src_buffer_sizes, num_of_src_buffers);

        std::size_t serialize_size;
        yami::core::result res = rb.get_serialize_buffer_size(serialize_size);
        assert(res == yami::core::ok);
        assert(serialize_size == sizeof(src_buf));

        char trg_buf[sizeof(src_buf)];

        char * trg_buffers[1];
        trg_buffers[0] = trg_buf;
        std::size_t trg_buffer_sizes[1];
        trg_buffer_sizes[0] = sizeof(trg_buf);
        std::size_t num_of_trg_buffers = 1;

        std::memset(trg_buf, 0, sizeof(trg_buf));
        res = rb.serialize(trg_buffers, trg_buffer_sizes, num_of_trg_buffers);

        assert(res == yami::core::ok);
        assert(std::memcmp(src_buf, trg_buf, sizeof(src_buf)) == 0);

        // additional test for simple constructor

        yami::core::raw_buffer_data_source rb2(src_buf, sizeof(src_buf));

        std::memset(trg_buf, 0, sizeof(trg_buf));
        res = rb.serialize(trg_buffers, trg_buffer_sizes, num_of_trg_buffers);

        assert(res == yami::core::ok);
        assert(std::memcmp(src_buf, trg_buf, sizeof(src_buf)) == 0);

    }

    // single buffer -> two buffers
    {
        const char src_buf[] = "abcdefghijk";

        const char * src_buffers[1];
        src_buffers[0] = src_buf;
        std::size_t src_buffer_sizes[1];
        src_buffer_sizes[0] = sizeof(src_buf);
        std::size_t num_of_src_buffers = 1;

        yami::core::raw_buffer_data_source rb(
            src_buffers, src_buffer_sizes, num_of_src_buffers);

        std::size_t serialize_size;
        yami::core::result res = rb.get_serialize_buffer_size(serialize_size);
        assert(res == yami::core::ok);
        assert(serialize_size == sizeof(src_buf));

        const std::size_t target_size1 = 8;
        const std::size_t target_size2 = sizeof(src_buf) - target_size1;

        char trg_buf1[target_size1];
        double dummy_stack_separator;
        char trg_buf2[target_size2];

        char * trg_buffers[2];
        trg_buffers[0] = trg_buf1;
        trg_buffers[1] = trg_buf2;
        std::size_t trg_buffer_sizes[2];
        trg_buffer_sizes[0] = sizeof(trg_buf1);
        trg_buffer_sizes[1] = sizeof(trg_buf2);
        std::size_t num_of_trg_buffers = 2;

        std::memset(trg_buf1, 0, sizeof(trg_buf1));
        std::memset(trg_buf2, 0, sizeof(trg_buf2));
        res = rb.serialize(trg_buffers, trg_buffer_sizes, num_of_trg_buffers);

        assert(res == yami::core::ok);
        assert(std::memcmp(src_buf, trg_buf1, sizeof(trg_buf1)) == 0);
        assert(std::memcmp(
                src_buf + sizeof(trg_buf1), trg_buf2, sizeof(trg_buf2)) == 0);

        // additional test for simple constructor

        yami::core::raw_buffer_data_source rb2(src_buf, sizeof(src_buf));

        std::memset(trg_buf1, 0, sizeof(trg_buf1));
        std::memset(trg_buf2, 0, sizeof(trg_buf2));
        res = rb.serialize(trg_buffers, trg_buffer_sizes, num_of_trg_buffers);

        assert(res == yami::core::ok);
        assert(std::memcmp(src_buf, trg_buf1, sizeof(trg_buf1)) == 0);
        assert(std::memcmp(
                src_buf + sizeof(trg_buf1), trg_buf2, sizeof(trg_buf2)) == 0);
    }

    // two buffers -> single buffer
    {
        const char src_buf1[] = "abc";
        double dummy_stack_separator;
        const char src_buf2[] = "efghijk";

        const char * src_buffers[2];
        src_buffers[0] = src_buf1;
        src_buffers[1] = src_buf2;
        std::size_t src_buffer_sizes[2];
        src_buffer_sizes[0] = sizeof(src_buf1);
        src_buffer_sizes[1] = sizeof(src_buf2);
        std::size_t num_of_src_buffers = 2;

        yami::core::raw_buffer_data_source rb(
            src_buffers, src_buffer_sizes, num_of_src_buffers);

        std::size_t serialize_size;
        yami::core::result res = rb.get_serialize_buffer_size(serialize_size);
        assert(res == yami::core::ok);
        assert(serialize_size == sizeof(src_buf1) + sizeof(src_buf2));

        char trg_buf[sizeof(src_buf1) + sizeof(src_buf2)];

        char * trg_buffers[1];
        trg_buffers[0] = trg_buf;
        std::size_t trg_buffer_sizes[1];
        trg_buffer_sizes[0] = sizeof(trg_buf);
        std::size_t num_of_trg_buffers = 1;

        std::memset(trg_buf, 0, sizeof(trg_buf));
        res = rb.serialize(trg_buffers, trg_buffer_sizes, num_of_trg_buffers);

        assert(res == yami::core::ok);
        assert(std::memcmp(src_buf1, trg_buf, sizeof(src_buf1)) == 0);
        assert(std::memcmp(
                src_buf2, trg_buf + sizeof(src_buf1), sizeof(src_buf2)) == 0);
    }

    // two buffers -> two buffers (of different sizes)
    {
        const char src_buf1[] = "abc";
        double dummy_stack_separator1;
        const char src_buf2[] = "efghijk";

        const char * src_buffers[2];
        src_buffers[0] = src_buf1;
        src_buffers[1] = src_buf2;
        std::size_t src_buffer_sizes[2];
        src_buffer_sizes[0] = sizeof(src_buf1);
        src_buffer_sizes[1] = sizeof(src_buf2);
        std::size_t num_of_src_buffers = 2;

        yami::core::raw_buffer_data_source rb(
            src_buffers, src_buffer_sizes, num_of_src_buffers);

        std::size_t serialize_size;
        yami::core::result res = rb.get_serialize_buffer_size(serialize_size);
        assert(res == yami::core::ok);
        assert(serialize_size == sizeof(src_buf1) + sizeof(src_buf2));

        const std::size_t target_size1 = 8;
        const std::size_t target_size2 = 4;

        char trg_buf1[target_size1];
        double dummy_stack_separator2;
        char trg_buf2[target_size2];

        char * trg_buffers[2];
        trg_buffers[0] = trg_buf1;
        trg_buffers[1] = trg_buf2;
        std::size_t trg_buffer_sizes[2];
        trg_buffer_sizes[0] = sizeof(trg_buf1);
        trg_buffer_sizes[1] = sizeof(trg_buf2);
        std::size_t num_of_trg_buffers = 2;

        std::memset(trg_buf1, 0, sizeof(trg_buf1));
        std::memset(trg_buf2, 0, sizeof(trg_buf2));
        res = rb.serialize(trg_buffers, trg_buffer_sizes, num_of_trg_buffers);

        assert(res == yami::core::ok);
        assert(std::memcmp(trg_buf1, "abc\0efgh", sizeof(trg_buf1)) == 0);
        assert(std::memcmp(trg_buf2, "ijk", sizeof(trg_buf2)) == 0);
    }
}

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
    test10();
}
