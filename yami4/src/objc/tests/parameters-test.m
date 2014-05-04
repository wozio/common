// Copyright Maciej Sobczak 2008-2014.
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

#import <yami4-objc/Exception.h>
#import <yami4-objc/Parameters.h>

#import <Foundation/NSAutoreleasePool.h>
#include <assert.h>
#include <stdlib.h>

void test1()
{
    YAMI4Parameters * params = [YAMI4Parameters new];

    assert([params size] == 0);

    assert([[params dump] isEqualToString:@""]);

    @try
    {
        [params remove:@"no such entry"];
        assert(false);
    }
    @catch (YAMI4Exception * e)
    {
        assert([[e reason] isEqualToString:@"No such name."]);
    }

    YAMI4ParametersIterator * it = [params begin];
    assert([it isEqual:[params end]]);

    YAMI4ParameterEntry * entry;
    BOOL found = [params find:@"no such entry" entry:&entry];
    assert(found == NO);

    size_t serializedSize = [params serializeBufferSize];
    assert(serializedSize == 4); // only number of entries

    char * buf = (char *)malloc(serializedSize);
    char * buffers[] = {buf};
    size_t sizes[] = {serializedSize};
    [params serialize:buffers bufferSizes:sizes numOfBuffers:1];
    const char expected[] = {0, 0, 0, 0}; // only num of entries
    assert(memcmp(&buf[0], expected, serializedSize) == 0);

    YAMI4Parameters * params2 = [YAMI4Parameters new];
    const char * cbuffers[] = {buf};
    [params2 deserialize:cbuffers bufferSizes:sizes numOfBuffers:1];
    assert([params2 size] == 0);

    free(buf);

    [params2 clear];
    assert([params2 size] == 0);
}

void checkSerialization(YAMI4Parameters * params,
    const char * expected, size_t serializedSize)
{
    {
        // test for serialization into continuous buffer

        char * buf = (char *)malloc(serializedSize);
        char * buffers[] = {buf};
        size_t sizes[] = {serializedSize};
        [params serialize:buffers bufferSizes:sizes numOfBuffers:1];
        assert(memcmp(buf, expected, serializedSize) == 0);

        // verify deserialization gives the same result

        YAMI4Parameters * params2 = [YAMI4Parameters new];
        const char * cbuffers[] = {buf};
        [params2 deserialize:cbuffers bufferSizes:sizes numOfBuffers:1];

        size_t serializedSize2 = [params2 serializeBufferSize];
        assert(serializedSize2 == serializedSize);

        char * buf2 = (char *)malloc(serializedSize2);
        char * buffers2[] = {buf2};
        size_t sizes2[] = {serializedSize2};
        [params serialize:buffers2 bufferSizes:sizes2 numOfBuffers:1];
        assert(memcmp(buf, buf2, serializedSize) == 0);
    }
    {
        // test for serialization into two buffers
        // for different size relations

        size_t firstSize;
        for (firstSize = 4; firstSize != serializedSize; firstSize += 4)
        {
            const size_t secondSize = serializedSize - firstSize;
            char * buf1 = (char *)malloc(firstSize);
            char * buf2 = (char *)malloc(secondSize);
            char * buffers[] = {buf1, buf2};
            size_t sizes[] = {firstSize, secondSize};
            [params serialize:buffers bufferSizes:sizes numOfBuffers:2];
            assert(memcmp(buf1, expected, firstSize) == 0);
            assert(memcmp(buf2, expected + firstSize,
                    secondSize) == 0);
        }
    }
}

// test for boolean
void test2()
{
    YAMI4Parameters * params = [YAMI4Parameters new];

    [params setBoolean:@"name" value:YES];

    assert([params size] == 1);

    enum YAMI4ParameterType type = [params type:@"name"];
    assert(type == YAMI4Boolean);

    BOOL value = [params getBoolean:@"name"];
    assert(value);
    
    assert([[params dump] isEqualToString:
        @"entry 0:\n"
        "name: name\n"
        "boolean: 1\n"
        "entry 1:\n"
        "unused\n"
        "entry 2:\n"
        "unused\n"
        "entry 3:\n"
        "unused\n"]);

    [params setBoolean:@"name" value:NO];

    assert([params size] == 1);

    value = [params getBoolean:@"name"];
    assert(value == false);

    @try
    {
        (void) [params getInteger:@"name"];
        assert(false);
    }
    @catch (YAMI4Exception * e)
    {
        assert([[e reason] isEqualToString:@"Bad type."]);
    }

    YAMI4ParametersIterator * it = [params begin];
    YAMI4ParameterEntry * e = [it entry];
    assert([e type] == YAMI4Boolean);
    assert([[e name] isEqualToString:@"name"]);
    value = [e getBoolean];
    assert(value == NO);
    
    @try
    {
        (void) [e getInteger];
        assert(false);
    }
    @catch (YAMI4Exception * ex)
    {
        assert([[ex reason] isEqualToString:@"Bad type."]);
    }

    [it next];
    assert([it isEqual:[params end]]);

    size_t serializedSize = [params serializeBufferSize];
    assert(serializedSize ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 4 // value
    );

    BOOL array[] = { YES, NO, YES };
    [params setBooleanArray:@"nameA" values:array
        arrayLength:(sizeof(array) / sizeof(BOOL))];

    assert([params size] == 2);

    type = [params type:@"nameA"];
    assert(type == YAMI4BooleanArray);

    size_t arrayLength;
    BOOL * values = [params getBooleanArray:@"nameA"
        arrayLength:&arrayLength];
    assert(arrayLength == 3);
    assert(values[0] == YES);
    assert(values[1] == NO);
    assert(values[2] == YES);
    values[2] = NO;  // this write does not affect params

    it = [params begin];
    e = [it entry];
    assert([e type] == YAMI4Boolean);
    assert([[e name] isEqualToString:@"name"]);
    [it next];
    assert([it isEqual:[params end]] == NO);
    e = [it entry];
    assert([e type] == YAMI4BooleanArray);
    assert([[e name] isEqualToString:@"nameA"]);
    values = [e getBooleanArray:&arrayLength];
    assert(arrayLength == 3);
    assert(values[0] == YES);
    assert(values[1] == NO);
    assert(values[2] == YES);
    
    [it next];
    assert([it isEqual:[params end]]);
    
    assert([[params dump] isEqualToString:
        @"entry 0:\n"
        "name: name\n"
        "boolean: 0\n"
        "entry 1:\n"
        "name: nameA\n"
        "boolean array: 1 0 1\n"
        "entry 2:\n"
        "unused\n"
        "entry 3:\n"
        "unused\n"]);

    serializedSize = [params serializeBufferSize];
    assert(serializedSize ==
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
            0x5, 0, 0, 0        // packed array (101 -> 0x5)
        };

    checkSerialization(params, expected, serializedSize);

    BOOL found = [params find:@"name" entry:&e];
    assert(found);
    assert([e type] == YAMI4Boolean);
    assert([[e name] isEqualToString:@"name"]);

    found = [params find:@"nameA" entry:&e];
    assert(found);
    assert([e type] == YAMI4BooleanArray);
    assert([[e name] isEqualToString:@"nameA"]);

    found = [params find:@"no such name" entry:&e];
    assert(found == NO);

    [params remove:@"name"];

    assert([params size] == 1);

    it = [params begin];
    e = [it entry];
    assert([e type] == YAMI4BooleanArray);
    assert([[e name] isEqualToString:@"nameA"]);

    [it next];
    assert([it isEqual:[params end]]);

    [params clear];
    assert([params size] == 0);
}

// test for integer
void test3()
{
    YAMI4Parameters * params = [YAMI4Parameters new];

    [params setInteger:@"name" value:123];

    assert([params size] == 1);

    enum YAMI4ParameterType type = [params type:@"name"];
    assert(type == YAMI4Integer);

    int value = [params getInteger:@"name"];
    assert(value == 123);

    size_t serializedSize = [params serializeBufferSize];
    assert(serializedSize ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 4 // value
    );

    int array[] = { 10, 20, 30 };
    [params setIntegerArray:@"nameA" values:array
        arrayLength:(sizeof(array) / sizeof(int))];

    assert([params size] == 2);

    type = [params type:@"nameA"];
    assert(type == YAMI4IntegerArray);

    size_t arrayLength;
    int * values = [params getIntegerArray:@"nameA" arrayLength:&arrayLength];
    assert(arrayLength == 3);
    assert(values[0] == 10);
    assert(values[1] == 20);
    assert(values[2] == 30);
    values[2] = 31;

    YAMI4ParametersIterator * it = [params begin];
    YAMI4ParameterEntry * e = [it entry];
    assert([e type] == YAMI4Integer);
    assert([[e name] isEqualToString:@"name"]);
    value = [e getInteger];
    assert(value == 123);
    [it next];
    assert([it isEqual:[params end]] == NO);
    e = [it entry];
    assert([[e name] isEqualToString:@"nameA"]);
    values = [e getIntegerArray:&arrayLength];
    assert(arrayLength == 3);
    assert(values[0] == 10);
    assert(values[1] == 20);
    assert(values[2] == 31);
    [it next];
    assert([it isEqual:[params end]]);

    assert([[params dump] isEqualToString:
        @"entry 0:\n"
        "name: name\n"
        "integer: 123\n"
        "entry 1:\n"
        "name: nameA\n"
        "integer array: 10 20 31\n"
        "entry 2:\n"
        "unused\n"
        "entry 3:\n"
        "unused\n"]);

    serializedSize = [params serializeBufferSize];
    assert(serializedSize ==
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

    checkSerialization(params, expected, serializedSize);

    it = [params begin];
    e = [it entry];
    assert([[e name] isEqualToString:@"name"]);
    [params removeAt:it];
    assert([params size] == 1);
}

// test for long long
void test4()
{
    YAMI4Parameters * params = [YAMI4Parameters new];

    [params setLongLong:@"name" value:1234567890LL];

    assert([params size] == 1);

    enum YAMI4ParameterType type = [params type:@"name"];
    assert(type == YAMI4LongLong);

    long long value = [params getLongLong:@"name"];
    assert(value == 1234567890LL);

    size_t serializedSize = [params serializeBufferSize];
    assert(serializedSize ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 8 // value
    );

    long long array[] = { 100LL, 200LL, 300LL };
    [params setLongLongArray:@"nameA" values:array
        arrayLength:(sizeof(array) / sizeof(long long))];

    assert([params size] == 2);

    type = [params type:@"nameA"];
    assert(type == YAMI4LongLongArray);

    size_t arrayLength;
    long long * values = [params getLongLongArray:@"nameA" arrayLength:&arrayLength];
    assert(arrayLength == 3);
    assert(values[0] == 100LL);
    assert(values[1] == 200LL);
    assert(values[2] == 300LL);
    values[2] = 310LL;

    YAMI4ParametersIterator * it = [params begin];
    YAMI4ParameterEntry * e = [it entry];
    assert([e type] == YAMI4LongLong);
    assert([[e name] isEqualToString:@"name"]);
    value = [e getLongLong];
    assert(value == 1234567890LL);
    [it next];
    assert([it isEqual:[params end]] == NO);
    e = [it entry];
    assert([[e name] isEqualToString:@"nameA"]);
    values = [e getLongLongArray:&arrayLength];
    assert(arrayLength == 3);
    assert(values[0] == 100LL);
    assert(values[1] == 200LL);
    assert(values[2] == 310LL);
    [it next];
    assert([it isEqual:[params end]]);

    assert([[params dump] isEqualToString:
        @"entry 0:\n"
        "name: name\n"
        "long_long: 1234567890\n"
        "entry 1:\n"
        "name: nameA\n"
        "long_long array: 100 200 310\n"
        "entry 2:\n"
        "unused\n"
        "entry 3:\n"
        "unused\n"]);

    serializedSize = [params serializeBufferSize];
    assert(serializedSize ==
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

            (char)210, 2,
            (char)150, 73,      // 1234567890

            0, 0, 0, 0,
            5, 0, 0, 0,         // length of "nameA"
            'n', 'a', 'm', 'e',
            'A', 0, 0, 0,
            9, 0, 0, 0,         // type code for long long array
            3, 0, 0, 0,         // length of array
            100, 0, 0, 0,       // array values
            0, 0, 0, 0,
            (char)200, 0, 0, 0, // array values
            0, 0, 0, 0,
            54, 1, 0, 0,        // array values
            0, 0, 0, 0
        };

    checkSerialization(params, expected, serializedSize);
}

// test for double
void test5()
{
    YAMI4Parameters * params = [YAMI4Parameters new];

    [params setDouble:@"name" value:3.125];

    assert([params size] == 1);

    enum YAMI4ParameterType type = [params type:@"name"];
    assert(type == YAMI4Double);

    double value = [params getDouble:@"name"];
    assert(value == 3.125);

    size_t serializedSize = [params serializeBufferSize];
    assert(serializedSize ==
        4 // number of entries
        + 4 // length of 1st entry name
        + 4 // "name"
        + 4 // type
        + 8 // value
    );

    double array[] = { 1.875, 2.875, 3.875 };
    [params setDoubleArray:@"nameA" values:array
        arrayLength:(sizeof(array) / sizeof(double))];

    assert([params size] == 2);

    type = [params type:@"nameA"];
    assert(type == YAMI4DoubleArray);

    size_t arrayLength;
    double * values = [params getDoubleArray:@"nameA" arrayLength:&arrayLength];
    assert(arrayLength == 3);
    assert(values[0] == 1.875);
    assert(values[1] == 2.875);
    assert(values[2] == 3.875);
    values[2] = 3.625;

    YAMI4ParametersIterator * it = [params begin];
    YAMI4ParameterEntry * e = [it entry];
    assert([e type] == YAMI4Double);
    assert([[e name] isEqualToString:@"name"]);
    value = [e getDouble];
    assert(value == 3.125);
    [it next];
    assert([it isEqual:[params end]] == NO);
    e = [it entry];
    assert([e type] == YAMI4DoubleArray);
    values = [e getDoubleArray:&arrayLength];
    assert(arrayLength == 3);
    assert(values[0] == 1.875);
    assert(values[1] == 2.875);
    assert(values[2] == 3.625);
    [it next];
    assert([it isEqual:[params end]]);

    assert([[params dump] isEqualToString:
        @"entry 0:\n"
        "name: name\n"
        "double: 3.125\n"
        "entry 1:\n"
        "name: nameA\n"
        "double array: 1.875 2.875 3.625\n"
        "entry 2:\n"
        "unused\n"
        "entry 3:\n"
        "unused\n"]);

    serializedSize = [params serializeBufferSize];
    assert(serializedSize ==
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

    checkSerialization(params, expected, serializedSize);
}

// test for strings
void test6()
{
    YAMI4Parameters * params = [YAMI4Parameters new];

    NSString * sourceValue =
        @"Kolorowe kredki w pudeleczku nosze,\n"
        "kolorowe kredki, bardzo lubie je!";

    [params setString:@"some rather longer name" value:sourceValue];

    assert([params size] == 1);

    enum YAMI4ParameterType type = [params type:@"some rather longer name"];
    assert(type == YAMI4String);

    NSString * value = [params getString:@"some rather longer name"];
    assert([value isEqualToString:sourceValue]);

    [params setString:@"other name" value:sourceValue];

    assert([params size] == 2);

    value = [params getString:@"other name"];
    assert([value isEqualToString:sourceValue]);

    size_t serializedSize = [params serializeBufferSize];
    assert(serializedSize ==
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

    NSString * array[] = {@"Kazio", @"Krzysio", @"Rysio", @"Zbysio"};
    [params createStringArray:@"nameA"
        arrayLength:(sizeof(array) / sizeof(NSString *))];

    type = [params type:@"nameA"];
    assert(type == YAMI4StringArray);

    assert([[params getStringInArray:@"nameA" index:0] length] == 0);
    assert([[params getStringInArray:@"nameA" index:3] length] == 0);

    @try
    {
        (void) [params getStringInArray:@"nameA" index:4];
        assert(false);
    }
    @catch (YAMI4Exception * e)
    {
        assert([[e reason] isEqualToString:@"No such index."]);
    }

    [params setStringInArray:@"nameA" index:0 value:array[0]];
    [params setStringInArray:@"nameA" index:1 value:array[1]];
    [params setStringInArray:@"nameA" index:2 value:array[2]];
    [params setStringInArray:@"nameA" index:3 value:array[3]];

    assert([[params getStringInArray:@"nameA" index:0] isEqualToString:array[0]]);
    assert([[params getStringInArray:@"nameA" index:3] isEqualToString:array[3]]);
    
    YAMI4ParametersIterator * it = [params begin];
    YAMI4ParameterEntry * e = [it entry];
    assert([e type] == YAMI4String);
    assert([[e getString] isEqualToString:sourceValue]);
    [it next];
    assert([it isEqual:[params end]] == NO);
    e = [it entry];
    assert([e type] == YAMI4String);
    assert([[e getString] isEqualToString:sourceValue]);
    [it next];
    assert([it isEqual:[params end]] == NO);
    e = [it entry];
    assert([e type] == YAMI4StringArray);
    assert([[e getStringInArray:1] isEqualToString:array[1]]);
    [it next];
    assert([it isEqual:[params end]]);

    assert([[params dump] isEqualToString:
        @"entry 0:\n"
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
        "unused\n"]);

    serializedSize = [params serializeBufferSize];
    assert(serializedSize ==
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

    checkSerialization(params, expected, serializedSize);
}

// test for binary
void test7()
{
    YAMI4Parameters * params = [YAMI4Parameters new];

    const void * sourceValue = "abc\0d";
    const size_t sourceLength = 5;

    [params setBinary:@"some rather longer name"
        value:sourceValue valueLength:sourceLength];

    assert([params size] == 1);

    enum YAMI4ParameterType type = [params type:@"some rather longer name"];
    assert(type == YAMI4Binary);

    size_t valueLength;
    const void * value =
        [params getBinary:@"some rather longer name" valueLength:&valueLength];
    assert(value != sourceValue);
    assert(valueLength == sourceLength);
    assert(memcmp(value, sourceValue, valueLength) == 0);

    [params setBinary:@"other name"
        value:sourceValue valueLength:sourceLength];

    assert([params size] == 2);

    value = [params getBinary:@"other name" valueLength:&valueLength];
    assert(memcmp(value, sourceValue, valueLength) == 0);

    size_t serializedSize = [params serializeBufferSize];
    assert(serializedSize ==
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
    [params createBinaryArray:@"nameA"
        arrayLength:(sizeof(array) / sizeof(const void *))];

    type = [params type:@"nameA"];
    assert(type == YAMI4BinaryArray);

    value = [params getBinaryInArray:@"nameA" index:0
        valueLength:&valueLength];
    assert(value == NULL);
    assert(valueLength == 0);
    value = [params getBinaryInArray:@"nameA" index:2
        valueLength:&valueLength];
    assert(value == NULL);
    assert(valueLength == 0);

    @try
    {
        (void) [params getBinaryInArray:@"nameA" index:3
            valueLength:&valueLength];
        assert(false);
    }
    @catch (YAMI4Exception * e)
    {
        assert([[e reason] isEqualToString:@"No such index."]);
    }

    [params setBinaryInArray:@"nameA" index:0 value:array[0] valueLength:3];
    [params setBinaryInArray:@"nameA" index:1 value:array[1] valueLength:2];
    [params setBinaryInArray:@"nameA" index:2 value:array[2] valueLength:3];

    value = [params getBinaryInArray:@"nameA" index:0
        valueLength:&valueLength];
    assert(valueLength == 3);
    assert(memcmp(value, "abc", 3) == 0);
    value = [params getBinaryInArray:@"nameA" index:1
        valueLength:&valueLength];
    assert(valueLength == 2);
    assert(memcmp(value, "klm", 2) == 0);
    
    YAMI4ParametersIterator * it = [params begin];
    YAMI4ParameterEntry * e = [it entry];
    assert([e type] == YAMI4Binary);
    value = [e getBinary:&valueLength];
    assert(valueLength == sourceLength);
    assert(memcmp(value, sourceValue, valueLength) == 0);
    [it next];
    assert([it isEqual:[params end]] == NO);
    e = [it entry];
    assert([e type] == YAMI4Binary);
    value = [e getBinary:&valueLength];
    assert(memcmp(value, sourceValue, valueLength) == 0);
    [it next];
    assert([it isEqual:[params end]] == NO);
    e = [it entry];
    assert([e type] == YAMI4BinaryArray);
    value = [e getBinaryInArray:1 valueLength:&valueLength];
    assert(valueLength == 2);
    assert(memcmp(value, "klm", 2) == 0);
    [it next];
    assert([it isEqual:[params end]]);

    assert([[params dump] isEqualToString:
        @"entry 0:\n"
        "name: some rather longer name\n"
        "binary of length 5\n"
        "entry 1:\n"
        "name: other name\n"
        "binary of length 5\n"
        "entry 2:\n"
        "name: nameA\n"
        "binary array of length 3\n"
        "entry 3:\n"
        "unused\n"]);

    serializedSize = [params serializeBufferSize];
    assert(serializedSize ==
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

    checkSerialization(params, expected, serializedSize);
}

// test for nesting
void test8()
{
    YAMI4Parameters * params = [YAMI4Parameters new];

    [params setInteger:@"name" value:123];

    assert([params size] == 1);

    YAMI4Parameters * nested = [params createNestedParameters:@"nested"];

    assert([params size] == 2);

    [nested setInteger:@"internal" value:456];

    assert([nested size] == 1);

    [nested setDouble:@"internal2" value:3.125];

    assert([params size] == 2);
    assert([nested size] == 2);

    YAMI4Parameters * nested2 =
        [nested createNestedParameters:@"more nested"];

    assert([params size] == 2);
    assert([nested size] == 3);
    
    [nested2 setInteger:@"more internal" value:789];

    assert([params size] == 2);
    assert([nested size] == 3);
    assert([nested2 size] == 1);

    enum YAMI4ParameterType type = [params type:@"name"];
    assert(type == YAMI4Integer);
    type = [params type:@"nested"];
    assert(type == YAMI4NestedParameters);
    type = [nested type:@"internal"];
    assert(type == YAMI4Integer);
    type = [nested type:@"internal2"];
    assert(type == YAMI4Double);
    type = [nested type:@"more nested"];
    assert(type == YAMI4NestedParameters);
    type = [nested2 type:@"more internal"];
    assert(type == YAMI4Integer);

    @try
    {
        (void) [nested type:@"blabla"];
        assert(false);
    }
    @catch (YAMI4Exception * e)
    {
        assert([[e reason] isEqualToString:@"No such name."]);
    }

    YAMI4ParametersIterator * it = [params begin];
    YAMI4ParameterEntry * e = [it entry];
    assert([e type] == YAMI4Integer);
    [it next];
    assert([it isEqual:[params end]] == NO);
    e = [it entry];
    assert([e type] == YAMI4NestedParameters);
    {
        YAMI4Parameters * nst = [e getNestedParameters];
        assert([nst size] == 3);
    }
    [it next];
    assert([it isEqual:[params end]]);

    assert([[params dump] isEqualToString:
        @"entry 0:\n"
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
        "unused\n"]);

    size_t serializedSize = [params serializeBufferSize];
    assert(serializedSize ==
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

    checkSerialization(params, expected, serializedSize);
}

// test for array of nested parameters
void test8a()
{
    YAMI4Parameters * params = [YAMI4Parameters new];

    [params createNestedArray:@"name" arrayLength:3];

    assert([params size] == 1);

    [params setInteger:@"i" value:7];

    assert([params size] == 2);
    
    size_t arrayLength = [params getNestedArrayLength:@"name"];
    
    assert(arrayLength == 3);
    
    // first nested
    
    YAMI4Parameters * nested1 = [params getNestedInArray:@"name" index:0];
    
    [nested1 setInteger:@"x" value:10];
    
    assert([nested1 size] == 1);
    
    // second nested

    YAMI4Parameters * nested2 = [params getNestedInArray:@"name" index:1];
    
    [nested2 setInteger:@"x" value:20];
    [nested2 setInteger:@"y" value:21];
    
    assert([nested2 size] == 2);
    
    // third nested

    YAMI4Parameters * nested3 = [params getNestedInArray:@"name" index:2];
    
    [nested3 setInteger:@"x" value:30];
    [nested3 setInteger:@"y" value:31];
    [nested3 setInteger:@"z" value:32];
    
    assert([nested3 size] == 3);
    
    // no more nested
    
    @try
    {
        (void) [params getNestedInArray:@"name" index:3];
        assert(false);
    }
    @catch (YAMI4Exception * e)
    {
        assert([[e reason] isEqualToString:@"No such index."]);
    }

    enum YAMI4ParameterType type = [params type:@"name"];
    assert(type == YAMI4NestedParametersArray);

    YAMI4ParametersIterator * it = [params begin];
    YAMI4ParameterEntry * e = [it entry];
    assert([e type] == YAMI4NestedParametersArray);
    [it next];
    assert([it isEqual:[params end]] == NO);
    e = [it entry];
    assert([e type] == YAMI4Integer);
    [it next];
    assert([it isEqual:[params end]]);

    assert([[params dump] isEqualToString:
        @"entry 0:\n"
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
        "unused\n"]);

    size_t serializedSize = [params serializeBufferSize];
    assert(serializedSize ==
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

    checkSerialization(params, expected, serializedSize);
}

int main()
{
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();
    test8();
    test8a();

    [pool drain];
    return 0;
}
