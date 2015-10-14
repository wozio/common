# Copyright Maciej Sobczak 2008-2015.
# This file is part of YAMI4.
#
# YAMI4 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# YAMI4 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

import yami

def test_1():
    """Test for empty parameters."""

    params = yami.Parameters()

    assert len(params) == 0

    assert str(params) == ""

    try:
        del params["no such entry"]
        assert False
    except KeyError, e:
        assert str(e) == \
            "\"Entry or object named 'no such entry' does not exist.\""

    # check for empty iteration
    for e in params:
        assert False

    try:
        e = params["no such name"]
        assert False
    except KeyError:
        pass

    assert "no such name" not in params

    serialized = "\x00\x00\x00\x00"

    buf = params.serialize()
    assert buf == serialized

    params2 = yami.Parameters()
    params2.deserialize(buf)
    assert len(params2) == 0

    # additional test for plain dictionary
    assert yami.serialize({}) == serialized

def test_2():
    """Test for bool."""

    params = yami.Parameters()

    params["name"] = True

    assert len(params) == 1

    assert type(params["name"]) == bool

    assert params["name"] == True

    assert str(params) == (
        "entry 0:\n" +
        "name: name\n" +
        "bool: 1\n")

    params["name"] = False

    assert len(params) == 1

    it = iter(params)
    e = it.next()
    assert e == "name" and params[e] == False
    try:
        it.next()
        assert False
    except StopIteration:
        pass

    array = [True, False, True]

    params["nameA"] = array

    assert len(params) == 2

    assert type(params["nameA"]) == list
    assert type(params["nameA"][0]) == bool

    assert params["nameA"] == [True, False, True]

    params["nameA"][2] = False

    it = iter(params)
    e = it.next()
    assert e == "name" and params[e] == False
    e = it.next()
    assert e == "nameA" and params[e] == [True, False, False]
    try:
        it.next()
        assert False
    except StopIteration:
        pass

    assert str(params) == (
        "entry 0:\n" +
        "name: name\n" +
        "bool: 0\n" +
        "entry 1:\n" +
        "name: nameA\n" +
        "bool array: 1 0 0\n")

    expected = (
        "\x02\x00\x00\x00" + # num of entries
        "\x04\x00\x00\x00" + # length of "name"
        "name" +
        "\x01\x00\x00\x00" + # type code for boolean
        "\x00\x00\x00\x00" + # false
        "\x05\x00\x00\x00" + # length of "nameA"
        "nameA\x00\x00\x00" +
        "\x07\x00\x00\x00" + # type code for boolean array
        "\x03\x00\x00\x00" + # length of array
        "\x01\x00\x00\x00")  # packed array (100 -> 0x1)

    check_serialization(params, expected)

    it = iter(params)
    e = it.next()
    assert e == "name" and params[e] == False
    e = it.next()
    assert e == "nameA" and params[e] == [True, False, False]
    try:
        it.next()
        assert False
    except StopIteration:
        pass

    del params["name"]

    assert len(params) == 1

    it = iter(params)
    e = it.next()
    assert e == "nameA" and params[e] == [True, False, False]
    try:
        it.next()
        assert False
    except StopIteration:
        pass

def test_3():
    """Test for int."""

    params = yami.Parameters()

    params["name"] = 123

    assert len(params) == 1

    assert type(params["name"]) == int

    assert params["name"] == 123

    array = [10, 20, 30]

    params["nameA"] = array

    assert len(params) == 2

    assert type(params["nameA"]) == list
    assert type(params["nameA"][0]) == int

    assert params["nameA"] == [10, 20, 30]

    params["nameA"][2] = 31

    it = iter(params)
    e = it.next()
    assert e == "name" and params[e] == 123
    e = it.next()
    assert e == "nameA" and params[e] == [10, 20, 31]
    try:
        it.next()
        assert False
    except StopIteration:
        pass

    assert str(params) == (
        "entry 0:\n" +
        "name: name\n" +
        "int: 123\n" +
        "entry 1:\n" +
        "name: nameA\n" +
        "int array: 10 20 31\n")

    expected = (
        "\x02\x00\x00\x00" + # num of entries
        "\x04\x00\x00\x00" + # length of "name"
        "name" +
        "\x02\x00\x00\x00" + # type code for integer
        "\x7b\x00\x00\x00" + # 123
        "\x05\x00\x00\x00" + # length of "nameA"
        "nameA\x00\x00\x00" +
        "\x08\x00\x00\x00" + # type code for integer array
        "\x03\x00\x00\x00" + # length of array
        "\x0a\x00\x00\x00" + # array values (10)
        "\x14\x00\x00\x00" + # array values (20)
        "\x1f\x00\x00\x00")  # array values (31)

    check_serialization(params, expected)

    it = iter(params)
    e = it.next()
    assert e == "name" and params[e] == 123
    e = it.next()
    assert e == "nameA" and params[e] == [10, 20, 31]
    try:
        it.next()
        assert False
    except StopIteration:
        pass

    del params["name"]

    assert len(params) == 1

    it = iter(params)
    e = it.next()
    assert e == "nameA" and params[e] == [10, 20, 31]
    try:
        it.next()
        assert False
    except StopIteration:
        pass

def test_4():
    """Test for long."""

    params = yami.Parameters()

    params["name"] = 1234567890L

    assert len(params) == 1

    assert type(params["name"]) == long

    assert params["name"] == 1234567890L

    array = [100L, 200L, 300L]

    params["nameA"] = array

    assert len(params) == 2

    assert type(params["nameA"]) == list
    assert type(params["nameA"][0]) == long

    assert params["nameA"] == [100L, 200L, 300L]

    params["nameA"][2] = 310L

    it = iter(params)
    e = it.next()
    assert e == "name" and params[e] == 1234567890L
    e = it.next()
    assert e == "nameA" and params[e] == [100, 200, 310]
    try:
        it.next()
        assert False
    except StopIteration:
        pass

    assert str(params) == (
        "entry 0:\n" +
        "name: name\n" +
        "long: 1234567890\n" +
        "entry 1:\n" +
        "name: nameA\n" +
        "long array: 100 200 310\n")

    expected = (
        "\x02\x00\x00\x00" + # num of entries
        "\x04\x00\x00\x00" + # length of "name"
        "name" +
        "\x03\x00\x00\x00" + # type code for long
        "\xd2\x02\x96\x49" + # 1234567890
        "\x00\x00\x00\x00" +
        "\x05\x00\x00\x00" + # length of "nameA"
        "nameA\x00\x00\x00" +
        "\x09\x00\x00\x00" + # type code for long array
        "\x03\x00\x00\x00" + # length of array
        "\x64\x00\x00\x00" + # array values (100)
        "\x00\x00\x00\x00" +
        "\xc8\x00\x00\x00" + # array values (200)
        "\x00\x00\x00\x00" +
        "\x36\x01\x00\x00" + # array values (310)
        "\x00\x00\x00\x00")

    check_serialization(params, expected)

def test_5():
    """Test for float."""

    params = yami.Parameters()

    params["name"] = 3.125

    assert len(params) == 1

    assert type(params["name"]) == float

    assert params["name"] == 3.125

    array = [1.875, 2.875, 3.875]

    params["nameA"] = array

    assert len(params) == 2

    assert type(params["nameA"]) == list
    assert type(params["nameA"][0]) == float

    assert params["nameA"] == [1.875, 2.875, 3.875]

    params["nameA"][2] = 3.625

    it = iter(params)
    e = it.next()
    assert e == "name" and params[e] == 3.125
    e = it.next()
    assert e == "nameA" and params[e] == [1.875, 2.875, 3.625]
    try:
        it.next()
        assert False
    except StopIteration:
        pass

    assert str(params) == (
        "entry 0:\n" +
        "name: name\n" +
        "float: 3.125\n" +
        "entry 1:\n" +
        "name: nameA\n" +
        "float array: 1.875 2.875 3.625\n")

    expected = (
        "\x02\x00\x00\x00" + # num of entries
        "\x04\x00\x00\x00" + # length of "name"
        "name" +
        "\x04\x00\x00\x00" + # type code for float
        "\x00\x00\x00\x00" +
        "\x00\x00\x09\x40" + # 3.125
        "\x05\x00\x00\x00" + # length of "nameA"
        "nameA\x00\x00\x00" +
        "\x0a\x00\x00\x00" + # type code for long array
        "\x03\x00\x00\x00" + # length of array
        "\x00\x00\x00\x00" + # array values (1.875)
        "\x00\x00\xfe\x3f" +
        "\x00\x00\x00\x00" + # array values (2.875)
        "\x00\x00\x07\x40" +
        "\x00\x00\x00\x00" + # array values (3.625)
        "\x00\x00\x0d\x40")

    check_serialization(params, expected)

def test_6():
    """Test for string."""

    source_value = ("Kolorowe kredki w pudeleczku nosze,\n" +
                    "kolorowe kredki, bardzo lubie je!")

    name = "some rather longer name"

    params = yami.Parameters()

    params[name] = source_value

    assert len(params) == 1

    assert type(params[name]) == str

    assert params[name] == source_value

    params["other name"] = source_value

    assert len(params) == 2

    assert params["other name"] == source_value

    array = ["Kazio", "Krzysio", "Rysio", "Zbysio"]

    params["nameA"] = array

    assert len(params) == 3

    assert type(params["nameA"]) == list
    assert type(params["nameA"][0]) == str

    assert params["nameA"] == ["Kazio", "Krzysio", "Rysio", "Zbysio"]

    it = iter(params)
    e = it.next()
    assert e == name and params[e] == source_value
    e = it.next()
    assert e == "other name" and params[e] == source_value
    e = it.next()
    assert e == "nameA" and \
        params[e] == ["Kazio", "Krzysio", "Rysio", "Zbysio"]
    try:
        it.next()
        assert False
    except StopIteration:
        pass

    assert str(params) == (
        "entry 0:\n" +
        "name: some rather longer name\n" +
        "string: Kolorowe kredki w pudeleczku nosze,\n" +
        "kolorowe kredki, bardzo lubie je!\n" +
        "entry 1:\n" +
        "name: other name\n" +
        "string: Kolorowe kredki w pudeleczku nosze,\n" +
        "kolorowe kredki, bardzo lubie je!\n" +
        "entry 2:\n" +
        "name: nameA\n" +
        "string array: Kazio Krzysio Rysio Zbysio\n")

    expected = (
        "\x03\x00\x00\x00" + # num of entries
        "\x17\x00\x00\x00" + # length of name
        "some rather longer name\x00" +
        "\x05\x00\x00\x00" + # type code for string
        "\x45\x00\x00\x00" + # length
        "Kolorowe kredki w pudeleczku nosze,\n" +
        "kolorowe kredki, bardzo lubie je!\x00\x00\x00" +
        "\x0a\x00\x00\x00" + # length of "other name"
        "other name\x00\x00" +
        "\x05\x00\x00\x00" + # type code for string
        "\x45\x00\x00\x00" + # length
        "Kolorowe kredki w pudeleczku nosze,\n" +
        "kolorowe kredki, bardzo lubie je!\x00\x00\x00" +
        "\x05\x00\x00\x00" + # length of "nameA"
        "nameA\x00\x00\x00" +
        "\x0b\x00\x00\x00" + # type code for string array
        "\x04\x00\x00\x00" + # length of array
        "\x05\x00\x00\x00" + # length of first entry
        "Kazio\x00\x00\x00" +
        "\x07\x00\x00\x00" + # length of first entry
        "Krzysio\x00" +
        "\x05\x00\x00\x00" + # length of first entry
        "Rysio\x00\x00\x00" +
        "\x06\x00\x00\x00" + # length of first entry
        "Zbysio\x00\x00")

    check_serialization(params, expected)

def test_7():
    """Test for binary."""

    source_value = "abc\x00d"

    name = "some rather longer name"

    params = yami.Parameters()

    params[name] = yami.Bytes(source_value)

    assert len(params) == 1

    assert type(params[name]) == yami.Bytes

    assert params[name].value == "abc\x00d"

    params["other name"] = yami.Bytes(source_value)

    assert len(params) == 2

    assert params["other name"].value == "abc\x00d"

    array = [yami.Bytes("abc"), yami.Bytes("kl"), yami.Bytes("xyz")]

    params["nameA"] = array

    assert len(params) == 3

    assert type(params["nameA"]) == list
    assert type(params["nameA"][0]) == yami.Bytes

    assert params["nameA"][0].value == "abc"
    assert params["nameA"][1].value == "kl"
    assert params["nameA"][2].value == "xyz"

    it = iter(params)
    e = it.next()
    assert e == name and params[e].value == source_value
    e = it.next()
    assert e == "other name" and params[e].value == source_value
    e = it.next()
    assert e == "nameA"
    assert params[e][0].value == "abc"
    assert params[e][1].value == "kl"
    assert params[e][2].value == "xyz"
    try:
        it.next()
        assert False
    except StopIteration:
        pass

    assert str(params) == (
        "entry 0:\n" +
        "name: some rather longer name\n" +
        "binary of length 5\n" +
        "entry 1:\n" +
        "name: other name\n" +
        "binary of length 5\n" +
        "entry 2:\n" +
        "name: nameA\n" +
        "binary array of length 3\n")

    expected = (
        "\x03\x00\x00\x00" + # num of entries
        "\x17\x00\x00\x00" + # length of name
        "some rather longer name\x00" +
        "\x06\x00\x00\x00" + # type code for binary
        "\x05\x00\x00\x00" + # length
        "abc\x00d\x00\x00\x00" +
        "\x0a\x00\x00\x00" + # length of "other name"
        "other name\x00\x00" +
        "\x06\x00\x00\x00" + # type code for string
        "\x05\x00\x00\x00" + # length
        "abc\x00d\x00\x00\x00" +
        "\x05\x00\x00\x00" + # length of "nameA"
        "nameA\x00\x00\x00" +
        "\x0c\x00\x00\x00" + # type code for string array
        "\x03\x00\x00\x00" + # length of array
        "\x03\x00\x00\x00" + # length of first entry
        "abc\x00" +
        "\x02\x00\x00\x00" + # length of first entry
        "kl\x00\x00" +
        "\x03\x00\x00\x00" + # length of first entry
        "xyz\x00")

    check_serialization(params, expected)

def test_8():
    """Test for nested."""

    params = yami.Parameters()

    params["name"] = 123

    assert len(params) == 1

    nested = yami.Parameters()

    params["nested"] = nested

    assert len(params) == 2

    nested["internal"] = 456
    nested["internal2"] = 3.125

    assert len(params) == 2
    assert len(nested) == 2

    assert params["nested"] == nested

    nested2 = yami.Parameters()

    nested["more nested"] = nested2

    assert len(params) == 2
    assert len(nested) == 3

    nested2["more internal"] = 789

    assert len(params) == 2
    assert len(nested) == 3
    assert len(nested2) == 1

    assert type(params["name"]) == int
    assert type(params["nested"]) == yami.Parameters
    assert type(nested["internal"]) == int
    assert type(nested["internal2"]) == float
    assert type(nested["more nested"]) == yami.Parameters
    assert type(nested2["more internal"]) == int
    
    it = iter(params)
    e = it.next()
    assert e == "name" and params[e] == 123
    e = it.next()
    assert e == "nested"
    assert type(params[e]) == yami.Parameters
    assert len(params[e]) == 3
    try:
        it.next()
        assert False
    except StopIteration:
        pass

    assert str(params) == (
        "entry 0:\n" +
        "name: name\n" +
        "int: 123\n" +
        "entry 1:\n" +
        "name: nested\n" +
        "nested parameters:\n" +
        "  entry 0:\n" +
        "  name: internal\n" +
        "  int: 456\n" +
        "  entry 1:\n" +
        "  name: internal2\n" +
        "  float: 3.125\n" +
        "  entry 2:\n" +
        "  name: more nested\n" +
        "  nested parameters:\n" +
        "    entry 0:\n" +
        "    name: more internal\n" +
        "    int: 789\n")

    expected = (
        "\x02\x00\x00\x00" + # num of entries
        "\x04\x00\x00\x00" + # length of "name"
        "name" +
        "\x02\x00\x00\x00" + # type code for integer
        "\x7b\x00\x00\x00" + # value
        "\x06\x00\x00\x00" + # length of "nested"
        "nested\x00\x00" +
        "\x0d\x00\x00\x00" + # type code for nested
        "\x03\x00\x00\x00" + # num of entries in nested
        "\x08\x00\x00\x00" + # length of "internal"
        "internal" +
        "\x02\x00\x00\x00" + # type code for integer
        "\xc8\x01\x00\x00" + # value
        "\x09\x00\x00\x00" + # length o "internal2"
        "internal2\x00\x00\x00" +
        "\x04\x00\x00\x00" + # type code for double
        "\x00\x00\x00\x00" +
        "\x00\x00\x09\x40" + # 3.125
        "\x0b\x00\x00\x00" + # length of "more nested"
        "more nested\x00" +
        "\x0d\x00\x00\x00" + # type code for nested
        "\x01\x00\x00\x00" + # num of entries in more nested
        "\x0d\x00\x00\x00" + # length of "more internal"
        "more internal\x00\x00\x00" +
        "\x02\x00\x00\x00" + # type code for integer
        "\x15\x03\x00\x00")  # value

    check_serialization(params, expected)

def test_8a():
    """Test for array of nested parameters."""

    params = yami.Parameters()

    nested = [yami.Parameters(), yami.Parameters(), yami.Parameters()]

    params["name"] = nested

    assert len(params) == 1

    params["i"] = 7
    
    assert len(params) == 2

    # first nested
    
    n1 = params["name"][0]
    n1["x"] = 10
    assert len(n1) == 1
    
    # second nested
    
    n2 = params["name"][1]
    n2["x"] = 20
    n2["y"] = 21
    assert len(n2) == 2
    
    # third nested
    
    n3 = params["name"][2]
    n3["x"] = 30
    n3["y"] = 31
    n3["z"] = 32
    assert len(n3) == 3
    
    assert type(params["name"]) == list
    assert type(params["name"][0]) == yami.Parameters
    assert params["name"][0]["x"] == 10
    assert params["name"][1]["x"] == 20
    assert params["name"][2]["x"] == 30

    assert type(params["i"]) == int
    
    it = iter(params)
    e = it.next()
    assert type(params[e]) == list
    assert type(params[e][0]) == yami.Parameters
    e = it.next()
    assert e == "i"
    assert type(params[e]) == int
    assert params[e] == 7
    try:
        it.next()
        assert False
    except StopIteration:
        pass

    assert str(params) == (
        "entry 0:\n" +
        "name: name\n" +
        "nested parameters array of length 3:\n" +
        "  nested at index 0:\n" +
        "    entry 0:\n" +
        "    name: x\n" +
        "    int: 10\n" +
        "  nested at index 1:\n" +
        "    entry 0:\n" +
        "    name: x\n" +
        "    int: 20\n" +
        "    entry 1:\n" +
        "    name: y\n" +
        "    int: 21\n" +
        "  nested at index 2:\n" +
        "    entry 0:\n" +
        "    name: x\n" +
        "    int: 30\n" +
        "    entry 1:\n" +
        "    name: y\n" +
        "    int: 31\n" +
        "    entry 2:\n" +
        "    name: z\n" +
        "    int: 32\n" +
        "entry 1:\n" +
        "name: i\n" +
        "int: 7\n")

    expected = (
        "\x02\x00\x00\x00" + # num of entries
        "\x04\x00\x00\x00" + # length of "name"
        "name" +
        "\x0e\x00\x00\x00" + # type code for nested params array
        "\x03\x00\x00\x00" + # length of array

        # first nested

        "\x01\x00\x00\x00" + # num of entries
        "\x01\x00\x00\x00" + # length of "x"
        "x\x00\x00\x00" +
        "\x02\x00\x00\x00" + # type code for integer
        "\x0a\x00\x00\x00" + # value

        # second nested

        "\x02\x00\x00\x00" + # num of entries
        "\x01\x00\x00\x00" + # length of "x"
        "x\x00\x00\x00" +
        "\x02\x00\x00\x00" + # type code for integer
        "\x14\x00\x00\x00" + # value
        "\x01\x00\x00\x00" + # length of "y"
        "y\x00\x00\x00" +
        "\x02\x00\x00\x00" + # type code for integer
        "\x15\x00\x00\x00" + # value

        # third nested

        "\x03\x00\x00\x00" + # num of entries
        "\x01\x00\x00\x00" + # length of "x"
        "x\x00\x00\x00" +
        "\x02\x00\x00\x00" + # type code for integer
        "\x1e\x00\x00\x00" + # value
        "\x01\x00\x00\x00" + # length of "y"
        "y\x00\x00\x00" +
        "\x02\x00\x00\x00" + # type code for integer
        "\x1f\x00\x00\x00" + # value
        "\x01\x00\x00\x00" + # length of "z"
        "z\x00\x00\x00" +
        "\x02\x00\x00\x00" + # type code for integer
        "\x20\x00\x00\x00" + # value

        "\x01\x00\x00\x00" + # length of "i"
        "i\x00\x00\x00" +
        "\x02\x00\x00\x00" + # type code for integer
        "\x07\x00\x00\x00")  # value

    check_serialization(params, expected)

def check_serialization(params, expected):
    """Checks whether the serialization gives expected results."""

    buffer = params.serialize()

    assert buffer == expected

    # verify deserialization gives the same results
    params2 = yami.Parameters()
    params2.deserialize(buffer)
    buffer2 = params2.serialize()

    if buffer2 != buffer:
        print("buffers:", buffer2, buffer)
    assert buffer2 == buffer

test_1()
test_2()
test_3()
test_4()
test_5()
test_6()
test_7()
test_8()
test_8a()

