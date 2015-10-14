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

package com.inspirel.yami;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

public class ParametersTest {

    private Parameters params;

    @Before
    public void setUp() {
        params = new Parameters();
    }

    /**
     * test for empty parameters
     */
    @Test
    public void testForEmpty() {
        assertTrue(params.size() == 0);

        assertTrue(params.toString().equals(""));

        try {
            params.remove("no such entry");
            fail("should never reach this point");
        } catch (NoSuchNameException e) {
            assertTrue(e.getMessage().equals(
                    "Entry or object named 'no such entry' " +
                    "does not exist."));
        }

        // check for empty iteration
        Iterator<Parameters.Entry> it = params.iterator();
        assertFalse(it.hasNext());

        Parameters.Entry e = params.find("no such name");
        assertNull(e);

        List<byte[]> buffers = params.serialize(Integer.MAX_VALUE);
        assertTrue(buffers.size() == 1);
        assertTrue(Arrays.equals(buffers.get(0), new byte[4]));

        Parameters params2 = new Parameters();
        params2.deserialize(buffers);
        assertTrue(params.size() == 0);
    }

    /**
     * test for boolean
     */
    @Test
    public void testForBoolean() {
        params.setBoolean("name", true);
        assertTrue(params.size() == 1);

        Parameters.EntryType t = params.getType("name");
        assertEquals(t, Parameters.EntryType.BOOLEAN);

        assertTrue(params.getBoolean("name"));

        assertTrue(params.toString().equals(
                "entry 0:\n" +
                "name: name\n" +
                "boolean: 1\n"));

        params.setBoolean("name", false);
        assertTrue(params.size() == 1);

        try {
            params.getInteger("name");
            fail("should never reach this point");
        } catch (BadTypeException e) {
            assertTrue(e.getMessage().equals(
                    "The entry 'name' does not have" +
                    " the expected type INTEGER"));
        }

        Iterator<Parameters.Entry> it = params.iterator();
        assertTrue(it.hasNext());
        Parameters.Entry e = it.next();
        assertEquals(e.type(), Parameters.EntryType.BOOLEAN);
        assertTrue(e.name().equals("name"));
        assertFalse(e.getBoolean());

        try {
            e.getInteger();
            fail("should never reach this point");
        } catch (BadTypeException ex) {
            // ignore
        }

        assertFalse(it.hasNext());

        boolean[] array = {true, false, true};
        params.setBooleanArray("nameA", array);

        assertTrue(params.size() == 2);

        assertEquals(params.getType("nameA"),
                Parameters.EntryType.BOOLEAN_ARRAY);

        assertEquals(params.getBooleanArray("nameA"), array);
        assertTrue(array[0] && array[1] == false && array[2]);

        array[2] = false;

        it = params.iterator();
        assertTrue(it.hasNext());
        e = it.next();
        assertEquals(e.type(), Parameters.EntryType.BOOLEAN);
        assertTrue(e.name().equals("name"));
        assertTrue(it.hasNext());
        e = it.next();
        assertEquals(e.type(), Parameters.EntryType.BOOLEAN_ARRAY);
        assertTrue(e.name().equals("nameA"));
        assertEquals(e.getBooleanArray(), array);
        assertTrue(array[0] && array[1] == false && array[2] == false);
        assertFalse(it.hasNext());

        assertTrue(params.toString().equals(
                "entry 0:\n" +
                "name: name\n" +
                "boolean: 0\n" +
                "entry 1:\n" +
                "name: nameA\n" +
                "boolean array: 1 0 0\n"));

        byte[] expected = {
            2, 0, 0, 0, // num of entries
            4, 0, 0, 0, // length of "name"
            'n', 'a', 'm', 'e',
            1, 0, 0, 0, // type code for boolean
            0, 0, 0, 0, // false
            5, 0, 0, 0, // length of "nameA"
            'n', 'a', 'm', 'e',
            'A', 0, 0, 0,
            7, 0, 0, 0, // type code for boolean array
            3, 0, 0, 0, // length of array
            0x1, 0, 0, 0        // packed array (100 -> 0x1)
        };

        checkSerialization(params, expected);

        e = params.find("name");
        assertEquals(e.type(), Parameters.EntryType.BOOLEAN);
        assertTrue(e.name().equals("name"));

        e = params.find("nameA");
        assertEquals(e.type(), Parameters.EntryType.BOOLEAN_ARRAY);
        assertTrue(e.name().equals("nameA"));

        e = params.find("no such name");
        assertNull(e);

        params.remove("name");

        assertTrue(params.size() == 1);

        it = params.iterator();
        assertTrue(it.hasNext());
        e = it.next();
        assertEquals(e.type(), Parameters.EntryType.BOOLEAN_ARRAY);
        assertTrue(e.name().equals("nameA"));
        assertFalse(it.hasNext());
    }

    /**
     * test for integer
     */
    @Test
    public void testForInteger() {
        params.setInteger("name", 123);
        assertTrue(params.size() == 1);

        Parameters.EntryType t = params.getType("name");
        assertEquals(t, Parameters.EntryType.INTEGER);

        assertTrue(params.getInteger("name") == 123);

        int[] array = {10, 20, 30};
        params.setIntegerArray("nameA", array);

        assertTrue(params.size() == 2);

        assertEquals(params.getType("nameA"),
                Parameters.EntryType.INTEGER_ARRAY);

        assertEquals(params.getIntegerArray("nameA"), array);
        assertTrue(array[0] == 10 && array[1] == 20 && array[2] == 30);

        array[2] = 31;

        Iterator<Parameters.Entry> it = params.iterator();
        assertTrue(it.hasNext());
        Parameters.Entry e = it.next();
        assertEquals(e.type(), Parameters.EntryType.INTEGER);
        assertTrue(e.name().equals("name"));
        assertTrue(e.getInteger() == 123);
        assertTrue(it.hasNext());
        e = it.next();
        assertEquals(e.type(), Parameters.EntryType.INTEGER_ARRAY);
        assertTrue(e.name().equals("nameA"));
        assertEquals(e.getIntegerArray(), array);
        assertTrue(array[0] == 10 && array[1] == 20 && array[2] == 31);
        assertFalse(it.hasNext());

        assertTrue(params.toString().equals(
                "entry 0:\n" +
                "name: name\n" +
                "integer: 123\n" +
                "entry 1:\n" +
                "name: nameA\n" +
                "integer array: 10 20 31\n"));

        byte[] expected = {
            2, 0, 0, 0, // num of entries
            4, 0, 0, 0, // length of "name"
            'n', 'a', 'm', 'e',
            2, 0, 0, 0, // type code for integer
            123, 0, 0, 0, // 123
            5, 0, 0, 0, // length of "nameA"
            'n', 'a', 'm', 'e',
            'A', 0, 0, 0,
            8, 0, 0, 0, // type code for integer array
            3, 0, 0, 0, // length of array
            10, 0, 0, 0, // array values
            20, 0, 0, 0, // array values
            31, 0, 0, 0         // array values
        };

        checkSerialization(params, expected);

        it = params.iterator();
        assertTrue(it.hasNext());
        e = it.next();
        assertTrue(e.name().equals("name"));
        params.remove("name");

        assertTrue(params.size() == 1);

        it = params.iterator();
        assertTrue(it.hasNext());
        e = it.next();
        assertEquals(e.type(), Parameters.EntryType.INTEGER_ARRAY);
        assertTrue(e.name().equals("nameA"));
        assertFalse(it.hasNext());
    }

    /**
     * test for long
     */
    @Test
    public void testForLong() {
        params.setLong("name", 1234567890L);
        assertTrue(params.size() == 1);

        Parameters.EntryType t = params.getType("name");
        assertEquals(t, Parameters.EntryType.LONG);

        assertTrue(params.getLong("name") == 1234567890L);

        long[] array = {100L, 200L, 300L};
        params.setLongArray("nameA", array);

        assertTrue(params.size() == 2);

        assertEquals(params.getType("nameA"),
                Parameters.EntryType.LONG_ARRAY);

        assertEquals(params.getLongArray("nameA"), array);
        assertTrue(array.length == 3);
        assertTrue(array[0] == 100L && array[1] == 200L && array[2] == 300L);

        array[2] = 310L;

        Iterator<Parameters.Entry> it = params.iterator();
        assertTrue(it.hasNext());
        Parameters.Entry e = it.next();
        assertEquals(e.type(), Parameters.EntryType.LONG);
        assertTrue(e.name().equals("name"));
        assertTrue(e.getLong() == 1234567890L);
        assertTrue(it.hasNext());
        e = it.next();
        assertEquals(e.type(), Parameters.EntryType.LONG_ARRAY);
        assertTrue(e.name().equals("nameA"));
        assertEquals(e.getLongArray(), array);
        assertTrue(array.length == 3);
        assertTrue(array[0] == 100L && array[1] == 200L && array[2] == 310L);
        assertFalse(it.hasNext());

        assertTrue(params.toString().equals(
                "entry 0:\n" +
                "name: name\n" +
                "long: 1234567890\n" +
                "entry 1:\n" +
                "name: nameA\n" +
                "long array: 100 200 310\n"));

        byte[] expected = {
            2, 0, 0, 0, // num of entries
            4, 0, 0, 0, // length of "name"
            'n', 'a', 'm', 'e',
            3, 0, 0, 0, // type code for long long
            (byte) 210, 2, (byte) 150, 73, // 1234567890
            0, 0, 0, 0,
            5, 0, 0, 0, // length of "nameA"
            'n', 'a', 'm', 'e',
            'A', 0, 0, 0,
            9, 0, 0, 0, // type code for long long array
            3, 0, 0, 0, // length of array
            100, 0, 0, 0, // array values
            0, 0, 0, 0,
            (byte) 200, 0, 0, 0, // array values
            0, 0, 0, 0,
            54, 1, 0, 0, // array values
            0, 0, 0, 0
        };

        checkSerialization(params, expected);
        
        // additional test for bit extension bug
        
        params = new Parameters();
        params.setLong("a", (long) 1 << 31);
        expected = new byte[] {
            1, 0, 0, 0, // num of entries
            1, 0, 0, 0, // length of "a"
            'a', 0, 0, 0,
            3, 0, 0, 0, // type code for long long
            0, 0, 0, (byte) 128,
            0, 0, 0, 0
        };
        
        checkSerialization(params, expected);
    }

    /**
     * test for double
     */
    @Test
    public void testForDouble() {
        params.setDouble("name", 3.125);
        assertTrue(params.size() == 1);

        Parameters.EntryType t = params.getType("name");
        assertEquals(t, Parameters.EntryType.DOUBLE);

        assertEquals(params.getDouble("name"), 3.125, 0.0);

        double[] array = {1.875, 2.875, 3.875};
        params.setDoubleArray("nameA", array);

        assertTrue(params.size() == 2);

        assertEquals(params.getType("nameA"),
                Parameters.EntryType.DOUBLE_ARRAY);

        assertEquals(params.getDoubleArray("nameA"), array);
        assertTrue(array.length == 3);
        assertTrue(array[0] == 1.875 &&
                array[1] == 2.875 &&
                array[2] == 3.875);

        array[2] = 3.625;

        Iterator<Parameters.Entry> it = params.iterator();
        assertTrue(it.hasNext());
        Parameters.Entry e = it.next();
        assertEquals(e.type(), Parameters.EntryType.DOUBLE);
        assertTrue(e.name().equals("name"));
        assertEquals(e.getDouble(), 3.125, 0.0);
        assertTrue(it.hasNext());
        e = it.next();
        assertEquals(e.type(), Parameters.EntryType.DOUBLE_ARRAY);
        assertTrue(e.name().equals("nameA"));
        assertEquals(e.getDoubleArray(), array);
        assertTrue(array.length == 3);
        assertTrue(array[0] == 1.875 &&
                array[1] == 2.875 &&
                array[2] == 3.625);
        assertFalse(it.hasNext());

        assertTrue(params.toString().equals(
                "entry 0:\n" +
                "name: name\n" +
                "double: 3.125\n" +
                "entry 1:\n" +
                "name: nameA\n" +
                "double array: 1.875 2.875 3.625\n"));

        byte[] expected = {
            2, 0, 0, 0, // num of entries
            4, 0, 0, 0, // length of "name"
            'n', 'a', 'm', 'e',
            4, 0, 0, 0, // type code for double
            0, 0, 0, 0,
            0, 0, 9, 64, // 3.125
            5, 0, 0, 0, // length of "nameA"
            'n', 'a', 'm', 'e',
            'A', 0, 0, 0,
            10, 0, 0, 0, // type code for double array
            3, 0, 0, 0, // length of array
            0, 0, 0, 0, // array values
            0, 0, -2, 63,
            0, 0, 0, 0, // array values
            0, 0, 7, 64,
            0, 0, 0, 0, // array values
            0, 0, 13, 64
        };

        checkSerialization(params, expected);
    }

    /**
     * test for string
     */
    @Test
    public void testForString() {
        String sourceValue =
                "Kolorowe kredki w pudeleczku nosze,\n" +
                "kolorowe kredki, bardzo lubie je!";

        String name = "some rather longer name";

        params.setString(name, sourceValue);
        assertTrue(params.size() == 1);

        Parameters.EntryType t = params.getType(name);
        assertEquals(t, Parameters.EntryType.STRING);

        assertTrue(params.getString(name).equals(sourceValue));

        params.setString("other name", sourceValue);
        assertTrue(params.size() == 2);

        assertTrue(params.getString("other name").equals(sourceValue));

        String[] array = {"Kazio", "Krzysio", "Rysio", "Zbysio"};
        params.setStringArray("nameA", array);

        assertTrue(params.size() == 3);

        assertEquals(params.getType("nameA"),
                Parameters.EntryType.STRING_ARRAY);

        assertArrayEquals(params.getStringArray("nameA"), array);
        assertTrue(array.length == 4);
        assertTrue(array[0].equals("Kazio") &&
                array[1].equals("Krzysio") &&
                array[2].equals("Rysio") &&
                array[3].equals("Zbysio"));

        Iterator<Parameters.Entry> it = params.iterator();
        assertTrue(it.hasNext());
        Parameters.Entry e = it.next();
        assertEquals(e.type(), Parameters.EntryType.STRING);
        assertTrue(e.name().equals(name));
        assertTrue(e.getString().equals(sourceValue));
        assertTrue(it.hasNext());
        e = it.next();
        assertEquals(e.type(), Parameters.EntryType.STRING);
        assertTrue(e.name().equals("other name"));
        assertTrue(e.getString().equals(sourceValue));
        assertTrue(it.hasNext());
        e = it.next();
        assertEquals(e.type(), Parameters.EntryType.STRING_ARRAY);
        assertTrue(e.name().equals("nameA"));
        assertArrayEquals(e.getStringArray(), array);
        assertTrue(array.length == 4);
        assertTrue(array[0].equals("Kazio") &&
                array[1].equals("Krzysio") &&
                array[2].equals("Rysio") &&
                array[3].equals("Zbysio"));
        assertFalse(it.hasNext());

        assertTrue(params.toString().equals(
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
                "string array: Kazio Krzysio Rysio Zbysio\n"));

        byte[] expected = {
            3, 0, 0, 0, // num of entries
            23, 0, 0, 0, // length of "some rather longer name"
            's', 'o', 'm', 'e',
            ' ', 'r', 'a', 't',
            'h', 'e', 'r', ' ',
            'l', 'o', 'n', 'g',
            'e', 'r', ' ', 'n',
            'a', 'm', 'e', 0,
            5, 0, 0, 0, // type code for string
            69, 0, 0, 0, // length
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
            10, 0, 0, 0, // length of "other name"
            'o', 't', 'h', 'e',
            'r', ' ', 'n', 'a',
            'm', 'e', 0, 0,
            5, 0, 0, 0, // type code for string
            69, 0, 0, 0, // length
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
            5, 0, 0, 0, // length of "nameA"
            'n', 'a', 'm', 'e',
            'A', 0, 0, 0,
            11, 0, 0, 0, // type code for string array
            4, 0, 0, 0, // length of array
            5, 0, 0, 0, // length of first entry
            'K', 'a', 'z', 'i',
            'o', 0, 0, 0,
            7, 0, 0, 0, // length of second entry
            'K', 'r', 'z', 'y',
            's', 'i', 'o', 0,
            5, 0, 0, 0, // length of third entry
            'R', 'y', 's', 'i',
            'o', 0, 0, 0,
            6, 0, 0, 0, // length of fourth entry
            'Z', 'b', 'y', 's',
            'i', 'o', 0, 0
        };

        checkSerialization(params, expected);
        
        // additional test for serialization of multibyte characters
        
        String nonASCIIString = "Unicode \u00a3 pound sign";
        Parameters p2 = new Parameters ();
        p2.setString("a", nonASCIIString);
        List<byte[]> buffers = p2.serialize(Integer.MAX_VALUE);
        Parameters p3 = new Parameters ();
        p3.deserialize(buffers);
        assertTrue(p2.getString("a").equals(p3.getString("a")));
    }

    /**
     * test for binary
     */
    @Test
    public void testForBinary() {
        byte[] sourceValue = {'a', 'b', 'c', '\0', 'd'};

        String name = "some rather longer name";

        params.setBinary(name, sourceValue);
        assertTrue(params.size() == 1);

        Parameters.EntryType t = params.getType(name);
        assertEquals(t, Parameters.EntryType.BINARY);

        assertTrue(Arrays.equals(params.getBinary(name), sourceValue));

        params.setBinary("other name", sourceValue);
        assertTrue(params.size() == 2);

        assertTrue(Arrays.equals(params.getBinary("other name"),
                sourceValue));

        byte[] a0 = {'a', 'b', 'c'};
        byte[] a1 = {'k', 'l'};
        byte[] a2 = {'x', 'y', 'z'};
        byte[][] array = {a0, a1, a2};
        params.setBinaryArray("nameA", array);

        assertTrue(params.size() == 3);

        assertEquals(params.getType("nameA"),
                Parameters.EntryType.BINARY_ARRAY);

        assertArrayEquals(params.getBinaryArray("nameA"), array);
        assertTrue(array.length == 3);
        assertTrue(Arrays.equals(array[0], a0) &&
                Arrays.equals(array[1], a1) &&
                Arrays.equals(array[2], a2));

        Iterator<Parameters.Entry> it = params.iterator();
        assertTrue(it.hasNext());
        Parameters.Entry e = it.next();
        assertEquals(e.type(), Parameters.EntryType.BINARY);
        assertTrue(e.name().equals(name));
        assertTrue(Arrays.equals(e.getBinary(), sourceValue));
        assertTrue(it.hasNext());
        e = it.next();
        assertEquals(e.type(), Parameters.EntryType.BINARY);
        assertTrue(e.name().equals("other name"));
        assertTrue(Arrays.equals(e.getBinary(), sourceValue));
        assertTrue(it.hasNext());
        e = it.next();
        assertEquals(e.type(), Parameters.EntryType.BINARY_ARRAY);
        assertTrue(e.name().equals("nameA"));
        assertArrayEquals(e.getBinaryArray(), array);
        assertTrue(array.length == 3);
        assertTrue(Arrays.equals(array[0], a0) &&
                Arrays.equals(array[1], a1) &&
                Arrays.equals(array[2], a2));
        assertFalse(it.hasNext());

        assertTrue(params.toString().equals(
                "entry 0:\n" +
                "name: some rather longer name\n" +
                "binary of length 5\n" +
                "entry 1:\n" +
                "name: other name\n" +
                "binary of length 5\n" +
                "entry 2:\n" +
                "name: nameA\n" +
                "binary array of length 3\n"));

        byte[] expected = {
            3, 0, 0, 0, // num of entries
            23, 0, 0, 0, // length of "some rather longer name"
            's', 'o', 'm', 'e',
            ' ', 'r', 'a', 't',
            'h', 'e', 'r', ' ',
            'l', 'o', 'n', 'g',
            'e', 'r', ' ', 'n',
            'a', 'm', 'e', 0,
            6, 0, 0, 0, // type code for binary
            5, 0, 0, 0, // length
            'a', 'b', 'c', '\0',
            'd', 0, 0, 0,
            10, 0, 0, 0, // length of "other name"
            'o', 't', 'h', 'e',
            'r', ' ', 'n', 'a',
            'm', 'e', 0, 0,
            6, 0, 0, 0, // type code for binary
            5, 0, 0, 0, // length
            'a', 'b', 'c', '\0',
            'd', 0, 0, 0,
            5, 0, 0, 0, // length of "nameA"
            'n', 'a', 'm', 'e',
            'A', 0, 0, 0,
            12, 0, 0, 0, // type code for binary array
            3, 0, 0, 0, // length of array
            3, 0, 0, 0, // length of first entry
            'a', 'b', 'c', 0,
            2, 0, 0, 0, // length of second entry
            'k', 'l', 0, 0,
            3, 0, 0, 0, // length of third entry
            'x', 'y', 'z', 0
        };

        checkSerialization(params, expected);
    }

    /**
     * test for nested
     */
    @Test
    public void testForNesting() {
        params.setInteger("name", 123);
        assertTrue(params.size() == 1);

        Parameters nested = new Parameters();

        params.setNestedParameters("nested", nested);
        assertTrue(params.size() == 2);

        nested.setInteger("internal", 456);
        assertTrue(nested.size() == 1);

        nested.setDouble("internal2", 3.125);

        assertTrue(params.size() == 2);
        assertTrue(nested.size() == 2);

        Parameters tmp = params.getNestedParameters("nested");
        assertEquals(tmp, nested);

        Parameters nested2 = new Parameters();

        nested.setNestedParameters("more nested", nested2);

        assertTrue(params.size() == 2);
        assertTrue(nested.size() == 3);

        nested2.setInteger("more internal", 789);

        assertTrue(params.size() == 2);
        assertTrue(nested.size() == 3);
        assertTrue(nested2.size() == 1);

        assertEquals(params.getType("name"), Parameters.EntryType.INTEGER);
        assertEquals(params.getType("nested"),
                Parameters.EntryType.NESTED_PARAMETERS);
        assertEquals(nested.getType("internal"),
                Parameters.EntryType.INTEGER);
        assertEquals(nested.getType("internal2"),
                Parameters.EntryType.DOUBLE);
        assertEquals(nested.getType("more nested"),
                Parameters.EntryType.NESTED_PARAMETERS);
        assertEquals(nested2.getType("more internal"),
                Parameters.EntryType.INTEGER);

        try {
            nested.getType("blabla");
            fail("should never reach this point");
        } catch (NoSuchNameException e) {
            // expected, ignore
        }

        Iterator<Parameters.Entry> it = params.iterator();
        assertTrue(it.hasNext());
        Parameters.Entry e = it.next();
        assertEquals(e.type(), Parameters.EntryType.INTEGER);
        assertTrue(it.hasNext());
        e = it.next();
        assertEquals(e.type(), Parameters.EntryType.NESTED_PARAMETERS);
        Parameters n = e.getNestedParameters();
        assertTrue(n.size() == 3);
        assertFalse(it.hasNext());

        assertTrue(params.toString().equals(
                "entry 0:\n" +
                "name: name\n" +
                "integer: 123\n" +
                "entry 1:\n" +
                "name: nested\n" +
                "nested parameters:\n" +
                "  entry 0:\n" +
                "  name: internal\n" +
                "  integer: 456\n" +
                "  entry 1:\n" +
                "  name: internal2\n" +
                "  double: 3.125\n" +
                "  entry 2:\n" +
                "  name: more nested\n" +
                "  nested parameters:\n" +
                "    entry 0:\n" +
                "    name: more internal\n" +
                "    integer: 789\n"));

        byte[] expected = {
            2, 0, 0, 0, // num of entries
            4, 0, 0, 0, // length of "name"
            'n', 'a', 'm', 'e',
            2, 0, 0, 0, // type code for integer
            123, 0, 0, 0, // value
            6, 0, 0, 0, // length of "nested"
            'n', 'e', 's', 't',
            'e', 'd', 0, 0,
            13, 0, 0, 0, // type code for nested
            3, 0, 0, 0, // num of entries in nested
            8, 0, 0, 0, // length o "internal"
            'i', 'n', 't', 'e',
            'r', 'n', 'a', 'l',
            2, 0, 0, 0, // type code for integer
            -56, 1, 0, 0, // value
            9, 0, 0, 0, // length o "internal2"
            'i', 'n', 't', 'e',
            'r', 'n', 'a', 'l',
            '2', 0, 0, 0,
            4, 0, 0, 0, // type code for double
            0, 0, 0, 0,
            0, 0, 9, 64, // 3.125
            11, 0, 0, 0, // length of "more nested"
            'm', 'o', 'r', 'e',
            ' ', 'n', 'e', 's',
            't', 'e', 'd', 0,
            13, 0, 0, 0, // type code for nested
            1, 0, 0, 0, // num of entries in more nested
            13, 0, 0, 0, // length of "more internal"
            'm', 'o', 'r', 'e',
            ' ', 'i', 'n', 't',
            'e', 'r', 'n', 'a',
            'l', 0, 0, 0,
            2, 0, 0, 0, // type code for integer
            21, 3, 0, 0         // value
        };

        checkSerialization(params, expected);
    }

    /**
     * test for nested arrays
     */
    @Test
    public void testForNestedArrays() {
    	Parameters[] nested = new Parameters[3];
    	nested[0] = new Parameters();
    	nested[1] = new Parameters();
    	nested[2] = new Parameters();

    	params.setNestedArray("name", nested);
        assertTrue(params.size() == 1);
    	
        params.setInteger("i", 7);
        assertTrue(params.size() == 2);

        // first nested
        nested[0].setInteger("x", 10);
        assertTrue(nested[0].size() == 1);
        
        // second nested
        nested[1].setInteger("x", 20);
        nested[1].setInteger("y", 21);
        assertTrue(nested[1].size() == 2);

        // third nested
        nested[2].setInteger("x", 30);
        nested[2].setInteger("y", 31);
        nested[2].setInteger("z", 32);
        assertTrue(nested[2].size() == 3);

        assertEquals(params.getType("name"),
        			Parameters.EntryType.NESTED_PARAMETERS_ARRAY);

        Iterator<Parameters.Entry> it = params.iterator();
        assertTrue(it.hasNext());
        Parameters.Entry e = it.next();
        assertEquals(e.type(), Parameters.EntryType.NESTED_PARAMETERS_ARRAY);
        Parameters[] na = e.getNestedArray();
        assertTrue(na == nested);
        assertTrue(it.hasNext());
        e = it.next();
        assertEquals(e.type(), Parameters.EntryType.INTEGER);
        assertFalse(it.hasNext());

        assertTrue(params.toString().equals(
                "entry 0:\n" +
                "name: name\n" +
                "nested parameters array of length 3:\n" +
                "  nested at index 0:\n" +
                "    entry 0:\n" +
                "    name: x\n" +
                "    integer: 10\n" +
                "  nested at index 1:\n" +
                "    entry 0:\n" +
                "    name: x\n" +
                "    integer: 20\n" +
                "    entry 1:\n" +
                "    name: y\n" +
                "    integer: 21\n" +
                "  nested at index 2:\n" +
                "    entry 0:\n" +
                "    name: x\n" +
                "    integer: 30\n" +
                "    entry 1:\n" +
                "    name: y\n" +
                "    integer: 31\n" +
                "    entry 2:\n" +
                "    name: z\n" +
                "    integer: 32\n" +
                "entry 1:\n" +
                "name: i\n" +
                "integer: 7\n"));

        byte[] expected = {
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

        checkSerialization(params, expected);
    }

    /*
     * test for locking removed
     */

    // helper
    private static void checkSerialization(Parameters params,
            byte[] expected) {

        // check for serialization into continuous buffer
        List<byte[]> buffers = params.serialize(Integer.MAX_VALUE);

        assertTrue(Arrays.equals(buffers.get(0), expected));

        // verify deserialization gives the same results
        Parameters params2 = new Parameters();
        params2.deserialize(buffers);
        List<byte[]> buffers2 = params2.serialize(Integer.MAX_VALUE);

        assertTrue(Arrays.equals(buffers2.get(0), expected));

        // check for serialization into multiple buffers
        // of different sizes
        for (int chunkSize = 4; chunkSize != expected.length;
                chunkSize += 4) {

            List<byte[]> buffers3 = params.serialize(chunkSize);
            int i = 0;
            for (byte[] buffer : buffers3) {
                assertTrue(buffer.length <= chunkSize);
                for (byte b : buffer) {
                    assertTrue(b == expected[i++]);
                }
            }
        }
    }
}
