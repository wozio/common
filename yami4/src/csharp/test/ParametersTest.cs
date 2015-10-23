// Copyright Paweł Kierski 2010, 2015.
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

namespace Inspirel.YAMI
{
    using NUnit.Framework;
    using System.Collections.Generic;

    [TestFixture]
    public class ParametersTest
    {
        private Parameters parameters = null;

        [SetUp]
        public void SetUp()
        {
            parameters = new Parameters();
        }

        [Test]
        public void testForEmpty()
        {
            Assert.IsTrue(parameters.Count == 0);

            Assert.AreEqual(string.Empty, parameters.ToString());

            try
            {
                parameters.Remove("no such entry");
                Assert.Fail("should never reach this point");
            }
            catch(NoSuchNameException ex)
            {
                Assert.AreEqual("Entry or object named 'no such entry' "
                    + "does not exist.", ex.Message);
            }

            // check for empty iteration
            IEnumerator<Parameters.Entry> it = parameters.GetEnumerator();
            Assert.IsFalse(it.MoveNext());

            Parameters.Entry e = parameters.Find("no such name");
            Assert.IsNull(e);

            IList<byte[]> buffers = parameters.Serialize(int.MaxValue);
            Assert.IsTrue(buffers.Count == 1);
            Assert.AreEqual(buffers[0], new byte[4]);

            Parameters params2 = new Parameters();
            params2.Deserialize(buffers);
            Assert.IsTrue(parameters.Count == 0);
        }

        [Test]
        public void testForBoolean()
        {
            parameters.SetBoolean("name", true);
            Assert.IsTrue(parameters.Count == 1);

            Parameters.EntryType t = parameters.GetType("name");
            Assert.AreEqual(t, Parameters.EntryType.BOOLEAN);

            Assert.IsTrue(parameters.GetBoolean("name"));

            Assert.AreEqual("entry 0:\n"
                + "name: name\n"
                + "boolean: 1\n",
                parameters.ToString());

            parameters.SetBoolean("name", false);
            Assert.IsTrue(parameters.Count == 1);

            try
            {
                parameters.GetInteger("name");
                Assert.Fail("should never reach this point");
            }
            catch(BadTypeException ex)
            {
                Assert.AreEqual("The entry 'name' does not have"
                    + " the expected type INTEGER", ex.Message);
            }

            IEnumerator<Parameters.Entry> it = parameters.GetEnumerator();
            Assert.IsTrue(it.MoveNext());
            Parameters.Entry e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.BOOLEAN);
            Assert.AreEqual("name", e.Name);
            Assert.IsFalse(e.GetBoolean());

            try
            {
                e.GetInteger();
                Assert.Fail("should never reach this point");
            }
            catch(BadTypeException)
            {
                // ignore
            }

            Assert.IsFalse(it.MoveNext());

            bool[] array = { true, false, true };
            parameters.SetBooleanArray("nameA", array);

            Assert.IsTrue(parameters.Count == 2);

            Assert.AreEqual(parameters.GetType("nameA"), 
                Parameters.EntryType.BOOLEAN_ARRAY);

            Assert.AreEqual(parameters.GetBooleanArray("nameA"), array);
            Assert.IsTrue(array[0] && array[1] == false && array[2]);

            array[2] = false;

            it = parameters.GetEnumerator();
            Assert.IsTrue(it.MoveNext());
            e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.BOOLEAN);
            Assert.AreEqual("name", e.Name);
            Assert.IsTrue(it.MoveNext());
            e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.BOOLEAN_ARRAY);
            Assert.AreEqual("nameA", e.Name);
            Assert.AreEqual(e.GetBooleanArray(), array);
            Assert.IsTrue(
                array[0] && array[1] == false && array[2] == false);
            Assert.IsFalse(it.MoveNext());

            Assert.AreEqual(
                "entry 0:\n"
                + "name: name\n"
                + "boolean: 0\n"
                + "entry 1:\n"
                + "name: nameA\n"
                + "boolean array: 1 0 0\n",
                parameters.ToString());

            byte[] expected = { 
                    2, 0, 0, 0, // num of entries
                    4, 0, 0, 0, // length of "name"
                    (byte)'n', (byte)'a', (byte)'m', (byte)'e',
                    1, 0, 0, 0, // type code for boolean
                    0, 0, 0, 0, // false
                    5, 0, 0, 0, // length of "nameA"
                    (byte)'n', (byte)'a', (byte)'m', (byte)'e',
                    (byte)'A', 0, 0, 0,
                    7, 0, 0, 0, // type code for boolean array
                    3, 0, 0, 0, // length of array
                    0x1, 0, 0, 0        // packed array (100 -> 0x1)
                };

            checkSerialization(parameters, expected);

            e = parameters.Find("name");
            Assert.AreEqual(e.Type, Parameters.EntryType.BOOLEAN);
            Assert.AreEqual("name", e.Name);

            e = parameters.Find("nameA");
            Assert.AreEqual(e.Type, Parameters.EntryType.BOOLEAN_ARRAY);
            Assert.AreEqual("nameA", e.Name);

            e = parameters.Find("no such name");
            Assert.IsNull(e);

            parameters.Remove("name");

            Assert.IsTrue(parameters.Count == 1);

            it = parameters.GetEnumerator();
            Assert.IsTrue(it.MoveNext());
            e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.BOOLEAN_ARRAY);
            Assert.AreEqual("nameA", e.Name);
            Assert.IsFalse(it.MoveNext());
        }

        [Test]
        public void testForInteger()
        {
            parameters.SetInteger("name", 123);
            Assert.IsTrue(parameters.Count == 1);

            Parameters.EntryType t = parameters.GetType("name");
            Assert.AreEqual(t, Parameters.EntryType.INTEGER);

            Assert.IsTrue(parameters.GetInteger("name") == 123);

            int[] array = { 10, 20, 30 };
            parameters.SetIntegerArray("nameA", array);

            Assert.IsTrue(parameters.Count == 2);

            Assert.AreEqual(parameters.GetType("nameA"), 
                Parameters.EntryType.INTEGER_ARRAY);

            Assert.AreEqual(parameters.GetIntegerArray("nameA"), array);
            Assert.IsTrue(
                array[0] == 10 && array[1] == 20 && array[2] == 30);

            array[2] = 31;

            IEnumerator<Parameters.Entry> it = parameters.GetEnumerator();
            Assert.IsTrue(it.MoveNext());
            Parameters.Entry e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.INTEGER);
            Assert.AreEqual("name", e.Name);
            Assert.IsTrue(e.GetInteger() == 123);
            Assert.IsTrue(it.MoveNext());
            e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.INTEGER_ARRAY);
            Assert.AreEqual("nameA", e.Name);
            Assert.AreEqual(e.GetIntegerArray(), array);
            Assert.IsTrue(
                array[0] == 10 && array[1] == 20 && array[2] == 31);
            Assert.IsFalse(it.MoveNext());

            Assert.AreEqual("entry 0:\n"
                + "name: name\n"
                + "integer: 123\n"
                + "entry 1:\n"
                + "name: nameA\n"
                + "integer array: 10 20 31\n",
                parameters.ToString());

            byte[] expected = { 
                    2, 0, 0, 0, // num of entries
                    4, 0, 0, 0, // length of "name"
                    (byte)'n', (byte)'a', (byte)'m', (byte)'e',
                    2, 0, 0, 0, // type code for integer
                    123, 0, 0, 0, // 123
                    5, 0, 0, 0, // length of "nameA"
                    (byte)'n', (byte)'a', (byte)'m', (byte)'e',
                    (byte)'A', 0, 0, 0,
                    8, 0, 0, 0, // type code for integer array
                    3, 0, 0, 0, // length of array
                    10, 0, 0, 0, // array values
                    20, 0, 0, 0, // array values
                    31, 0, 0, 0         // array values
                };

            checkSerialization(parameters, expected);

            it = parameters.GetEnumerator();
            Assert.IsTrue(it.MoveNext());
            e = it.Current;
            Assert.AreEqual("name", e.Name);
            parameters.Remove("name");

            Assert.IsTrue(parameters.Count == 1);

            it = parameters.GetEnumerator();
            Assert.IsTrue(it.MoveNext());
            e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.INTEGER_ARRAY);
            Assert.AreEqual("nameA", e.Name);
            Assert.IsFalse(it.MoveNext());
        }

        [Test]
        public void testForLong()
        {
            parameters.SetLong("name", 1234567890L);
            Assert.IsTrue(parameters.Count == 1);

            Parameters.EntryType t = parameters.GetType("name");
            Assert.AreEqual(t, Parameters.EntryType.LONG);

            Assert.IsTrue(parameters.GetLong("name") == 1234567890L);

            long[] array = { 100L, 200L, 300L };
            parameters.SetLongArray("nameA", array);

            Assert.IsTrue(parameters.Count == 2);

            Assert.AreEqual(parameters.GetType("nameA"), 
                Parameters.EntryType.LONG_ARRAY);

            Assert.AreEqual(parameters.GetLongArray("nameA"), array);
            Assert.IsTrue(array.Length == 3);
            Assert.IsTrue(
                array[0] == 100L && array[1] == 200L && array[2] == 300L);

            array[2] = 310L;

            IEnumerator<Parameters.Entry> it = parameters.GetEnumerator();
            Assert.IsTrue(it.MoveNext());
            Parameters.Entry e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.LONG);
            Assert.AreEqual("name", e.Name);
            Assert.IsTrue(e.GetLong() == 1234567890L);
            Assert.IsTrue(it.MoveNext());
            e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.LONG_ARRAY);
            Assert.AreEqual("nameA", e.Name);
            Assert.AreEqual(e.GetLongArray(), array);
            Assert.IsTrue(array.Length == 3);
            Assert.IsTrue(
                array[0] == 100L && array[1] == 200L && array[2] == 310L);
            Assert.IsFalse(it.MoveNext());

            Assert.AreEqual("entry 0:\n"
                + "name: name\n"
                + "long: 1234567890\n"
                + "entry 1:\n"
                + "name: nameA\n"
                + "long array: 100 200 310\n",
                parameters.ToString());

            byte[] expected = { 
                    2, 0, 0, 0, // num of entries
                    4, 0, 0, 0, // length of "name"
                    (byte)'n', (byte)'a', (byte)'m', (byte)'e',
                    3, 0, 0, 0, // type code for long long
                    (byte) 210, 2, (byte) 150, 73, // 1234567890
                    0, 0, 0, 0,
                    5, 0, 0, 0, // length of "nameA"
                    (byte)'n', (byte)'a', (byte)'m', (byte)'e',
                    (byte)'A', 0, 0, 0,
                    9, 0, 0, 0, // type code for long long array
                    3, 0, 0, 0, // length of array
                    100, 0, 0, 0, // array values
                    0, 0, 0, 0,
                    (byte) 200, 0, 0, 0, // array values
                    0, 0, 0, 0,
                    54, 1, 0, 0, // array values
                    0, 0, 0, 0
                };

            checkSerialization(parameters, expected);

            // additional test for bit extension bug

            parameters = new Parameters();
            parameters.SetLong("a", (long)1 << 31);
            expected = new byte[] { 
                    1, 0, 0, 0, // num of entries
                    1, 0, 0, 0, // length of "a"
                    (byte)'a', 0, 0, 0,
                    3, 0, 0, 0, // type code for long long
                    0, 0, 0, (byte) 128,
                    0, 0, 0, 0
                };

            checkSerialization(parameters, expected);
        }

        [Test]
        public void testForDouble()
        {
            parameters.SetDouble("name", 3.125);
            Assert.IsTrue(parameters.Count == 1);

            Parameters.EntryType t = parameters.GetType("name");
            Assert.AreEqual(t, Parameters.EntryType.DOUBLE);

            Assert.AreEqual(parameters.GetDouble("name"), 3.125, 0.0);

            double[] array = { 1.875, 2.875, 3.875 };
            parameters.SetDoubleArray("nameA", array);

            Assert.IsTrue(parameters.Count == 2);

            Assert.AreEqual(parameters.GetType("nameA"),
                Parameters.EntryType.DOUBLE_ARRAY);

            Assert.AreEqual(parameters.GetDoubleArray("nameA"), array);
            Assert.IsTrue(array.Length == 3);
            Assert.IsTrue(
                array[0] == 1.875 && array[1] == 2.875 && array[2] == 3.875);

            array[2] = 3.625;

            IEnumerator<Parameters.Entry> it = parameters.GetEnumerator();
            Assert.IsTrue(it.MoveNext());
            Parameters.Entry e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.DOUBLE);
            Assert.AreEqual("name", e.Name);
            Assert.AreEqual(e.GetDouble(), 3.125, 0.0);
            Assert.IsTrue(it.MoveNext());
            e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.DOUBLE_ARRAY);
            Assert.AreEqual("nameA", e.Name);
            Assert.AreEqual(e.GetDoubleArray(), array);
            Assert.IsTrue(array.Length == 3);
            Assert.IsTrue(
                array[0] == 1.875 && array[1] == 2.875 && array[2] == 3.625);
            Assert.IsFalse(it.MoveNext());

            Assert.AreEqual("entry 0:\n"
                + "name: name\n"
                + "double: 3.125\n"
                + "entry 1:\n"
                + "name: nameA\n"
                + "double array: 1.875 2.875 3.625\n",
                parameters.ToString());

            byte[] expected = { 
                    2, 0, 0, 0, // num of entries
                    4, 0, 0, 0, // length of "name"
                    (byte)'n', (byte)'a', (byte)'m', (byte)'e',
                    4, 0, 0, 0, // type code for double
                    0, 0, 0, 0,
                    0, 0, 9, 64, // 3.125
                    5, 0, 0, 0, // length of "nameA"
                    (byte)'n', (byte)'a', (byte)'m', (byte)'e',
                    (byte)'A', 0, 0, 0,
                    10, 0, 0, 0, // type code for double array
                    3, 0, 0, 0, // length of array
                    0, 0, 0, 0, // array values
                    0, 0, (byte)0xFE, 63,
                    0, 0, 0, 0, // array values
                    0, 0, 7, 64,
                    0, 0, 0, 0, // array values
                    0, 0, 13, 64
                };

            checkSerialization(parameters, expected);
        }

        [Test]
        public void testForString()
        {
            string sourceValue =
                "Kolorowe kredki w pudeleczku nosze,\n"
                + "kolorowe kredki, bardzo lubie je!";

            string name = "some rather longer name";

            parameters.SetString(name, sourceValue);
            Assert.IsTrue(parameters.Count == 1);

            Parameters.EntryType t = parameters.GetType(name);
            Assert.AreEqual(t, Parameters.EntryType.STRING);

            Assert.AreEqual(sourceValue, parameters.GetString(name));

            parameters.SetString("other name", sourceValue);
            Assert.IsTrue(parameters.Count == 2);

            Assert.AreEqual(sourceValue, parameters.GetString("other name"));

            string[] array = { "Kazio", "Krzysio", "Rysio", "Zbysio" };
            parameters.SetStringArray("nameA", array);

            Assert.IsTrue(parameters.Count == 3);

            Assert.AreEqual(parameters.GetType("nameA"), 
                Parameters.EntryType.STRING_ARRAY);

            Assert.AreEqual(parameters.GetStringArray("nameA"), array);
            Assert.IsTrue(array.Length == 4);
            Assert.IsTrue(
                array[0] == "Kazio"
                && array[1] == "Krzysio"
                && array[2] == "Rysio"
                && array[3] == "Zbysio"
            );

            IEnumerator<Parameters.Entry> it = parameters.GetEnumerator();
            Assert.IsTrue(it.MoveNext());
            Parameters.Entry e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.STRING);
            Assert.AreEqual(name, e.Name);
            Assert.AreEqual(sourceValue, e.GetString());
            Assert.IsTrue(it.MoveNext());
            e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.STRING);
            Assert.AreEqual("other name", e.Name);
            Assert.AreEqual(sourceValue, e.GetString());
            Assert.IsTrue(it.MoveNext());
            e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.STRING_ARRAY);
            Assert.AreEqual("nameA", e.Name);
            Assert.AreEqual(array, e.GetStringArray());
            Assert.IsTrue(array.Length == 4);
            Assert.IsTrue(
                array[0] == "Kazio"
                && array[1] == "Krzysio"
                && array[2] == "Rysio"
                && array[3] == "Zbysio"
            );
            Assert.IsFalse(it.MoveNext());

            Assert.AreEqual(
                "entry 0:\n"
                + "name: some rather longer name\n"
                + "string: Kolorowe kredki w pudeleczku nosze,\n"
                + "kolorowe kredki, bardzo lubie je!\n"
                + "entry 1:\n"
                + "name: other name\n"
                + "string: Kolorowe kredki w pudeleczku nosze,\n"
                + "kolorowe kredki, bardzo lubie je!\n"
                + "entry 2:\n"
                + "name: nameA\n"
                + "string array: Kazio Krzysio Rysio Zbysio\n",
                parameters.ToString());

            byte[] expected = { 
                    3, 0, 0, 0, // num of entries
                    23, 0, 0, 0, // length of "some rather longer name"
                    (byte)'s', (byte)'o', (byte)'m', (byte)'e',
                    (byte)' ', (byte)'r', (byte)'a', (byte)'t',
                    (byte)'h', (byte)'e', (byte)'r', (byte)' ',
                    (byte)'l', (byte)'o', (byte)'n', (byte)'g',
                    (byte)'e', (byte)'r', (byte)' ', (byte)'n',
                    (byte)'a', (byte)'m', (byte)'e', 0,
                    5, 0, 0, 0, // type code for string
                    69, 0, 0, 0, // length
                    (byte)'K', (byte)'o', (byte)'l', (byte)'o',
                    (byte)'r', (byte)'o', (byte)'w', (byte)'e',
                    (byte)' ', (byte)'k', (byte)'r', (byte)'e',
                    (byte)'d', (byte)'k', (byte)'i', (byte)' ',
                    (byte)'w', (byte)' ', (byte)'p', (byte)'u',
                    (byte)'d', (byte)'e', (byte)'l', (byte)'e',
                    (byte)'c', (byte)'z', (byte)'k', (byte)'u',
                    (byte)' ', (byte)'n', (byte)'o', (byte)'s',
                    (byte)'z', (byte)'e', (byte)',', (byte)'\n',
                    (byte)'k', (byte)'o', (byte)'l', (byte)'o',
                    (byte)'r', (byte)'o', (byte)'w', (byte)'e',
                    (byte)' ', (byte)'k', (byte)'r', (byte)'e',
                    (byte)'d', (byte)'k', (byte)'i', (byte)',',
                    (byte)' ', (byte)'b', (byte)'a', (byte)'r',
                    (byte)'d', (byte)'z', (byte)'o', (byte)' ',
                    (byte)'l', (byte)'u', (byte)'b', (byte)'i',
                    (byte)'e', (byte)' ', (byte)'j', (byte)'e',
                    (byte)'!', 0, 0, 0,
                    10, 0, 0, 0, // length of "other name"
                    (byte)'o', (byte)'t', (byte)'h', (byte)'e',
                    (byte)'r', (byte)' ', (byte)'n', (byte)'a',
                    (byte)'m', (byte)'e', 0, 0,
                    5, 0, 0, 0, // type code for string
                    69, 0, 0, 0, // length
                    (byte)'K', (byte)'o', (byte)'l', (byte)'o',
                    (byte)'r', (byte)'o', (byte)'w', (byte)'e',
                    (byte)' ', (byte)'k', (byte)'r', (byte)'e',
                    (byte)'d', (byte)'k', (byte)'i', (byte)' ',
                    (byte)'w', (byte)' ', (byte)'p', (byte)'u',
                    (byte)'d', (byte)'e', (byte)'l', (byte)'e',
                    (byte)'c', (byte)'z', (byte)'k', (byte)'u',
                    (byte)' ', (byte)'n', (byte)'o', (byte)'s',
                    (byte)'z', (byte)'e', (byte)',', (byte)'\n',
                    (byte)'k', (byte)'o', (byte)'l', (byte)'o',
                    (byte)'r', (byte)'o', (byte)'w', (byte)'e',
                    (byte)' ', (byte)'k', (byte)'r', (byte)'e',
                    (byte)'d', (byte)'k', (byte)'i', (byte)',',
                    (byte)' ', (byte)'b', (byte)'a', (byte)'r',
                    (byte)'d', (byte)'z', (byte)'o', (byte)' ',
                    (byte)'l', (byte)'u', (byte)'b', (byte)'i',
                    (byte)'e', (byte)' ', (byte)'j', (byte)'e',
                    (byte)'!', 0, 0, 0,
                    5, 0, 0, 0, // length of "nameA"
                    (byte)'n', (byte)'a', (byte)'m', (byte)'e',
                    (byte)'A', 0, 0, 0,
                    11, 0, 0, 0, // type code for string array
                    4, 0, 0, 0, // length of array
                    5, 0, 0, 0, // length of first entry
                    (byte)'K', (byte)'a', (byte)'z', (byte)'i',
                    (byte)'o', 0, 0, 0,
                    7, 0, 0, 0, // length of second entry
                    (byte)'K', (byte)'r', (byte)'z', (byte)'y',
                    (byte)'s', (byte)'i', (byte)'o', 0,
                    5, 0, 0, 0, // length of third entry
                    (byte)'R', (byte)'y', (byte)'s', (byte)'i',
                    (byte)'o', 0, 0, 0,
                    6, 0, 0, 0, // length of fourth entry
                    (byte)'Z', (byte)'b', (byte)'y', (byte)'s',
                    (byte)'i', (byte)'o', 0, 0
                };

            checkSerialization(parameters, expected);

            // additional test for serialization of multibyte characters

            string nonASCIIString = "Unicode \u00a3 pound sign";
            Parameters p2 = new Parameters();
            p2.SetString("a", nonASCIIString);
            IList<byte[]> buffers = p2.Serialize(int.MaxValue);
            Parameters p3 = new Parameters();
            p3.Deserialize(buffers);
            Assert.IsTrue(p2.GetString("a") == p3.GetString("a"));
        }

        [Test]
        public void testForBinary()
        {
            byte[] sourceValue = { 
                    (byte)'a', (byte)'b', (byte)'c', (byte)'\0', (byte)'d' 
                };

            string name = "some rather longer name";

            parameters.SetBinary(name, sourceValue);
            Assert.IsTrue(parameters.Count == 1);

            Parameters.EntryType t = parameters.GetType(name);
            Assert.AreEqual(t, Parameters.EntryType.BINARY);

            Assert.AreEqual(sourceValue, parameters.GetBinary(name));

            parameters.SetBinary("other name", sourceValue);
            Assert.IsTrue(parameters.Count == 2);

            Assert.AreEqual(sourceValue, parameters.GetBinary("other name"));

            byte[] a0 = { (byte)'a', (byte)'b', (byte)'c' };
            byte[] a1 = { (byte)'k', (byte)'l' };
            byte[] a2 = { (byte)'x', (byte)'y', (byte)'z' };
            byte[][] array = { a0, a1, a2 };
            parameters.SetBinaryArray("nameA", array);

            Assert.IsTrue(parameters.Count == 3);

            Assert.AreEqual(parameters.GetType("nameA"), 
                Parameters.EntryType.BINARY_ARRAY);

            Assert.AreEqual(parameters.GetBinaryArray("nameA"), array);
            Assert.IsTrue(array.Length == 3);
            Assert.AreEqual(a0, array[0]);
            Assert.AreEqual(a1, array[1]);
            Assert.AreEqual(a2, array[2]);

            IEnumerator<Parameters.Entry> it = parameters.GetEnumerator();
            Assert.IsTrue(it.MoveNext());
            Parameters.Entry e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.BINARY);
            Assert.AreEqual(name, e.Name);
            Assert.AreEqual(sourceValue, e.GetBinary());
            Assert.IsTrue(it.MoveNext());
            e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.BINARY);
            Assert.AreEqual("other name", e.Name);
            Assert.AreEqual(sourceValue, e.GetBinary());
            Assert.IsTrue(it.MoveNext());
            e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.BINARY_ARRAY);
            Assert.AreEqual("nameA", e.Name);
            Assert.AreEqual(array, e.GetBinaryArray());
            Assert.IsTrue(array.Length == 3);
            Assert.AreEqual(a0, array[0]);
            Assert.AreEqual(a1, array[1]);
            Assert.AreEqual(a2, array[2]);
            Assert.IsFalse(it.MoveNext());

            Assert.AreEqual("entry 0:\n"
                + "name: some rather longer name\n"
                + "binary of length 5\n"
                + "entry 1:\n"
                + "name: other name\n"
                + "binary of length 5\n"
                + "entry 2:\n"
                + "name: nameA\n"
                + "binary array of length 3\n",
                parameters.ToString());

            byte[] expected = { 
                    3, 0, 0, 0, // num of entries
                    23, 0, 0, 0, // length of "some rather longer name"
                    (byte)'s', (byte)'o', (byte)'m', (byte)'e',
                    (byte)' ', (byte)'r', (byte)'a', (byte)'t',
                    (byte)'h', (byte)'e', (byte)'r', (byte)' ',
                    (byte)'l', (byte)'o', (byte)'n', (byte)'g',
                    (byte)'e', (byte)'r', (byte)' ', (byte)'n',
                    (byte)'a', (byte)'m', (byte)'e', 0,
                    6, 0, 0, 0, // type code for binary
                    5, 0, 0, 0, // length
                    (byte)'a', (byte)'b', (byte)'c', (byte)'\0',
                    (byte)'d', 0, 0, 0,
                    10, 0, 0, 0, // length of "other name"
                    (byte)'o', (byte)'t', (byte)'h', (byte)'e',
                    (byte)'r', (byte)' ', (byte)'n', (byte)'a',
                    (byte)'m', (byte)'e', 0, 0,
                    6, 0, 0, 0, // type code for binary
                    5, 0, 0, 0, // length
                    (byte)'a', (byte)'b', (byte)'c', (byte)'\0',
                    (byte)'d', 0, 0, 0,
                    5, 0, 0, 0, // length of "nameA"
                    (byte)'n', (byte)'a', (byte)'m', (byte)'e',
                    (byte)'A', 0, 0, 0,
                    12, 0, 0, 0, // type code for binary array
                    3, 0, 0, 0, // length of array
                    3, 0, 0, 0, // length of first entry
                    (byte)'a', (byte)'b', (byte)'c', 0,
                    2, 0, 0, 0, // length of second entry
                    (byte)'k', (byte)'l', 0, 0,
                    3, 0, 0, 0, // length of third entry
                    (byte)'x', (byte)'y', (byte)'z', 0
                };

            checkSerialization(parameters, expected);
        }

        [Test]
        public void testForNesting()
        {
            parameters.SetInteger("name", 123);
            Assert.IsTrue(parameters.Count == 1);

            Parameters nested = new Parameters();

            parameters.SetNestedParameters("nested", nested);
            Assert.IsTrue(parameters.Count == 2);

            nested.SetInteger("internal", 456);
            Assert.IsTrue(nested.Count == 1);

            nested.SetDouble("internal2", 3.125);

            Assert.IsTrue(parameters.Count == 2);
            Assert.IsTrue(nested.Count == 2);

            Parameters tmp = parameters.GetNestedParameters("nested");
            Assert.AreEqual(tmp, nested);

            Parameters nested2 = new Parameters();

            nested.SetNestedParameters("more nested", nested2);

            Assert.IsTrue(parameters.Count == 2);
            Assert.IsTrue(nested.Count == 3);

            nested2.SetInteger("more internal", 789);

            Assert.IsTrue(parameters.Count == 2);
            Assert.IsTrue(nested.Count == 3);
            Assert.IsTrue(nested2.Count == 1);

            Assert.AreEqual(parameters.GetType("name"), 
                Parameters.EntryType.INTEGER);
            Assert.AreEqual(parameters.GetType("nested"), 
                Parameters.EntryType.NESTED_PARAMETERS);
            Assert.AreEqual(nested.GetType("internal"), 
                Parameters.EntryType.INTEGER);
            Assert.AreEqual(nested.GetType("internal2"), 
                Parameters.EntryType.DOUBLE);
            Assert.AreEqual(nested.GetType("more nested"), 
                Parameters.EntryType.NESTED_PARAMETERS);
            Assert.AreEqual(nested2.GetType("more internal"), 
                Parameters.EntryType.INTEGER);

            try
            {
                nested.GetType("blabla");
                Assert.Fail("should never reach this point");
            }
            catch(NoSuchNameException)
            {
                // expected, ignore
            }

            IEnumerator<Parameters.Entry> it = parameters.GetEnumerator();
            Assert.IsTrue(it.MoveNext());
            Parameters.Entry e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.INTEGER);
            Assert.IsTrue(it.MoveNext());
            e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.NESTED_PARAMETERS);
            Parameters n = e.GetNestedParameters();
            Assert.IsTrue(n.Count == 3);
            Assert.IsFalse(it.MoveNext());

            Assert.AreEqual("entry 0:\n"
                + "name: name\n"
                + "integer: 123\n"
                + "entry 1:\n"
                + "name: nested\n"
                + "nested parameters:\n"
                + "  entry 0:\n"
                + "  name: internal\n"
                + "  integer: 456\n"
                + "  entry 1:\n"
                + "  name: internal2\n"
                + "  double: 3.125\n"
                + "  entry 2:\n"
                + "  name: more nested\n"
                + "  nested parameters:\n"
                + "    entry 0:\n"
                + "    name: more internal\n"
                + "    integer: 789\n",
                parameters.ToString());

            byte[] expected = { 
                    2, 0, 0, 0, // num of entries
                    4, 0, 0, 0, // length of "name"
                    (byte)'n', (byte)'a', (byte)'m', (byte)'e',
                    2, 0, 0, 0, // type code for integer
                    123, 0, 0, 0, // value
                    6, 0, 0, 0, // length of "nested"
                    (byte)'n', (byte)'e', (byte)'s', (byte)'t',
                    (byte)'e', (byte)'d', 0, 0,
                    13, 0, 0, 0, // type code for nested
                    3, 0, 0, 0, // num of entries in nested
                    8, 0, 0, 0, // length o "internal"
                    (byte)'i', (byte)'n', (byte)'t', (byte)'e',
                    (byte)'r', (byte)'n', (byte)'a', (byte)'l',
                    2, 0, 0, 0, // type code for integer
                    256-56, 1, 0, 0, // value
                    9, 0, 0, 0, // length o "internal2"
                    (byte)'i', (byte)'n', (byte)'t', (byte)'e',
                    (byte)'r', (byte)'n', (byte)'a', (byte)'l',
                    (byte)'2', 0, 0, 0,
                    4, 0, 0, 0, // type code for double
                    0, 0, 0, 0,
                    0, 0, 9, 64, // 3.125
                    11, 0, 0, 0, // length of "more nested"
                    (byte)'m', (byte)'o', (byte)'r', (byte)'e',
                    (byte)' ', (byte)'n', (byte)'e', (byte)'s',
                    (byte)'t', (byte)'e', (byte)'d', 0,
                    13, 0, 0, 0, // type code for nested
                    1, 0, 0, 0, // num of entries in more nested
                    13, 0, 0, 0, // length of "more internal"
                    (byte)'m', (byte)'o', (byte)'r', (byte)'e',
                    (byte)' ', (byte)'i', (byte)'n', (byte)'t',
                    (byte)'e', (byte)'r', (byte)'n', (byte)'a',
                    (byte)'l', 0, 0, 0,
                    2, 0, 0, 0, // type code for integer
                    21, 3, 0, 0         // value
                };

            checkSerialization(parameters, expected);
        }

        [Test]
        public void testForNestedArrays() {
    	    Parameters[] nested = new Parameters[3];
    	    nested[0] = new Parameters();
    	    nested[1] = new Parameters();
    	    nested[2] = new Parameters();

    	    parameters.SetNestedArray("name", nested);
            Assert.IsTrue(parameters.Count == 1);
        	
            parameters.SetInteger("i", 7);
            Assert.IsTrue(parameters.Count == 2);

            // first nested
            nested[0].SetInteger("x", 10);
            Assert.IsTrue(nested[0].Count == 1);
            
            // second nested
            nested[1].SetInteger("x", 20);
            nested[1].SetInteger("y", 21);
            Assert.IsTrue(nested[1].Count == 2);

            // third nested
            nested[2].SetInteger("x", 30);
            nested[2].SetInteger("y", 31);
            nested[2].SetInteger("z", 32);
            Assert.IsTrue(nested[2].Count == 3);

            Assert.AreEqual(parameters.GetType("name"),
        			    Parameters.EntryType.NESTED_PARAMETERS_ARRAY);

            IEnumerator<Parameters.Entry> it = parameters.GetEnumerator();
            Assert.IsTrue(it.MoveNext());
            Parameters.Entry e = it.Current;
            Assert.AreEqual(e.Type, 
                Parameters.EntryType.NESTED_PARAMETERS_ARRAY);
            Parameters[] na = e.GetNestedArray();
            Assert.IsTrue(na == nested);
            Assert.IsTrue(it.MoveNext());
            e = it.Current;
            Assert.AreEqual(e.Type, Parameters.EntryType.INTEGER);
            Assert.IsFalse(it.MoveNext());

            Assert.IsTrue(parameters.ToString() ==
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
                    "integer: 7\n");

            byte[] expected = {
                2, 0, 0, 0,         // num of entries
                4, 0, 0, 0,         // length of "name"
                (byte)'n', (byte)'a', (byte)'m', (byte)'e',
                14, 0, 0, 0,        // type code for nested params array
                3, 0, 0, 0,         // array length

                // first nested

                1, 0, 0, 0,         // num of entries
                1, 0, 0, 0,         // length of "x"
                (byte)'x', 0, 0, 0,
                2, 0, 0, 0,         // type code for integer
                10, 0, 0, 0,        // value

                // second nested

                2, 0, 0, 0,         // num of entries
                1, 0, 0, 0,         // length of "x"
                (byte)'x', 0, 0, 0,
                2, 0, 0, 0,         // type code for integer
                20, 0, 0, 0,        // value
                1, 0, 0, 0,         // length of "y"
                (byte)'y', 0, 0, 0,
                2, 0, 0, 0,         // type code for integer
                21, 0, 0, 0,        // value

                // third nested

                3, 0, 0, 0,         // num of entries
                1, 0, 0, 0,         // length of "x"
                (byte)'x', 0, 0, 0,
                2, 0, 0, 0,         // type code for integer
                30, 0, 0, 0,        // value
                1, 0, 0, 0,         // length of "y"
                (byte)'y', 0, 0, 0,
                2, 0, 0, 0,         // type code for integer
                31, 0, 0, 0,        // value
                1, 0, 0, 0,         // length of "z"
                (byte)'z', 0, 0, 0,
                2, 0, 0, 0,         // type code for integer
                32, 0, 0, 0,        // value

                1, 0, 0, 0,         // length of "i"
                (byte)'i', 0, 0, 0,
                2, 0, 0, 0,         // type code for integer
                7, 0, 0, 0          // value
            };

            checkSerialization(parameters, expected);
        }

    /*
     * test for locking removed
     */

        private static void checkSerialization(
            Parameters parameters, 
            byte[] expected)
        {
            // check for serialization into continuous buffer
            IList<byte[]> buffers = parameters.Serialize(int.MaxValue);

            Assert.AreEqual(expected, buffers[0]);

            // verify deserialization gives the same results
            Parameters params2 = new Parameters();
            params2.Deserialize(buffers);
            IList<byte[]> buffers2 = params2.Serialize(int.MaxValue);

            Assert.AreEqual(expected, buffers2[0]);

            // check for serialization into multiple buffers
            // of different sizes
            for(int chunkSize = 4; 
                chunkSize != expected.Length; 
                chunkSize += 4)
            {
                IList<byte[]> buffers3 = parameters.Serialize(chunkSize);
                int i = 0;
                foreach(byte[] buffer in buffers3)
                {
                    Assert.IsTrue(buffer.Length <= chunkSize);
                    foreach(byte b in buffer)
                    {
                        Assert.IsTrue(b == expected[i++]);
                    }
                }
            }
        }

    }
}
