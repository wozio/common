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

using System.Collections.Generic;
using System.Text;
using Inspirel.YAMI.details;

namespace Inspirel.YAMI
{
    /// <summary>
    /// Collection of message parameters. 
    /// </summary>
    /// <remarks>
    /// A collection of message parameters is a list of typed 
    /// {name, value} pairs. <br />Each entry in this collection 
    /// has a unique name and can have one of the following types:
    /// <list type="bullet">
    /// <item><description><c>bool</c> or <c>bool</c> array</description>
    /// </item> 
    /// <item><description><c>int</c> or <c>int</c> array</description>
    /// </item> 
    /// <item><description><c>long</c> or <c>long</c> array</description>
    /// </item> 
    /// <item><description><c>double</c> or <c>double</c> array</description>
    /// </item> 
    /// <item><description><c>string</c> or <c>string</c> array</description>
    /// </item> 
    /// <item><description><c>byte[]</c> buffers or their arrays
    /// </description></item>
    /// <item><description>nested <c>Parameters</c> object or their arrays,
    /// which provides its own scope for naming.</description></item> 
    /// </list>
    /// <para> The names 
    /// of entries are searched for using case-sensitive 
    /// comparisons. </para> 
    /// <para> <b>Note:</b> The instances of this 
    /// class should not be used from multiple threads without 
    /// synchronization; it is safe to use separate instances 
    /// in separate threads. </para> 
    /// <para> <b>Note:</b> The entries are <i>ordered</i> - the order 
    /// in which they are created influences the final serialized 
    /// form of the message payload.<br /> Newly created entries 
    /// are appended to the end of the collection unless there is 
    /// an existing empty slot that can be reused - the appropriate 
    /// slot is searched for from the beginning to the end of the 
    /// collection and if no free slot is found the collection is 
    /// extended at the end.<br /> The above guarantee concerns the 
    /// user code that relies on predictable serialization. </para>
    /// </remarks>
    public sealed class Parameters
        : YAMISerializable, IEnumerable<Parameters.Entry>
    {

        /// <summary>
        /// Enumeration defining all possible entry types.
        /// </summary>
        public enum EntryType
        {

            /// <summary>
            /// bool value
            /// </summary>
            BOOLEAN,

            /// <summary>
            /// int value (32-bit, signed)
            /// </summary>
            INTEGER,

            /// <summary>
            /// long integer value (64-bit, signed)
            /// </summary>
            LONG,

            /// <summary>
            /// double value (64-bit)
            /// </summary>
            DOUBLE,

            /// <summary>
            /// string
            /// </summary>
            STRING,

            /// <summary>
            /// Binary value (<c>byte[]</c>)
            /// </summary>
            BINARY,

            /// <summary>
            /// Array of bool values
            /// </summary>
            BOOLEAN_ARRAY,

            /// <summary>
            /// Array of integer values
            /// </summary>
            INTEGER_ARRAY,

            /// <summary>
            /// Array of long integer values
            /// </summary>
            LONG_ARRAY,

            /// <summary>
            /// Array od double values
            /// </summary>
            DOUBLE_ARRAY,

            /// <summary>
            /// Array of strings
            /// </summary>
            STRING_ARRAY,

            /// <summary>
            /// Array of binary values (array of <c>byte[]</c>)
            /// </summary>
            BINARY_ARRAY,

            /// <summary>
            /// Nested <c>Parameters</c> object
            /// </summary>
            NESTED_PARAMETERS,

            /// <summary>
            /// Nested <c>Parameters</c> array.
            /// </summary>
            NESTED_PARAMETERS_ARRAY
        }


        /// <summary>
        /// Class representing a single entry in the collection.  
        /// </summary>
        /// <remarks>
        /// The Entry class allows only inspection of the data 
        /// and is provided for natural iteration usage. The underlying 
        /// data cannot be modified, except for the <c>bool[]</c>, 
        /// <c>int[]</c>, <c>long[]</c>, <c>double[]</c> 
        /// and binary arrays (<c>byte[]</c>), which are kept 
        /// by reference and which can be modified by the user. 
        /// </remarks>
        public class Entry
        {
            internal EntryType type;
            private string name;
            internal object value;

            /// <summary>
            /// Initializes a new instance of <see cref="Entry"/> class
            /// </summary>
            /// <remarks>New instace is always unlocked</remarks>
            /// <param name="type">Type of the entry</param>
            /// <param name="name">Name of the entry</param>
            /// <param name="value">Value of the entry</param>
            public Entry(EntryType type, string name, object value)
            {
                this.type = type;
                this.name = name;
                this.value = value;
            }

            /// <summary>
            /// Gets the type of this entry.  
            /// </summary>
            /// <returns>type of this entry</returns>
            public EntryType Type
            {
                get
                {
                    return type;
                }
            }

            /// <summary>
            /// Gets the name of this entry.  
            /// </summary>
            /// <returns>name of this entry</returns>
            public string Name
            {
                get
                {
                    return name;
                }
            }

            /// <summary>
            /// Gets the <c>bool</c> value from this entry.  
            /// </summary>
            /// <returns>
            /// value of this entry 
            /// </returns>
            /// <exception cref="Inspirel.YAMI.BadTypeException">
            /// if this entry does not contain <c>bool</c> value
            /// </exception>
            public bool GetBoolean()
            {
                return ((bool)extract(EntryType.BOOLEAN));
            }

            /// <summary>
            /// Gets the <c>int</c> value from this entry.  
            /// </summary>
            /// <returns>
            /// value of this entry 
            /// </returns>
            /// <exception cref="Inspirel.YAMI.BadTypeException">
            /// if this entry does not contain <c>int</c> value
            /// </exception>
            public int GetInteger()
            {
                return ((int)extract(EntryType.INTEGER));
            }

            /// <summary>
            /// Gets the <c>long</c> value from this entry.  
            /// </summary>
            /// <returns>
            /// value of this entry 
            /// </returns>
            /// <exception cref="Inspirel.YAMI.BadTypeException">
            /// if this entry does not contain <c>long</c> value
            /// </exception>
            public long GetLong()
            {
                return ((long)extract(EntryType.LONG));
            }

            /// <summary>
            /// Gets the <c>double</c> value from this entry.  
            /// </summary>
            /// <returns>
            /// value of this entry 
            /// </returns>
            /// <exception cref="Inspirel.YAMI.BadTypeException">
            /// if this entry does not contain <c>double</c> value
            /// </exception>
            public double GetDouble()
            {
                return ((double)extract(EntryType.DOUBLE));
            }

            /// <summary>
            /// Gets the <c>string</c> value from this entry.  
            /// </summary>
            /// <returns>
            /// value of this entry 
            /// </returns>
            /// <exception cref="Inspirel.YAMI.BadTypeException">
            /// if this entry does not contain <c>string</c> value
            /// </exception>
            public string GetString()
            {
                return (string)extract(EntryType.STRING);
            }

            /// <summary>
            /// Gets the <c>byte[]</c> value from this entry.  
            /// </summary>
            /// <returns>
            /// value of this entry 
            /// </returns>
            /// <exception cref="Inspirel.YAMI.BadTypeException">
            /// if this entry does not contain <c>byte[]</c> value
            /// </exception>
            public byte[] GetBinary()
            {
                return (byte[])extract(EntryType.BINARY);
            }

            /// <summary>
            /// Gets the <c>bool</c> array from this entry.  
            /// </summary>
            /// <returns>
            /// value of this entry 
            /// </returns>
            /// <exception cref="Inspirel.YAMI.BadTypeException">
            /// if this entry does not contain <c>bool</c> array
            /// </exception>
            public bool[] GetBooleanArray()
            {
                return (bool[])extract(EntryType.BOOLEAN_ARRAY);
            }

            /// <summary>
            /// Gets the <c>int</c> array from this entry.  
            /// </summary>                
            /// <returns>
            /// value of this entry 
            /// </returns>
            /// <exception cref="Inspirel.YAMI.BadTypeException">
            /// if this entry does not contain <c>int</c> array
            /// </exception>
            public int[] GetIntegerArray()
            {
                return (int[])extract(EntryType.INTEGER_ARRAY);
            }

            /// <summary>
            /// Gets the <c>long</c> array from this entry.  
            /// </summary>
            /// <returns>
            /// value of this entry 
            /// </returns>
            /// <exception cref="Inspirel.YAMI.BadTypeException">
            /// if this entry does not contain <c>long</c> array
            /// </exception>
            public long[] GetLongArray()
            {
                return (long[])extract(EntryType.LONG_ARRAY);
            }

            /// <summary>
            /// Gets the <c>double</c> array from this entry.  
            /// </summary>
            /// <returns>
            /// value of this entry 
            /// </returns>
            /// <exception cref="Inspirel.YAMI.BadTypeException">
            /// if this entry does not contain <c>double</c> array
            /// </exception>
            public double[] GetDoubleArray()
            {
                return (double[])extract(EntryType.DOUBLE_ARRAY);
            }

            /// <summary>
            /// Gets the <c>string</c> array from this entry.  
            /// </summary>
            /// <returns>
            /// value of this entry 
            /// </returns>
            /// <exception cref="Inspirel.YAMI.BadTypeException">
            /// if this entry does not contain <c>string</c> array
            /// </exception>
            public string[] GetStringArray()
            {
                return (string[])extract(EntryType.STRING_ARRAY);
            }

            /// <summary>
            /// Gets the <c>byte[]</c> array (that is, array of 
            /// byte arrays) from this entry.  
            /// </summary>
            /// <returns>
            /// value of this entry 
            /// </returns>
            /// <exception cref="Inspirel.YAMI.BadTypeException">
            /// if this entry does not contain <c>byte[]</c> array
            /// </exception>
            public byte[][] GetBinaryArray()
            {
                return (byte[][])extract(EntryType.BINARY_ARRAY);
            }

            /// <summary>
            /// Gets the nested <c>Parameters</c> object from this 
            /// entry.  
            /// </summary>
            /// <returns>
            /// value of this entry 
            /// </returns>
            /// <exception cref="Inspirel.YAMI.BadTypeException">
            /// if this entry does not contain nested 
            /// <c>Parameters</c> object
            /// </exception>
            public Parameters GetNestedParameters()
            {
                return (Parameters)extract(EntryType.NESTED_PARAMETERS);
            }

            /// <summary
            /// Gets the <c>Parameters</c> array
            /// (that is, array of <c>Parameters</c> objects) from this entry.
            /// <returns>
            /// value of this entry
            /// </returns>
            /// <exception cref="Inspirel.YAMI.BadTypeException">
            /// if this entry does not contain <c>Parameters</c> array
            /// </exception>
            public Parameters[] GetNestedArray()
            {
                return (Parameters[])extract(EntryType.NESTED_PARAMETERS_ARRAY);
            }

            private object extract(EntryType expectedType)
            {
                if(type != expectedType)
                {
                    throw new BadTypeException(name,
                        expectedType.ToString());
                }
                return value;
            }
        }

        private readonly List<Entry> entries = new List<Entry>();
        private readonly Dictionary<string, int> entryIndexes
            = new Dictionary<string, int>();

        /// <summary>
        /// Default constructor, creates an empty collection of parameters.
        /// </summary>
        public Parameters()
        {
        }

        /// <summary>
        /// Sets a <c>bool</c> value with the given name.  
        /// </summary>
        /// <param name="name">
        /// name of the new entry or the entry to be replaced
        /// </param>
        /// <param name="value">
        /// value to be set
        /// </param>
        public void SetBoolean(string name, bool value)
        {
            set(name, EntryType.BOOLEAN, value);
        }

        /// <summary>
        /// Gets the <c>bool</c> value from the given entry.  
        /// </summary>
        /// <param name="name">
        /// name of the entry to be inspected 
        /// </param>
        /// <returns>
        /// value from the given entry 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.BadTypeException">
        /// if the given entry does not contain <c>bool</c> value 
        /// </exception>
        /// <exception cref="Inspirel.YAMI.NoSuchNameException">
        /// if the given name is not found
        /// </exception>
        public bool GetBoolean(string name)
        {
            return (bool)extract(name, EntryType.BOOLEAN);
        }

        /// <summary>
        /// Sets an <c>int</c> value with the given name.  
        /// </summary>
        /// <param name="name">
        /// name of the new entry or the entry to be replaced
        /// </param>
        /// <param name="value">
        /// value to be set
        /// </param>
        public void SetInteger(string name, int value)
        {
            set(name, EntryType.INTEGER, value);
        }

        /// <summary>
        /// Gets the <c>int</c> value from the given entry.  
        /// </summary>
        /// <param name="name">
        /// name of the entry to be inspected 
        /// </param>
        /// <returns>
        /// value from the given entry 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.BadTypeException">
        /// if the given entry does not contain <c>int</c> value 
        /// </exception>
        /// <exception cref="Inspirel.YAMI.NoSuchNameException">
        /// if the given name is not found
        /// </exception>
        public int GetInteger(string name)
        {
            return (int)extract(name, EntryType.INTEGER);
        }

        /// <summary>
        /// Sets a <c>long</c> value with the given name.  
        /// </summary>
        /// <param name="name">
        /// name of the new entry or the entry to be replaced
        /// </param>
        /// <param name="value">
        /// value to be set
        /// </param>
        public void SetLong(string name, long value)
        {
            set(name, EntryType.LONG, value);
        }

        /// <summary>
        /// Gets the <c>long</c> value from the given entry.  
        /// </summary>
        /// <param name="name">
        /// name of the entry to be inspected 
        /// </param>
        /// <returns>
        /// value from the given entry 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.BadTypeException">
        /// if the given entry does not contain <c>long</c> value 
        /// </exception>
        /// <exception cref="Inspirel.YAMI.NoSuchNameException">
        /// if the given name is not found
        /// </exception>
        public long GetLong(string name)
        {
            return (long)extract(name, EntryType.LONG);
        }

        /// <summary>
        /// Sets a <c>double</c> value with the given name.  
        /// </summary>
        /// <param name="name">
        /// name of the new entry or the entry to be replaced
        /// </param>
        /// <param name="value">
        /// value to be set
        /// </param>
        public void SetDouble(string name, double value)
        {
            set(name, EntryType.DOUBLE, value);
        }

        /// <summary>
        /// Gets the <c>double</c> value from the given entry.  
        /// </summary>
        /// <param name="name">
        /// name of the entry to be inspected 
        /// </param>
        /// <returns>
        /// value from the given entry 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.BadTypeException">
        /// if the given entry does not contain <c>double</c> value 
        /// </exception>
        /// <exception cref="Inspirel.YAMI.NoSuchNameException">
        /// if the given name is not found
        /// </exception>
        public double GetDouble(string name)
        {
            return (double)extract(name, EntryType.DOUBLE);
        }

        /// <summary>
        /// Sets a <c>string</c> value with the given name.  
        /// </summary>
        /// <param name="name">
        /// name of the new entry or the entry to be replaced
        /// </param>
        /// <param name="value">
        /// value to be set
        /// </param>
        public void SetString(string name, string value)
        {
            set(name, EntryType.STRING, value);
        }

        /// <summary>
        /// Gets the <c>string</c> value from the given entry.  
        /// </summary>
        /// <param name="name">
        /// name of the entry to be inspected 
        /// </param>
        /// <returns>
        /// value from the given entry 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.BadTypeException">
        /// if the given entry does not contain <c>string</c> value 
        /// </exception>
        /// <exception cref="Inspirel.YAMI.NoSuchNameException">
        /// if the given name is not found
        /// </exception>
        public string GetString(string name)
        {
            return (string)extract(name, EntryType.STRING);
        }

        /// <summary>
        /// Sets a <c>byte[]</c> value with the given name.  
        /// </summary>
        /// <param name="name">
        /// name of the new entry or the entry to be replaced
        /// </param>
        /// <param name="value">
        /// value to be set
        /// </param>
        public void SetBinary(string name, byte[] value)
        {
            set(name, EntryType.BINARY, value);
        }

        /// <summary>
        /// Gets the <c>byte[]</c> value from the given entry.  
        /// </summary>
        /// <param name="name">
        /// name of the entry to be inspected 
        /// </param>
        /// <returns>
        /// value from the given entry 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.BadTypeException">
        /// if the given entry does not contain <c>byte[]</c> value 
        /// </exception>
        /// <exception cref="Inspirel.YAMI.NoSuchNameException">
        /// if the given name is not found
        /// </exception>
        public byte[] GetBinary(string name)
        {
            return (byte[])extract(name, EntryType.BINARY);
        }

        /// <summary>
        /// Sets the <c>bool</c> array with the given name.  
        /// </summary>
        /// <remarks>
        /// The array is <i>not</i> copied internally. 
        /// </remarks>
        /// <param name="name">
        /// name of the new entry or the entry to be replaced
        /// </param>
        /// <param name="array">
        /// value to be set
        /// </param>
        public void SetBooleanArray(string name, bool[] array)
        {
            set(name, EntryType.BOOLEAN_ARRAY, array);
        }

        /// <summary>
        /// Gets the <c>bool</c> array from the given entry.  
        /// </summary>
        /// <param name="name">
        /// name of the entry to be inspected 
        /// </param>
        /// <returns>
        /// array from the given entry 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.BadTypeException">
        /// if the given entry does not contain <c>bool</c> array 
        /// </exception>
        /// <exception cref="Inspirel.YAMI.NoSuchNameException">
        /// if the given name is not found
        /// </exception>
        public bool[] GetBooleanArray(string name)
        {
            return (bool[])extract(name, EntryType.BOOLEAN_ARRAY);
        }

        /// <summary>
        /// Sets the <c>int</c> array with the given name.  
        /// </summary>
        /// <remarks>
        /// The array is <i>not</i> copied internally. 
        /// </remarks>
        /// <param name="name">
        /// name of the new entry or the entry to be replaced
        /// </param>
        /// <param name="array">
        /// value to be set
        /// </param>
        public void SetIntegerArray(string name, int[] array)
        {
            set(name, EntryType.INTEGER_ARRAY, array);
        }

        /// <summary>
        /// Gets the <c>int</c> array from the given entry.  
        /// </summary>
        /// <param name="name">
        /// name of the entry to be inspected 
        /// </param>
        /// <returns>
        /// array from the given entry 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.BadTypeException">
        /// if the given entry does not contain <c>int</c> array 
        /// </exception>
        /// <exception cref="Inspirel.YAMI.NoSuchNameException">
        /// if the given name is not found
        /// </exception>
        public int[] GetIntegerArray(string name)
        {
            return (int[])extract(name, EntryType.INTEGER_ARRAY);
        }

        /// <summary>
        /// Sets the <c>long</c> array with the given name.  
        /// </summary>
        /// <remarks>
        /// The array is <i>not</i> copied internally. 
        /// </remarks>
        /// <param name="name">
        /// name of the new entry or the entry to be replaced
        /// </param>
        /// <param name="array">
        /// value to be set
        /// </param>
        public void SetLongArray(string name, long[] array)
        {
            set(name, EntryType.LONG_ARRAY, array);
        }

        /// <summary>
        /// Gets the <c>long</c> array from the given entry.  
        /// </summary>
        /// <param name="name">
        /// name of the entry to be inspected 
        /// </param>
        /// <returns>
        /// array from the given entry 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.BadTypeException">
        /// if the given entry does not contain <c>long</c> array 
        /// </exception>
        /// <exception cref="Inspirel.YAMI.NoSuchNameException">
        /// if the given name is not found
        /// </exception>
        public long[] GetLongArray(string name)
        {
            return (long[])extract(name, EntryType.LONG_ARRAY);
        }

        /// <summary>
        /// Sets the <c>double</c> array with the given name.  
        /// </summary>
        /// <remarks>
        /// The array is <i>not</i> copied internally. 
        /// </remarks>
        /// <param name="name">
        /// name of the new entry or the entry to be replaced
        /// </param>
        /// <param name="array">
        /// value to be set
        /// </param>
        public void SetDoubleArray(string name, double[] array)
        {
            set(name, EntryType.DOUBLE_ARRAY, array);
        }

        /// <summary>
        /// Gets the <c>double</c> array from the given entry.  
        /// </summary>
        /// <param name="name">
        /// name of the entry to be inspected 
        /// </param>
        /// <returns>
        /// array from the given entry 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.BadTypeException">
        /// if the given entry does not contain <c>long</c> array 
        /// </exception>
        /// <exception cref="Inspirel.YAMI.NoSuchNameException">
        /// if the given name is not found
        /// </exception>
        public double[] GetDoubleArray(string name)
        {
            return (double[])extract(name, EntryType.DOUBLE_ARRAY);
        }

        /// <summary>
        /// Sets the <c>string</c> array with the given name.  
        /// </summary>
        /// <remarks>
        /// The array is <i>not</i> copied internally. 
        /// </remarks>
        /// <param name="name">
        /// name of the new entry or the entry to be replaced
        /// </param>
        /// <param name="array">
        /// value to be set
        /// </param>
        public void SetStringArray(string name, string[] array)
        {
            set(name, EntryType.STRING_ARRAY, array);
        }

        /// <summary>
        /// Gets the <c>string</c> array from the given entry.  
        /// </summary>
        /// name of the entry to be inspected 
        /// <returns>
        /// array from the given entry 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.BadTypeException">
        /// if the given entry does not contain <c>string</c> 
        /// array 
        /// </exception>
        /// <exception cref="Inspirel.YAMI.NoSuchNameException">
        /// if the given name is not found
        /// </exception>
        public string[] GetStringArray(string name)
        {
            return (string[])extract(name, EntryType.STRING_ARRAY);
        }

        /// <summary>
        /// Sets the <c>byte[]</c> array (array of byte arrays)
        /// with the given name.  
        /// </summary>
        /// <remarks>
        /// The arrays are <i>not</i> copied internally. 
        /// </remarks>
        /// <param name="name">
        /// name of the new entry or the entry to be replaced
        /// </param>
        /// <param name="array">
        /// value to be set
        /// </param>
        public void SetBinaryArray(string name, byte[][] array)
        {
            set(name, EntryType.BINARY_ARRAY, array);
        }

        /// <summary>
        /// Gets the array of <c>byte[]</c> buffers from the 
        /// given entry.  
        /// </summary>
        /// <param name="name">
        /// name of the entry to be inspected 
        /// </param>
        /// <returns>
        /// array from the given entry 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.BadTypeException">
        /// if the given entry does not contain <c>byte[]</c> 
        /// array 
        /// </exception>
        /// <exception cref="Inspirel.YAMI.NoSuchNameException">
        /// if the given name is not found
        /// </exception>
        public byte[][] GetBinaryArray(string name)
        {
            return (byte[][])extract(name, EntryType.BINARY_ARRAY);
        }

        /// <summary>
        /// Sets the nested <c>Parameters</c> object with the given name.
        /// </summary>
        /// <remarks>
        /// The nested object is not cloned internally. 
        /// </remarks> 
        /// <param name="name">
        /// name of the new entry or the entry to be replaced
        /// </param>
        /// <param name="nested">
        /// value to be set
        /// </param>
        public void SetNestedParameters(string name, Parameters nested)
        {
            set(name, EntryType.NESTED_PARAMETERS, nested);
        }

        /// <summary>
        /// Gets the nested <c>Parameters</c> object from the given 
        /// entry.  
        /// </summary>
        /// <param name="name">
        /// name of the entry to be inspected 
        /// </param>
        /// <returns>
        /// nested object from the given entry 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.BadTypeException">
        /// if the given entry does not contain nested 
        /// <c>Parameters</c> object 
        /// </exception>
        /// <exception cref="Inspirel.YAMI.NoSuchNameException">
        /// if the given name is not found
        /// </exception>
        public Parameters GetNestedParameters(string name)
        {
            return (Parameters)extract(name, EntryType.NESTED_PARAMETERS);
        }

        /// <summary>
        /// Sets the nested <c>Parameters</c> array with the given name.
        /// </summary>
        ///        
        /// <para>
        /// <b>Note:</b> the nested objects are not cloned internally.
        /// </para>
        /// <param name="name">
        /// name of the new entry or the entry to be replaced
        /// </param>
        /// <param name="nested">
        /// nested array of objects to nest
        /// </param>
        public void SetNestedArray(string name, Parameters[] nested)
        {
            set(name, EntryType.NESTED_PARAMETERS_ARRAY, nested);
        }

        /// <summary>
        /// Gets the nested <c>Parameters</c> array from the given entry.
        /// </summary>
        /// <param name="name">
        /// name name of the entry to be inspected
        /// </param>
        /// <returns>
        /// nested array of objects from the given entry
        /// </returns>
        /// <exception cref="Inspirel.YAMI.BadTypeException">
        /// if the given entry does not contain nested <c>Parameters</c> 
        /// array
        /// </exception>
        /// <exception cref="Inspirel.YAMI.NoSuchNameException">
        /// if the given name is not found
        /// </exception>
        public Parameters[] GetNestedArray(string name)
        {
            return (Parameters[])extract(name, EntryType.NESTED_PARAMETERS_ARRAY);
        }

        /// <summary>
        /// Removes the given entry from the collection.  
        /// </summary>
        /// <remarks>The removed entry leaves a <i>hole</i> 
        /// (empty slot) in the collection that can be reused by newly 
        /// added entries.
        /// </remarks>
        /// <param name="name">
        /// the name of the entry to be removed
        /// </param>
        public void Remove(string name)
        {
            if(!entryIndexes.ContainsKey(name))
                throw new NoSuchNameException(name);

            int index = entryIndexes[name];

            Entry e = entries[index];

            entryIndexes.Remove(name);
            entries[index] = null;
        }

        /// <summary>
        /// Removes all entries from the collection.
        /// </summary>
        public void Clear()
        {
            entries.Clear();
            entryIndexes.Clear();
        }

        /// <summary>
        /// Gets the current size of the collection.  
        /// </summary>
        /// <value>
        /// size of the collection
        /// </value>
        public int Count
        {
            get
            {
                return entryIndexes.Count;
            }
        }

        /// <summary>
        /// Gets the type of the given entry.  
        /// </summary>
        /// name of the given entry 
        /// <returns>
        /// the type of the entry 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.NoSuchNameException">
        /// if the given name is not found
        /// </exception>
        public EntryType GetType(string name)
        {
            Entry e = findExisting(name);
            return e.Type;
        }

        /// <summary>
        /// Finds the entry by name.  
        /// </summary>
        /// <param name="name">
        /// the name of entry 
        /// </param>
        /// <returns>
        /// <c>Entry</c> or <c>null</c> if the given name 
        /// was not found
        /// </returns>
        public Entry Find(string name)
        {
            return entryIndexes.ContainsKey(name) ?
                entries[entryIndexes[name]] : null;
        }

        /// <summary>
        /// Serializes the content of the whole collection.  
        /// <para> The content is serialized into chunks of the given 
        /// size, after serialization only the last chunk can be 
        /// smaller than the requested size. </para>  
        /// </summary>
        /// <param name="chunkSize">
        /// chunk size, should be a multiple of 4 or 
        /// <c>int.MAX_VALUE</c> 
        /// </param>
        /// <returns>
        /// list of buffers (chunks) with serialized data 
        /// </returns>
        /// <exception cref="System.ArgumentException">
        /// if requested chunk size is not a multiple of 4 and is not 
        /// <c>int.MAX_VALUE</c></exception>
        public List<byte[]> Serialize(int chunkSize)
        {
            return Serialization.Serialize(this, chunkSize);
        }

        /// <summary>
        /// Deserializes from the given list of buffers.    
        /// </summary>
        /// <remarks>
        /// The current content of this object is not 
        /// cleared before attempting deserialization and each 
        /// retrieved data element is <i>merged</i> into the current 
        /// content as if done by individual calls to appropriate 
        /// <c>setXYZ</c> functions.<br /> In most cases 
        /// deserialization will be performed to the empty 
        /// <c>Parameters</c> object (to reconstruct it to the 
        /// form that was used for serialization), but 
        /// deserialization onto non-empty object might be occasionally 
        /// useful as a way of merging two collections.
        /// each buffer should have a size that is multiple of 4
        /// </remarks>
        /// <exception cref="System.ArgumentException">
        /// if any of the given buffers has a size that is not 
        /// a multiple of 4 or when the given data cannot be properly 
        /// deserialized
        /// </exception>
        public void Deserialize(IList<byte[]> buffers)
        {
            Serialization.Deserialize(this, buffers);
        }

        /// <summary>
        /// Returns a string representation of the content.  
        /// <para> This function is supposed to be used by unit tests 
        /// and for simple debugging. </para>  
        /// </summary>
        /// <returns>
        /// <c>string</c> representation of the collection
        /// </returns>
        public override string ToString()
        {
            System.Globalization.CultureInfo threadCulture =
                System.Threading.Thread.CurrentThread.CurrentCulture;

            System.Threading.Thread.CurrentThread.CurrentCulture =
                System.Globalization.CultureInfo.InvariantCulture;

            StringBuilder builder = new StringBuilder();
            try
            {
                buildString(builder, "");
            }
            finally
            {
                System.Threading.Thread.CurrentThread.CurrentCulture =
                    threadCulture;
            }
            return builder.ToString();
        }

        private void buildString(StringBuilder builder, string prefix)
        {
            for(int i = 0; i != entries.Count; ++i)
            {
                builder.Append(prefix + "entry " + i + ":\n");
                bool addNewline = true;
                Entry e = entries[i];
                if(e == null)
                {
                    builder.Append(prefix + "unused");
                }
                else
                {
                    builder.Append(prefix + "name: " + e.Name + '\n');
                    switch(e.Type)
                    {
                    case EntryType.BOOLEAN:
                        {
                            bool value = e.GetBoolean();
                            builder.Append(prefix + "boolean: " +
                                    (value ? "1" : "0"));
                        }
                        break;
                    case EntryType.INTEGER:
                        {
                            int value = e.GetInteger();
                            builder.Append(prefix + "integer: " +
                                    value);
                        }
                        break;
                    case EntryType.LONG:
                        {
                            long value = e.GetLong();
                            builder.Append(prefix + "long: " +
                                    value);
                        }
                        break;
                    case EntryType.DOUBLE:
                        {
                            double value = e.GetDouble();
                            builder.Append(prefix + "double: " +
                                    value);
                        }
                        break;
                    case EntryType.STRING:
                        {
                            string value = e.GetString();
                            builder.Append(prefix + "string: " + value);
                        }
                        break;
                    case EntryType.BINARY:
                        {
                            byte[] value = e.GetBinary();
                            builder.Append(prefix + "binary of length "
                                    + value.Length);
                        }
                        break;
                    case EntryType.BOOLEAN_ARRAY:
                        {
                            builder.Append(prefix + "boolean array:");
                            bool[] array = e.GetBooleanArray();
                            for(int j = 0; j != array.Length; ++j)
                            {
                                builder.Append(" " +
                                        (array[j] ? "1" : "0"));
                            }
                        }
                        break;
                    case EntryType.INTEGER_ARRAY:
                        {
                            builder.Append(prefix + "integer array:");
                            int[] array = e.GetIntegerArray();
                            for(int j = 0; j != array.Length; ++j)
                            {
                                builder.Append(" " + array[j]);
                            }
                        }
                        break;
                    case EntryType.LONG_ARRAY:
                        {
                            builder.Append(prefix + "long array:");
                            long[] array = e.GetLongArray();
                            for(int j = 0; j != array.Length; ++j)
                            {
                                builder.Append(" " + array[j]);
                            }
                        }
                        break;
                    case EntryType.DOUBLE_ARRAY:
                        {
                            builder.Append(prefix + "double array:");
                            double[] array = e.GetDoubleArray();
                            for(int j = 0; j != array.Length; ++j)
                            {
                                builder.Append(" " + array[j]);
                            }
                        }
                        break;
                    case EntryType.STRING_ARRAY:
                        {
                            builder.Append(prefix + "string array:");
                            string[] array = e.GetStringArray();
                            for(int j = 0; j != array.Length; ++j)
                            {
                                builder.Append(" " + array[j]);
                            }
                        }
                        break;
                    case EntryType.BINARY_ARRAY:
                        {
                            byte[][] array = e.GetBinaryArray();
                            builder.Append(prefix
                                + "binary array of length "
                                + array.Length);
                        }
                        break;
                    case EntryType.NESTED_PARAMETERS:
                        {
                            builder.Append(prefix
                                + "nested parameters:\n");
                            Parameters nested = e.GetNestedParameters();
                            nested.buildString(builder, prefix + "  ");
                            addNewline = false;
                        }
                        break;
                    case EntryType.NESTED_PARAMETERS_ARRAY:
                        {
                            Parameters[] array = (Parameters[])e.value;
                            builder.Append(prefix + 
                                "nested parameters array of length " +
                                array.Length + ":\n");
                            for(int j = 0; j != array.Length; ++j)
                            {
                                builder.Append(
                                    "  nested at index " + j + ":\n");
                                array[j].buildString(
                                    builder, prefix + "    ");
                            }
                            addNewline = false;
                        }
                        break;

                    }
                }

                if(addNewline)
                {
                    builder.Append("\n");
                }
            }
        }

        private void set(string name, EntryType type, object value)
        {
            if(entryIndexes.ContainsKey(name))
            {
                // replace existing entry
                Entry e = entries[entryIndexes[name]];

                e.type = type;
                e.value = value;
            }
            else
            {
                // create new entry
                Entry e = new Entry(type, name, value);
                int newIndex = findEmptySlot();
                entries[newIndex] = e;
                entryIndexes[name] = newIndex;
            }
        }

        private object extract(string name, EntryType expectedType)
        {
            Entry e = findExisting(name);
            if(e.Type != expectedType)
            {
                throw new BadTypeException(name,
                    expectedType.ToString());
            }
            return e.value;
        }

        private Entry findExisting(string name)
        {
            Entry e = Find(name);
            if(e != null)
                return e;
            throw new NoSuchNameException(name);
        }

        private int findEmptySlot()
        {
            int i;
            for(i = 0; i != entries.Count; ++i)
            {
                if(entries[i] == null)
                {
                    break;
                }
            }

            if(i == entries.Count)
            {
                // the list is empty or does not contain any empty entry
                // -> expand the list to make room for the new entry
                entries.Add(null);
            }

            return i;
        }



        #region IEnumerable<Entry> Members

        /// <summary>
        /// Returns an enumerator that can iterate through a collection.
        /// </summary>
        /// <returns>strong typed 
        /// <see cref="System.Collections.Generic.IEnumerator{T}"/>
        /// that can be used to
        /// iterate trough <see cref="Parameters.Entry"/>collection</returns>
        public IEnumerator<Parameters.Entry> GetEnumerator()
        {
            foreach(Entry e in entries)
                if(e != null)
                    yield return e;
        }

        #endregion

        #region IEnumerable Members

        /// <summary>
        /// Returns an enumerator that can iterate through a collection.
        /// </summary>
        /// <returns><see cref="System.Collections.IEnumerator"/>
        /// that can be used to
        /// iterate trough <see cref="Parameters.Entry"/>collection</returns>
        System.Collections.IEnumerator
            System.Collections.IEnumerable.GetEnumerator()
        {
            foreach(Entry e in entries)
                if(e != null)
                    yield return e;
        }

        #endregion
    }
}
