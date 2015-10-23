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

import com.inspirel.yami.details.Serialization;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

/**
 * Collection of message parameters.
 *
 * <p>
 * A collection of message parameters is a list of typed {name, value} pairs.
 * <br />Each entry in this collection has a unique name and can have
 * one of the following types:
 * </p>
 * <ul>
 *  <li><code>boolean</code> or <code>boolean</code> array</li>
 *  <li><code>int</code> or <code>int</code> array</li>
 *  <li><code>long</code> or <code>long</code> array</li>
 *  <li><code>double</code> or <code>double</code> array</li>
 *  <li><code>String</code> or <code>String</code> array</li>
 *  <li><code>byte[]</code> buffers or their arrays</li>
 *  <li>nested <code>Parameters</code> objects or their arrays,
 *      which provides its own scope for naming.</li>
 * </ul>
 *
 * <p>
 * The names of entries are searched for using
 * case-sensitive comparisons.
 * </p>
 *
 * <p>
 * <b>Note:</b>
 * The instances of this class should not be used from multiple threads
 * without synchronization;
 * it is safe to use separate instances in separate threads.
 * </p>
 * <p>
 * <b>Note:</b>
 * The entries are <i>ordered</i> - the order in which they are created
 * influences the final serialized form of the message payload.<br />
 * Newly created entries are appended to the end of the collection unless
 * there is an existing empty slot that can be reused - the appropriate
 * slot is searched for from the beginning to the end of the collection
 * and if no free slot is found the collection is extended at the end.<br />
 * The above guarantee concerns the user code that relies on
 * predictable serialization.
 * </p>
 */
public final class Parameters
    implements YAMISerializable, Iterable<Parameters.Entry> {

    /**
     * Enumeration defining all possible entry types.
     */
    public enum EntryType {

        /**
         * Boolean value
         */
        BOOLEAN,
        /**
         * Integer value (32-bit, signed)
         */
        INTEGER,
        /**
         * Long integer value (64-bit, signed)
         */
        LONG,
        /**
         * Double value (64-bit)
         */
        DOUBLE,
        /**
         * String
         */
        STRING,
        /**
         * Binary value (<code>byte[]</code>)
         */
        BINARY,
        /**
         * Array of boolean values
         */
        BOOLEAN_ARRAY,
        /**
         * Array of integer values
         */
        INTEGER_ARRAY,
        /**
         * Array of long integer values
         */
        LONG_ARRAY,
        /**
         * Array od double values
         */
        DOUBLE_ARRAY,
        /**
         * Array of strings
         */
        STRING_ARRAY,
        /**
         * Array of binary values (array of <code>byte[]</code>)
         */
        BINARY_ARRAY,
        /**
         * Nested <code>Parameters</code> object
         */
        NESTED_PARAMETERS,
        /**
         * Nested <code>Parameters</code> array.
         */
        NESTED_PARAMETERS_ARRAY
    }

    /**
     * Class representing a single entry in the collection.
     * 
     * <p>
     * The Entry class allows only inspection of the data
     * and is provided for natural iteration usage.
     * The underlying data cannot be modified, except for the
     * <code>boolean[]</code>, <code>int[]</code>, <code>long[]</code>,
     * <code>double[]</code> and binary arrays (<code>byte[]</code>),
     * which are kept by reference and which can be modified by the user.
     * </p>
     */
    public static class Entry {

        EntryType type;
        String name;
        Object value;

        Entry(EntryType type, String name, Object value) {
            this.type = type;
            this.name = name;
            this.value = value;
        }

        /**
         * Gets the type of this entry.
         * 
         * @return type of this entry
         */
        public EntryType type() {
            return type;
        }

        /**
         * Gets the name of this entry.
         * 
         * @return name of this entry
         */
        public String name() {
            return name;
        }

        /**
         * Gets the <code>boolean</code> value from this entry.
         * 
         * @return value of this entry
         * @throws BadTypeException
         *         if this entry does not contain <code>boolean</code> value
         */
        public boolean getBoolean() {
            return ((Boolean) extract(EntryType.BOOLEAN)).booleanValue();
        }

        /**
         * Gets the <code>int</code> value from this entry.
         * 
         * @return value of this entry
         * @throws BadTypeException
         *         if this entry does not contain <code>int</code> value
         */
        public int getInteger() {
            return ((Integer) extract(EntryType.INTEGER)).intValue();
        }

        /**
         * Gets the <code>long</code> value from this entry.
         * 
         * @return value of this entry
         * @throws BadTypeException
         *         if this entry does not contain <code>long</code> value
         */
        public long getLong() {
            return ((Long) extract(EntryType.LONG)).longValue();
        }

        /**
         * Gets the <code>double</code> value from this entry.
         * 
         * @return value of this entry
         * @throws BadTypeException
         *         if this entry does not contain <code>double</code> value
         */
        public double getDouble() {
            return ((Double) extract(EntryType.DOUBLE)).doubleValue();
        }

        /**
         * Gets the <code>String</code> value from this entry.
         * 
         * @return value of this entry
         * @throws BadTypeException
         *         if this entry does not contain <code>String</code> value
         */
        public String getString() {
            return (String) extract(EntryType.STRING);
        }

        /**
         * Gets the <code>byte[]</code> value from this entry.
         * 
         * @return value of this entry
         * @throws BadTypeException
         *         if this entry does not contain <code>byte[]</code> value
         */
        public byte[] getBinary() {
            return (byte[]) extract(EntryType.BINARY);
        }

        /**
         * Gets the <code>boolean</code> array from this entry.
         * 
         * @return value of this entry
         * @throws BadTypeException
         *         if this entry does not contain <code>boolean</code> array
         */
        public boolean[] getBooleanArray() {
            return (boolean[]) extract(EntryType.BOOLEAN_ARRAY);
        }

        /**
         * Gets the <code>int</code> array from this entry.
         * 
         * @return value of this entry
         * @throws BadTypeException
         *         if this entry does not contain <code>int</code> array
         */
        public int[] getIntegerArray() {
            return (int[]) extract(EntryType.INTEGER_ARRAY);
        }

        /**
         * Gets the <code>long</code> array from this entry.
         * 
         * @return value of this entry
         * @throws BadTypeException
         *         if this entry does not contain <code>long</code> array
         */
        public long[] getLongArray() {
            return (long[]) extract(EntryType.LONG_ARRAY);
        }

        /**
         * Gets the <code>double</code> array from this entry.
         * 
         * @return value of this entry
         * @throws BadTypeException
         *         if this entry does not contain <code>double</code> array
         */
        public double[] getDoubleArray() {
            return (double[]) extract(EntryType.DOUBLE_ARRAY);
        }

        /**
         * Gets the <code>String</code> array from this entry.
         * 
         * @return value of this entry
         * @throws BadTypeException
         *         if this entry does not contain <code>String</code> array
         */
        public String[] getStringArray() {
            return (String[]) extract(EntryType.STRING_ARRAY);
        }

        /**
         * Gets the <code>byte[]</code> array
         * (that is, array of byte arrays) from this entry.
         * 
         * @return value of this entry
         * @throws BadTypeException
         *         if this entry does not contain <code>byte[]</code> array
         */
        public byte[][] getBinaryArray() {
            return (byte[][]) extract(EntryType.BINARY_ARRAY);
        }

        /**
         * Gets the nested <code>Parameters</code> object from this entry.
         * 
         * @return nested object
         * @throws BadTypeException
         *         if this entry does not contain
         *         nested <code>Parameters</code> object
         */
        public Parameters getNestedParameters() {
            return (Parameters) extract(EntryType.NESTED_PARAMETERS);
        }

        /**
         * Gets the <code>Parameters</code> array
         * (that is, array of <code>Parameters</code> objects) from this entry.
         * 
         * @return value of this entry
         * @throws BadTypeException
         *         if this entry does not contain <code>Parameters</code> array
         */
        public Parameters[] getNestedArray() {
            return (Parameters[]) extract(EntryType.NESTED_PARAMETERS_ARRAY);
        }

        private Object extract(EntryType expectedType) {
            if (type != expectedType) {
                throw new BadTypeException(name, expectedType.toString());
            }
            return value;
        }
    }

    /**
     * Iterator class for visiting all entries in the collection.
     */
    public static class EntryIterator implements Iterator<Entry> {

        private final List<Entry> entries;
        private int nextIndex;

        EntryIterator(List<Entry> entries) {
            this.entries = entries;

            // find the first used entry in the list
            // and use it as the "next" item to visit
            // if the list is empty or contains no used slots
            // then this iterator will appear to be empty as well
            // due to nextIndex == entries.size()
            for (nextIndex = 0; nextIndex != entries.size(); ++nextIndex) {
                if (entries.get(nextIndex) != null) {
                    break;
                }
            }
        }

        /**
         * Returns <code>true</code> if the iteration has more entries.
         * (In other words, returns <code>true</code> if <code>next</code>
         * would return a valid <code>Parameters.Entry</code> object
         * rather than throwing an exception.)
         * 
         * @return <code>true</code> if the iteration has more entries
         */
        @Override
        public boolean hasNext() {
            // the "next" element exists if there is at least
            // one used slot in the list of entries
            int i;
            for (i = nextIndex; i != entries.size(); ++i) {
                if (entries.get(i) != null) {
                    break;
                }
            }
            return nextIndex != entries.size();
        }

        /**
         * Returns the next <code>Parameters.Entry</code> in the iteration.
         * 
         * @return the next entry in the iteration
         * @throws NoSuchElementException - iteration has no more entries
         */
        @Override
        public Entry next() {
            if (hasNext()) {
                Entry e = entries.get(nextIndex++);

                // skip unused slots
                for (; nextIndex != entries.size(); ++nextIndex) {
                    if (entries.get(nextIndex) != null) {
                        break;
                    }
                }

                return e;
            }

            throw new NoSuchElementException();
        }

        /**
         * This operation is not supported.
         * 
         * @throws UnsupportedOperationException
         */
        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }
    }
    
    private final List<Entry> entries;
    private final Map<String, Integer> entryIndexes;

    /**
     * Default constructor, creates an empty collection of parameters.
     */
    public Parameters() {
        entries = new ArrayList<Entry>();
        entryIndexes = new HashMap<String, Integer>();
    }

    /**
     * Sets a <code>boolean</code> value with the given name.
     * 
     * @param name name of the new entry or the entry to be replaced
     * @param value value to be set
     */
    public void setBoolean(String name, boolean value) {
        set(name, EntryType.BOOLEAN, Boolean.valueOf(value));
    }

    /**
     * Gets the <code>boolean</code> value from the given entry.
     * 
     * @param name name of the entry to be inspected
     * @return value from the given entry
     * @throws BadTypeException
     *         if the given entry does not contain <code>boolean</code> value
     * @throws NoSuchNameException
     *         if the given name is not found
     */
    public boolean getBoolean(String name) {
        return ((Boolean) extract(name, EntryType.BOOLEAN)).booleanValue();
    }

    /**
     * Sets an <code>int</code> value with the given name.
     * 
     * @param name name of the new entry or the entry to be replaced
     * @param value value to be set
     */
    public void setInteger(String name, int value) {
        set(name, EntryType.INTEGER, Integer.valueOf(value));
    }

    /**
     * Gets the <code>int</code> value from the given entry.
     * 
     * @param name name of the entry to be inspected
     * @return value from the given entry
     * @throws BadTypeException
     *         if the given entry does not contain <code>int</code> value
     * @throws NoSuchNameException
     *         if the given name is not found
     */
    public int getInteger(String name) {
        return ((Integer) extract(name, EntryType.INTEGER)).intValue();
    }

    /**
     * Sets a <code>long</code> value with the given name.
     * 
     * @param name name of the new entry or the entry to be replaced
     * @param value value to be set
     */
    public void setLong(String name, long value) {
        set(name, EntryType.LONG, Long.valueOf(value));
    }

    /**
     * Gets the <code>long</code> value from the given entry.
     * 
     * @param name name of the entry to be inspected
     * @return value from the given entry
     * @throws BadTypeException
     *         if the given entry does not contain <code>long</code> value
     * @throws NoSuchNameException
     *         if the given name is not found
     */
    public long getLong(String name) {
        return ((Long) extract(name, EntryType.LONG)).longValue();
    }

    /**
     * Sets a <code>double</code> value with the given name.
     * 
     * @param name name of the new entry or the entry to be replaced
     * @param value value to be set
     */
    public void setDouble(String name, double value) {
        set(name, EntryType.DOUBLE, new Double(value));
    }

    /**
     * Gets the <code>double</code> value from the given entry.
     * 
     * @param name name of the entry to be inspected
     * @return value from the given entry
     * @throws BadTypeException
     *         if the given entry does not contain <code>double</code> value
     * @throws NoSuchNameException
     *         if the given name is not found
     */
    public double getDouble(String name) {
        return ((Double) extract(name, EntryType.DOUBLE)).doubleValue();
    }

    /**
     * Sets a <code>String</code> value with the given name.
     * 
     * @param name name of the new entry or the entry to be replaced
     * @param value value to be set
     */
    public void setString(String name, String value) {
        set(name, EntryType.STRING, value);
    }

    /**
     * Gets the <code>String</code> value from the given entry.
     * 
     * @param name name of the entry to be inspected
     * @return value from the given entry
     * @throws BadTypeException
     *         if the given entry does not contain <code>String</code> value
     * @throws NoSuchNameException
     *         if the given name is not found
     */
    public String getString(String name) {
        return (String) extract(name, EntryType.STRING);
    }

    /**
     * Sets a <code>byte[]</code> value with the given name.
     * 
     * @param name name of the new entry or the entry to be replaced
     * @param value value to be set
     */
    public void setBinary(String name, byte[] value) {
        set(name, EntryType.BINARY, value);
    }

    /**
     * Gets the <code>byte[]</code> value from the given entry.
     * 
     * @param name name of the entry to be inspected
     * @return value from the given entry
     * @throws BadTypeException
     *         if the given entry does not contain <code>byte[]</code> value
     * @throws NoSuchNameException
     *         if the given name is not found
     */
    public byte[] getBinary(String name) {
        return (byte[]) extract(name, EntryType.BINARY);
    }

    /**
     * Sets the <code>boolean</code> array with the given name.
     * 
     * <p>
     * <b>Note:</b> the array is <i>not</i> copied internally.
     * </p>
     * 
     * @param name name of the new entry or the entry to be replaced
     * @param array value to be set
     */
    public void setBooleanArray(String name, boolean[] array) {
        set(name, EntryType.BOOLEAN_ARRAY, array);
    }

    /**
     * Gets the <code>boolean</code> array from the given entry.
     * 
     * @param name name of the entry to be inspected
     * @return array from the given entry
     * @throws BadTypeException
     *         if the given entry does not contain <code>boolean</code> array
     * @throws NoSuchNameException
     *         if the given name is not found
     */
    public boolean[] getBooleanArray(String name) {
        return (boolean[]) extract(name, EntryType.BOOLEAN_ARRAY);
    }

    /**
     * Sets the <code>int</code> array with the given name.
     * 
     * <p>
     * <b>Note:</b> the array is <i>not</i> copied internally.
     * </p>
     * 
     * @param name name of the new entry or the entry to be replaced
     * @param array value to be set
     */
    public void setIntegerArray(String name, int[] array) {
        set(name, EntryType.INTEGER_ARRAY, array);
    }

    /**
     * Gets the <code>int</code> array from the given entry.
     * 
     * @param name name of the entry to be inspected
     * @return array from the given entry
     * @throws BadTypeException
     *         if the given entry does not contain <code>int</code> array
     * @throws NoSuchNameException
     *         if the given name is not found
     */
    public int[] getIntegerArray(String name) {
        return (int[]) extract(name, EntryType.INTEGER_ARRAY);
    }

    /**
     * Sets the <code>long</code> array with the given name.
     * 
     * <p>
     * <b>Note:</b> the array is <i>not</i> copied internally.
     * </p>
     * 
     * @param name name of the new entry or the entry to be replaced
     * @param array value to be set
     */
    public void setLongArray(String name, long[] array) {
        set(name, EntryType.LONG_ARRAY, array);
    }

    /**
     * Gets the <code>long</code> array from the given entry.
     * 
     * @param name name of the entry to be inspected
     * @return array from the given entry
     * @throws BadTypeException
     *         if the given entry does not contain <code>long</code> array
     * @throws NoSuchNameException
     *         if the given name is not found
     */
    public long[] getLongArray(String name) {
        return (long[]) extract(name, EntryType.LONG_ARRAY);
    }

    /**
     * Sets the <code>double</code> array with the given name.
     * 
     * <p>
     * <b>Note:</b> the array is <i>not</i> copied internally.
     * </p>
     * 
     * @param name name of the new entry or the entry to be replaced
     * @param array value to be set
     */
    public void setDoubleArray(String name, double[] array) {
        set(name, EntryType.DOUBLE_ARRAY, array);
    }

    /**
     * Gets the <code>double</code> array from the given entry.
     * 
     * @param name name of the entry to be inspected
     * @return array from the given entry
     * @throws BadTypeException
     *         if the given entry does not contain <code>long</code> array
     * @throws NoSuchNameException
     *         if the given name is not found
     */
    public double[] getDoubleArray(String name) {
        return (double[]) extract(name, EntryType.DOUBLE_ARRAY);
    }

    /**
     * Sets the <code>String</code> array with the given name.
     * 
     * <p>
     * <b>Note:</b> the array is <i>not</i> copied internally.
     * </p>
     * 
     * @param name name of the new entry or the entry to be replaced
     * @param array value to be set
     */
    public void setStringArray(String name, String[] array) {
        set(name, EntryType.STRING_ARRAY, array);
    }

    /**
     * Gets the <code>String</code> array from the given entry.
     * 
     * @param name name of the entry to be inspected
     * @return array from the given entry
     * @throws BadTypeException
     *         if the given entry does not contain <code>String</code> array
     * @throws NoSuchNameException
     *         if the given name is not found
     */
    public String[] getStringArray(String name) {
        return (String[]) extract(name, EntryType.STRING_ARRAY);
    }

    /**
     * Sets the <code>byte[]</code> array (note: array of byte arrays)
     * with the given name.
     * 
     * <p>
     * <b>Note:</b> the arrays are <i>not</i> copied internally.
     * </p>
     * 
     * @param name name of the new entry or the entry to be replaced
     * @param array value to be set
     */
    public void setBinaryArray(String name, byte[][] array) {
        set(name, EntryType.BINARY_ARRAY, array);
    }

    /**
     * Gets the array of <code>byte[]</code> buffers
     * from the given entry.
     * 
     * @param name name of the entry to be inspected
     * @return array from the given entry
     * @throws BadTypeException
     *         if the given entry does not contain <code>byte[]</code> array
     * @throws NoSuchNameException
     *         if the given name is not found
     */
    public byte[][] getBinaryArray(String name) {
        return (byte[][]) extract(name, EntryType.BINARY_ARRAY);
    }

    /**
     * Sets the nested <code>Parameters</code> object with the given name.
     * 
     * <p>
     * <b>Note:</b> the nested object is not cloned internally.
     * </p>
     * 
     * @param name name of the new entry or the entry to be replaced
     * @param nested object to nest
     */
    public void setNestedParameters(String name, Parameters nested) {
        set(name, EntryType.NESTED_PARAMETERS, nested);
    }

    /**
     * Gets the nested <code>Parameters</code> object from the given entry.
     * 
     * @param name name of the entry to be inspected
     * @return nested object from the given entry
     * @throws BadTypeException
     *         if the given entry does not contain nested
     *         <code>Parameters</code> object
     * @throws NoSuchNameException
     *         if the given name is not found
     */
    public Parameters getNestedParameters(String name) {
        return (Parameters) extract(name, EntryType.NESTED_PARAMETERS);
    }

    /**
     * Sets the nested <code>Parameters</code> array with the given name.
     * 
     * <p>
     * <b>Note:</b> the nested objects are not cloned internally.
     * </p>
     * 
     * @param name name of the new entry or the entry to be replaced
     * @param nested array of objects to nest
     */
    public void setNestedArray(String name, Parameters[] nested) {
        set(name, EntryType.NESTED_PARAMETERS_ARRAY, nested);
    }

    /**
     * Gets the nested <code>Parameters</code> array from the given entry.
     * 
     * @param name name of the entry to be inspected
     * @return nested array of objects from the given entry
     * @throws BadTypeException
     *         if the given entry does not contain nested
     *         <code>Parameters</code> array
     * @throws NoSuchNameException
     *         if the given name is not found
     */
    public Parameters[] getNestedArray(String name) {
        return (Parameters[]) extract(name, EntryType.NESTED_PARAMETERS_ARRAY);
    }

    /**
     * Removes the given entry from the collection.
     * 
     * <p>
     * <b>Note:</b> The removed entry leaves a <i>hole</i> (empty slot) in
     * the collection that can be reused by newly added entries.
     * </p>
     * 
     * @param name the name of the entry to be removed
     */
    public void remove(String name) {
        Integer index = entryIndexes.get(name);
        if (index == null) {
            throw new NoSuchNameException(name);
        }

        Entry e = entries.get(index.intValue());

        entryIndexes.remove(name);
        entries.set(index.intValue(), null);
    }

    /**
     * Removes all entries from the collection.
     */
    public void clear() {
        entries.clear();
        entryIndexes.clear();
    }

    /**
     * Gets the current size of the collection.
     * 
     * @return size of the collection
     */
    public int size() {
        return entryIndexes.size();
    }

    /**
     * Gets the type of the given entry.
     * 
     * @param name name of the given entry
     * @return the type of the entry
     * @throws NoSuchNameException
     *         if the given name is not found
     */
    public EntryType getType(String name) {
        Entry e = findExisting(name);
        return e.type;
    }

    /**
     * Gets the iterator over the list of entries.
     * 
     * <p>
     * The iterator visits only those entries which are used
     * (in other words, it skips unused slots).
     * </p>
     * 
     * @return iterator to the list of entries
     */
    @Override
    public Iterator<Entry> iterator() {
        return new EntryIterator(entries);
    }

    /**
     * Finds the entry by name.
     * 
     * @param name the name of entry
     * @return <code>Entry</code>
     *         or <code>null</code> if the given name was not found
     */
    public Entry find(String name) {
        Integer index = entryIndexes.get(name);
        if (index != null) {
            return entries.get(index.intValue());
        }

        return null;
    }

    /**
     * Serializes the content of the whole collection.
     * 
     * <p>
     * The content is serialized into chunks of the given size,
     * after serialization only the last chunk can be smaller than
     * the requested size.
     * </p>
     * 
     * @param chunkSize chunk size, should be a multiple of 4
     *        or <code>Integer.MAX_VALUE</code>
     * @return list of buffers (chunks) with serialized data
     * @throws IllegalArgumentException if requested chunk size
     *         is not a multiple of 4 and is not
     *         <code>Integer.MAX_VALUE</code>
     */
    @Override
    public List<byte[]> serialize(int chunkSize) {
        return Serialization.serialize(this, chunkSize);
    }

    /**
     * Deserializes from the given list of buffers.
     * 
     * <p>
     * <b>Note:</b> The current content of this object is not cleared
     * before attempting deserialization and each retrieved data element
     * is <i>merged</i> into the current content as if done by individual
     * calls to appropriate <code>setXYZ</code> functions.<br />
     * In most cases deserialization will be performed to the empty
     * <code>Parameters</code> object
     * (to reconstruct it to the form that was used
     * for serialization), but deserialization onto non-empty object
     * might be occasionally useful as a way of merging two collections.
     * </p>
     * 
     * @param buffers each buffer should have a size that is multiple of 4
     * @throws IllegalArgumentException if any of the given
     *         buffers has a size that is not a multiple of 4
     *         or when the given data cannot be properly deserialized
     */
    public void deserialize(List<byte[]> buffers) {
        Serialization.deserialize(this, buffers);
    }

    /**
     * Returns a string representation of the content.
     * 
     * <p>
     * This function is supposed to be used by unit tests and for
     * simple debugging.
     * </p>
     * 
     * @return <code>String</code> representation of the collection
     */
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        buildString(builder, "");
        return builder.toString();
    }

    private void buildString(StringBuilder builder, String prefix) {
        for (int i = 0; i != entries.size(); ++i) {
            builder.append(prefix + "entry " + i + ":\n");
            boolean addNewline = true;
            Entry e = entries.get(i);
            if (e == null) {
                builder.append(prefix + "unused");
            } else {
                builder.append(prefix + "name: " + e.name + '\n');
                switch (e.type) {
                case BOOLEAN:
                     {
                        Boolean value = (Boolean) e.value;
                        builder.append(prefix + "boolean: " +
                                (value.booleanValue() ? "1" : "0"));
                    }
                    break;
                case INTEGER:
                     {
                        Integer value = (Integer) e.value;
                        builder.append(prefix + "integer: " +
                                value.intValue());
                    }
                    break;
                case LONG:
                     {
                        Long value = (Long) e.value;
                        builder.append(prefix + "long: " +
                                value.longValue());
                    }
                    break;
                case DOUBLE:
                     {
                        Double value = (Double) e.value;
                        builder.append(prefix + "double: " +
                                value.doubleValue());
                    }
                    break;
                case STRING:
                     {
                        String value = (String) e.value;
                        builder.append(prefix + "string: " + value);
                    }
                    break;
                case BINARY:
                     {
                        byte[] value = (byte[]) e.value;
                        builder.append(prefix + "binary of length " +
                                value.length);
                    }
                    break;
                case BOOLEAN_ARRAY:
                     {
                        builder.append(prefix + "boolean array:");
                        boolean[] array = (boolean[]) e.value;
                        for (int j = 0; j != array.length; ++j) {
                            builder.append(" " +
                                    (array[j] ? "1" : "0"));
                        }
                    }
                    break;
                case INTEGER_ARRAY:
                     {
                        builder.append(prefix + "integer array:");
                        int[] array = (int[]) e.value;
                        for (int j = 0; j != array.length; ++j) {
                            builder.append(" " + array[j]);
                        }
                    }
                    break;
                case LONG_ARRAY:
                     {
                        builder.append(prefix + "long array:");
                        long[] array = (long[]) e.value;
                        for (int j = 0; j != array.length; ++j) {
                            builder.append(" " + array[j]);
                        }
                    }
                    break;
                case DOUBLE_ARRAY:
                     {
                        builder.append(prefix + "double array:");
                        double[] array = (double[]) e.value;
                        for (int j = 0; j != array.length; ++j) {
                            builder.append(" " + array[j]);
                        }
                    }
                    break;
                case STRING_ARRAY:
                     {
                        builder.append(prefix + "string array:");
                        String[] array = (String[]) e.value;
                        for (int j = 0; j != array.length; ++j) {
                            builder.append(" " + array[j]);
                        }
                    }
                    break;
                case BINARY_ARRAY:
                     {
                        byte[][] array = (byte[][]) e.value;
                        builder.append(prefix + "binary array of length " +
                                array.length);
                    }
                    break;
                case NESTED_PARAMETERS:
                     {
                        builder.append(prefix + "nested parameters:\n");
                        Parameters nested = (Parameters) e.value;
                        nested.buildString(builder, prefix + "  ");
                        addNewline = false;
                    }
                    break;
                case NESTED_PARAMETERS_ARRAY:
                     {
                        Parameters[] array = (Parameters[]) e.value;
                    	builder.append(prefix + "nested parameters array of length " +
                    			array.length + ":\n");
                        for (int j = 0; j != array.length; ++j) {
                            builder.append("  nested at index " + j + ":\n");
                            array[j].buildString(builder, prefix + "    ");
                        }
                    	addNewline = false;
                    }
                break;
                }
            }

            if (addNewline) {
                builder.append("\n");
            }
        }
    }

    private void set(String name, EntryType type, Object value) {
        Integer index = entryIndexes.get(name);
        if (index != null) {
            // replace existing entry
            Entry e = entries.get(index.intValue());
            
            e.type = type;
            e.value = value;
        } else {
            // create new entry
            Entry e = new Entry(type, name, value);
            int newIndex = findEmptySlot();
            entries.set(newIndex, e);
            entryIndexes.put(name, Integer.valueOf(newIndex));
        }
    }

    private Object extract(String name, EntryType expectedType) {
        Entry e = findExisting(name);
        if (e.type != expectedType) {
            throw new BadTypeException(name, expectedType.toString());
        }
        return e.value;
    }

    private Entry findExisting(String name) {
        Integer index = entryIndexes.get(name);
        if (index != null) {
            return entries.get(index.intValue());
        }

        throw new NoSuchNameException(name);
    }

    private int findEmptySlot() {
        int i;
        for (i = 0; i != entries.size(); ++i) {
            if (entries.get(i) == null) {
                break;
            }
        }

        if (i == entries.size()) {
            // the list is empty or does not contain any empty entry
            // -> expand the list to make room for the new entry
            entries.add(null);
        }

        return i;
    }
}
