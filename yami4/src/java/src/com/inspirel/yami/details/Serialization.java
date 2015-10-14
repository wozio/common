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

package com.inspirel.yami.details;

import com.inspirel.yami.Parameters;
import com.inspirel.yami.UnexpectedValueException;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class Serialization {

    // encoding name for serializing and deserializing Strings
    public static final String ENCODING = "UTF-8";
    
    private static final int BITS_IN_BYTE = 8;
    private static final int SIZE_OF_INT = 4;
    private static final int SIZE_OF_LONG = 8;
    private static final int SIZE_OF_DOUBLE = 8;
    
    private static class BuffersState {
        ArrayList<byte[]> buffers;
        int bufferIndex;
        int byteIndex;

        void checkIndices() {
            byte[] buf = buffers.get(bufferIndex);
            if (byteIndex == buf.length) {
                byteIndex = 0;
                bufferIndex++;
            }
        }
    }

    public static List<byte[]> serialize(Parameters params, int chunkSize) {
        if (chunkSize != Integer.MAX_VALUE && chunkSize % 4 != 0) {
            throw new IllegalArgumentException();
        }

        LinkedList<byte[]> stringCache = new LinkedList<byte[]>();
        
        int numberOfBytes = serializationSize(params, stringCache);

        // allocate all buffers
        
        ArrayList<byte[]> buffers = new ArrayList<byte[]>();
        while (numberOfBytes > 0) {
            int sizeOfBuffer = Math.min(numberOfBytes, chunkSize);
            buffers.add(new byte[sizeOfBuffer]);
            numberOfBytes -= sizeOfBuffer;
        }
        
        // fill all buffers with data
        
        BuffersState bufState = new BuffersState();
        bufState.buffers = buffers;
        bufState.bufferIndex = 0;
        bufState.byteIndex = 0;
        
        serialize(bufState, params, stringCache);
        
        return buffers;
    }
    
    private static void serialize(BuffersState bufState, Parameters params,
            LinkedList<byte[]> stringCache) {

        // number of entries
        appendInt(bufState, params.size());

        for (Parameters.Entry e : params) {
            // name of this entry
            appendString(bufState, stringCache);

            // type code
            appendInt(bufState, typeToCode(e.type()));

            // entry value itself
            switch (e.type()) {
            case BOOLEAN:
                appendInt(bufState, e.getBoolean() ? 1 : 0);
                break;
            case INTEGER:
                appendInt(bufState, e.getInteger());
                break;
            case LONG:
                appendLong(bufState, e.getLong());
                break;
            case DOUBLE:
                appendDouble(bufState, e.getDouble());
                break;
            case STRING:
                appendString(bufState, stringCache);
                break;
            case BINARY:
                appendBinary(bufState, e.getBinary());
                break;
            case BOOLEAN_ARRAY:
                {
                    boolean[] array = e.getBooleanArray();

                    // pack the array
                    int bytesNeeded =
                        (array.length + BITS_IN_BYTE - 1) / BITS_IN_BYTE;
                    byte[] packedArray = new byte[bytesNeeded];
                    for (int i = 0; i != array.length; ++i) {
                        int bytePosition = i / BITS_IN_BYTE;
                        int bitPosition = i % BITS_IN_BYTE;
                        if (array[i]) {
                            packedArray[bytePosition] |= 1 << bitPosition;
                        }
                    }
                    appendInt(bufState, array.length);
                    appendBytes(bufState, packedArray);
                }
                break;
            case INTEGER_ARRAY:
                {
                    int[] array = e.getIntegerArray();
                    appendInt(bufState, array.length);
                    for (int i = 0; i != array.length; ++i) {
                        appendInt(bufState, array[i]);
                    }
                }
                break;
            case LONG_ARRAY:
                {
                    long[] array = e.getLongArray();
                    appendInt(bufState, array.length);
                    for (int i = 0; i != array.length; ++i) {
                        appendLong(bufState, array[i]);
                    }
                }
                break;
            case DOUBLE_ARRAY:
                {
                    double[] array = e.getDoubleArray();
                    appendInt(bufState, array.length);
                    for (int i = 0; i != array.length; ++i) {
                        appendDouble(bufState, array[i]);
                    }
                }
                break;
            case STRING_ARRAY:
                {
                    String[] array = e.getStringArray();
                    appendInt(bufState, array.length);
                    for (int i = 0; i != array.length; ++i) {
                        appendString(bufState, stringCache);
                    }
                }
                break;
            case BINARY_ARRAY:
                {
                    byte[][] array = e.getBinaryArray();
                    appendInt(bufState, array.length);
                    for (int i = 0; i != array.length; ++i) {
                        appendBinary(bufState, array[i]);
                    }
                }
                break;
            case NESTED_PARAMETERS:
                {
                    Parameters nested = e.getNestedParameters();
                    serialize(bufState, nested, stringCache);
                }
                break;
            case NESTED_PARAMETERS_ARRAY:
            	{
            		Parameters[] nested = e.getNestedArray();
                    appendInt(bufState, nested.length);
                    for (int i = 0; i != nested.length; ++i) {
                		serialize(bufState, nested[i], stringCache);
                    }
            	}
            break;
            }
        }
    }
    
    private static int serializationSize(byte[] buf) {
        return SIZE_OF_INT + roundTo4(buf.length);
    }
    
    private static int serializationSize(
            String s, LinkedList<byte[]> stringCache) {
        
        int size = 0;
        try {
            byte[] bytes = s.getBytes(ENCODING);
            stringCache.add(bytes);
            size = serializationSize(bytes);
        } catch (UnsupportedEncodingException e) {
            throw new UnexpectedValueException(e.getMessage());
        }
        return size;
    }
    
    private static int serializationSize(
            Parameters params, LinkedList<byte[]> stringCache) {
        
        int size = 0;
        
        // number of entries
        size += SIZE_OF_INT;

        for (Parameters.Entry e : params) {
            // name of this entry
            size += serializationSize(e.name(), stringCache);

            // type code
            size += SIZE_OF_INT;

            // entry value itself
            switch (e.type()) {
            case BOOLEAN:
                size += SIZE_OF_INT;
                break;
            case INTEGER:
                size += SIZE_OF_INT;
                break;
            case LONG:
                size += SIZE_OF_LONG;
                break;
            case DOUBLE:
                size += SIZE_OF_DOUBLE;
                break;
            case STRING:
                size += serializationSize(e.getString(), stringCache);
                break;
            case BINARY:
                size += serializationSize(e.getBinary());
                break;
            case BOOLEAN_ARRAY:
                {
                    size += SIZE_OF_INT;
                     
                    // pack the array
                    boolean[] array = e.getBooleanArray();
                    int bytesNeeded =
                        (array.length + BITS_IN_BYTE - 1) / BITS_IN_BYTE;
                    size += roundTo4(bytesNeeded);
                }
                break;
            case INTEGER_ARRAY:
                {
                    size += SIZE_OF_INT;

                    int[] array = e.getIntegerArray();
                    size += SIZE_OF_INT * array.length;
                }
                break;
            case LONG_ARRAY:
                {
                    size += SIZE_OF_INT;

                    long[] array = e.getLongArray();
                    size += SIZE_OF_LONG * array.length;
                }
                break;
            case DOUBLE_ARRAY:
                {
                    size += SIZE_OF_INT;

                    double[] array = e.getDoubleArray();
                    size += SIZE_OF_DOUBLE * array.length;
                }
                break;
            case STRING_ARRAY:
                {
                    size += SIZE_OF_INT;

                    String[] array = e.getStringArray();
                    for (int i = 0; i != array.length; ++i) {
                        size += serializationSize(array[i], stringCache);
                    }
                }
                break;
            case BINARY_ARRAY:
                {
                    size += SIZE_OF_INT;

                    byte[][] array = e.getBinaryArray();
                    for (int i = 0; i != array.length; ++i) {
                        size += serializationSize(array[i]);
                    }
                }
                break;
            case NESTED_PARAMETERS:
                {
                    Parameters nested = e.getNestedParameters();
                    size += serializationSize(nested, stringCache);
                }
                break;
            case NESTED_PARAMETERS_ARRAY:
	            {
	            	size += SIZE_OF_INT;
	            	
	                Parameters[] nested = e.getNestedArray();
                    for (int i = 0; i != nested.length; ++i) {
                        size += serializationSize(nested[i], stringCache);
                    }
	            }
            break;
            }
        }

        return size;
    }

    private static class ByteIterator {

        private final List<byte[]> buffers;
        private byte[] currentBuffer;
        private int i; // buffer index
        private int j; // byte index within the current buffer

        ByteIterator(List<byte[]> buffers) {
            this.buffers = buffers;
            currentBuffer = buffers.get(0);
            i = 0;
            j = 0;
        }

        byte next() {
            if (j == currentBuffer.length) {
                currentBuffer = buffers.get(++i);
                j = 0;
            }
            return currentBuffer[j++];
        }
    }

    public static void deserialize(Parameters params,
            List<byte[]> buffers) {

        ByteIterator bytes = new ByteIterator(buffers);

        deserializeFromBytes(params, bytes);
    }

    private static void deserializeFromBytes(Parameters params,
            ByteIterator bytes) {

        int numOfEntries = consumeInt(bytes);
        for (int i = 0; i != numOfEntries; ++i) {

            String entryName = consumeString(bytes);

            int typeCode = consumeInt(bytes);
            Parameters.EntryType type = codeToType(typeCode);
            switch (type) {
            case BOOLEAN:
                 {
                    int value = consumeInt(bytes);
                    params.setBoolean(entryName, value != 0);
                }
                break;
            case INTEGER:
                 {
                    int value = consumeInt(bytes);
                    params.setInteger(entryName, value);
                }
                break;
            case LONG:
                 {
                    long value = consumeLong(bytes);
                    params.setLong(entryName, value);
                }
                break;
            case DOUBLE:
                 {
                    double value = consumeDouble(bytes);
                    params.setDouble(entryName, value);
                }
                break;
            case STRING:
                 {
                    String value = consumeString(bytes);
                    params.setString(entryName, value);
                }
                break;
            case BINARY:
                 {
                    byte[] value = consumeBinary(bytes);
                    params.setBinary(entryName, value);
                }
                break;
            case BOOLEAN_ARRAY:
                 {
                    int arrayLength = consumeInt(bytes);
                    int bytesNeeded =
                            (arrayLength + BITS_IN_BYTE - 1) / BITS_IN_BYTE;
                    byte[] packedArray = consumeBytes(bytes, bytesNeeded);
                    boolean[] array = new boolean[arrayLength];
                    for (int j = 0; j != arrayLength; ++j) {
                        int bytePosition = j / BITS_IN_BYTE;
                        int bitPosition = j % BITS_IN_BYTE;
                        int mask = 1 << bitPosition;
                        if ((packedArray[bytePosition] & mask) != 0) {
                            array[j] = true;
                        }
                    }

                    params.setBooleanArray(entryName, array);
                }
                break;
            case INTEGER_ARRAY:
                 {
                    int arrayLength = consumeInt(bytes);
                    int[] array = new int[arrayLength];
                    for (int j = 0; j != arrayLength; ++j) {
                        array[j] = consumeInt(bytes);
                    }

                    params.setIntegerArray(entryName, array);
                }
                break;
            case LONG_ARRAY:
                 {
                    int arrayLength = consumeInt(bytes);
                    long[] array = new long[arrayLength];
                    for (int j = 0; j != arrayLength; ++j) {
                        array[j] = consumeLong(bytes);
                    }

                    params.setLongArray(entryName, array);
                }
                break;
            case DOUBLE_ARRAY:
                 {
                    int arrayLength = consumeInt(bytes);
                    double[] array = new double[arrayLength];
                    for (int j = 0; j != arrayLength; ++j) {
                        array[j] = consumeDouble(bytes);
                    }

                    params.setDoubleArray(entryName, array);
                }
                break;
            case STRING_ARRAY:
                 {
                    int arrayLength = consumeInt(bytes);
                    String[] array = new String[arrayLength];
                    for (int j = 0; j != arrayLength; ++j) {
                        array[j] = consumeString(bytes);
                    }

                    params.setStringArray(entryName, array);
                }
                break;
            case BINARY_ARRAY:
                 {
                    int arrayLength = consumeInt(bytes);
                    byte[][] array = new byte[arrayLength][];
                    for (int j = 0; j != arrayLength; ++j) {
                        array[j] = consumeBinary(bytes);
                    }

                    params.setBinaryArray(entryName, array);
                }
                break;
            case NESTED_PARAMETERS:
                 {
                    Parameters nested = new Parameters();
                    deserializeFromBytes(nested, bytes);
                    params.setNestedParameters(entryName, nested);
                }
                break;
            case NESTED_PARAMETERS_ARRAY:
            	 {
                    int arrayLength = consumeInt(bytes);
	                Parameters[] nested = new Parameters[arrayLength];
                    for (int j = 0; j != arrayLength; ++j) {
                    	nested[j] = new Parameters();
                    	deserializeFromBytes(nested[j], bytes);
                    }
	                params.setNestedArray(entryName, nested);
	            }
            	break;
            }
        }
    }

    // places an int value in the given buffer
    static void storeInt(byte[] buf, int from, int value) {
        buf[from + 0] = (byte) (value & 0xff);
        buf[from + 1] = (byte) ((value & 0xff00) >>> 8);
        buf[from + 2] = (byte) ((value & 0xff0000) >>> 16);
        buf[from + 3] = (byte) ((value & 0xff000000) >>> 24);
    }

    // places an integer in the buffer
    private static void appendInt(BuffersState bufState, int value) {
        byte[] buf = bufState.buffers.get(bufState.bufferIndex);
        storeInt(buf, bufState.byteIndex, value);
        bufState.byteIndex += SIZE_OF_INT;
    
        bufState.checkIndices();
    }

    // places a long in the buffer
    private static void appendLong(BuffersState bufState, long value) {
        appendInt(bufState, (int) (value & 0xffffffffL));
        appendInt(bufState, (int) (value >>> 32));
    }

    // places a double in the buffer
    private static void appendDouble(BuffersState bufState, double value) {
        long longBits = Double.doubleToLongBits(value);
        appendLong(bufState, longBits);
    }

    // places a String in the buffer
    private static void appendString(BuffersState bufState,
            LinkedList<byte[]> stringCache) {
        
        byte[] bytes = stringCache.remove();
            
        //  first place the length of String
        appendInt(bufState, bytes.length);

        // then the content itself, but padded to full 4-byte word
        appendBytes(bufState, bytes);
    }

    // places a binary object in the buffer
    private static void appendBinary(BuffersState bufState, byte[] value) {
        // first place the length of binary buffer
        appendInt(bufState, value.length);

        // then the content itself, but padded to full 4-byte word
        appendBytes(bufState, value);
    }

    // places a byte array, but without bounds
    private static void appendBytes(BuffersState bufState, byte[] array) {
        int currentPos = 0;
        int bytesLeft = array.length;
        while (bytesLeft >= SIZE_OF_INT) {
            byte[] buf = bufState.buffers.get(bufState.bufferIndex);
            
            int contiguousBlockSize =
                Math.min(buf.length - bufState.byteIndex, bytesLeft & ~0x03);
            
            for (int i = 0; i != contiguousBlockSize; ++i) {
                buf[bufState.byteIndex + i] = array[currentPos + i];
            }
            currentPos += contiguousBlockSize;
            bytesLeft -= contiguousBlockSize;
            bufState.byteIndex += contiguousBlockSize;
            
            bufState.checkIndices();
        }
        
        if (bytesLeft != 0) {
            // padding required

            byte[] buf = bufState.buffers.get(bufState.bufferIndex);
            
            for (int i = 0; i != bytesLeft; ++i) {
                buf[bufState.byteIndex + i] = array[currentPos + i];
            }
            bufState.byteIndex += SIZE_OF_INT;
            
            bufState.checkIndices();
        }
    }

    // rounds up for aligning to the 4-byte word boundary
    private static int roundTo4(int ival) {
        return ((ival + 3) & ~3);
    }

    // converts from entry type to type code
    private static int typeToCode(Parameters.EntryType type) {
        int code = 0;
        switch (type) {
        case BOOLEAN:
            code = 1;
            break;
        case INTEGER:
            code = 2;
            break;
        case LONG:
            code = 3;
            break;
        case DOUBLE:
            code = 4;
            break;
        case STRING:
            code = 5;
            break;
        case BINARY:
            code = 6;
            break;
        case BOOLEAN_ARRAY:
            code = 7;
            break;
        case INTEGER_ARRAY:
            code = 8;
            break;
        case LONG_ARRAY:
            code = 9;
            break;
        case DOUBLE_ARRAY:
            code = 10;
            break;
        case STRING_ARRAY:
            code = 11;
            break;
        case BINARY_ARRAY:
            code = 12;
            break;
        case NESTED_PARAMETERS:
            code = 13;
            break;
        case NESTED_PARAMETERS_ARRAY:
            code = 14;
            break;
        }

        return code;
    }

    // converts from type code to entry type
    private static Parameters.EntryType codeToType(int code) {
        Parameters.EntryType type;
        switch (code) {
        case 1:
            type = Parameters.EntryType.BOOLEAN;
            break;
        case 2:
            type = Parameters.EntryType.INTEGER;
            break;
        case 3:
            type = Parameters.EntryType.LONG;
            break;
        case 4:
            type = Parameters.EntryType.DOUBLE;
            break;
        case 5:
            type = Parameters.EntryType.STRING;
            break;
        case 6:
            type = Parameters.EntryType.BINARY;
            break;
        case 7:
            type = Parameters.EntryType.BOOLEAN_ARRAY;
            break;
        case 8:
            type = Parameters.EntryType.INTEGER_ARRAY;
            break;
        case 9:
            type = Parameters.EntryType.LONG_ARRAY;
            break;
        case 10:
            type = Parameters.EntryType.DOUBLE_ARRAY;
            break;
        case 11:
            type = Parameters.EntryType.STRING_ARRAY;
            break;
        case 12:
            type = Parameters.EntryType.BINARY_ARRAY;
            break;
        case 13:
            type = Parameters.EntryType.NESTED_PARAMETERS;
            break;
        case 14:
            type = Parameters.EntryType.NESTED_PARAMETERS_ARRAY;
            break;

        default:
            throw new UnexpectedValueException("Invalid type code.");
        }

        return type;
    }

    // reads int from the given position in the buffer
    static int readInt(byte[] buf, int from) {
        int value = buf[from] & 0xff;
        value |= (buf[from + 1] & 0xff) << 8;
        value |= (buf[from + 2] & 0xff) << 16;
        value |= (buf[from + 3] & 0xff) << 24;

        return value;
    }
    
    // comsumes integer value from buffers
    private static int consumeInt(ByteIterator bytes) {
        byte[] buf = {
            bytes.next(),
            bytes.next(),
            bytes.next(),
            bytes.next()
        };
        return readInt(buf, 0);
    }

    // reads long value from buffers
    private static long consumeLong(ByteIterator bytes) {
        long lower = consumeInt(bytes);
        long lowerMasked = (lower & 0x00000000ffffffffl);
        return lowerMasked + ((long) consumeInt(bytes) << 32);
    }

    // reads double value from the buffer with given endianness
    private static double consumeDouble(ByteIterator bytes) {
        long longBits = consumeLong(bytes);
        return Double.longBitsToDouble(longBits);
    }

    // reads a byte array of given length
    private static byte[] consumeBytes(ByteIterator bytes, int length) {
        int paddedLength = roundTo4(length);

        byte[] buf = new byte[length];
        for (int i = 0; i != length; ++i) {
            buf[i] = bytes.next();
        }

        // consume the remaining padding bytes (if any)
        for (int i = length; i != paddedLength; ++i) {
            bytes.next();
        }

        return buf;
    }

    // reads a String object
    private static String consumeString(ByteIterator bytes) {
        int length = consumeInt(bytes);
        byte[] buf = consumeBytes(bytes, length);
        try {
            return new String(buf, ENCODING);
        } catch (UnsupportedEncodingException e) {
            throw new UnexpectedValueException(e.getMessage());
        }
    }
    // reads a binary object
    private static byte[] consumeBinary(ByteIterator bytes) {
        int length = consumeInt(bytes);
        byte[] buf = consumeBytes(bytes, length);
        return buf;
    }
}
