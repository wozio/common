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
using System;
using System.Text;

namespace Inspirel.YAMI.details
{
    internal static class Serialization
    {
        private static readonly Encoding encoding =
            Encoding.GetEncoding(
                ASCIIEncoding.UTF8.CodePage,
                new EncoderExceptionFallback(),
                new DecoderExceptionFallback()
            );

        private const int BITS_IN_BYTE = 8;
        private const int SIZE_OF_INT = 4;
        private const int SIZE_OF_LONG = 8;
        private const int SIZE_OF_DOUBLE = 8;

        private class BuffersState
        {
            public List<byte[]> buffers = null;
            public int bufferIndex;
            public int byteIndex;

            public void checkIndices()
            {
                byte[] buf = buffers[bufferIndex];
                if(byteIndex == buf.Length)
                {
                    byteIndex = 0;
                    bufferIndex++;
                }
            }
        }

        public static List<byte[]> Serialize(Parameters parameters,
            int chunkSize)
        {
            if(chunkSize != int.MaxValue && chunkSize % 4 != 0)
            {
                throw new System.ArgumentException();
            }

            List<byte[]> stringCache = new List<byte[]>();

            int numberOfBytes = serializationSize(parameters, stringCache);

            // allocate all buffers
            List<byte[]> buffers = new List<byte[]>();
            while (numberOfBytes > 0) 
            {
                int sizeOfBuffer = Math.Min(numberOfBytes, chunkSize);
                buffers.Add(new byte[sizeOfBuffer]);
                numberOfBytes -= sizeOfBuffer;
            }

            // fill all buffers with data
            BuffersState bufState = new BuffersState();
            bufState.buffers = buffers;
            bufState.bufferIndex = 0;
            bufState.byteIndex = 0;
            
            serialize(bufState, parameters, stringCache);
            
            return buffers;
        }

        private static void serialize(BuffersState bufState, 
            Parameters parameters, List<byte[]> stringCache)
        {
            // number of entries
            appendInt(bufState, parameters.Count);

            foreach(Parameters.Entry e in parameters)
            {
                // name of this entry
                appendString(bufState, stringCache);

                // type code
                appendInt(bufState, typeToCode(e.Type));

                // entry value itself
                switch(e.Type)
                {
                case Parameters.EntryType.BOOLEAN:
                    appendInt(bufState, e.GetBoolean() ? 1 : 0);
                    break;
                case Parameters.EntryType.INTEGER:
                    appendInt(bufState, e.GetInteger());
                    break;
                case Parameters.EntryType.LONG:
                    appendLong(bufState, e.GetLong());
                    break;
                case Parameters.EntryType.DOUBLE:
                    appendDouble(bufState, e.GetDouble());
                    break;
                case Parameters.EntryType.STRING:
                    appendString(bufState, stringCache);
                    break;
                case Parameters.EntryType.BINARY:
                    appendBinary(bufState, e.GetBinary());
                    break;
                case Parameters.EntryType.BOOLEAN_ARRAY:
                    {
                        bool[] array = e.GetBooleanArray();

                        // pack the array
                        int bytesNeeded =
                                (array.Length + BITS_IN_BYTE - 1) 
                                / BITS_IN_BYTE;
                        byte[] packedArray = new byte[bytesNeeded];
                        for(int i = 0; i != array.Length; ++i)
                        {
                            int bytePosition = i / BITS_IN_BYTE;
                            int bitPosition = i % BITS_IN_BYTE;
                            if(array[i])
                            {
                                packedArray[bytePosition] |= 
                                    (byte)(1 << bitPosition);
                            }
                        }
                        appendInt(bufState, array.Length);
                        appendBytes(bufState, packedArray);
                    }
                    break;
                case Parameters.EntryType.INTEGER_ARRAY:
                    {
                        int[] array = e.GetIntegerArray();
                        appendInt(bufState, array.Length);
                        for(int i = 0; i != array.Length; ++i)
                        {
                            appendInt(bufState, array[i]);
                        }
                    }
                    break;
                case Parameters.EntryType.LONG_ARRAY:
                    {
                        long[] array = e.GetLongArray();
                        appendInt(bufState, array.Length);
                        for(int i = 0; i != array.Length; ++i)
                        {
                            appendLong(bufState, array[i]);
                        }
                    }
                    break;
                case Parameters.EntryType.DOUBLE_ARRAY:
                    {
                        double[] array = e.GetDoubleArray();
                        appendInt(bufState, array.Length);
                        for(int i = 0; i != array.Length; ++i)
                        {
                            appendDouble(bufState, array[i]);
                        }
                    }
                    break;
                case Parameters.EntryType.STRING_ARRAY:
                    {
                        string[] array = e.GetStringArray();
                        appendInt(bufState, array.Length);
                        for(int i = 0; i != array.Length; ++i)
                        {
                            appendString(bufState, stringCache);
                        }
                    }
                    break;
                case Parameters.EntryType.BINARY_ARRAY:
                    {
                        byte[][] array = e.GetBinaryArray();
                        appendInt(bufState, array.Length);
                        for(int i = 0; i != array.Length; ++i)
                        {
                            appendBinary(bufState, array[i]);
                        }
                    }
                    break;
                case Parameters.EntryType.NESTED_PARAMETERS:
                    {
                        Parameters nested = e.GetNestedParameters();
                        serialize(bufState, nested, stringCache);
                    }
                    break;
                case Parameters.EntryType.NESTED_PARAMETERS_ARRAY:
                    {
                        Parameters[] nested = e.GetNestedArray();
                        appendInt(bufState, nested.Length);
                        for(int i = 0; i != nested.Length; ++i)
                        {
                            serialize(bufState, nested[i], stringCache);
                        }
                    }
                    break;
                }
            }
        }

        private static int serializationSize(byte[] buf)
        {
            return SIZE_OF_INT + roundTo4(buf.Length);
        }

        private static int serializationSize(string s, 
            List<byte[]> stringCache)
        {
            int size = 0;
            try
            {
                byte[] encoded = encoding.GetBytes(s);
                stringCache.Add(encoded);
                size = serializationSize(encoded);
            }
            catch(EncoderFallbackException e)
            {
                throw new UnexpectedValueException(e.Message);
            }
            return size;
        }
        
        private static int serializationSize(Parameters parameters, 
            List<byte[]> stringCache)
        {
            int size = 0;

            // number of entries
            size += SIZE_OF_INT;

            foreach(Parameters.Entry e in parameters)
            {
                // name of this entry
                size += serializationSize(e.Name, stringCache);

                // type code
                size += SIZE_OF_INT;

                // entry value itself
                switch(e.Type)
                {
                case Parameters.EntryType.BOOLEAN:
                    size += SIZE_OF_INT;
                    break;
                case Parameters.EntryType.INTEGER:
                    size += SIZE_OF_INT;
                    break;
                case Parameters.EntryType.LONG:
                    size += SIZE_OF_LONG;
                    break;
                case Parameters.EntryType.DOUBLE:
                    size += SIZE_OF_DOUBLE;
                    break;
                case Parameters.EntryType.STRING:
                    size += serializationSize(e.GetString(), stringCache);
                    break;
                case Parameters.EntryType.BINARY:
                    size += serializationSize(e.GetBinary());
                    break;
                case Parameters.EntryType.BOOLEAN_ARRAY:
                    {
                        size += SIZE_OF_INT;

                        // pack the array
                        bool[] array = e.GetBooleanArray();
                        int bytesNeeded =
                            (array.Length + BITS_IN_BYTE - 1) / BITS_IN_BYTE;
                        size += roundTo4(bytesNeeded);
                    }
                    break;
                case Parameters.EntryType.INTEGER_ARRAY:
                    {
                        size += SIZE_OF_INT;

                        int[] array = e.GetIntegerArray();
                        size += SIZE_OF_INT * array.Length;
                    }
                    break;
                case Parameters.EntryType.LONG_ARRAY:
                    {
                        size += SIZE_OF_INT;

                        long[] array = e.GetLongArray();
                        size += SIZE_OF_LONG * array.Length;
                    }
                    break;
                case Parameters.EntryType.DOUBLE_ARRAY:
                    {
                        size += SIZE_OF_INT;

                        double[] array = e.GetDoubleArray();
                        size += SIZE_OF_DOUBLE * array.Length;
                    }
                    break;
                case Parameters.EntryType.STRING_ARRAY:
                    {
                        size += SIZE_OF_INT;

                        String[] array = e.GetStringArray();
                        for(int i = 0; i != array.Length; ++i)
                        {
                            size += serializationSize(array[i], stringCache);
                        }
                    }
                    break;
                case Parameters.EntryType.BINARY_ARRAY:
                    {
                        size += SIZE_OF_INT;

                        byte[][] array = e.GetBinaryArray();
                        for(int i = 0; i != array.Length; ++i)
                        {
                            size += serializationSize(array[i]);
                        }
                    }
                    break;
                case Parameters.EntryType.NESTED_PARAMETERS:
                    {
                        Parameters nested = e.GetNestedParameters();
                        size += serializationSize(nested, stringCache);
                        break;
                    }
                case Parameters.EntryType.NESTED_PARAMETERS_ARRAY:
                    {
                        size += SIZE_OF_INT;

                        Parameters[] nested = e.GetNestedArray();
                        for(int i = 0; i != nested.Length; ++i)
                        {
                            size += serializationSize(nested[i], stringCache);
                        }
                    }
                    break;
                }
            }

            return size;
        }

        private class ByteIterator
        {

            private readonly IList<byte[]> buffers;
            private byte[] currentBuffer;
            private int i; // buffer index
            private int j; // byte index within the current buffer

            public ByteIterator(IList<byte[]> buffers)
            {
                this.buffers = buffers;
                currentBuffer = buffers[0];
                i = 0;
                j = 0;
            }

            public byte next()
            {
                if(j == currentBuffer.Length)
                {
                    currentBuffer = buffers[++i];
                    j = 0;
                }
                return currentBuffer[j++];
            }
        }

        public static void Deserialize(Parameters parameters,
                IList<byte[]> buffers)
        {
            ByteIterator bytes = new ByteIterator(buffers);

            deserializeFromBytes(parameters, bytes);
        }

        private static void deserializeFromBytes(Parameters parameters,
                ByteIterator bytes)
        {
            int numOfEntries = consumeInt(bytes);
            for(int i = 0; i != numOfEntries; ++i)
            {
                string entryName = consumeString(bytes);

                int typeCode = consumeInt(bytes);
                Parameters.EntryType type = codeToType(typeCode);
                switch(type)
                {
                case Parameters.EntryType.BOOLEAN:
                    {
                        int value = consumeInt(bytes);
                        parameters.SetBoolean(entryName, value != 0);
                    }
                    break;
                case Parameters.EntryType.INTEGER:
                    {
                        int value = consumeInt(bytes);
                        parameters.SetInteger(entryName, value);
                    }
                    break;
                case Parameters.EntryType.LONG:
                    {
                        long value = consumeLong(bytes);
                        parameters.SetLong(entryName, value);
                    }
                    break;
                case Parameters.EntryType.DOUBLE:
                    {
                        double value = consumeDouble(bytes);
                        parameters.SetDouble(entryName, value);
                    }
                    break;
                case Parameters.EntryType.STRING:
                    {
                        string value = consumeString(bytes);
                        parameters.SetString(entryName, value);
                    }
                    break;
                case Parameters.EntryType.BINARY:
                    {
                        byte[] value = consumeBinary(bytes);
                        parameters.SetBinary(entryName, value);
                    }
                    break;
                case Parameters.EntryType.BOOLEAN_ARRAY:
                    {
                        int arrayLength = consumeInt(bytes);
                        int bytesNeeded =
                                (arrayLength + BITS_IN_BYTE - 1) 
                                / BITS_IN_BYTE;
                        byte[] packedArray = 
                            consumeBytes(bytes, bytesNeeded);
                        bool[] array = new bool[arrayLength];
                        for(int j = 0; j != arrayLength; ++j)
                        {
                            int bytePosition = j / BITS_IN_BYTE;
                            int bitPosition = j % BITS_IN_BYTE;
                            int mask = 1 << bitPosition;
                            if((packedArray[bytePosition] & mask) != 0)
                            {
                                array[j] = true;
                            }
                        }

                        parameters.SetBooleanArray(entryName, array);
                    }
                    break;
                case Parameters.EntryType.INTEGER_ARRAY:
                    {
                        int arrayLength = consumeInt(bytes);
                        int[] array = new int[arrayLength];
                        for(int j = 0; j != arrayLength; ++j)
                        {
                            array[j] = consumeInt(bytes);
                        }

                        parameters.SetIntegerArray(entryName, array);
                    }
                    break;
                case Parameters.EntryType.LONG_ARRAY:
                    {
                        int arrayLength = consumeInt(bytes);
                        long[] array = new long[arrayLength];
                        for(int j = 0; j != arrayLength; ++j)
                        {
                            array[j] = consumeLong(bytes);
                        }

                        parameters.SetLongArray(entryName, array);
                    }
                    break;
                case Parameters.EntryType.DOUBLE_ARRAY:
                    {
                        int arrayLength = consumeInt(bytes);
                        double[] array = new double[arrayLength];
                        for(int j = 0; j != arrayLength; ++j)
                        {
                            array[j] = consumeDouble(bytes);
                        }

                        parameters.SetDoubleArray(entryName, array);
                    }
                    break;
                case Parameters.EntryType.STRING_ARRAY:
                    {
                        int arrayLength = consumeInt(bytes);
                        string[] array = new string[arrayLength];
                        for(int j = 0; j != arrayLength; ++j)
                        {
                            array[j] = consumeString(bytes);
                        }

                        parameters.SetStringArray(entryName, array);
                    }
                    break;
                case Parameters.EntryType.BINARY_ARRAY:
                    {
                        int arrayLength = consumeInt(bytes);
                        byte[][] array = new byte[arrayLength][];
                        for(int j = 0; j != arrayLength; ++j)
                        {
                            array[j] = consumeBinary(bytes);
                        }

                        parameters.SetBinaryArray(entryName, array);
                    }
                    break;
                case Parameters.EntryType.NESTED_PARAMETERS:
                    {
                        Parameters nested = new Parameters();
                        deserializeFromBytes(nested, bytes);
                        parameters.SetNestedParameters(entryName, nested);
                    }
                    break;
                case Parameters.EntryType.NESTED_PARAMETERS_ARRAY:
            	     {
                        int arrayLength = consumeInt(bytes);
	                    Parameters[] nested = new Parameters[arrayLength];
                        for (int j = 0; j != arrayLength; ++j) {
                    	    nested[j] = new Parameters();
                    	    deserializeFromBytes(nested[j], bytes);
                        }
	                    parameters.SetNestedArray(entryName, nested);
	                }
            	    break;
                }
            }
        }

        // places an int value in the given buffer
        internal static void storeInt(byte[] buf, int from, int value)
        {
            buf[from + 0] = (byte)(value & 0xff);
            buf[from + 1] = (byte)((value & 0xff00) >> 8);
            buf[from + 2] = (byte)((value & 0xff0000) >> 16);
            buf[from + 3] = (byte)((value & 0xff000000) >> 24);
        }

        // places an integer in the buffer
        private static void appendInt(BuffersState bufState, int value)
        {
            byte[] buf = bufState.buffers[bufState.bufferIndex];
            storeInt(buf, bufState.byteIndex, value);
            bufState.byteIndex += SIZE_OF_INT;

            bufState.checkIndices();
        }

        // places a long in the buffer
        private static void appendLong(BuffersState bufState, long value)
        {
            appendInt(bufState, (int)(value & 0xffffffffL));
            appendInt(bufState, (int)(value >> 32));
        }

        // places a double in the buffer
        private static void appendDouble(BuffersState bufState, 
            double value)
        {
            long longBits = BitConverter.DoubleToInt64Bits(value);
            appendLong(bufState, longBits);
        }

        // places a string in the buffer
        private static void appendString(BuffersState bufState, 
            List<byte[]> stringCache)
        {
            byte[] valueAsBytes = stringCache[0];
            stringCache.RemoveAt(0);

            //  first place the length of string
            appendInt(bufState, valueAsBytes.Length);

            // then the content itself, but padded to full 4-byte word
            appendBytes(bufState, valueAsBytes);
        }

        // places a binary object in the buffer
        private static void appendBinary(BuffersState bufState, 
            byte[] value)
        {
            // first place the length of binary buffer
            appendInt(bufState, value.Length);

            // then the content itself, but padded to full 4-byte word
            appendBytes(bufState, value);
        }

        // places a byte array, but without bounds
        private static void appendBytes(BuffersState bufState, 
            byte[] array)
        {
            int currentPos = 0;
            int bytesLeft = array.Length;
            while(bytesLeft >= SIZE_OF_INT)
            {
                byte[] buf = bufState.buffers[bufState.bufferIndex];

                int contiguousBlockSize =
                    Math.Min(
                        buf.Length - bufState.byteIndex, bytesLeft & ~0x03
                    );

                for(int i = 0; i != contiguousBlockSize; ++i)
                {
                    buf[bufState.byteIndex + i] = array[currentPos + i];
                }
                currentPos += contiguousBlockSize;
                bytesLeft -= contiguousBlockSize;
                bufState.byteIndex += contiguousBlockSize;

                bufState.checkIndices();
            }

            if(bytesLeft != 0)
            {
                // padding required

                byte[] buf = bufState.buffers[bufState.bufferIndex];

                for(int i = 0; i != bytesLeft; ++i)
                {
                    buf[bufState.byteIndex + i] = array[currentPos + i];
                }
                bufState.byteIndex += SIZE_OF_INT;

                bufState.checkIndices();
            }
        }

        // rounds up for aligning to the 4-byte word boundary
        private static int roundTo4(int ival)
        {
            return ((ival + 3) & ~3);
        }

        // converts from entry type to type code
        private static int typeToCode(Parameters.EntryType type)
        {
            int code = 0;
            switch(type)
            {
            case Parameters.EntryType.BOOLEAN:
                code = 1;
                break;
            case Parameters.EntryType.INTEGER:
                code = 2;
                break;
            case Parameters.EntryType.LONG:
                code = 3;
                break;
            case Parameters.EntryType.DOUBLE:
                code = 4;
                break;
            case Parameters.EntryType.STRING:
                code = 5;
                break;
            case Parameters.EntryType.BINARY:
                code = 6;
                break;
            case Parameters.EntryType.BOOLEAN_ARRAY:
                code = 7;
                break;
            case Parameters.EntryType.INTEGER_ARRAY:
                code = 8;
                break;
            case Parameters.EntryType.LONG_ARRAY:
                code = 9;
                break;
            case Parameters.EntryType.DOUBLE_ARRAY:
                code = 10;
                break;
            case Parameters.EntryType.STRING_ARRAY:
                code = 11;
                break;
            case Parameters.EntryType.BINARY_ARRAY:
                code = 12;
                break;
            case Parameters.EntryType.NESTED_PARAMETERS:
                code = 13;
                break;
            case Parameters.EntryType.NESTED_PARAMETERS_ARRAY:
                code = 14;
                break;
            }

            return code;
        }

        // converts from type code to entry type
        private static Parameters.EntryType codeToType(int code)
        {
            Parameters.EntryType type;
            switch(code)
            {
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
        internal static int readInt(byte[] buf, int from)
        {
            int value = buf[from] & 0xff;
            value |= (buf[from + 1] & 0xff) << 8;
            value |= (buf[from + 2] & 0xff) << 16;
            value |= (buf[from + 3] & 0xff) << 24;

            return value;
        }

        // comsumes integer value from buffers
        private static int consumeInt(ByteIterator bytes)
        {
            byte[] buf = {
            bytes.next(),
            bytes.next(),
            bytes.next(),
            bytes.next()
        };
            return readInt(buf, 0);
        }

        // reads long value from buffers
        private static long consumeLong(ByteIterator bytes)
        {
            long lower = consumeInt(bytes);
            long lowerMasked = (lower & 0x00000000ffffffffL);
            return lowerMasked + ((long)consumeInt(bytes) << 32);
        }

        // reads double value from the buffer with given endianness
        private static double consumeDouble(ByteIterator bytes)
        {
            long longBits = consumeLong(bytes);
            return BitConverter.Int64BitsToDouble(longBits);
        }

        // reads a byte array of given length
        private static byte[] consumeBytes(ByteIterator bytes, int length)
        {
            int paddedLength = roundTo4(length);

            byte[] buf = new byte[length];
            for(int i = 0; i != length; ++i)
            {
                buf[i] = bytes.next();
            }

            // consume the remaining padding bytes (if any)
            for(int i = length; i != paddedLength; ++i)
            {
                bytes.next();
            }

            return buf;
        }

        // reads a string object
        private static string consumeString(ByteIterator bytes)
        {
            int length = consumeInt(bytes);
            byte[] buf = consumeBytes(bytes, length);
            try
            {
                return encoding.GetString(buf);
            }
            catch(DecoderFallbackException e)
            {
                throw new UnexpectedValueException(e.Message);
            }
        }
        // reads a binary object
        private static byte[] consumeBinary(ByteIterator bytes)
        {
            int length = consumeInt(bytes);
            byte[] buf = consumeBytes(bytes, length);
            return buf;
        }
    }
}
