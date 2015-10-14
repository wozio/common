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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Serializable wrapper for the raw binary data.
 *
 * Serializable wrapper for the raw binary data allows to use
 * already serialized content for message sending.
 * The two major use cases for this class are efficient message forwarding
 * (when the message is received and its content is used for another message)
 * and support for custom data models and serializers.
 */
public class RawBinaryDataSource implements YAMISerializable {

    private final byte[] buffer;
    
    /**
     * Constructor.
     * 
     * Wraps the given binary buffer.
     * 
     * @param buffer binary buffer to be wrapped
     */
    public RawBinaryDataSource(byte[] buffer) {
        if (buffer.length % 4 != 0) {
            throw new UnexpectedValueException(
                "binary buffer should have a size that is multiple of 4.");
        }
        
        this.buffer = buffer;
    }
    
    /**
     * Serializes the content of this object.
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
        final int numOfChunks = (buffer.length - 1) / chunkSize + 1;
        List<byte[]> result = new ArrayList<byte[]>(numOfChunks);
        int position = 0;
        int left = buffer.length;
        for (int i = 0; i != numOfChunks; ++i) {
            int sizeOfThisChunk = left < chunkSize ? left : chunkSize;
            result.add(Arrays.copyOfRange(
                    buffer, position, position + sizeOfThisChunk));
            position += sizeOfThisChunk;
            left -= sizeOfThisChunk;
        }
        return result;
    }
}
