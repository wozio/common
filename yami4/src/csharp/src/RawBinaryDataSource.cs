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

namespace Inspirel.YAMI
{
    /// <summary>
    /// Serializable wrapper for the raw binary data. 
    /// </summary>
    /// <remarks>
    /// Serializable 
    /// wrapper for the raw binary data allows to use already 
    /// serialized content for message sending. The two major use 
    /// cases for this class are efficient message forwarding 
    /// (when the message is received and its content is used for 
    /// another message) and support for custom data models and 
    /// serializers.
    /// </remarks>
    public class RawBinaryDataSource : YAMISerializable
    {
        private readonly byte[] buffer;

        /// <summary>
        /// Constructor. Wraps the given binary buffer.  
        /// </summary>
        /// <param name="buffer">binary buffer to be wrapped</param>
        public RawBinaryDataSource(byte[] buffer)
        {
            if(buffer.Length % 4 != 0)
            {
                throw new UnexpectedValueException(
                    "binary buffer should have a size "
                    + "that is multiple of 4.");
            }

            this.buffer = buffer;
        }

        /// <summary>
        /// Serializes the content of this object.  
        /// <para> The content is serialized into chunks of the given 
        /// size, after serialization only the last chunk can be 
        /// smaller than the requested size. </para>  
        /// </summary>
        /// <param name="chunkSize">
        /// chunk size, should be a multiple of 4 
        /// or <c>Integer.MAX_VALUE</c> 
        /// </param>
        /// <returns>
        /// list of buffers (chunks) with serialized data 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.UnexpectedValueException">
        /// if requested chunk size is not a multiple of 4 
        /// and is not <c>Integer.MAX_VALUE</c>
        /// </exception>
        public List<byte[]> Serialize(int chunkSize)
        {
            int numOfChunks = (buffer.Length - 1) / chunkSize + 1;
            List<byte[]> result = new List<byte[]>(numOfChunks);
            int position = 0;
            int left = buffer.Length;
            for(int i = 0; i != numOfChunks; ++i)
            {
                int sizeOfThisChunk = left < chunkSize ? left : chunkSize;
                byte[] chunk = new byte[sizeOfThisChunk];
                System.Array.Copy(buffer, position, chunk, 0, buffer.Length);
                result.Add(chunk);
                position += sizeOfThisChunk;
                left -= sizeOfThisChunk;
            }
            return result;
        }
    }
}
