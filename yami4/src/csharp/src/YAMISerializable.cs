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
    /// <summary>
    /// Common interface for serializable entities in YAMI.
    /// </summary>
    public interface YAMISerializable
    {
        /// <summary>
        /// Serializes the content of this object.
        /// </summary>
        /// <remarks>The content is serialized into chunks of the given 
        /// size, after serialization only the last chunk can be 
        /// smaller than the requested size. </remarks>
        /// <param name="chunkSize">
        /// chunk size, should be a multiple of 4 or 
        /// <c>Integer.MAX_VALUE</c> 
        /// </param>
        /// <returns>
        /// list of buffers (chunks) with serialized data 
        /// </returns>
        /// <exception cref="Inspirel.YAMI.UnexpectedValueException">
        /// if requested chunk size is not a multiple of 4 and 
        /// is not <c>Integer.MAX_VALUE</c>
        /// </exception>
        System.Collections.Generic.List<byte[]> Serialize(int chunkSize);
    }
}