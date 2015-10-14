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
    /// Exception class, used to indicate that the value used to 
    /// serialize or deserialize data cannot be properly processed.
    /// </summary>
    public class UnexpectedValueException : ExceptionBase
    {
        /// <summary>
        /// Initializes a new instace of the 
        /// <see cref="UnexpectedValueException"/> class
        /// </summary>
        /// <param name="details">details of the thrown exception</param>
        public UnexpectedValueException(string details)
            : base("Unexpected value: " + details)
        {
        }
    }

}
