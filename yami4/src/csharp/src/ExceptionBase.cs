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
    /// Exception base class - all YAMI4 exception are derived
    /// from this class
    /// </summary>
    public class ExceptionBase : System.Exception
    {
        /// <summary>
        /// Initializes a new instace of the 
        /// <see cref="ExceptionBase"/> class
        /// </summary>
        /// <param name="msg">exception message</param>
        protected ExceptionBase(string msg)
            : base(msg)
        {
        }

        /// <summary>
        /// Initializes a new instace of the 
        /// <see cref="ExceptionBase"/> class
        /// </summary>
        /// <param name="msg">exception message</param>
        /// <param name="ex">inner exception</param>
        protected ExceptionBase(string msg, System.Exception ex)
            : base(msg, ex)
        {
        }
    }
}
