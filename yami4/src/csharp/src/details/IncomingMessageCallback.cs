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

namespace Inspirel.YAMI.details
{
    /// <summary> The callback interface that is used to 
    /// that the new incoming message has been received. </summary>
    internal interface IncomingMessageCallback
    {

        /// <summary> Callback function that is automatically called by the 
        /// agent whenever a new message arrives.
        /// 
        /// <para>
        /// <b>Note:</b> Implementations of this interface should be
        /// thread-safe if they are used with the agent that has more than
        /// one dispatcher thread.
        /// </para>
        /// <para>
        /// <b>Note:</b> All exceptions thrown from the user code that
        /// overrides this function are translated into rejection
        /// notifications and sent back to the source site.
        /// </para>
        /// </summary>
        /// <param name="message"> the message descriptor </param>
        void call(IncomingMessage message);
    }
}
