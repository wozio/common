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
    /// Provides data for <see cref="IncomingMessageHandler"/>
    /// implementation.
    /// </summary>
    public class IncomingMessageArgs : System.EventArgs
    {
        private IncomingMessage message;

        /// <summary>
        /// Gets received message
        /// </summary>
        /// <value>Received message</value>
        public IncomingMessage Message
        {
            get
            {
                return message;
            }
        }
        
        private string objectName;
        /// <summary>
        /// Gets destination object name
        /// </summary>
        /// <value>Destination object name</value>
        public string ObjectName
        {
            get
            {
                return objectName;
            }
        }

        /// <summary>
        /// Initializes a new instance of the 
        /// <see cref="IncomingMessageArgs"/> class
        /// </summary>
        /// <param name="objectName">Destination object name</param>
        /// <param name="message">Incoming message</param>
        public IncomingMessageArgs(
            string objectName, IncomingMessage message)
        {
            this.objectName = objectName;
            this.message = message;
        }
    }

    /// <summary>
    /// Representes the method responsible for handling incoming messages.
    /// Methods of this type are registered as message handlers via 
    /// <see cref="Agent.RegisterObject"/>.
    /// </summary>
    /// <param name="sender">The source of the event 
    /// (always <see cref="Agent"/> object)</param>
    /// <param name="args"><see cref="IncomingMessageArgs"/> object
    /// that contains event data</param>
    public delegate void IncomingMessageHandler(object sender,
        IncomingMessageArgs args);

}