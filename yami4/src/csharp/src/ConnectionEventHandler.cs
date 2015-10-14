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
    /// Provides data for notification the user code about the change 
    /// in the set of connections.
    /// </summary>
    /// <seealso cref="Agent.ConnectionsChanged"/>
    public class ConnectionEventArgs : System.EventArgs
    {
        /// <summary>
        /// Type of connection event.
        /// </summary>
        public enum ConnectionEvent
        {
            /// <summary>
            /// New incoming connection reported
            /// </summary>
            /// 
            NEW_INCOMING_CONNECTION,
            /// <summary>
            /// New outgoing connection reported
            /// </summary>
            NEW_OUTGOING_CONNECTION,

            /// <summary>
            /// Closing connection repoted
            /// </summary>
            CONNECTION_CLOSED
        }

        /// <summary>
        /// Creates new instance of <see cref="ConnectionEventArgs"/> class
        /// </summary>
        /// <param name="name">name of the reported connnection</param>
        /// <param name="eventType">kind of the event</param>
        public ConnectionEventArgs(string name, ConnectionEvent eventType)
        {
            this.name = name;
            this.eventType = eventType;
        }
        
        private string name;
        
        /// <summary>
        /// Gets the name of the reported connection
        /// </summary>
        public string Name
        {
            get
            {
                return name;
            }
        }

        private ConnectionEvent eventType;

        /// <summary>
        /// Gets the connection event type
        /// </summary>
        public ConnectionEvent Event
        {
            get
            {
                return eventType;
            }
        }
    }

    /// <summary>
    /// Representes the method that will handle the 
    /// <see cref="Agent.ConnectionsChanged"/> event
    /// </summary>
    /// <param name="sender">The source of the event 
    /// (always <see cref="Agent"/> object)</param>
    /// <param name="args"><see cref="ConnectionEventArgs"/> object
    /// that contains event data</param>
    /// <seealso cref="ConnectionEventArgs"/>
    public delegate void ConnectionEventHandler(
        object sender, ConnectionEventArgs args);

}	// end namespace