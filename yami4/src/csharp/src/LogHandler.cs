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
    /// Provides data for logging events. Used by <see cref="Agent.Log"/>
    /// event to provide data for logging.</summary>
    /// <seealso cref="LogHandler"/>
    /// <seealso cref="Agent.Log"/>
    public class LogEventArgs : System.EventArgs
    {
        /// <summary>
        /// Logging level
        /// </summary>
        public enum LogLevel
        {
            /// <summary>
            /// Low logging level - creating/destroing 
            /// agents, channels and listeners
            /// </summary>
            LOW,
            /// <summary>
            /// Medium logging level - as <see cref="LOW"/> 
            /// but also events related to object and messages
            /// (registering/unreginstering objects, sending/receiving
            /// messages, replies and rejections)
            /// </summary>
            MEDIUM,
            /// <summary>
            /// High logging level - as <see cref="MEDIUM"/>
            /// but also events related to frame transmission
            /// </summary>
            HIGH
        }

        /// <summary>
        /// Initializes a new instace of the 
        /// <see cref="LogEventArgs"/> class
        /// </summary>
        /// <param name="level">logging level</param>
        /// <param name="message">message to log</param>
        public LogEventArgs(LogLevel level, string message)
        {
            this.message = message;
            this.level = level;
        }

        private LogLevel level;

        /// <summary>
        /// Gets the logging level for event
        /// </summary>
        public LogLevel Level
        {
            get
            {
                return level;
            }
        }

        private string message;

        /// <summary>
        /// Gets the message to log
        /// </summary>
        public string Message
        {
            get
            {
                return message;
            }
        }
    }

    /// <summary>
    /// Representes the method that will handle the <see cref="Agent.Log"/>
    /// event
    /// </summary>
    /// <param name="sender">The source of the event 
    /// (always <see cref="Agent"/> object)</param>
    /// <param name="args"><see cref="LogEventArgs"/> object
    /// that contains event data</param>
    /// <seealso cref="Agent.Log"/>
    /// <seealso cref="LogEventArgs"/>
    public delegate void LogHandler(object sender, LogEventArgs args);
}