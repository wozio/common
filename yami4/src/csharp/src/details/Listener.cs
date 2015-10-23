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

using System.IO;
using System.Net.Sockets;
namespace Inspirel.YAMI.details
{
    internal abstract class Listener
    {

        public class ListeningResult
        {

            public ListeningResult(Channel ch)
            {
                this.channel = ch;
            }

            public ListeningResult(string target, MemoryStream buffer)
            {
                this.target = target;
                this.buffer = buffer;
            }

        // non-null if the result of accept is a new channel
            public Channel channel;

        // non-null if the result is a frame ready to inject
            public string target;
            public MemoryStream buffer;
        }

        private readonly Socket channel;
        private readonly string resolvedTarget;
        //protected internal Socket lastUsedSelectionKey;

        internal readonly LogCallback logCallback;
        internal readonly LogEventArgs.LogLevel logLevel;

        internal Listener(
            Socket channel, string resolvedTarget, 
            LogCallback logCallback, LogEventArgs.LogLevel logLevel)
        {

            this.channel = channel;
            this.resolvedTarget = resolvedTarget;

            this.logCallback = logCallback;
            this.logLevel = logLevel;
        }

        public virtual void close()
        {
            try
            {
                //if (lastUsedSelectionKey != null)
                //{
                //    lastUsedSelectionKey.cancel();
                //}
                channel.Close();
            }
            catch (System.Exception)
            {
            // ignore
            }
        }

        public virtual string ResolvedTarget
        {
            get
            {
                return resolvedTarget;
            }
        }

        // to be defined by concrete listener
        internal abstract ListeningResult accept();

        // to be defined by concrete listener
        internal abstract Socket registerForSelection(Selector selector);
    }

}