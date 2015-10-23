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

using System.Net.Sockets;
using System.Net;
namespace Inspirel.YAMI.details
{
    internal class TcpListener : Listener
    {

        private readonly Socket channel;
        private readonly IncomingMessageDispatchCallback
            incomingMessageDispatchCallback;
        private readonly Options options;

        internal TcpListener(
            Socket channel, string resolvedTarget, 
            IncomingMessageDispatchCallback incomingMessageDispatchCallback, 
            Options options, 
            LogCallback logCallback, LogEventArgs.LogLevel logLevel) 
            : base(channel, resolvedTarget, logCallback, logLevel)
        {
            this.channel = channel;
            this.incomingMessageDispatchCallback = 
                incomingMessageDispatchCallback;
            this.options = options;
        }

        internal override Socket registerForSelection(Selector selector)
        {
            selector.Add(channel, Selector.Direction.ACCEPT);
            return channel;
        }

        internal override ListeningResult accept()
        {
            Socket s = channel.Accept();

            NetworkUtils.configureTcpChannel(s, options);

            IPEndPoint address = (IPEndPoint)s.RemoteEndPoint;
            string hostName = address.Address.ToString();
            int port = address.Port;

            string sourceTarget = 
                NetworkUtils.formatTcpTarget(hostName, port);

            Channel newChannel = new Channel(
                s, sourceTarget, incomingMessageDispatchCallback, 
                options, logCallback, logLevel);

            return new ListeningResult(newChannel);
        }
    }

}