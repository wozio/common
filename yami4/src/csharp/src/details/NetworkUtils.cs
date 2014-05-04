// Copyright Paweł Kierski 2010, 2014.
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

using System;
using System.Net.Sockets;
using System.Net;
using System.Collections.Generic;

namespace Inspirel.YAMI.details
{
    internal static class NetworkUtils
    {
        private const int WSAETIMEDOUT = 10060;
        private const int WSAEWOULDBLOCK = 10035;

        internal const string tcpPrefix = "tcp://";
        internal const string udpPrefix = "udp://";

        internal static bool protocolIsTcp(string target)
        {
            return target.StartsWith(tcpPrefix);
        }

        internal static bool protocolIsUdp(string target)
        {
            return target.StartsWith(udpPrefix);
        }

        internal static Socket CreateTCPSocket()
        {
            return new Socket(AddressFamily.InterNetwork,
                SocketType.Stream, ProtocolType.Tcp);
        }

        internal static Socket CreateUDPSocket()
        {
            return new Socket(AddressFamily.InterNetwork,
                SocketType.Dgram, ProtocolType.Udp);
        }

        internal class IpComponents
        {
            internal readonly string hostName;
            internal readonly int port;

            internal IpComponents(string hostName, int port)
            {
                this.hostName = hostName;
                this.port = port;
            }
        }

        internal static IpComponents parseTcp(string target)
        {
        // assume target was already recognized as TCP

            return parseIp(target.Substring(tcpPrefix.Length));
        }

        internal static IpComponents parseUdp(string target)
        {
        // assume target was already recognized as UDP

            return parseIp(target.Substring(udpPrefix.Length));
        }

        internal static IpComponents parseIp(string targetName)
        {

            string hostName;
            string portAsString;

            int colonIndex = targetName.IndexOf(":");
            if (colonIndex == -1)
            {
            // if there is no host:port separator,
            // assume host to be wildcard

                hostName = "*";
                portAsString = targetName;
            }
            else
            {
                hostName = targetName.Substring(0, colonIndex);
                portAsString = targetName.Substring(colonIndex + 1);
            }

            int port;
            if (portAsString.Equals("*"))
            {
                port = 0;
            }
            else
            {
                try
                {
                    port = Convert.ToInt32(portAsString);
                }
                catch(FormatException)
                {
                    throw new BadProtocolException(targetName);
                }
            }

            return new IpComponents(hostName, port);
        }

        internal static void configureTcpChannel(Socket channel, 
            Options options)
        {
            channel.Blocking = false;
            channel.NoDelay = true;
            channel.SetSocketOption(
                SocketOptionLevel.Socket, 
                SocketOptionName.KeepAlive, 
                options.tcpKeepAlive ? 1 : 0
                );
        }

        //<parameter channel is not currently used
        internal static void configureUdpChannel(Socket channel, 
            Options options)
        {
            channel.Blocking = false;
        }

        internal class TransportChannel
        {
        // non-null for TCP channels
            internal readonly Socket connectedChannel;

        // nnon-null for UDP channels
            internal readonly Socket datagramChannel;
            internal readonly IPEndPoint targetAddress;

            internal TransportChannel(Socket connectedChannel)
            {
                this.connectedChannel = connectedChannel;
                this.datagramChannel = null;
                this.targetAddress = null;
            }

            internal TransportChannel(Socket datagramChannel, 
                IPEndPoint targetAddress)
            {
                this.connectedChannel = null;
                this.datagramChannel = datagramChannel;
                this.targetAddress = targetAddress;
            }

            internal virtual Socket register(Selector selector, 
                Selector.Direction operations)
            {
                if (connectedChannel != null)
                {
                    selector.Add(connectedChannel, operations);
                    return connectedChannel;
                }
                else
                {
                    selector.Add(datagramChannel, operations);
                    return datagramChannel;
                }
            }

            internal virtual void close()
            {
                if (connectedChannel != null)
                {
                    connectedChannel.Close();
                }
                else
                {
                    datagramChannel.Close();
                }
            }
        }

        internal static TransportChannel connectTcp(string target, 
            Options options)
        {
            IpComponents tcpComponents = parseTcp(target);

            Socket connection = CreateTCPSocket();
            connection.Blocking = false;
            try
            {
                connection.Connect(
                    tcpComponents.hostName, tcpComponents.port);
            }
            catch(SocketException ex)
            {
                // ignore if operaion in progress
                if(ex.SocketErrorCode != SocketError.WouldBlock
                    && ex.SocketErrorCode != SocketError.AlreadyInProgress
                    && ex.SocketErrorCode != SocketError.IsConnected)
                    throw ex;
            }
            // convert milliseconds to microseconds expected by
            // Socket.Select()
            int timeout = options.tcpConnectTimeout * 1000;
            if(timeout == 0) // Sockets.Select needs -1 for infinite wait
                timeout = -1;

            if(!connection.Connected)
            {
                connection.Blocking = true;
                List<Socket> writeable = new List<Socket>();
                writeable.Add(connection);
                List<Socket> error = new List<Socket>();
                error.Add(connection);
                Socket.Select(null, writeable, error, timeout);
                if(writeable.Count == 0)
                    throw new SocketException(WSAETIMEDOUT);
                connection.Blocking = false;
            }

            configureTcpChannel(connection, options);

            return new TransportChannel(connection);
        }

        internal static TransportChannel createUdp(
            string target, Options options)
        {
            IpComponents udpComponents = parseUdp(target);

            Socket ch = CreateUDPSocket();

            configureUdpChannel(ch, options);

            // remember the target address, so that it can be used
            // for each sent message
            IPEndPoint address = new IPEndPoint(
                Dns.GetHostAddresses(udpComponents.hostName)[0], 
                udpComponents.port);

            return new TransportChannel(ch, address);
        }

        internal static string formatTcpTarget(string hostName, int port)
        {
            return tcpPrefix + hostName + ":" + port;
        }

        internal static string formatUdpTarget(string hostName, int port)
        {
            return udpPrefix + hostName + ":" + port;
        }

        internal static int getPreferredFrameSize(
            Options options, string target)
        {
            if (protocolIsTcp(target))
            {
                return options.tcpFrameSize;
            }

            if (protocolIsUdp(target))
            {
                return options.udpFrameSize;
            }

            throw new UnexpectedValueException("unknown protocol");
        }

        internal static Listener prepareServer(string target, 
            IncomingMessageDispatchCallback incomingMessageDispatchCallback, 
            Options options, 
            LogCallback logCallback, LogEventArgs.LogLevel logLevel)
        {
            if(protocolIsTcp(target))
            {
                return prepareTcpServer(target, 
                    incomingMessageDispatchCallback, options, 
                    logCallback, logLevel);
            }
            else if(protocolIsUdp(target))
            {
                return prepareUdpServer(target, 
                    incomingMessageDispatchCallback, options, 
                    logCallback, logLevel);
            }
            else
            {
                throw new BadProtocolException(target);
            }
        }

        private static TcpListener prepareTcpServer(string target, 
            IncomingMessageDispatchCallback incomingMessageDispatchCallback, 
            Options options, 
            LogCallback logCallback, LogEventArgs.LogLevel logLevel)
        {
            IpComponents tcpComponents = parseTcp(target);

            string hostName = tcpComponents.hostName;
            int port = tcpComponents.port;

            Socket s = CreateTCPSocket();

            IPEndPoint address;
            string boundHostName;
            if(hostName == "*")
            {
                // bind to the wildcard local address
                // and resolve to the local hostname
                address = new IPEndPoint(IPAddress.Any, port);
                boundHostName = Dns.GetHostName();
            }
            else
            {
                //TODO: translation for empty result (if possible)
                address = new IPEndPoint(
                    Dns.GetHostAddresses(hostName)[0], port);
                boundHostName = hostName;
            }

            s.SetSocketOption(
                SocketOptionLevel.Socket, 
                SocketOptionName.ReuseAddress, 
                options.tcpReuseAddress ? 1 : 0);
            s.Blocking = false;

            s.Bind(address); // , options.tcpListenBacklog);
            s.Listen(options.tcpListenBacklog);

            // get the actual address of this server socket

            int boundPort = ((IPEndPoint)s.LocalEndPoint).Port;

            string resolvedTarget = 
                formatTcpTarget(boundHostName, boundPort);

            return new TcpListener(s, resolvedTarget, 
                incomingMessageDispatchCallback, options, 
                logCallback, logLevel);
        }

        private static UdpListener prepareUdpServer(string target, 
            IncomingMessageDispatchCallback incomingMessageDispatchCallback,
            Options options, 
            LogCallback logCallback, LogEventArgs.LogLevel logLevel)
        {

            IpComponents udpComponents = parseUdp(target);

            string hostName = udpComponents.hostName;
            int port = udpComponents.port;

            Socket s = CreateUDPSocket();

            IPEndPoint address;
            string boundHostName;
            if(hostName == "*")
            {
                // bind to the wildcard local address
                // and resolve to the local hostname
                address = new IPEndPoint(IPAddress.Any, port);
                boundHostName = Dns.GetHostName();
            }
            else
            {
                //TODO: translation for empty result (if possible)
                address = new IPEndPoint(
                    Dns.GetHostAddresses(hostName)[0], port);
                boundHostName = hostName;
            }

            s.Blocking = false;

            s.Bind(address);

        // get the actual address of this socket

            int boundPort = ((IPEndPoint)s.LocalEndPoint).Port;

            string resolvedTarget = 
                formatUdpTarget(boundHostName, boundPort);

            return new UdpListener(s, resolvedTarget, 
                incomingMessageDispatchCallback, options, 
                logCallback, logLevel);
        }
    }
}
