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

using System;
using System.Net;
using System.Net.Security;
using System.Net.Sockets;
using System.Collections.Generic;
using System.Security.Authentication;
using System.Security.Cryptography.X509Certificates;
using System.Threading;

namespace Inspirel.YAMI.details
{
    internal static class NetworkUtils
    {
        private const int WSAETIMEDOUT = 10060;
        private const int WSAEWOULDBLOCK = 10035;

        internal class Buffer
        {
            private byte[] area;
            private int used;
            private int ip;
            private int ig;
            private bool eof;

            internal Buffer(int size)
            {
                area = new byte[size];
                used = 0;
                ip = 0;
                ig = 0;
                eof = false;
            }

            private bool waitForPlace(int needSize)
            {
                while (area != null && (used + needSize >= area.Length))
                {
                    try
                    {
                        Monitor.Wait(this);
                    }
                    catch (System.Exception)
                    {
                        // ignore
                    }
                }

                return area != null;
            }

            private void doPut(byte[] buf, int from, int to)
            {
                for (int i = from; i != to; ++i)
                {
                    area[ip] = buf[i];
                    ++ip;
                    ++used;
                    if (ip == area.Length)
                    {
                        ip = 0;
                    }
                }
            }

            // returns true if the buffer was empty and new data was put into it
            // and null if no data was inserted (due to close)
            internal bool? Put(byte[] buf)
            {
                lock (this)
                {
                    if (buf == null) {
                        eof = true;
                        
                        return null;
                    }
                    
                    bool wasEmpty = used == 0;
                    
                    bool hasPlace = waitForPlace(buf.Length);
                    
                    if (hasPlace) {
                        doPut(buf, 0, buf.Length);
                        
                        Monitor.Pulse(this);
                        
                        return wasEmpty;
                    } else {
                        return null;
                    }
                }
            }

            internal int PutMany(IList<ArraySegment<byte>> buffers)
            {
                lock (this)
                {
                    bool wasEmpty = used == 0;

                    int totalSize = 0;
                    foreach (ArraySegment<byte> buf in buffers)
                    {
                        totalSize += buf.Count;
                    }

                    bool hasPlace = waitForPlace(totalSize);

                    if (hasPlace)
                    {
                        foreach (ArraySegment<byte> buf in buffers)
                        {
                            doPut(buf.Array, buf.Offset, buf.Offset + buf.Count);
                        }

                        Monitor.Pulse(this);

                        return totalSize;
                    }
                    else
                    {
                        return 0;
                    }
                }
            }

            internal void Close()
            {
                lock (this)
                {
                    area = null;
                    used = 0;
                
                    Monitor.Pulse(this);
                }
            }

            internal bool HasDataOrEOF()
            {
                lock (this)
                {
                    return area == null || eof || used != 0;
                }
            }

            private bool waitForData()
            {
                while (area != null && eof == false && used == 0)
                {
                    try
                    {
                        Monitor.Wait(this);
                    }
                    catch (System.Exception)
                    {
                        // ignore
                    }
                }

                return area != null && eof == false;
            }

            private int doGet(byte[] buf, int from, int to)
            {
                bool hasData = waitForData();

                if (hasData)
                {
                    int toRead = Math.Min(to - from, used);

                    for (int i = from; i != from + toRead; ++i)
                    {
                        buf[i] = area[ig];
                        ++ig;
                        --used;
                        if (ig == area.Length)
                        {
                            ig = 0;
                        }
                    }

                    Monitor.Pulse(this);

                    return toRead;
                }

                return -1;
            }

            // returns available block or waits for minimum size of data (4)
            // or returns null if buffer was closed
            internal byte[] Get() {
                lock (this)
                {
                    int sizeOfSingleBlock =
                        Math.Max(Math.Min(used, area.Length - ig), 4);

                    byte[] buf = new byte[sizeOfSingleBlock];
                    int readn = doGet(buf, 0, sizeOfSingleBlock);
                
                    if (readn == sizeOfSingleBlock) {
                        return buf;
                    } else {
                        return null;
                    }
                }
            }

            internal int Receive(byte[] buf, int offset, int toRead)
            {
                lock (this)
                {
                    int sizeOfBlock =
                        Math.Min(Math.Min(used, area.Length - ig), toRead);

                    int readn = doGet(buf, offset, offset + sizeOfBlock);

                    return readn;
                }
            }
        }

        internal const string tcpPrefix = "tcp://";
        internal const string tcpsPrefix = "tcps://";
        internal const string udpPrefix = "udp://";

        internal static bool protocolIsTcp(string target)
        {
            return target.StartsWith(tcpPrefix);
        }

        internal static bool protocolIsTcps(string target)
        {
            return target.StartsWith(tcpsPrefix);
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

        internal static IpComponents parseTcps(string target)
        {
            // assume target was already recognized as TCPs

            return parseIp(target.Substring(tcpsPrefix.Length));
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

            // non-null for UDP channels
            internal readonly Socket datagramChannel;
            internal readonly IPEndPoint targetAddress;

            // non-null for blocking SSL sockets
            internal readonly SslStream ssl;
            internal readonly Buffer readingQueue;
            internal readonly Thread socketReader;
            internal readonly Buffer writingQueue;
            internal readonly Thread socketWriter;
            internal readonly IOWorker ioWorker;
            internal readonly int frameSize;

            internal class SocketReader
            {
                private TransportChannel outer;

                internal SocketReader(TransportChannel outer)
                {
                    this.outer = outer;
                }

                public void run()
                {
                    while (true)
                    {
                        byte[] bytes = null;
                        int readn;
                        try
                        {
                            int toRead = 1024;
                            byte[] buf = new byte[1024];
                            
                            readn = outer.ssl.Read(buf, 0, toRead);

                            if (readn > 0)
                            {
                                bytes = new byte[readn];
                                System.Buffer.BlockCopy(buf, 0, bytes, 0, readn);
                            }
                        }
                        catch (Exception)
                        {
                            outer.readingQueue.Close();
                            outer.writingQueue.Close();

                            return;
                        }

                        if (bytes == null)
                        {
                            // EOF

                            outer.readingQueue.Put(null);

                            return;
                        }

                        bool? stored = outer.readingQueue.Put(bytes);
                        if (stored.HasValue == false)
                        {
                            return;
                        }
                        else
                        {
                            if (stored.Value)
                            {
                                outer.ioWorker.wakeup();
                            }
                        }
                    }
                }
            }

            internal class SocketWriter
            {
                private TransportChannel outer;

                internal SocketWriter(TransportChannel outer)
                {
                    this.outer = outer;
                }

                public void run()
                {
                    while (true)
                    {
                        byte[] bytes = outer.writingQueue.Get();
                        if (bytes == null)
                        {
                            outer.ssl.Close();

                            return;
                        }

                        try
                        {
                            outer.ssl.Write(bytes, 0, bytes.Length);
                        }
                        catch (Exception)
                        {
                            outer.writingQueue.Close();

                            return;
                        }
                    }
                }
            }

            internal TransportChannel(Socket connectedChannel)
            {
                this.connectedChannel = connectedChannel;
                this.datagramChannel = null;
                this.targetAddress = null;

                this.ssl = null;
                this.readingQueue = null;
                this.socketReader = null;
                this.writingQueue = null;
                this.socketWriter = null;
                this.ioWorker = null;
                frameSize = 0;
            }

            internal TransportChannel(SslStream ssl, int bufferSize, IOWorker worker)
            {
                this.connectedChannel = null;
                this.datagramChannel = null;
                this.targetAddress = null;

                this.ssl = ssl;

                this.readingQueue = new Buffer(bufferSize * 2);
                this.writingQueue = new Buffer(bufferSize * 2);
                this.frameSize = bufferSize;

                this.ioWorker = worker;

                this.socketReader = new Thread(new SocketReader(this).run);
                this.socketReader.Name = "YAMI4 SSL Reader";
                this.socketReader.IsBackground = true;
                this.socketReader.Start();

                this.socketWriter = new Thread(new SocketWriter(this).run);
                this.socketWriter.Name = "YAMI4 SSL Writer";
                this.socketWriter.IsBackground = true;
                this.socketWriter.Start();
            }

            internal TransportChannel(Socket datagramChannel, 
                IPEndPoint targetAddress)
            {
                this.connectedChannel = null;
                this.datagramChannel = datagramChannel;
                this.targetAddress = targetAddress;

                this.ssl = null;
                this.readingQueue = null;
                this.socketReader = null;
                this.writingQueue = null;
                this.socketWriter = null;
                this.ioWorker = null;
                frameSize = 0;
            }

            internal virtual Socket register(Selector selector,
                Selector.Direction operations, bool useBlockingOnly)
            {
                if (useBlockingOnly)
                {
                    return null;
                }
                
                if (connectedChannel != null)
                {
                    selector.Add(connectedChannel, operations);
                    return connectedChannel;
                }
                else if (datagramChannel != null)
                {
                    selector.Add(datagramChannel, operations);
                    return datagramChannel;
                }
                else
                {
                    return null;
                }
            }

            internal virtual void close()
            {
                if (connectedChannel != null)
                {
                    connectedChannel.Close();
                }
                else if (datagramChannel != null)
                {
                    datagramChannel.Close();
                }
                else
                {
                    ssl.Close();
                    readingQueue.Close();
                    writingQueue.Close();

                    try
                    {
                        socketReader.Join();
                        socketWriter.Join();
                    }
                    catch (System.Exception)
                    {
                        // ignore
                    }
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

        internal static TransportChannel connectTcps(string target,
            Options options, IOWorker ioWorker)
        {
            IpComponents tcpComponents = parseTcps(target);

            TcpClient tcpClient = new System.Net.Sockets.TcpClient();

            Int32 timeout;
            if (options.tcpConnectTimeout > 0)
            {
                timeout = options.tcpConnectTimeout;
            }
            else
            {
                timeout = Timeout.Infinite;
            }

            IAsyncResult result = tcpClient.BeginConnect(tcpComponents.hostName, tcpComponents.port, null, null);
            bool connectionAttempted = result.AsyncWaitHandle.WaitOne(timeout);

            if (!connectionAttempted || !tcpClient.Connected)
            {
                throw new YAMIIOException("Cannot create SSL connection");
            }

            SslStream ssl = new SslStream(tcpClient.GetStream(), false,
                new RemoteCertificateValidationCallback(
                    (object sender, X509Certificate cert, X509Chain chain, System.Net.Security.SslPolicyErrors errors) =>
                {
                    return true;
                })
            );

            try
            {
                ssl.AuthenticateAsClient(tcpComponents.hostName, new X509CertificateCollection(),
                    System.Security.Authentication.SslProtocols.Default, false);
            }
            catch (AuthenticationException)
            {
                throw new YAMIIOException("SSL authentication error");
            }

            tcpClient.NoDelay = options.tcpNoDelay;
            tcpClient.Client.SetSocketOption(
                SocketOptionLevel.Socket, 
                SocketOptionName.KeepAlive, 
                options.tcpKeepAlive ? 1 : 0);

            return new TransportChannel(ssl, options.tcpFrameSize, ioWorker);
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
            if (protocolIsTcp(target) || protocolIsTcps(target))
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
