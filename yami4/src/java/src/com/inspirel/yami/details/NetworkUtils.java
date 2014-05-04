// Copyright Maciej Sobczak 2008-2014.
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

package com.inspirel.yami.details;

import com.inspirel.yami.BadProtocolException;
import com.inspirel.yami.LogCallback;
import com.inspirel.yami.UnexpectedValueException;

import java.io.IOException;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.UnresolvedAddressException;

public final class NetworkUtils {
    
    static final String tcpPrefix = "tcp://";
    static final String udpPrefix = "udp://";
    
    static boolean protocolIsTcp(String target) {
        return target.startsWith(tcpPrefix);
    }
    
    static boolean protocolIsUdp(String target) {
        return target.startsWith(udpPrefix);
    }

    private static class IpComponents {
        final String hostName;
        final int port;

        IpComponents(String hostName, int port) {
            this.hostName = hostName;
            this.port = port;
        }
    }
    
    static IpComponents parseTcp(String target) {
        // assume target was already recognized as TCP
        
        return parseIp(target.substring(tcpPrefix.length()));
    }

    static IpComponents parseUdp(String target) {
        // assume target was already recognized as UDP

        return parseIp(target.substring(udpPrefix.length()));
    }

    static IpComponents parseIp(String targetName) {

        String hostName;
        String portAsString;
        
        int colonIndex = targetName.indexOf(":");
        if (colonIndex == -1) {
            // if there is no host:port separator,
            // assume host to be wildcard
            
            hostName = "*";
            portAsString = targetName;
        } else {
            hostName = targetName.substring(0, colonIndex);
            portAsString = targetName.substring(colonIndex + 1);
        }
        
        int port;
        if (portAsString.equals("*")) {
            port = 0;
        } else {
            try {
                port = Integer.parseInt(portAsString);
            } catch (NumberFormatException ex) {
                throw new BadProtocolException(targetName);
            }
        }
        
        return new IpComponents(hostName, port);
    }
    
    static void configureTcpChannel(SocketChannel channel, Options options)
            throws IOException {
        
        channel.configureBlocking(false);
        channel.socket().setTcpNoDelay(options.tcpNoDelay);
        channel.socket().setKeepAlive(options.tcpKeepAlive);
    }

    /**
     * @param options not currently used  
     */
    static void configureUdpChannel(DatagramChannel channel, Options options)
            throws IOException {

        channel.configureBlocking(false);
    }

    static class TransportChannel {
        // non-null for TCP channels
        final SocketChannel connectedChannel;

        // nnon-null for UDP channels
        final DatagramChannel datagramChannel;
        final InetSocketAddress targetAddress;

        TransportChannel(SocketChannel connectedChannel) {
            this.connectedChannel = connectedChannel;
            this.datagramChannel = null;
            this.targetAddress = null;
        }

        TransportChannel(DatagramChannel datagramChannel,
                InetSocketAddress targetAddress) {
            this.connectedChannel = null;
            this.datagramChannel = datagramChannel;
            this.targetAddress = targetAddress;
        }

        SelectionKey register(Selector selector, int operations)
                throws ClosedChannelException {
            
            if (connectedChannel != null) {
                return connectedChannel.register(selector, operations);
            } else {
                return datagramChannel.register(selector, operations);
            }
        }

        void close() throws IOException {
            if (connectedChannel != null) {
                connectedChannel.close();
            } else {
                datagramChannel.close();
            }
        }
    }

    static TransportChannel connectTcp(String target, Options options)
            throws IOException {

        IpComponents tcpComponents = parseTcp(target);
        
        try {
            SocketChannel connection = SocketChannel.open();

            // work-around for the JDK bug - file descriptor can be leaked
            // if close is not called explicitly in the case of unresolved
            // address
            
            Selector selector = null;
            try {
                // perform non-blocking connect
                
                connection.configureBlocking(false);
                
                connection.connect(new InetSocketAddress(
                        tcpComponents.hostName, tcpComponents.port));
                
                selector = Selector.open();
                SelectionKey selKey = connection.register(
                        selector, SelectionKey.OP_CONNECT);
                selector.select(options.tcpConnectTimeout);
                connection.finishConnect();
                selKey.cancel();
                selector.selectNow();
                
            } catch (Exception e) {
                connection.close();
                throw e;
            } finally {
                if (selector != null) {
                    selector.close();
                }
            }
            
            configureTcpChannel(connection, options);
            
            return new TransportChannel(connection);

        } catch (UnresolvedAddressException ex) {
            // this exception has empty message, so is translated explicitly
            throw new IOException("Unresolved address.");
        } catch (Exception ex) {
            throw new IOException(ex.getMessage());
        }
    }
    
    static TransportChannel createUdp(String target, Options options)
            throws IOException {

        IpComponents udpComponents = parseUdp(target);

        try {
            DatagramChannel ch = DatagramChannel.open();

            configureUdpChannel(ch, options);

            // remember the target address, so that it can be used
            // for each sent message
            InetSocketAddress address = new InetSocketAddress(
                    udpComponents.hostName, udpComponents.port);

            return new TransportChannel(ch, address);
            
        } catch (UnresolvedAddressException ex) {
            // this exception has empty message, so is translated explicitly
            throw new IOException("Unresolved address.");
        } catch (Exception ex) {
            throw new IOException(ex.getMessage());
        }
    }

    static String formatTcpTarget(String hostName, int port) {
        return tcpPrefix + hostName + ":" + port;
    }
    
    static String formatUdpTarget(String hostName, int port) {
        return udpPrefix + hostName + ":" + port;
    }
    
    public static int getPreferredFrameSize(Options options, String target) {
        if (protocolIsTcp(target)) {
            return options.tcpFrameSize;
        }
        
        if (protocolIsUdp(target)) {
            return options.udpFrameSize;
        }
        
        throw new UnexpectedValueException("unknown protocol");
    }

    public static Listener prepareServer(String target,
            IncomingMessageDispatchCallback incomingMessageDispatchCallback,
            Options options,
            LogCallback logCallback, LogCallback.LogLevel logLevel)
            throws IOException {

        if (protocolIsTcp(target)) {
            return prepareTcpServer(target, incomingMessageDispatchCallback,
                    options, logCallback, logLevel);
        } else if (protocolIsUdp(target)) {
            return prepareUdpServer(target, incomingMessageDispatchCallback,
                    options, logCallback, logLevel);
        } else {
            throw new BadProtocolException(target);
        }
    }

    private static TcpListener prepareTcpServer(String target,
            IncomingMessageDispatchCallback incomingMessageDispatchCallback,
            Options options,
            LogCallback logCallback, LogCallback.LogLevel logLevel)
            throws IOException {
        
        IpComponents tcpComponents = parseTcp(target);
        
        String hostName = tcpComponents.hostName;
        int port = tcpComponents.port;
        
        ServerSocketChannel channel = ServerSocketChannel.open();
        ServerSocket s = channel.socket();

        InetSocketAddress address;
        String boundHostAddress;
        if (hostName.equals("*")) {
            // bind to the wildcard local address
            // and resolve to the local hostname
            address = new InetSocketAddress(port);
            boundHostAddress = InetAddress.getLocalHost().getHostAddress();
        } else {
            address = new InetSocketAddress(hostName, port);
            boundHostAddress = hostName;
        }
                
        s.setReuseAddress(options.tcpReuseAddress);
        channel.configureBlocking(false);
        
        s.bind(address, options.tcpListenBacklog);
        
        // get the actual address of this server socket
        
        int boundPort = s.getLocalPort();
        
        String resolvedTarget = formatTcpTarget(boundHostAddress, boundPort);
        
        return new TcpListener(channel, resolvedTarget,
                incomingMessageDispatchCallback, options,
                logCallback, logLevel);
    }

    private static UdpListener prepareUdpServer(String target,
            IncomingMessageDispatchCallback incomingMessageDispatchCallback,
            Options options,
            LogCallback logCallback, LogCallback.LogLevel logLevel)
            throws IOException {

        IpComponents udpComponents = parseUdp(target);

        String hostName = udpComponents.hostName;
        int port = udpComponents.port;

        DatagramChannel channel = DatagramChannel.open();
        DatagramSocket s = channel.socket();

        InetSocketAddress address;
        String boundHostAddress;
        if (hostName.equals("*")) {
            // bind to the wildcard local address
            // and resolve to the local hostname
            address = new InetSocketAddress(port);
            boundHostAddress = InetAddress.getLocalHost().getHostAddress();
        } else {
            address = new InetSocketAddress(hostName, port);
            boundHostAddress = hostName;
        }

        channel.configureBlocking(false);

        s.bind(address);

        // get the actual address of this socket

        int boundPort = s.getLocalPort();

        String resolvedTarget = formatUdpTarget(boundHostAddress, boundPort);

        return new UdpListener(channel, resolvedTarget,
                incomingMessageDispatchCallback, options,
                logCallback, logLevel);
    }
}
