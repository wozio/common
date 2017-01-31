// Copyright Maciej Sobczak 2008-2015.
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
import com.inspirel.yami.YAMIIOException;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.UnresolvedAddressException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

public final class NetworkUtils {
    
    static class Buffer {
        private byte[] area;
        private int used;
        private int ip;
        private int ig;
        private boolean eof;
	
        Buffer(int size) {
            area = new byte[size];
            used = 0;
            ip = 0;
            ig = 0;
            eof = false;
        }
	
        private boolean waitForPlace(int needSize) {
            while (area != null && (used + needSize >= area.length)) {
                try {
                    this.wait();
                } catch (InterruptedException e) {
                    // ignore
                }
            }
            
            return area != null;
        }
	
        private void doPut(byte[] buf, int from, int to) {
            for (int i = from; i != to; ++i) {
                area[ip] = buf[i];
                ++ip;
                ++used;
                if (ip == area.length) {
                    ip = 0;
                }
            }
        }
	
        // returns true if the buffer was empty and new data was put into it
        // and null if no data was inserted (due to close)
        Boolean put(byte[] buf) {
            synchronized (this) {
                if (buf == null) {
                    eof = true;
                    
                    return null;
                }
		
                boolean wasEmpty = used == 0;
		
                boolean hasPlace = waitForPlace(buf.length);
		
                if (hasPlace) {
                    doPut(buf, 0, buf.length);
                    
                    this.notify();
                    
                    return wasEmpty;
                } else {
                    return null;
                }
            }
        }
        
        void write(ByteBuffer[] buffers) {
            synchronized (this) {
                boolean writtenSomething = false;
                for (int i = 0; i != buffers.length; ++i) {
                    ByteBuffer buf = buffers[i];
                    
                    byte[] a = buf.array();
                    int position = buf.position();
                    int limit = buf.limit();
                    
                    int needSize = limit - position;
                    
                    boolean hasPlace = waitForPlace(needSize);
                    
                    if (hasPlace) {
                        doPut(a, position, limit);
			
                        buf.position(limit);
			
                        writtenSomething = true;
                    } else {
                        // no need to try other buffers
                        
                        break;
                    }
                }
		
                if (writtenSomething) {
                    this.notify();
                }
            }
        }
	
        void close() {
            synchronized (this) {
                area = null;
                used = 0;
		
                this.notify();
            }
        }
        
        boolean hasDataOrEOF() {
            synchronized (this) {
                return area == null || eof || used != 0;
            }
        }
	
        private boolean waitForData() {
            while (area != null && eof == false && used == 0) {
                try {
                    this.wait();
                } catch (InterruptedException e) {
                    // ignore
                }
            }
            
            return area != null && eof == false;
        }
	
        private int doGet(byte[] buf, int from, int to) {
            boolean hasData = waitForData();
            
            if (hasData) {
                int toRead = Math.min(to - from, used);
		
                for (int i = from; i != from + toRead; ++i) {
                    buf[i] = area[ig];
                    ++ig;
                    --used;
                    if (ig == area.length) {
                        ig = 0;
                    }
                }
                
                this.notify();
		
                return toRead;
            }
            
            return -1;
        }
	
        // returns available block or waits for minimum size of data (4)
        // or returns null if buffer was closed
        byte[] get() {
            synchronized (this) {
                int sizeOfSingleBlock =
                    Math.max(Math.min(used, area.length - ig), 4);
                
                byte[] buf = new byte[sizeOfSingleBlock];
                int readn = doGet(buf, 0, sizeOfSingleBlock);
		
                if (readn == sizeOfSingleBlock) {
                    return buf;
                } else {
                    return null;
                }
            }
        }
	
        int read(ByteBuffer buf) {
            synchronized (this) {
                byte[] a = buf.array();
                int position = buf.position();
                int limit = buf.limit();
                
                int readn = doGet(a, position, limit);
                if (readn != -1) {
                    buf.position(position + readn);
                }
		
                return readn;
            }
        }
    }
    
    private static SSLSocketFactory sslSocketFactory;
    
    static final String tcpPrefix = "tcp://";
    static final String tcpsPrefix = "tcps://";
    static final String udpPrefix = "udp://";
    
    static boolean protocolIsTcp(String target) {
        return target.startsWith(tcpPrefix);
    }
    
    static boolean protocolIsTcps(String target) {
        return target.startsWith(tcpsPrefix);
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

    static IpComponents parseTcps(String target) {
        // assume target was already recognized as TCPs
        
        return parseIp(target.substring(tcpsPrefix.length()));
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

        // non-null for UDP channels
        final DatagramChannel datagramChannel;
        final InetSocketAddress targetAddress;
        
        // non-null for blocking sockets
        final Socket blockingSocket;
        final Buffer readingQueue;
        final InputStream input;
        final Thread socketReader;
        final Buffer writingQueue;
        final OutputStream output;
        final Thread socketWriter;
        final IOWorker ioWorker;
        final int frameSize;
        
        private class SocketReader implements Runnable {
        	
            @Override
            public void run() {
                while (true) {
                    byte[] bytes;
                    int readn;
                    try {
                        int toRead =
                            Math.max(Math.min(input.available(), frameSize), 4);
                        bytes = new byte[toRead];
						
                        readn = input.read(bytes);
                    } catch (IOException e) {
                        readingQueue.close();
                        writingQueue.close();
						
                        return;
                    }
					
                    if (readn == -1) {
                        // EOF
						
                        readingQueue.put(null);
						
                        return;
                    }
					
                    Boolean stored = readingQueue.put(bytes);
                    if (stored == null) {
                        return;
                    } else {
                        if (stored.booleanValue()) {
                            ioWorker.wakeup();
                        }
                    }
                }
            }
        }

        private class SocketWriter implements Runnable {
        
            @Override
            public void run() {
                while (true) {
                    byte[] bytes = writingQueue.get();
                    if (bytes == null) {
                        try {
                            output.close();
                        } catch (IOException e) {
                            // ignore
                        }
						
                        return;
                    }

                    try {
                        output.write(bytes);
                    } catch (IOException e) {
                        writingQueue.close();
						
                        return;
                    }
                }
            }
        }
        	
        TransportChannel(SocketChannel connectedChannel) {
            this.connectedChannel = connectedChannel;
            this.datagramChannel = null;
            this.targetAddress = null;
            
            blockingSocket = null;
            readingQueue = null;
            input = null;
            socketReader = null;
            writingQueue = null;
            output = null;
            socketWriter = null;
            ioWorker = null;
            frameSize = 0;
        }

        TransportChannel(Socket blockingSocket, int bufferSize, IOWorker worker)
            throws YAMIIOException {
        	
            this.connectedChannel = null;
            this.datagramChannel = null;
            this.targetAddress = null;
            
            this.blockingSocket = blockingSocket;
            
            readingQueue = new Buffer(bufferSize * 2);
            writingQueue = new Buffer(bufferSize * 2);
            frameSize = bufferSize;

            this.ioWorker = worker;
            
            try {
                input = blockingSocket.getInputStream();
                output = blockingSocket.getOutputStream();
            } catch (IOException e) {
                throw new YAMIIOException("Cannot create SSL client socket");
            }
            
            socketReader = new Thread(this.new SocketReader(), "YAMI4 SSL Reader");
            socketReader.setDaemon(true);
            socketReader.start();
            
            socketWriter = new Thread(this.new SocketWriter(), "YAMI4 SSL Writer");
            socketWriter.setDaemon(true);
            socketWriter.start();
        }

        TransportChannel(DatagramChannel datagramChannel,
            InetSocketAddress targetAddress) {
            this.connectedChannel = null;
            this.datagramChannel = datagramChannel;
            this.targetAddress = targetAddress;
        
            blockingSocket = null;
            readingQueue = null;
            input = null;
            socketReader = null;
            writingQueue = null;
            output = null;
            socketWriter = null;
            ioWorker = null;
            frameSize = 0;
        }

        SelectionKey register(Selector selector, int operations,
            boolean useBlockingOnly)
            throws ClosedChannelException {
            
            if (useBlockingOnly) {
                return null;
            }
        	
            if (connectedChannel != null) {
                return connectedChannel.register(selector, operations);
            } else if (datagramChannel != null) {
                return datagramChannel.register(selector, operations);
            } else {
                return null;
            }
        }

        void close() throws IOException {
            if (connectedChannel != null) {
                connectedChannel.close();
            } else if (datagramChannel != null) {
                datagramChannel.close();
            } else {
                blockingSocket.close();
                readingQueue.close();
                writingQueue.close();
                
                try {
                    socketReader.join();
                    socketWriter.join();
                } catch (InterruptedException e) {
                    // ignore
                }
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
    
    static SSLSocketFactory getSSLSocketFactory() throws IOException {
    	if (sslSocketFactory == null) {
            // SSL connections are implemented in terms of blocking SSL sockets
	    	
            // create a trust manager that does not validate certificate chains
            // to avoid problems with certificates that are not installed in the local Java cert store:
            TrustManager[] trustAllCerts = new TrustManager[] { new X509TrustManager() {
                    public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                        return null;
                    }
	                
                    public void checkClientTrusted(java.security.cert.X509Certificate[] certs, String authType) {
                        // nothing to do
                    }
	                
                    public void checkServerTrusted(java.security.cert.X509Certificate[] certs, String authType) {
                        // nothing to do
                    }
                }
            };

            SSLContext sc;
            try {
                sc = SSLContext.getInstance("TLS");
            } catch (NoSuchAlgorithmException ex) {
                throw new IOException("No TLS algorithm available for the SSL context.");
            }
	        
            try {
                // install the all-trusting trust manager
                sc.init(null, trustAllCerts, new SecureRandom());
            } catch (KeyManagementException ex) {
                throw new IOException("Cannot initialize SSL context.");
            }
        
            sslSocketFactory = sc.getSocketFactory();
    	}

        return sslSocketFactory;
    }
    
    static TransportChannel connectTcps(String target, Options options, IOWorker ioWorker)
        throws IOException {

        IpComponents tcpComponents = parseTcps(target);
        
        try {
            SSLSocketFactory socketFactory = getSSLSocketFactory();
            
            Socket sslSocket = socketFactory.createSocket();
            
            InetSocketAddress address = new InetSocketAddress(tcpComponents.hostName, tcpComponents.port);
            
            sslSocket.connect(address, options.tcpConnectTimeout);

            sslSocket.setTcpNoDelay(options.tcpNoDelay);
            sslSocket.setKeepAlive(options.tcpKeepAlive);

            return new TransportChannel(sslSocket, options.tcpFrameSize, ioWorker);

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
        if (protocolIsTcp(target) || protocolIsTcps(target)) {
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
