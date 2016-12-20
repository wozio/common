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

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.List;

public class Channel {

    private final String target;
    private final Options options;
    
    ChannelWriter channelWriter;
    ChannelReader channelReader;

    private NetworkUtils.TransportChannel connection;
    
    private final LogCallback logCallback;
    private final LogCallback.LogLevel logLevel;

    public Channel(String target, Options options,
            IncomingMessageDispatchCallback incomingMessageDispatchCallback,
            IOWorker ioWorker,
            LogCallback logCallback, LogCallback.LogLevel logLevel)
            throws IOException {

        this.target = target;
        this.options = options;
                
        this.logCallback = logCallback;
        this.logLevel = logLevel;

        connect(incomingMessageDispatchCallback, ioWorker);

        if (logCallback != null) {
            logCallback.log(LogCallback.LogLevel.LOW,
                    "Connected to " + target);
        }
    }
    
    // used by listener when accepting new connections
    Channel(SocketChannel acceptedChannel, String sourceTarget,
            IncomingMessageDispatchCallback incomingMessageDispatchCallback,
            Options options,
            LogCallback logCallback, LogCallback.LogLevel logLevel) {
        
        this.target = sourceTarget;
        this.options = options;
        
        this.connection = new NetworkUtils.TransportChannel(acceptedChannel);
        
        this.logCallback = logCallback;
        this.logLevel = logLevel;

        createReaderWriter(incomingMessageDispatchCallback);

        if (logCallback != null) {
            logCallback.log(LogCallback.LogLevel.LOW,
                    "Accepted connection from " + target);
        }
    }

    public boolean post(int transportId, int priority, List<byte[]> buffers,
            int messageHeaderSize,
            MessageProgressCallback messageProgressCallback) {

        return channelWriter.post(transportId, priority,
                buffers, messageHeaderSize, messageProgressCallback);
    }

    public boolean postClose(int priority) {        
        return channelWriter.postClose(priority);
    }
    
    public void close() {
        if (connection != null) {
            try {
                connection.close();
            } catch (Exception ex) {
                // ignore
            }
            connection = null;
            
            channelWriter.notifyCancellation();

            if (logCallback != null) {
                logCallback.log(LogCallback.LogLevel.LOW,
                        "Closed connection to " + target);
            }
        }
    }

    public static int groupBuffers(ArrayList<byte[]> allBuffers,
            List<byte[]> headerBuffers, List<byte[]> contentBuffers,
            int chunkSize) {

        // optimize for the common case where the header and content
        // can fit in a single frame

        int messageHeaderSize = 0;
        int totalSerializedSize = 0;
        for (byte[] buf : headerBuffers) {
            messageHeaderSize += buf.length;
            totalSerializedSize += buf.length;
        }
        for (byte[] buf : contentBuffers) {
            totalSerializedSize += buf.length;
        }
        
        if (totalSerializedSize <= chunkSize) {
            
            // all buffers can fit in a single frame - repackage them
            
            byte[] singleBuffer = new byte[totalSerializedSize];
            int bufIndex = 0;
            for (byte[] buf : headerBuffers) {
                for (byte b : buf) {
                    singleBuffer[bufIndex++] = b;
                }
            }
            for (byte[] buf : contentBuffers) {
                for (byte b : buf) {
                    singleBuffer[bufIndex++] = b;
                }
            }
            allBuffers.add(singleBuffer);
        } else {
            
            // buffers are too big to fit in a single frame
            // - post them separately
            
            allBuffers.ensureCapacity(
                headerBuffers.size() + contentBuffers.size());
            allBuffers.addAll(headerBuffers);
            allBuffers.addAll(contentBuffers);
        }

        return messageHeaderSize;
    }

    void injectFullFrame(ByteBuffer frameBuffer) {
        channelReader.injectFullFrame(frameBuffer);
    }
    
    static class SelectionKeys {
        SelectionKey key;
        boolean blockingChannelReadyForReading;
        boolean blockingChannelReadyForWriting;
    }
    
    SelectionKeys registerForSelection(Selector selector,
            boolean allowInput, boolean allowOutput,
            boolean useBlockingOnly) {

        int operations = 0;
        if (allowInput) {
            operations |= SelectionKey.OP_READ;
        }
        
        // do not register for output if there is nothing to write
        boolean hasSomeOutgoingFrames =
                channelWriter.hasSomeOutgoingFrames();
        
        if (allowOutput && hasSomeOutgoingFrames) {
            operations |= SelectionKey.OP_WRITE;
        }

        SelectionKey key = null;
        if (operations != 0) {
            try {
                key = connection.register(selector, operations, useBlockingOnly);
            } catch (ClosedChannelException ex) {
                // ignore, will never happen
            }
        }
        
        SelectionKeys keys = new SelectionKeys();
        keys.key = key;
        keys.blockingChannelReadyForReading =
            ((operations & SelectionKey.OP_READ) != 0) &&
            connection.readingQueue != null &&
            connection.readingQueue.hasDataOrEOF();
        keys.blockingChannelReadyForWriting = (operations & SelectionKey.OP_WRITE) != 0;
        
        return keys;
    }
    
    boolean doSomeWork(boolean allowInput, boolean allowOutput)
            throws IOException {
    	
    	boolean queueBecameEmpty = false;
    	
        if (allowInput) {
            channelReader.doSomeInput();
        }
        if (allowOutput) {
            queueBecameEmpty = channelWriter.doSomeOutput();
        }
        
        return queueBecameEmpty;
    }
    
    String getTarget() {
        return target;
    }
    
    private void connect(
            IncomingMessageDispatchCallback incomingMessageDispatchCallback,
            IOWorker ioWorker)
            throws IOException {
        
        if (NetworkUtils.protocolIsTcp(target)) {
            
            connection = NetworkUtils.connectTcp(target, options);

        } else if (NetworkUtils.protocolIsTcps(target)) {
            
            connection = NetworkUtils.connectTcps(target, options, ioWorker);
            
            // additionally, instruct I/O Worker that blocking sockets
            // are the only ones that should be acted upon
            
            ioWorker.useBlockingChannelsOnly();
            
        } else if (NetworkUtils.protocolIsUdp(target)) {

            connection = NetworkUtils.createUdp(target, options);
            
        } else if (target.equals("null")) {
            // do nothing - this protocol is used for testing only
            channelWriter = new ChannelWriter(
                    null, null, null, LogCallback.LogLevel.LOW);
        } else {
            throw new BadProtocolException(target);
        }
        
        createReaderWriter(incomingMessageDispatchCallback);
    }
    
    private void createReaderWriter(
            IncomingMessageDispatchCallback
            incomingMessageDispatchCallback) {
        
        channelWriter = new ChannelWriter(connection,
                target, logCallback, logLevel);
        channelReader = new ChannelReader(
                connection, incomingMessageDispatchCallback,
                target, options, logCallback, logLevel);
    }
    
    // for unit tests
    List<OutgoingFrame> getOutgoingFrames() {
        return channelWriter.getOutgoingFrames();
    }
}
