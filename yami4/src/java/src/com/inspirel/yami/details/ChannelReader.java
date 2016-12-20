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

import com.inspirel.yami.LogCallback;
import com.inspirel.yami.Parameters;
import com.inspirel.yami.UnexpectedValueException;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class ChannelReader {

    private NetworkUtils.TransportChannel connection;

    private enum InputState { READING_FRAME_HEADER, READING_FRAME_PAYLOAD,
        READING_WHOLE_FRAMES }
    
    InputState state;

    // transportId -> manager object for a single message
    private final Map<Integer, IncomingMessageFrames> incomingFrames;

    ByteBuffer headerBuffer;
    ByteBuffer wholeFrameBuffer;
    
    private int currentIncomingFrameTransportId;
    private int currentIncomingFrameNumber;
    private int currentIncomingMessageHeaderSize;
    private IncomingFrame currentIncomingFrame;
    
    private final IncomingMessageDispatchCallback
            incomingMessageDispatchCallback;
    
    private final String target;
    
    private final int incomingFramePayloadLimit;
    private final boolean deliverAsRawBinary;
    
    private final LogCallback logCallback;
    private final LogCallback.LogLevel logLevel;

    public ChannelReader(NetworkUtils.TransportChannel connection,
            IncomingMessageDispatchCallback
                incomingMessageDispatchCallback,
                String target,
                Options options,
                LogCallback logCallback, LogCallback.LogLevel logLevel) {
        
        this.connection = connection;
        this.incomingMessageDispatchCallback =
                incomingMessageDispatchCallback;
        this.target = target;
        this.logCallback = logCallback;
        this.logLevel = logLevel;
        
        incomingFrames = new HashMap<Integer, IncomingMessageFrames>();

        if (connection != null) {
            if (connection.connectedChannel != null || connection.blockingSocket != null) {
                // stream-based connection
                headerBuffer = ByteBuffer.allocate(Frame.FRAME_HEADER_SIZE);
                setReadingFrameHeaderState();
            } else {
                // datagram-based connection
                wholeFrameBuffer = ByteBuffer.allocate(options.udpFrameSize);
                state = InputState.READING_WHOLE_FRAMES;
            }
        }
        
        incomingFramePayloadLimit = options.incomingFramePayloadLimit;
        
        deliverAsRawBinary = options.deliverAsRawBinary;
    }

    void doSomeInput() throws IOException {
        switch (state) {
        case READING_FRAME_HEADER:
            doReadHeader();
            break;
        case READING_FRAME_PAYLOAD:
            doReadPayload();
            break;
        case READING_WHOLE_FRAMES:
            doReadWholeFrame();
            break;
        }
    }

    void injectFullFrame(ByteBuffer frameBuffer) {
        // the process of injecting a complete frame
        // is implemented in terms of existing steps that are also used
        // for reading data from the physical channel
        // 1. the frame header is parsed
        // 2. the frame payload is consumed

        // injecting full frames is allowed only when the channel
        // is prepared to process whole frames
        assert(state == InputState.READING_WHOLE_FRAMES);

        byte[] rawBuffer = frameBuffer.array();

        byte[] header = Arrays.copyOfRange(
                rawBuffer, 0, Frame.FRAME_HEADER_SIZE);

        int payloadSize = parseFrameHeader(header);
        createCurrentIncomingFrame(payloadSize);

        currentIncomingFrame.dataBuffer.put(
                rawBuffer, Frame.FRAME_HEADER_SIZE, payloadSize);
        
        if (logCallback != null) {
            if (logLevel == LogCallback.LogLevel.HIGH) {

                logCallback.log(LogCallback.LogLevel.HIGH,
                        "Frame received:" +
                        " from: " + target +
                        " tid: " + currentIncomingFrameTransportId +
                        " fid: " + currentIncomingFrameNumber);
            }
        }

        // store this frame together with other frames
        // for the same message

        storeOrDispatchCurrentFrame();
    }

    private int parseFrameHeader(byte[] header) {

        currentIncomingFrameTransportId = Serialization.readInt(header, 0);
        currentIncomingFrameNumber = Serialization.readInt(header, 4);
        int messageHeaderSize = Serialization.readInt(header, 8);
        int payloadSize = Serialization.readInt(header, 12);

        // sanity check

        if (messageHeaderSize < 0 || payloadSize < 0 ||
        		payloadSize > incomingFramePayloadLimit) {
        	
            throw new UnexpectedValueException(
                    "Corrupted message received from " + target);
        }

        if (currentIncomingFrameNumber == 1 ||
                currentIncomingFrameNumber == -1) {
            // this is the first (or the only) frame in a message

            currentIncomingMessageHeaderSize = messageHeaderSize;
        }

        return payloadSize;
    }

    private void doReadHeader() throws IOException {
        assert(state == InputState.READING_FRAME_HEADER);
        
        int readn = -1;
        
        if (connection.connectedChannel != null) {
        	readn = connection.connectedChannel.read(headerBuffer);
        } else if (connection.blockingSocket != null) {
        	readn = connection.readingQueue.read(headerBuffer);
        }
        
        if (readn == -1) {
            // EOF - signal it as an artificial exception
            // so that the channel can be closed and removed
            // by higher layers
            throw new IOException("EOF");
        }
        
        if (headerBuffer.hasRemaining() == false) {
            // the whole header has been read - decode it
            byte[] buf = headerBuffer.array();

            int payloadSize = parseFrameHeader(buf);
            setReadingFramePayloadState(payloadSize);
        }
    }
    
    private void doReadPayload() throws IOException {
        assert(state == InputState.READING_FRAME_PAYLOAD);

        ByteBuffer dataBuffer = currentIncomingFrame.dataBuffer;
        int readn = -1;
        
        if (connection.connectedChannel != null) {
        	readn = connection.connectedChannel.read(dataBuffer);
        } else if (connection.blockingSocket != null) {
        	readn = connection.readingQueue.read(dataBuffer);
        }

        if (readn == -1) {
            // EOF - signal it as an artificial exception
            // so that the channel can be closed and removed
            // by higher layers
            throw new IOException("EOF");
        }
        
        if (dataBuffer.hasRemaining() == false) {
            // the whole payload has been read
            
            if (logCallback != null) {
                if (logLevel == LogCallback.LogLevel.HIGH) {
                
                    logCallback.log(LogCallback.LogLevel.HIGH,
                            "Frame received:" +
                            " from: " + target +
                            " tid: " + currentIncomingFrameTransportId +
                            " fid: " + currentIncomingFrameNumber);
                }
            }

            // store this frame together with other frames
            // for the same message

            storeOrDispatchCurrentFrame();
            
            setReadingFrameHeaderState();
        }
    }

    private void doReadWholeFrame() throws IOException {
        // ignore the remote address here, it is anyway the same
        // as the address that was used when this channel was created
        connection.datagramChannel.receive(wholeFrameBuffer);

        injectFullFrame(wholeFrameBuffer);
    }
    
    private void storeOrDispatchCurrentFrame() {

        if (currentIncomingFrameNumber == -1) {
            // typical case:
            // this is a single-frame message and it is complete
            
            dispatchShortMessage(currentIncomingFrame.data);
            
        } else {
            // this is a multi-frame message
            
            IncomingMessageFrames messageFrames = incomingFrames.get(
                    Integer.valueOf(currentIncomingFrameTransportId));
            if (messageFrames == null) {
                messageFrames = new IncomingMessageFrames();
                incomingFrames.put(
                        Integer.valueOf(currentIncomingFrameTransportId),
                        messageFrames);
            }
            
            boolean messageIsComplete = messageFrames.accumulate(
                    currentIncomingFrameNumber,
                    currentIncomingMessageHeaderSize,
                    currentIncomingFrame);

            if (messageIsComplete) {
                
                // collect all frames together
                
                List<byte[]> headerBuffers = new ArrayList<byte[]>();
                List<byte[]> bodyBuffers = new ArrayList<byte[]>();
                
                messageFrames.getAllBuffers(headerBuffers, bodyBuffers);
                
                // the frame manager for this message is no longer needed
                incomingFrames.remove(
                        Integer.valueOf(currentIncomingFrameTransportId));
                                
                dispatch(headerBuffers, bodyBuffers);
            }                
        }
    }

    private void dispatchShortMessage(byte[] buf) {
        
        if (currentIncomingMessageHeaderSize > buf.length) {
            throw new UnexpectedValueException(
                    "Corrupted message received.");
        }
        
        // extract message header and message body from this single buffer
        
        byte[] headerBuf = Arrays.copyOfRange(
                buf, 0, currentIncomingMessageHeaderSize);
        byte[] bodyBuf = Arrays.copyOfRange(
                buf, currentIncomingMessageHeaderSize, buf.length);
        
        List<byte[]> headerBuffers = new ArrayList<byte[]>();
        headerBuffers.add(headerBuf);
        List<byte[]> bodyBuffers = new ArrayList<byte[]>();
        bodyBuffers.add(bodyBuf);

        dispatch(headerBuffers, bodyBuffers);
    }
    
    private void dispatch(
            List<byte[]> headerBuffers, List<byte[]> bodyBuffers) {

        Parameters header = new Parameters();
        header.deserialize(headerBuffers);
        
        Parameters body = null;
        byte[] rawBody = null;
        
        if (deliverAsRawBinary) {
            // repackage all body buffers into one
            int totalRawSize = 0;
            for (byte[] buf : bodyBuffers) {
                totalRawSize += buf.length;
            }
            
            rawBody = new byte[totalRawSize];
            int pos = 0;
            for (byte[] buf : bodyBuffers) {
                for (int i = 0; i != buf.length; ++i) {
                    rawBody[pos++] = buf[i];
                }
            }
        } else {
            body = new Parameters();
            body.deserialize(bodyBuffers);
        }
        
        incomingMessageDispatchCallback.dispatch(
                target, header, body, rawBody);
    }
    
    private void setReadingFrameHeaderState() {
        headerBuffer.clear();
        state = InputState.READING_FRAME_HEADER;
    }
    
    private void setReadingFramePayloadState(int payloadSize) {
        
        createCurrentIncomingFrame(payloadSize);
        state = InputState.READING_FRAME_PAYLOAD;
    }
    
    private void createCurrentIncomingFrame(int payloadSize) {
        currentIncomingFrame = new IncomingFrame(payloadSize);
    }
}
