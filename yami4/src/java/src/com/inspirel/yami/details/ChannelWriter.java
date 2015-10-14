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

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

class ChannelWriter {

    private final NetworkUtils.TransportChannel connection;
    private final String target;
    
    private final ArrayList<OutgoingFrame> outgoingFrames;
    
    private final LogCallback logCallback;
    private final LogCallback.LogLevel logLevel;

    ChannelWriter(NetworkUtils.TransportChannel connection, String target,
            LogCallback logCallback, LogCallback.LogLevel logLevel) {
        this.connection = connection;
        this.target = target;
        this.logCallback = logCallback;
        this.logLevel = logLevel;
        
        outgoingFrames = new ArrayList<OutgoingFrame>();        
    }

    boolean post(int transportId, int priority, List<byte[]> buffers,
            int messageHeaderSize,
            MessageProgressCallback messageProgressCallback) {

        boolean firstFrame;
        
        // add new buffers to the queue of outgoing frames
        final boolean closeFlag = false;
        int byteCount = 0;
        int totalByteCount = 0;
        for (byte[] buf : buffers) {
            totalByteCount += buf.length;
        }

        synchronized (outgoingFrames) {
            firstFrame = outgoingFrames.isEmpty();
            
            // find the proper place to insert into the queue
            int insertionIndex = findOutgoingInsertionIndex(priority);

            int frameNumber = 1;
            for (byte[] buf : buffers) {
                byteCount += buf.length;
                if (frameNumber == buffers.size()) {
                    // the frame number is negative for the last frame
                    frameNumber *= -1;
                }
                
                OutgoingFrame newFrame = new OutgoingFrame(buf, transportId,
                        frameNumber, messageHeaderSize,
                        messageProgressCallback, byteCount, totalByteCount,
                        priority, closeFlag);

                outgoingFrames.add(insertionIndex++, newFrame);
                
                ++frameNumber;
            }
        }
        
        return firstFrame;
    }

    boolean postClose(int priority) {
        boolean result;
        List<OutgoingFrame> tail = null;
        synchronized (outgoingFrames) {
            if (outgoingFrames.isEmpty()) {
                // there are no outgoing frames,
                // the channel can be closed immediately
                result = true;
            } else {
                OutgoingFrame poisonPill = new OutgoingFrame(
                        null, 0, 0, 0, null, 0, 0, 0, true);

                int insertionIndex = findOutgoingInsertionIndex(priority);
                outgoingFrames.add(insertionIndex, poisonPill);
                tail = cleanOutgoingFrames(insertionIndex + 1);

                result = false;
            }
        }
        
        if (result == false) {
            // notify cancellation for those messages
            // that are behind the poison pill
            // (they will be never transmitted)
            
            notifyCancellation(tail);
        }

        return result;
    }
    
    boolean hasSomeOutgoingFrames() {
        synchronized (outgoingFrames) {
            return outgoingFrames.isEmpty() == false;
        }
    }

    // returns true if the queue of frames becomes empty
    boolean doSomeOutput() throws IOException {
    	boolean queueBecameEmpty = false;
        OutgoingFrame frame;
        synchronized (outgoingFrames) {
            if (outgoingFrames.isEmpty()) {
                return true;
            }
            frame = outgoingFrames.get(0);
        
            if (frame.closeFlag) {
                outgoingFrames.remove(0);

                // this artificial exception forces the worker to
                // physically close this connection and remove it
                // from the set of channels
                throw new IOException();
            }
        }

        if (connection.connectedChannel != null) {
            // TCP connection
            connection.connectedChannel.write(frame.buffers);
        } else if (connection.blockingSocket != null) {
            // TCP SSL connection
            connection.writingQueue.write(frame.buffers);
        } else {
            // UDP connection
            connection.datagramChannel.send(frame.getSingleBuffer(),
                    connection.targetAddress);
        }
        
        if (frame.buffersConsumed()) {
            // the first frame (head in the queue) was completely
            // sent -> notify its progress callback and remove
            // the head from the queue
            synchronized (outgoingFrames) {
                outgoingFrames.remove(0);
                
                if (outgoingFrames.isEmpty()) {
                	queueBecameEmpty = true;
                }
            }
            
            MessageProgressCallback callback = frame.messageProgressCallback;
            if (callback != null) {
                callback.progress(frame.byteCount, frame.totalByteCount);
            }

            if (logCallback != null) {
                if (logLevel == LogCallback.LogLevel.HIGH) {
                
                    logCallback.log(LogCallback.LogLevel.HIGH,
                            "Frame sent:" +
                            " target: " + target +
                            " tid: " + frame.transportId +
                            " fid: " + frame.frameNumber +
                            " size: " + frame.byteCount +
                            "/" + frame.totalByteCount);
                }
            }
        }
        
        return queueBecameEmpty;
    }
    
    void notifyCancellation() {
        synchronized (outgoingFrames) {
            // notify cancellation for all messages that are still
            // waiting for being pushed out in this channel
            notifyCancellation(outgoingFrames);
        }
    }
    
    // for unit tests
    List<OutgoingFrame> getOutgoingFrames() {
        return outgoingFrames;
    }

    // synchronized by caller
    private int findOutgoingInsertionIndex(int priority) {
        if (outgoingFrames.isEmpty()) {
            return 0;
        }

        // always skip the first frame
        int i = 1;
        for (; i != outgoingFrames.size(); ++i) {
            if (outgoingFrames.get(i).priority < priority) {
                break;
            }
        }

        return i;
    }

    // cuts the outgoing queue at the given point and returns the tail
    // synchronized by caller
    private List<OutgoingFrame> cleanOutgoingFrames(int cutIndex) {
        List<OutgoingFrame> tailView =
                outgoingFrames.subList(cutIndex, outgoingFrames.size());
        List<OutgoingFrame> tail = new ArrayList<OutgoingFrame>(tailView);
        tailView.clear();
        return tail;
    }

    // notifies cancellation (via the progress callback) for all
    // messages that have complete frames in the given list
    private void notifyCancellation(List<OutgoingFrame> frames) {
        for (OutgoingFrame frame : frames) {
            if (frame.byteCount == frame.totalByteCount) {
                // this is the last frame for the given message

                MessageProgressCallback callback =
                        frame.messageProgressCallback;
                if (callback != null) {
                    callback.progress(0, 0);
                }
            }
        }
    }
}
