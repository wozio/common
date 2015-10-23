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

import com.inspirel.yami.UnexpectedValueException;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

// note: this class is only used for those messages
// that are composed of many frames, because they can have
// complex frame arrival patterns
// short (single-frame) messages are dispatched
// directly in the ChannelReader
class IncomingMessageFrames {

    // frame number -> frame
    private final Map<Integer, IncomingFrame> frames;
    private int messageHeaderSize;      // 0 if not yet known
    private int messageLastFrameNumber; // 0 if not yet known

    IncomingMessageFrames() {
        frames = new HashMap<Integer, IncomingFrame>();
        messageHeaderSize = 0;
        messageLastFrameNumber = 0;
    }

    boolean accumulate(int frameNumber, int headerSize,
            IncomingFrame frame) {

        if (frameNumber == 1) {
            // this is the first frame for this message
            // (the header size is taken from the first frame only,
            // information from other frames is ignored)

            messageHeaderSize = headerSize;
        }
        
        int realFrameNumber;
        if (frameNumber >= 0) {
            realFrameNumber = frameNumber;
        } else {
            // the given frame is a closing one
            
            realFrameNumber = -frameNumber;
            messageLastFrameNumber = realFrameNumber;
        }
        
        IncomingFrame previous =
            frames.put(Integer.valueOf(realFrameNumber), frame);
        if (previous != null) {
            // the frame with the same number was already store
            // - treat it as malformed network junk
            
            throw new UnexpectedValueException(
                    "Corrupted message received.");
        }
        
        if (messageLastFrameNumber != 0) {
            // the last frame was already stored for this message
            // - the message is complete if all other frames
            // are already collected
            
            return frames.size() == messageLastFrameNumber;
        }

        // the last frame was not yet received
        // - the message is not complete

        return false;
    }

    void getAllBuffers(
            List<byte[]> headerBuffers, List<byte[]> bodyBuffers) {

        int remainedForHeader = messageHeaderSize;
        for (int frameNumber = 1; frameNumber <= messageLastFrameNumber;
                ++frameNumber) {

            IncomingFrame frame = frames.get(Integer.valueOf(frameNumber));
            byte[] frameData = frame.data;
            int frameDataLength = frameData.length;
            
            if (remainedForHeader > 0) {
                // there is still some header data to be collected
                
                if (frameDataLength > remainedForHeader) {
                    // this frame needs to be split
                    // between header and body parts
                    
                    byte[] headerPart = Arrays.copyOfRange(
                            frameData, 0, remainedForHeader);
                    byte[] bodyPart = Arrays.copyOfRange(
                            frameData, remainedForHeader, frameDataLength);
                
                    headerBuffers.add(headerPart);
                    bodyBuffers.add(bodyPart);
                
                    remainedForHeader -= headerPart.length;
                } else {
                    // this frame contains only header data
                    
                    headerBuffers.add(frameData);
                    
                    remainedForHeader -= frameDataLength;
                }
            } else {
                // this frame contains only body data
                
                bodyBuffers.add(frameData);
            }
        }
        
        if (remainedForHeader > 0) {
            // the collection of frames does not have enough data
            // for the message header
            
            throw new UnexpectedValueException(
                    "Corrupted message received.");
        }
    }
}
