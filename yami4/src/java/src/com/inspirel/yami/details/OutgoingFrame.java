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

import java.nio.ByteBuffer;

final class OutgoingFrame {

    byte[] header;
    byte[] payload;
    
    ByteBuffer headerBuffer;
    ByteBuffer payloadBuffer;
    ByteBuffer[] buffers;
    boolean sentAsSingleBuffer;
    
    int transportId;
    int frameNumber;
    
    MessageProgressCallback messageProgressCallback;
    int byteCount;
    int totalByteCount;
    
    int priority;
    
    boolean closeFlag;

    OutgoingFrame(byte[] payload, int transportId, int frameNumber,
            int messageHeaderSize,
            MessageProgressCallback messageProgressCallback,
            int byteCount, int totalByteCount,
            int priority, boolean closeFlag) {
        this.payload = payload;
        this.transportId = transportId;
        this.frameNumber = frameNumber;
        this.messageProgressCallback = messageProgressCallback;
        this.byteCount = byteCount;
        this.totalByteCount = totalByteCount;
        this.priority = priority;
        this.closeFlag = closeFlag;
        this.sentAsSingleBuffer = false;

        // create the frame header

        if (payload != null) {
            header = new byte[Frame.FRAME_HEADER_SIZE];

            int offset = 0;
            Serialization.storeInt(header, offset, transportId);
            offset += 4;
            Serialization.storeInt(header, offset, frameNumber);
            offset += 4;
            Serialization.storeInt(header, offset, messageHeaderSize);
            offset += 4;
            Serialization.storeInt(header, offset, payload.length);

            // create buffers for future output
        
            headerBuffer = ByteBuffer.wrap(header);
            payloadBuffer = ByteBuffer.wrap(payload);
            buffers = new ByteBuffer[] { headerBuffer, payloadBuffer };
        }
    }

    // concatenates header and payload buffers
    // (used for atomic datagram output)
    ByteBuffer getSingleBuffer() {
        ByteBuffer singleBuffer = ByteBuffer.allocate(
                header.length + payload.length);
        singleBuffer.put(header);
        singleBuffer.put(payload);
        singleBuffer.position(0);

        sentAsSingleBuffer = true;

        return singleBuffer;
    }

    boolean buffersConsumed() {
        if (sentAsSingleBuffer) {
            return true;
        } else {
            return headerBuffer.hasRemaining() == false &&
                    payloadBuffer.hasRemaining() == false;
        }
    }
}
