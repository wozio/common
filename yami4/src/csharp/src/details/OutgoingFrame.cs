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

using System.Collections.Generic;
using System;

namespace Inspirel.YAMI.details
{
    internal sealed class OutgoingFrame
    {
        internal byte[] header;
        internal byte[] payload;

        private bool sentAsSingleBuffer;

        internal int transportId;
        internal int frameNumber;

        internal MessageProgressCallback messageProgressCallback;
        internal int byteCount;
        internal int totalByteCount;

        internal int priority;

        internal bool closeFlag;

        private int sentBytes = 0;

        internal OutgoingFrame(byte[] payload, int transportId, 
            int frameNumber, int messageHeaderSize, 
            MessageProgressCallback messageProgressCallback, 
            int byteCount, int totalByteCount, int priority, bool closeFlag)
        {
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

            if (payload != null)
            {
                header = new byte[Frame.FRAME_HEADER_SIZE];

                int offset = 0;
                Serialization.storeInt(header, offset, transportId);
                offset += 4;
                Serialization.storeInt(header, offset, frameNumber);
                offset += 4;
                Serialization.storeInt(header, offset, messageHeaderSize);
                offset += 4;
                Serialization.storeInt(header, offset, payload.Length);
            }
        }

    // concatenates header and payload buffers
    // (used for atomic datagram output)
        internal byte[] SingleBuffer
        {
            get
            {
                byte[] singleBuffer = 
                    new byte[header.Length + payload.Length];
                System.Array.Copy(header, 0, singleBuffer, 0, header.Length);
                System.Array.Copy(payload, 0, 
                    singleBuffer, header.Length, payload.Length);
    
                sentAsSingleBuffer = true;
    
                return singleBuffer;
            }
        }

        internal void AddSentBytes(int sent)
        {
            sentBytes += sent;
        }

        internal IList<ArraySegment<byte>> BuffersToSent
        {
            get
            {
                List<ArraySegment<byte>> rv = new List<ArraySegment<byte>>();
                if(sentBytes < header.Length)
                {
                    rv.Add(new ArraySegment<byte>(
                        header, sentBytes, header.Length - sentBytes
                        )
                    );
                    rv.Add(new ArraySegment<byte>(payload));
                }
                else
                {
                    rv.Add(new ArraySegment<byte>(
                        payload, 
                        sentBytes - header.Length, 
                        header.Length + payload.Length - sentBytes
                        )
                    );
                }
                return rv;
            }
        }

        internal bool buffersConsumed()
        {
            if (sentAsSingleBuffer)
            {
                return true;
            }
            else
            {
                return sentBytes >= header.Length + payload.Length;
            }
        }
    }

}