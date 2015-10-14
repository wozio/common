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
using System.Collections.Generic;

namespace Inspirel.YAMI.details
{
// note: this class is only used for those messages
// that are composed of many frames, because they can have
// complex frame arrival patterns
// short (single-frame) messages are dispatched
// directly in the ChannelReader
    internal class IncomingMessageFrames
    {
    // frame number -> frame
        private readonly IDictionary<int, IncomingFrame> frames;
        private int messageHeaderSize; // 0 if not yet known
        private int messageLastFrameNumber; // 0 if not yet known

        internal IncomingMessageFrames()
        {
            frames = new Dictionary<int, IncomingFrame>();
            messageHeaderSize = 0;
            messageLastFrameNumber = 0;
        }

        internal virtual bool accumulate(
            int frameNumber, int headerSize, IncomingFrame frame)
        {
            if (frameNumber == 1)
            {
            // this is the first frame for this message
            // (the header size is taken from the first frame only,
            // information from other frames is ignored)
                messageHeaderSize = headerSize;
            }

            int realFrameNumber;
            if (frameNumber >= 0)
            {
                realFrameNumber = frameNumber;
            }
            else
            {
            // the given frame is a closing one
                realFrameNumber = -frameNumber;
                messageLastFrameNumber = realFrameNumber;
            }

            if(frames.Keys.Contains(realFrameNumber))
            {
                // the frame with the same number was already store
                // - treat it as malformed network junk
                throw new UnexpectedValueException(
                    "Corrupted message received.");
            }

            frames.Add(realFrameNumber, frame);
            
            if (messageLastFrameNumber != 0)
            {
            // the last frame was already stored for this message
            // - the message is complete if all other frames
            // are already collected
                return frames.Count == messageLastFrameNumber;
            }

        // the last frame was not yet received
        // - the message is not complete
            return false;
        }

        internal virtual void getAllBuffers(
            IList<byte[]> headerBuffers, IList<byte[]> bodyBuffers)
        {

            int remainedForHeader = messageHeaderSize;
            for(int frameNumber = 1; 
                frameNumber <= messageLastFrameNumber; 
                ++frameNumber)
            {
                IncomingFrame frame = frames[Convert.ToInt32(frameNumber)];
                byte[] frameData = frame.data;
                int frameDataLength = frameData.Length;

                if (remainedForHeader > 0)
                {
                // there is still some header data to be collected

                    if (frameDataLength > remainedForHeader)
                    {
                    // this frame needs to be split
                    // between header and body parts

                        byte[] headerPart = new byte[remainedForHeader];
                        Array.Copy(frameData, headerPart, remainedForHeader);

                        byte[] bodyPart = 
                            new byte[frameDataLength - remainedForHeader];
                        Array.Copy(
                            frameData, remainedForHeader, 
                            bodyPart, 0, bodyPart.Length
                            );

                        headerBuffers.Add(headerPart);
                        bodyBuffers.Add(bodyPart);

                        remainedForHeader -= headerPart.Length;
                    }
                    else
                    {
                    // this frame contains only header data

                        headerBuffers.Add(frameData);

                        remainedForHeader -= frameDataLength;
                    }
                }
                else
                {
                // this frame contains only body data

                    bodyBuffers.Add(frameData);
                }
            }

            if (remainedForHeader > 0)
            {
            // the collection of frames does not have enough data
            // for the message header

                throw new UnexpectedValueException(
                    "Corrupted message received.");
            }
        }
    }

}