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
using System.IO;

namespace Inspirel.YAMI.details
{
    internal class ChannelWriter
    {

        private readonly NetworkUtils.TransportChannel connection;
        private readonly string target;

        private readonly List<OutgoingFrame> outgoingFrames;

        private readonly LogCallback logCallback;
        private readonly LogEventArgs.LogLevel logLevel;

        internal ChannelWriter(
            NetworkUtils.TransportChannel connection, string target, 
            LogCallback logCallback, LogEventArgs.LogLevel logLevel)
        {
            this.connection = connection;
            this.target = target;
            this.logCallback = logCallback;
            this.logLevel = logLevel;

            outgoingFrames = new List<OutgoingFrame>();
        }

        internal virtual void post(int transportId, int priority, 
            IList<byte[]> buffers, int messageHeaderSize, 
            MessageProgressCallback messageProgressCallback)
        {

        // add new buffers to the queue of outgoing frames
            const bool closeFlag = false;
            int byteCount = 0;
            int totalByteCount = 0;
            foreach (byte[] buf in buffers)
            {
                totalByteCount += buf.Length;
            }

            lock (outgoingFrames)
            {
            // find the proper place to insert into the queue
                int insertionIndex = findOutgoingInsertionIndex(priority);

                int frameNumber = 1;
                foreach (byte[] buf in buffers)
                {
                    byteCount += buf.Length;
                    if (frameNumber == buffers.Count)
                    {
                    // the frame number is negative for the last frame
                        frameNumber *= -1;
                    }

                    OutgoingFrame newFrame = new OutgoingFrame(
                        buf, transportId, frameNumber, 
                        messageHeaderSize, messageProgressCallback, 
                        byteCount, totalByteCount, priority, closeFlag);

                    outgoingFrames.Insert(insertionIndex++, newFrame);

                    ++frameNumber;
                }
            }
        }

        internal virtual bool postClose(int priority)
        {
            bool result;
            IList<OutgoingFrame> tail = null;
            lock (outgoingFrames)
            {
                if (outgoingFrames.Count == 0)
                {
                // there are no outgoing frames,
                // the channel can be closed immediately
                    result = true;
                }
                else
                {
                    OutgoingFrame poisonPill = 
                        new OutgoingFrame(
                            null, 0, 0, 0, null, 0, 0, 0, true
                            );

                    int insertionIndex = 
                        findOutgoingInsertionIndex(priority);
                    outgoingFrames.Insert(insertionIndex, poisonPill);
                    tail = cleanOutgoingFrames(insertionIndex + 1);

                    result = false;
                }
            }

            if (result == false)
            {
            // notify cancellation for those messages
            // that are behind the poison pill
            // (they will be never transmitted)

                notifyCancellation(tail);
            }

            return result;
        }

        internal virtual bool hasSomeOutgoingFrames()
        {
            lock (outgoingFrames)
            {
                return outgoingFrames.Count != 0;
            }
        }

        internal virtual void doSomeOutput()
        {
            OutgoingFrame frame;
            lock (outgoingFrames)
            {
                if (outgoingFrames.Count == 0)
                {
                    return;
                }
                frame = outgoingFrames[0];

                if (frame.closeFlag)
                {
                    outgoingFrames.RemoveAt(0);

                // this artificial exception forces the worker to
                // physically close this connection and remove it
                // from the set of channels
                    throw new YAMIIOException("dummy");
                }
            }

            if (connection.connectedChannel != null)
            {
            // TCP connection
                int sent = 
                    connection.connectedChannel.Send(frame.BuffersToSent);
                frame.AddSentBytes(sent);
            }
            else if (connection.ssl != null)
            {
                // TCP SSL connection
                int sent =
                    connection.writingQueue.PutMany(frame.BuffersToSent);
                frame.AddSentBytes(sent);
            }
            else
            {
            // UDP connection
                connection.datagramChannel.SendTo(
                    frame.SingleBuffer, connection.targetAddress);
            }

            if (frame.buffersConsumed())
            {
            // the first frame (head in the queue) was complmesetely
            // sent -> notify its progress callback and remove
            // the head from the queue
                lock (outgoingFrames)
                {
                    outgoingFrames.RemoveAt(0);
                }

                MessageProgressCallback callback = 
                    frame.messageProgressCallback;
                if (callback != null)
                {
                    callback.progress(frame.byteCount, frame.totalByteCount);
                }

                if (logCallback != null)
                {
                    if (logLevel == LogEventArgs.LogLevel.HIGH)
                    {
                        logCallback.Log(LogEventArgs.LogLevel.HIGH, 
                            "Frame sent:" 
                            + " target: " + target 
                            + " tid: " + frame.transportId 
                            + " fid: " + frame.frameNumber 
                            + " size: " 
                            + frame.byteCount + "/" + frame.totalByteCount);
                    }
                }
            }
        }

        internal virtual void notifyCancellation()
        {
            lock (outgoingFrames)
            {
            // notify cancellation for all messages that are still
            // waiting for being pushed out in this channel
                notifyCancellation(outgoingFrames);
            }
        }

    // for unit tests
        internal virtual IList<OutgoingFrame> OutgoingFrames
        {
            get
            {
                return outgoingFrames;
            }
        }

    // synchronized by caller
        private int findOutgoingInsertionIndex(int priority)
        {
            if (outgoingFrames.Count == 0)
            {
                return 0;
            }

        // always skip the first frame
            int i = 1;
            for (; i != outgoingFrames.Count; ++i)
            {
                if (outgoingFrames[i].priority < priority)
                {
                    break;
                }
            }

            return i;
        }

    // cuts the outgoing queue at the given point and returns the tail
    // synchronized by caller
        private IList<OutgoingFrame> cleanOutgoingFrames(int cutIndex)
        {
            IList<OutgoingFrame> tail = outgoingFrames.GetRange(
                cutIndex, outgoingFrames.Count - cutIndex);
            outgoingFrames.RemoveRange(
                cutIndex, outgoingFrames.Count - cutIndex);
            return tail;
        }

    // notifies cancellation (via the progress callback) for all
    // messages that have complete frames in the given list
        private void notifyCancellation(IList<OutgoingFrame> frames)
        {
            foreach (OutgoingFrame frame in frames)
            {
                if (frame.byteCount == frame.totalByteCount)
                {
                // this is the last frame for the given message

                    MessageProgressCallback callback = 
                        frame.messageProgressCallback;
                    if (callback != null)
                    {
                        callback.progress(0, 0);
                    }
                }
            }
        }
    }

}
