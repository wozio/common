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
using System.IO;

namespace Inspirel.YAMI.details
{
    internal class ChannelReader
    {

        private NetworkUtils.TransportChannel connection;

        private enum InputState
        {
            READING_FRAME_HEADER,
            READING_FRAME_PAYLOAD,
            READING_WHOLE_FRAMES
        }

        private InputState state;

    // transportId -> manager object for a single message
        private readonly IDictionary<int, IncomingMessageFrames> 
            incomingFrames;

        internal MemoryStream headerBuffer;
        internal byte[] wholeFrameBuffer = null;

        private int currentIncomingFrameTransportId;
        private int currentIncomingFrameNumber;
        private int currentIncomingMessageHeaderSize;
        private IncomingFrame currentIncomingFrame;

        private readonly IncomingMessageDispatchCallback 
            incomingMessageDispatchCallback;

        private readonly string target;

        private readonly bool deliverAsRawBinary;

        private readonly LogCallback logCallback;
        private readonly LogEventArgs.LogLevel logLevel;

        public ChannelReader(
            NetworkUtils.TransportChannel connection, 
            IncomingMessageDispatchCallback incomingMessageDispatchCallback, 
            string target, Options options, 
            LogCallback logCallback, LogEventArgs.LogLevel logLevel)
        {

            this.connection = connection;
            this.incomingMessageDispatchCallback = 
                incomingMessageDispatchCallback;
            this.target = target;
            this.logCallback = logCallback;
            this.logLevel = logLevel;

            incomingFrames = new Dictionary<int, IncomingMessageFrames>();

            if (connection != null)
            {
                if (connection.connectedChannel != null || connection.ssl != null)
                {
                // stream-based connection
                    headerBuffer = new MemoryStream(Frame.FRAME_HEADER_SIZE);
                    setReadingFrameHeaderState();
                }
                else
                {
                // datagram-based connection
                    wholeFrameBuffer = new byte[options.udpFrameSize];
                    state = InputState.READING_WHOLE_FRAMES;
                }
            }

            deliverAsRawBinary = options.deliverAsRawBinary;
        }

        internal virtual void doSomeInput()
        {
            switch (state)
            {
            case InputState.READING_FRAME_HEADER:
                doReadHeader();
                break;
            case InputState.READING_FRAME_PAYLOAD:
                doReadPayload();
                break;
            case InputState.READING_WHOLE_FRAMES:
                doReadWholeFrame();
                break;
            }
        }

        internal virtual void injectFullFrame(byte[] frameBuffer)
        {
        // the process of injecting a complete frame
        // is implemented in terms of existing steps that are also used
        // for reading data from the physical channel
        // 1. the frame header is parsed
        // 2. the frame payload is consumed

        // injecting full frames is allowed only when the channel
        // is prepared to process whole frames
            System.Diagnostics.Debug.Assert(
                state == InputState.READING_WHOLE_FRAMES);

            byte[] rawBuffer = frameBuffer;

            byte[] header = new byte[Frame.FRAME_HEADER_SIZE];
            Array.Copy(rawBuffer, header, header.Length);

            int payloadSize = parseFrameHeader(header);
            createCurrentIncomingFrame(payloadSize);

            currentIncomingFrame.dataBuffer.Write(
                rawBuffer, Frame.FRAME_HEADER_SIZE, payloadSize);

            if (logCallback != null)
            {
                logCallback.Log(LogEventArgs.LogLevel.HIGH, 
                    "Frame received:" 
                    + " from: " + target 
                    + " tid: " + currentIncomingFrameTransportId 
                    + " fid: " + currentIncomingFrameNumber);
            }

        // store this frame together with other frames
        // for the same message

            storeOrDispatchCurrentFrame();
        }

        private int parseFrameHeader(byte[] header)
        {
            currentIncomingFrameTransportId = 
                Serialization.readInt(header, 0);
            currentIncomingFrameNumber = Serialization.readInt(header, 4);
            int messageHeaderSize = Serialization.readInt(header, 8);
            int payloadSize = Serialization.readInt(header, 12);

        // sanity check

            if(currentIncomingFrameNumber == 1 
                || currentIncomingFrameNumber == -1)
            {
            // this is the first (or the only) frame in a message

                if (messageHeaderSize < 0 || payloadSize < 0)
                {
                    throw new UnexpectedValueException(
                        "Corrupted message received.");
                }

                currentIncomingMessageHeaderSize = messageHeaderSize;
            }

            return payloadSize;
        }

        private void doReadHeader()
        {
            System.Diagnostics.Debug.Assert(
                state == InputState.READING_FRAME_HEADER);

            int readn = 0;

            try
            {
                if (connection.connectedChannel != null)
                {
                    readn = connection.connectedChannel.Receive(
                        headerBuffer.GetBuffer(),
                        (int)headerBuffer.Position,
                        (int)(headerBuffer.Capacity - headerBuffer.Position),
                        System.Net.Sockets.SocketFlags.None);
                }
                else if (connection.ssl != null)
                {
                    readn = connection.readingQueue.Receive(
                        headerBuffer.GetBuffer(),
                        (int)headerBuffer.Position,
                        (int)(headerBuffer.Capacity - headerBuffer.Position));
                }
            }
            catch(ObjectDisposedException)
            {
                // socket already closed after 
                // explicit CloseConnection call
                return;
            }

            if (readn == -1 || readn == 0)
            {
            // EOF - signal it as an artificial exception
            // so that the channel can be closed and removed
            // by higher layers
                throw new YAMIIOException("EOF");
            }

            headerBuffer.Position += readn;

            if (headerBuffer.Position == headerBuffer.Capacity)
            {
            // the whole header has been read - decode it
                byte[] buf = headerBuffer.GetBuffer();

                int payloadSize = parseFrameHeader(buf);
                setReadingFramePayloadState(payloadSize);
            }
        }

        private void doReadPayload()
        {
            System.Diagnostics.Debug.Assert(
                state == InputState.READING_FRAME_PAYLOAD);

            MemoryStream dataBuffer = currentIncomingFrame.dataBuffer;
            int readn = 0;

            if (connection.connectedChannel != null)
            {
                readn = connection.connectedChannel.Receive(
                    dataBuffer.GetBuffer(),
                    (int)dataBuffer.Position,
                    (int)(dataBuffer.Capacity - dataBuffer.Position),
                    System.Net.Sockets.SocketFlags.None);
            }
            else if (connection.ssl != null)
            {
                readn = connection.readingQueue.Receive(
                    dataBuffer.GetBuffer(),
                    (int)dataBuffer.Position,
                    (int)(dataBuffer.Capacity - dataBuffer.Position));
            }

            if (readn == -1 || readn == 0)
            {
            // EOF - signal it as an artificial exception
            // so that the channel can be closed and removed
            // by higher layers
                throw new YAMIIOException("EOF");
            }

            dataBuffer.Position += readn;
            if(dataBuffer.Position == dataBuffer.Capacity)
            {
            // the whole payload has been read

                if (logCallback != null)
                {
                    logCallback.Log(LogEventArgs.LogLevel.HIGH, 
                        "Frame received:" 
                        + " from: " + target 
                        + " tid: " + currentIncomingFrameTransportId 
                        + " fid: " + currentIncomingFrameNumber);
                }

            // store this frame together with other frames
            // for the same message

                storeOrDispatchCurrentFrame();

                setReadingFrameHeaderState();
            }
        }

        private void doReadWholeFrame()
        {
        // ignore the remote address here, it is anyway the same
        // as the address that was used when this channel was created

            //TODO: question - what happend, if sender will sent more than
            // wholeFrameBuffer.Length == options.udpFrameSize bytes as one
            // UDP message?
            connection.datagramChannel.Receive(wholeFrameBuffer);

            injectFullFrame(wholeFrameBuffer);
        }

        private void storeOrDispatchCurrentFrame()
        {

            if (currentIncomingFrameNumber == -1)
            {
            // typical case:
            // this is a single-frame message and it is complete

                dispatchShortMessage(currentIncomingFrame.data);

            }
            else
            {
            // this is a multi-frame message
                IncomingMessageFrames messageFrames = null;
                if(!incomingFrames.ContainsKey(
                    currentIncomingFrameTransportId))
                {
                    messageFrames = new IncomingMessageFrames();
                    incomingFrames.Add(
                        currentIncomingFrameTransportId, messageFrames);
                }
                else
                {
                    messageFrames = 
                        incomingFrames[currentIncomingFrameTransportId];
                }

                bool messageIsComplete = messageFrames.accumulate(
                    currentIncomingFrameNumber, 
                    currentIncomingMessageHeaderSize, 
                    currentIncomingFrame);

                if (messageIsComplete)
                {

                // collect all frames together

                    IList<byte[]> headerBuffers = new List<byte[]>();
                    IList<byte[]> bodyBuffers = new List<byte[]>();

                    messageFrames.getAllBuffers(headerBuffers, bodyBuffers);

                // the frame manager for this message is no longer needed
                    incomingFrames.Remove(currentIncomingFrameTransportId);

                    dispatch(headerBuffers, bodyBuffers);
                }
            }
        }

        private void dispatchShortMessage(byte[] buf)
        {

            if (currentIncomingMessageHeaderSize > buf.Length)
            {
                throw new UnexpectedValueException(
                    "Corrupted message received.");
            }

        // extract message header and message body from this single buffer
            byte[] headerBuf = new byte[currentIncomingMessageHeaderSize];
            Array.Copy(buf, headerBuf, currentIncomingMessageHeaderSize);

            byte[] bodyBuf = 
                new byte[buf.Length - currentIncomingMessageHeaderSize];
            Array.Copy(buf, currentIncomingMessageHeaderSize, 
                bodyBuf, 0, bodyBuf.Length);

            IList<byte[]> headerBuffers = new List<byte[]>();
            headerBuffers.Add(headerBuf);
            IList<byte[]> bodyBuffers = new List<byte[]>();
            bodyBuffers.Add(bodyBuf);

            dispatch(headerBuffers, bodyBuffers);
        }

        private void dispatch(
            IList<byte[]> headerBuffers, IList<byte[]> bodyBuffers)
        {

            Parameters header = new Parameters();
            header.Deserialize(headerBuffers);

            Parameters body = null;
            byte[] rawBody = null;

            if (deliverAsRawBinary)
            {
            // repackage all body buffers into one
                int totalRawSize = 0;
                foreach (byte[] buf in bodyBuffers)
                {
                    totalRawSize += buf.Length;
                }

                rawBody = new byte[totalRawSize];
                int pos = 0;
                foreach (byte[] buf in bodyBuffers)
                {
                    for (int i = 0; i != buf.Length; ++i)
                    {
                        rawBody[pos++] = buf[i];
                    }
                }
            }
            else
            {
                body = new Parameters();
                body.Deserialize(bodyBuffers);
            }

            incomingMessageDispatchCallback.dispatch(
                target, header, body, rawBody);
        }

        private void setReadingFrameHeaderState()
        {
            headerBuffer.Position = 0;
            state = InputState.READING_FRAME_HEADER;
        }

        private void setReadingFramePayloadState(int payloadSize)
        {
            createCurrentIncomingFrame(payloadSize);
            state = InputState.READING_FRAME_PAYLOAD;
        }

        private void createCurrentIncomingFrame(int payloadSize)
        {
            currentIncomingFrame = new IncomingFrame(payloadSize);
        }
    }

}
