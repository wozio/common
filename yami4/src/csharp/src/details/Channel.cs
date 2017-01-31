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
using System.Net.Sockets;
using System.IO;

namespace Inspirel.YAMI.details
{
    internal class Channel
    {
        private readonly string target;
        private readonly Options options;

        internal ChannelWriter channelWriter;
        internal ChannelReader channelReader;

        private NetworkUtils.TransportChannel connection;

        private readonly LogCallback logCallback;
        private readonly LogEventArgs.LogLevel logLevel;

        internal Channel(string target, Options options, 
            IncomingMessageDispatchCallback incomingMessageDispatchCallback,
            IOWorker ioWorker,
            LogCallback logCallback, LogEventArgs.LogLevel logLevel)
        {
            this.target = target;
            this.options = options;

            this.logCallback = logCallback;
            this.logLevel = logLevel;

            connect(incomingMessageDispatchCallback, ioWorker);

            if (logCallback != null)
            {
                logCallback.Log(LogEventArgs.LogLevel.LOW, 
                    "Connected to " + target);
            }
        }

        // used by listener when accepting new connections
        internal Channel(Socket acceptedChannel, string sourceTarget, 
            IncomingMessageDispatchCallback incomingMessageDispatchCallback, 
            Options options, LogCallback logCallback, 
            LogEventArgs.LogLevel logLevel)
        {
            this.target = sourceTarget;
            this.options = options;

            this.connection = 
                new NetworkUtils.TransportChannel(acceptedChannel);

            this.logCallback = logCallback;
            this.logLevel = logLevel;

            createReaderWriter(incomingMessageDispatchCallback);

            if (logCallback != null)
            {
                logCallback.Log(LogEventArgs.LogLevel.LOW, 
                    "Accepted connection from " + target);
            }
        }

        public virtual void post(int transportId, int priority, 
            IList<byte[]> buffers, int messageHeaderSize, 
            MessageProgressCallback messageProgressCallback)
        {
            channelWriter.post(transportId, priority, buffers, 
                messageHeaderSize, messageProgressCallback);
        }

        public virtual bool postClose(int priority)
        {
            return channelWriter.postClose(priority);
        }

        public virtual void close()
        {
            if (connection != null)
            {
                try
                {
                    connection.close();
                }
                catch(Exception)
                {
                // ignore
                }
                connection = null;

                channelWriter.notifyCancellation();

                if (logCallback != null)
                {
                    logCallback.Log(LogEventArgs.LogLevel.LOW, 
                        "Closed connection to " + target);
                }
            }
        }

        public static int groupBuffers(List<byte[]> allBuffers, 
            IList<byte[]> headerBuffers, IList<byte[]> contentBuffers, 
            int chunkSize)
        {
        // optimize for the common case where the header and content
        // can fit in a single frame
            int messageHeaderSize = 0;
            int totalSerializedSize = 0;
            foreach (byte[] buf in headerBuffers)
            {
                messageHeaderSize += buf.Length;
                totalSerializedSize += buf.Length;
            }
            foreach (byte[] buf in contentBuffers)
            {
                totalSerializedSize += buf.Length;
            }

            if (totalSerializedSize <= chunkSize)
            {
            // all buffers can fit in a single frame - repackage them

                byte[] singleBuffer = new byte[totalSerializedSize];
                int bufIndex = 0;
                foreach (byte[] buf in headerBuffers)
                {
                    foreach (byte b in buf)
                    {
                        singleBuffer[bufIndex++] = b;
                    }
                }
                foreach (byte[] buf in contentBuffers)
                {
                    foreach (byte b in buf)
                    {
                        singleBuffer[bufIndex++] = b;
                    }
                }
                allBuffers.Add(singleBuffer);
            }
            else
            {

            // buffers are too big to fit in a single frame
            // - post them separately

                allBuffers.Capacity = 
                    headerBuffers.Count + contentBuffers.Count;
                allBuffers.AddRange(headerBuffers);
                allBuffers.AddRange(contentBuffers);
            }

            return messageHeaderSize;
        }

        internal virtual void injectFullFrame(byte[] frameBuffer)
        {
            channelReader.injectFullFrame(frameBuffer);
        }

        internal class SelectionKeys
        {
            internal Socket key;
            internal bool blockingChannelReadyForReading;
            internal bool blockingChannelReadyForWriting;
        }

        internal virtual SelectionKeys registerForSelection(Selector selector,
            bool allowInput, bool allowOutput, bool useBlockingOnly)
        {

            Selector.Direction operations = Selector.Direction.NONE;
            if (allowInput)
            {
                operations |= Selector.Direction.READ;
            }

        // do not register for output if there is nothing to write
            bool hasSomeOutgoingFrames = 
                channelWriter.hasSomeOutgoingFrames();

            if (allowOutput && hasSomeOutgoingFrames)
            {
                operations |= Selector.Direction.WRITE;
            }

            Socket key = null;
            if (operations != 0)
            {
                try
                {
                    key = connection.register(selector, operations, useBlockingOnly);
                }
                    //TODO replace with proper exception
                catch
                //catch (ClosedChannelException ex)
                {
                // ignore, will never happen
                }
            }

            SelectionKeys keys = new SelectionKeys();
            keys.key = key;
            keys.blockingChannelReadyForReading =
                    ((operations & Selector.Direction.READ) != 0) &&
                    connection.readingQueue != null &&
                    connection.readingQueue.HasDataOrEOF();
            keys.blockingChannelReadyForWriting = (operations & Selector.Direction.WRITE) != 0;
            
            return keys;
        }

        internal virtual void doSomeWork(
            bool allowInput, bool allowOutput)
        {
            if (allowInput)
            {
                channelReader.doSomeInput();
            }
            if (allowOutput)
            {
                channelWriter.doSomeOutput();
            }
        }

        internal virtual string Target
        {
            get
            {
                return target;
            }
        }

        private void connect(
            IncomingMessageDispatchCallback incomingMessageDispatchCallback, IOWorker ioWorker)
        {
            if (NetworkUtils.protocolIsTcp(target))
            {
                connection = NetworkUtils.connectTcp(target, options);
            }
            else if (NetworkUtils.protocolIsTcps(target))
            {
                connection = NetworkUtils.connectTcps(target, options, ioWorker);

                // additionally, instruct I/O Worker that blocking sockets
                // are the only ones that should be acted upon

                ioWorker.UseBlockingChannelsOnly();
            }
            else if (NetworkUtils.protocolIsUdp(target))
            {
                connection = NetworkUtils.createUdp(target, options);
            }
            else if (target.Equals("null"))
            {
            // do nothing - this protocol is used for testing only
                channelWriter = new ChannelWriter(
                    null, null, null, LogEventArgs.LogLevel.LOW);
            }
            else
            {
                throw new BadProtocolException(target);
            }

            createReaderWriter(incomingMessageDispatchCallback);
        }

        private void createReaderWriter(
            IncomingMessageDispatchCallback incomingMessageDispatchCallback)
        {
            channelWriter = new ChannelWriter(
                connection, target, logCallback, logLevel);

            channelReader = 
                new ChannelReader(
                    connection, incomingMessageDispatchCallback, 
                    target, options, logCallback, logLevel);
        }

    // for unit tests
        internal virtual IList<OutgoingFrame> OutgoingFrames
        {
            get
            {
                return channelWriter.OutgoingFrames;
            }
        }
    }
}
