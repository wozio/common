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

namespace Inspirel.YAMI
{

    using NUnit.Framework;
    using System.Collections.Generic;
    using System.IO;
    using System.Net.Sockets;
    using System.Net;
    using System;
    using Inspirel.YAMI.details;

    [TestFixture]
    public class ChannelTest
    {
        public ChannelTest()
        {
        }

        [Test]
        public virtual void testForBadProtocol()
        {
            try
            {
                new Channel("abc", new Options(null), 
                    null, null, LogEventArgs.LogLevel.LOW);
                Assert.Fail("should never reach this point");
            }
            catch(BadProtocolException ex)
            {
                Assert.IsTrue(ex.Message.Equals(
                    "The protocol 'abc' is not supported."));
            }
            catch(YAMIIOException)
            {
                Assert.Fail("should never reach this point");
            }
            catch(SocketException)
            {
                Assert.Fail("should never reach this point");
            }
        }

        [Test]
        public virtual void testForNodeInsertion()
        {
            Channel ch = null;
            try
            {
                ch = new Channel("null", new Options(null), 
                    null, null, LogEventArgs.LogLevel.LOW);
            }
            catch(YAMIIOException)
            {
                Assert.Fail("should never reach this point");
            }
            catch(SocketException)
            {
                Assert.Fail("should never reach this point");
            }

            Assert.IsTrue(ch.OutgoingFrames.Count == 0);

            // first node

            byte[] buf = new byte[123];
            List<byte[]> buffers = new List<byte[]>();
            buffers.Add(buf);

            ch.post(0, 0, buffers, 0, null);

            IList<OutgoingFrame> frames = ch.OutgoingFrames;
            Assert.IsTrue(frames.Count == 1);
            Assert.IsTrue(frames[0].payload.Length == 123);

            // second node, added after 1st

            buf = new byte[456];
            buffers.Clear();
            buffers.Add(buf);

            ch.post(1, 0, buffers, 0, null);

            frames = ch.OutgoingFrames;
            Assert.IsTrue(frames.Count == 2);
            Assert.IsTrue(frames[0].payload.Length == 123);
            Assert.IsTrue(frames[1].payload.Length == 456);

            // third node, at the end

            buf = new byte[789];
            buffers.Clear();
            buffers.Add(buf);

            ch.post(2, 0, buffers, 0, null);

            frames = ch.OutgoingFrames;
            Assert.IsTrue(frames.Count == 3);
            Assert.IsTrue(frames[0].payload.Length == 123);
            Assert.IsTrue(frames[1].payload.Length == 456);
            Assert.IsTrue(frames[2].payload.Length == 789);

            // three buffers inserted after first node

            buffers.Clear();
            buffers.Add(new byte[1]);
            buffers.Add(new byte[2]);
            buffers.Add(new byte[3]);

            ch.post(3, 3, buffers, 0, null);

            frames = ch.OutgoingFrames;
            Assert.IsTrue(frames.Count == 6);
            Assert.IsTrue(frames[0].payload.Length == 123);
            Assert.IsTrue(frames[1].payload.Length == 1);
            Assert.IsTrue(frames[2].payload.Length == 2);
            Assert.IsTrue(frames[3].payload.Length == 3);
            Assert.IsTrue(frames[4].payload.Length == 456);
            Assert.IsTrue(frames[5].payload.Length == 789);

            // two new nodes after the previous three

            buffers.Clear();
            buffers.Add(new byte[4]);
            buffers.Add(new byte[5]);

            ch.post(4, 1, buffers, 0, null);

            frames = ch.OutgoingFrames;
            Assert.IsTrue(frames.Count == 8);
            Assert.IsTrue(frames[0].payload.Length == 123);
            Assert.IsTrue(frames[1].payload.Length == 1);
            Assert.IsTrue(frames[2].payload.Length == 2);
            Assert.IsTrue(frames[3].payload.Length == 3);
            Assert.IsTrue(frames[4].payload.Length == 4);
            Assert.IsTrue(frames[5].payload.Length == 5);
            Assert.IsTrue(frames[6].payload.Length == 456);
            Assert.IsTrue(frames[7].payload.Length == 789);

            // poison pill added between 3 and 4

            bool closeMe = ch.postClose(2);
            Assert.IsFalse(closeMe);

            frames = ch.OutgoingFrames;
            Assert.AreEqual(5, frames.Count);
            Assert.IsTrue(frames[0].payload.Length == 123);
            Assert.IsTrue(frames[1].payload.Length == 1);
            Assert.IsTrue(frames[2].payload.Length == 2);
            Assert.IsTrue(frames[3].payload.Length == 3);
            Assert.IsTrue(frames[4].closeFlag);
        }

        [Test]
        public virtual void testForImmediateClose()
        {
            Channel ch = null;
            try
            {
                ch = new Channel("null", new Options(null), 
                    null, null, LogEventArgs.LogLevel.LOW);
            }
            catch(YAMIIOException)
            {
                Assert.Fail("should never reach this point");
            }
            catch(SocketException)
            {
                Assert.Fail("should never reach this point");
            }

            Assert.IsTrue(ch.OutgoingFrames.Count == 0);

            bool closeMe = ch.postClose(0);
            Assert.IsTrue(closeMe);
            Assert.IsTrue(ch.OutgoingFrames.Count == 0);
        }

        [Test]
        public virtual void testForInvalidConnection()
        {
            try
            {
                new Channel("tcp://invalid", new Options(null), 
                    null, null, LogEventArgs.LogLevel.LOW);
                Assert.Fail("should never reach this point");
            }
            catch(YAMIIOException)
            {
                Assert.Fail("should never reach this point");
            }
            catch(SocketException)
            {
                Assert.Fail("should never reach this point");
            }
            catch(BadProtocolException)
            {
                // this is the right exception,
                // because the port number was not provided
            }

            bool wasCatched = false;
            try
            {
                new Channel("tcp://invalidhostname:12345", 
                    new Options(null), null, 
                    null, LogEventArgs.LogLevel.LOW);
                Assert.Fail("should never reach this point");
            }
            catch(YAMIIOException)
            {
                // this is the right exception,
                // because the target host does not exist
                wasCatched = true;
            }
            catch(SocketException)
            {
                // this is the right exception,
                // because the target host does not exist
                wasCatched = true;
            }
            catch(BadProtocolException)
            {
                Assert.Fail("should never reach this point");
            }

            Assert.IsTrue(wasCatched);
        }

        [Test]
        public void testForEstablishedConnection()
        {
            try
            {
                // artificial listening socket for this test
                Socket server = new Socket(
                    AddressFamily.InterNetwork, 
                    SocketType.Stream, ProtocolType.Tcp);
                // bind to system-assigned port
                server.Bind(new IPEndPoint(IPAddress.Loopback, 0)); 
                server.Listen(1);

                string target = "tcp://127.0.0.1:" + 
                    ((IPEndPoint)server.LocalEndPoint).Port.ToString();

                try
                {
                    Parameters parameters = new Parameters();
                    parameters.SetInteger(
                        OptionNames.TCP_CONNECT_TIMEOUT, 1000);
                    Channel ch = new Channel(
                        target, new Options(parameters), null, 
                        null, LogEventArgs.LogLevel.LOW);

                    Socket accepted = server.Accept();

                    ch.close();
                    accepted.Close();
                    server.Close();
                }
                catch(Exception)
                {
                    Assert.Fail("should never reach this point");
                }
            }
            catch(Exception)
            {
                Assert.Fail("should never reach this point");
            }
        }

        [Test]
        public void testForSimpleOutput()
        {
            try
            {
                // artificial listening socket for this test
                Socket server = new Socket(
                    AddressFamily.InterNetwork, 
                    SocketType.Stream, ProtocolType.Tcp);
                // bind to system-assigned port
                server.Bind(new IPEndPoint(IPAddress.Loopback, 0)); 
                server.Listen(1);

                string target = "tcp://127.0.0.1:" +
                    ((IPEndPoint)server.LocalEndPoint).Port.ToString();

                try
                {
                    Channel ch = new Channel(target, new Options(null), 
                        null, null, LogEventArgs.LogLevel.LOW);

                    Socket accepted = server.Accept();

                    IList<byte[]> buffers = new List<byte[]>();
                    buffers.Add(new byte[] { 10, 11, 12, 13 });

                    ProgressCallback callback = new ProgressCallback();

                    int messageHeaderSize = 4;
                    ch.post(123, 0, buffers, messageHeaderSize, callback);

                    ch.doSomeWork(false, true);

                    // the buffers are short and the whole can be expected
                    // to be pushed in a single operation

                    Assert.IsTrue(callback.progressCalled);
                    Assert.IsTrue(callback.sentBytes == 
                        callback.totalByteCount);
                    Assert.IsFalse(callback.cancelledCalled);

                    // read the data on the receiving end

                    byte[] buf = new byte[1024];
                    int readBytes = accepted.Receive(buf);

                    Assert.IsTrue(readBytes == 20);

                    // message id
                    Assert.IsTrue(buf[0] == 123);
                    Assert.IsTrue(buf[1] == 0);
                    Assert.IsTrue(buf[2] == 0);
                    Assert.IsTrue(buf[3] == 0);

                    // frame number (-1 -> it is the only one)
                    Assert.IsTrue(buf[4] == 0xFF);
                    Assert.IsTrue(buf[5] == 0xFF);
                    Assert.IsTrue(buf[6] == 0xFF);
                    Assert.IsTrue(buf[7] == 0xFF);

                    // size of message header
                    Assert.IsTrue(buf[8] == 4);
                    Assert.IsTrue(buf[9] == 0);
                    Assert.IsTrue(buf[10] == 0);
                    Assert.IsTrue(buf[11] == 0);

                    // frame payload
                    Assert.IsTrue(buf[12] == 4);
                    Assert.IsTrue(buf[13] == 0);
                    Assert.IsTrue(buf[14] == 0);
                    Assert.IsTrue(buf[15] == 0);

                    // payload
                    Assert.IsTrue(buf[16] == 10);
                    Assert.IsTrue(buf[17] == 11);
                    Assert.IsTrue(buf[18] == 12);
                    Assert.IsTrue(buf[19] == 13);

                    ch.close();
                    accepted.Close();
                    server.Close();
                }
                catch(Exception ex)
                {
                    Console.WriteLine(ex.Message);
                    Console.WriteLine(ex.StackTrace);
                    Assert.Fail("should never reach this point");
                }
            }
            catch(Exception)
            {
                Assert.Fail("should never reach this point");
            }
        }

        private class ProgressCallback : MessageProgressCallback
        {
            internal int sentBytes;
            internal int totalByteCount;
            internal bool progressCalled = false;
            internal bool cancelledCalled = false;

            public void progress(int progrSentBytes, int progrTotalByteCount)
            {
                this.sentBytes = progrSentBytes;
                this.totalByteCount = progrTotalByteCount;
                progressCalled = true;
                if(sentBytes == totalByteCount && sentBytes == 0)
                {
                    cancelledCalled = true;
                }
            }

            public void replied(Parameters body, byte[] rawBody)
            {
                // not needed in this test
            }

            public void rejected(string reason)
            {
                // not needed in this test
            }
        }
    }

}