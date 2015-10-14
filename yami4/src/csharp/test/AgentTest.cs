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

// define WAIT_FOR_CLOSING to make testConnectionEvents() 
// wait for connection closing

using System;
using System.Threading;
using NUnit.Framework;
using System.Diagnostics;
using System.Net.Sockets;
using System.Text;

namespace Inspirel.YAMI
{
    [TestFixture]
    public class AgentTest
    {

        private const string objectName = "object";
        private const string messageName = "message";

        private Agent serverAgent;
        private string serverAddress;

        private Agent clientAgent;

    // helper for passing ackowledgements
        private class BooleanHolder
        {
            internal bool value;

            internal BooleanHolder(bool value)
            {
                this.value = value;
            }
        }

//    private static class MyLogSink implements LogCallback {
//        private String name;
//        
//        private MyLogSink(String name) {
//            this.name = name;
//        }
//        
//        @Override
//        public void log(LogLevel level, String message) {
//            System.out.println(name + level.toString() + ": " + message);
//        }
//    }

        private int threadsCount = 0;

        [SetUp]
        public void setUp()
        {
            threadsCount = Process.GetCurrentProcess().Threads.Count;

            serverAgent = new Agent();
            serverAddress = serverAgent.AddListener("tcp://*:*");

            clientAgent = new Agent();
        }

        [TearDown]
        public void tearDown()
        {
            serverAgent.Close();
            clientAgent.Close();

        // there should be no YAMI threads at this point

            //TODO - I don't know how to check if all YAMI threads end
            //Thread.Sleep(1000);
            //Assert.AreEqual(threadsCount, 
            //    Process.GetCurrentProcess().Threads.Count);

            //const int maxThreads = 20; // arbitrary
            //Thread[] threads = new Thread[maxThreads];
            //int numOfThreads = Thread.enumerate(threads);
            //for (int i = 0; i != numOfThreads; ++i)
            //{
            //    string threadName = threads[i].Name;
            //    Assert.IsFalse(threadName.Contains("YAMI"));
            //}
        }

        /// <summary> 
        /// attempt to send a message to non-existing agent 
        /// </summary>
        [Test]
        public void testNoReceiver()
        {
            try
            {
                clientAgent.SendOneWay(
                    "tcp://nosuchaddress:12345", 
                    "nosuchobject", "badmessage", null);
                Assert.Fail("should not reach this point");
            }
            catch (YAMIIOException ex)
            {
                Assert.AreNotEqual(null, ex.InnerException);
                SocketException innerEx = 
                    ex.InnerException as SocketException;
                Assert.AreNotEqual(null, innerEx);
                Assert.AreEqual(
                    SocketError.HostNotFound, innerEx.SocketErrorCode);
            }

            try
            {
                // a bit dodgy, but 4 is an unassigned port in the list
                // of well-known services, so there is a chance that
                // no existing process uses it on the machine where this test
                // is executed
                // - if this test fails then it is a sign that some process
                // has a listening socket on port 4 - pick another dummy 
                // number

                clientAgent.SendOneWay("tcp://localhost:4", 
                    "nosuchobject", "badmessage", null);
                Assert.Fail("should not reach this point");
            }
            catch(YAMIIOException ex)
            {
                Assert.AreNotEqual(null, ex.InnerException);
                SocketException innerEx = 
                    ex.InnerException as SocketException;
                Assert.AreNotEqual(null, innerEx);
                Assert.AreEqual(
                    SocketError.TimedOut, innerEx.SocketErrorCode);
            }
        }

        /// <summary> 
        /// message sent to nonexisting object 
        /// </summary>
        [Test]
        public void testNoSuchObjectOneWay()
        {

            // one-way message does not report any error
            clientAgent.SendOneWay(
                serverAddress, "nosuchobject", "badmessage", null);

            // two-way message is rejected
            OutgoingMessage message = clientAgent.Send(
                serverAddress, "nosuchobject", "badmessage", null);
            message.WaitForCompletion();
            Assert.AreEqual(
                OutgoingMessage.MessageState.REJECTED, message.State);
            Assert.AreEqual("Unknown destination object.", 
                message.ExceptionMsg);

            message.Close();
        }

        /// <summary> 
        /// message sent to nonexisting object, 
        /// explicit connection management 
        /// </summary>
        [Test]
        public void testNoSuchObjectOneWayExplicitConn()
        {

            bool autoConnect = false;

            // message fails if there is no channel and no auto-connect
            try
            {
                clientAgent.Send(serverAddress, 
                    "nosuchobject", "badmessage", null, 0, autoConnect);
                Assert.Fail("should not reach this point");
            }
            catch(YAMIIOException ex)
            {
                Assert.IsTrue(ex.Message.Contains("I/O Error"));
            }

            // explicitly open the channel
            clientAgent.OpenConnection(serverAddress);

            // message is successfully sent over existing channel,
            // but later rejected by server
            OutgoingMessage message = clientAgent.Send(
                serverAddress, "nosuchobject", "badmessage", 
                null, 0, autoConnect);
            message.WaitForCompletion();
            Assert.AreEqual(
                OutgoingMessage.MessageState.REJECTED, message.State);
            Assert.AreEqual(
                "Unknown destination object.", message.ExceptionMsg);

            message.Close();
        }

        /// <summary> message sent and replied to </summary>
        [Test]
        public void testSendReply()
        {

            //BooleanHolder gotMessage = new BooleanHolder(false);
            bool gotMessage = false;

            serverAgent.RegisterObject(
                objectName, 
                delegate(object sender, IncomingMessageArgs args)
                {
                    gotMessage = true;
                    Assert.AreEqual(objectName, args.Message.ObjectName);
                    Assert.AreEqual(messageName, args.Message.MessageName);
                    Parameters receivedContent = args.Message.Parameters;
                    Assert.AreEqual(1, receivedContent.Count);
                    Assert.AreEqual(
                        "ping", receivedContent.GetString("value"));
                    receivedContent.SetString("value", "pong");
                    args.Message.Reply(receivedContent);
                }
            );

            Parameters content = new Parameters();
            content.SetString("value", "ping");

            OutgoingMessage message = clientAgent.Send(
                serverAddress, objectName, messageName, content);

            message.WaitForTransmission();

        // after transmission the whole message is pushed out
            OutgoingMessage.MessageStateInfo messageInfo = message.StateInfo;
            Assert.IsTrue(
                messageInfo.SentBytes == messageInfo.TotalByteCount);

            message.WaitForCompletion();

            Assert.IsTrue(gotMessage);
            Assert.AreEqual(
                OutgoingMessage.MessageState.REPLIED, message.State);
            content = message.Reply;
            Assert.AreEqual("pong", content.GetString("value"));

            message.Close();
        }

        /// <summary> 
        /// message rejected by server 
        /// </summary>
        [Test]
        public void testSendReject()
        {
            bool gotMessage = false;

                // expect empty parameters if null is sent
            serverAgent.RegisterObject(objectName,
                delegate(object sender, IncomingMessageArgs args)
                {
                    gotMessage = true;
                    Parameters content = args.Message.Parameters;
                    Assert.AreEqual(0, content.Count);
                    args.Message.Reject("some reason");
                });

            OutgoingMessage message = clientAgent.Send(
                serverAddress, objectName, messageName, null);

            message.WaitForCompletion();

            Assert.IsTrue(gotMessage);

            Assert.AreEqual(
                OutgoingMessage.MessageState.REJECTED, message.State);
            Assert.AreEqual("some reason", message.ExceptionMsg);

            message.Close();
        }

        /// <summary> 
        /// message rejected due to exception 
        /// in user code at the server side 
        /// </summary>
        [Test]
        public void testSendRejectDueToException()
        {
            bool gotMessage = false;

            serverAgent.RegisterObject(objectName,
                delegate(object sender, IncomingMessageArgs args)
                {
                    gotMessage = true;
                    throw new Exception("something bad happened");
                });

            OutgoingMessage message = clientAgent.Send(
                serverAddress, objectName, messageName, null);

            message.WaitForCompletion();

            Assert.IsTrue(gotMessage);

            Assert.AreEqual(
                OutgoingMessage.MessageState.REJECTED, message.State);
            //TODO: check if message should be explicit equal to 
            // message set in exception on server side
            Assert.IsTrue(
                message.ExceptionMsg.Contains("something bad happened"));

            message.Close();
        }

        /// <summary> 
        /// big messages sent with different priorities 
        /// </summary>
        [Test]
        public void testBigMessages()
        {

        // Note:
        // The messages are sent with different priorities, which means
        // that they might complete in the order that is different from the
        // order of posting them to the outgoing queue.
        // The messages are posted with increasing priorities (first message
        // is sent with lowest priority, last message with highest),
        // so it is *very likely* that they will be received by server
        // in the reversed order, but this cannot be guaranteed as there is
        // no relation between the speed of posting and the speed
        // of transmission.

            int numOfMessages = 10;
            bool[] gotMessages = new bool[numOfMessages];

            int sizeOfBigString = 1000000;
            StringBuilder bigStringBld = new StringBuilder(sizeOfBigString);
            for (int i = 0; i != sizeOfBigString; ++i)
            {
                bigStringBld.Append('x');
            }
            string bigString = bigStringBld.ToString();


            serverAgent.RegisterObject(objectName, 
                delegate(object sender, IncomingMessageArgs args)
                {
                    Parameters receivedContent = args.Message.Parameters; 
                    
                    int id = receivedContent.GetInteger("id");
                    // uncomment it to see how the messages get reordered
                    // verify the big value
                    //Console.WriteLine("received message {0}", id);
                    
                    gotMessages[id] = true; 
                    Assert.AreEqual(bigString, 
                        receivedContent.GetString("big")); 
                    args.Message.Reply(null); 
                });

            Parameters content = new Parameters();
            content.SetString("big", bigString);

            OutgoingMessage[] messages = 
                new OutgoingMessage[numOfMessages];

        // send all messages with different ids and priorities
            for (int i = 0; i != numOfMessages; ++i)
            {
                int id = i;
                int priority = i; // increasing priority

                content.SetInteger("id", id);
                messages[i] = clientAgent.Send(
                    serverAddress, objectName, 
                    messageName, content, priority);
            }

        // wait for all messages to complete
            for (int i = 0; i != numOfMessages; ++i)
            {
                messages[i].WaitForCompletion();
                messages[i].Close();
            }

            for (int i = 0; i != numOfMessages; ++i)
            {
                Assert.IsTrue(gotMessages[i]);
            }
        }

        /// <summary> 
        /// message sent to load-balanced pair of destinations 
        /// </summary>
        [Test]
        public void testLoadBalancing()
        {
            Agent serverAgent1 = new Agent();
            string serverAddress1 = serverAgent1.AddListener("tcp://*:*");

            Agent serverAgent2 = new Agent();
            string serverAddress2 = serverAgent2.AddListener("tcp://*:*");

            string loadBalancedTarget = string.Format(
                "failover:({0}|{1})", serverAddress1, serverAddress2);

            bool gotMessage1 = false;
            bool gotMessage2 = false;

            serverAgent1.RegisterObject(objectName, 
                delegate(object sender, IncomingMessageArgs args)
                {
                    gotMessage1 = true;
                    args.Message.Reply(null);
                });

            serverAgent2.RegisterObject(objectName, 
                delegate(object sender, IncomingMessageArgs args)
                {
                    gotMessage2 = true;
                    args.Message.Reply(null);
                });


            OutgoingMessage message = clientAgent.Send(
                loadBalancedTarget, objectName, messageName, null);

            // since this is a load-balanced (and failover) target,
            // the message is implicitly waited for completion
            Assert.AreEqual(
                OutgoingMessage.MessageState.REPLIED, message.State);

            // exactly one of two servers got the message 
            // (exclusive-or of 1st or 2nd receiver gotMessage)
            Assert.IsTrue(gotMessage1 ^ gotMessage2);

            message.Close();

            serverAgent1.Close();
            serverAgent2.Close();
        }

        /// <summary> 
        /// message sent to failover pair of destinations 
        /// </summary>
        [Test]
        public void testFailOver()
        {
        // the failover pair consists of one proper address and one
        // that is certainly not working

            string brokenTarget = "tcp://nosuchhost:4";
            string failoverTarget = string.Format(
                "failover:({0}|{1})", serverAddress, brokenTarget);

            bool gotMessage = false;

            serverAgent.RegisterObject(objectName, 
                delegate(object sender, IncomingMessageArgs args)
                {
                    gotMessage = true;
                    args.Message.Reply(null);
                });

            OutgoingMessage message = clientAgent.Send(
                failoverTarget, objectName, messageName, null);

            // since this is a failover target,
            // the message is implicitly waited for completion
            Assert.AreEqual(
                OutgoingMessage.MessageState.REPLIED, message.State);

            // the working server in the failover pair got the message
            Assert.IsTrue(gotMessage);

            message.Close();
        }

        /// <summary> 
        /// empty failover group is an error 
        /// </summary>
        [Test]
        public void testEmptyFailover()
        {
            try
            {
                clientAgent.SendOneWay(
                    "failover:()", objectName, messageName, null);
                Assert.Fail("should not reach this point");
            }
            catch(Exception ex)
            {
                Assert.IsTrue(ex.Message.Contains(
                    "Empty failover group is not allowed."));
            }
        }

        /// <summary> 
        /// raw binary message and reply 
        /// </summary>
        [Test]
        public void testRawBinary()
        {
            bool gotMessage = false;

            Encoding encoding = Encoding.UTF8;
            byte[] messageContent = 
                encoding.GetBytes("Hello this is raw binary message");
            byte[] replyContent = 
                encoding.GetBytes("Hi and this is raw binary response !");

            Parameters options = new Parameters();
            options.SetBoolean(OptionNames.DELIVER_AS_RAW_BINARY, true);

            Agent rawServerAgent = new Agent(options);
            string rawServerAddress = 
                rawServerAgent.AddListener("tcp://*:*");

            Agent rawClientAgent = new Agent(options);

            rawServerAgent.RegisterObject(objectName, 
                delegate(object sender, IncomingMessageArgs args)
                {
                    gotMessage = true;
                    byte[] content = args.Message.RawContent;
                    Assert.AreEqual(messageContent, content);
                    args.Message.Reply(
                        new RawBinaryDataSource(replyContent));
                });
 
            OutgoingMessage message = rawClientAgent.Send(
                rawServerAddress, objectName, messageName, 
                new RawBinaryDataSource(messageContent));

            message.WaitForCompletion();
            Assert.IsTrue(gotMessage);

            Assert.AreEqual(
                OutgoingMessage.MessageState.REPLIED, message.State);

            byte[] reply = message.RawReply;
            Assert.AreEqual(replyContent, reply);

            message.Close();

            rawClientAgent.Close();
            rawServerAgent.Close();
        }

        /// <summary>
        /// helper for testConnectionEvents
        /// </summary>
        class EventCallback
        {
            private StringBuilder events = new StringBuilder();
            
            public string Events
            {
                get
                {
                    return events.ToString();
                }
            }

            public void Report(object sender, ConnectionEventArgs args)
            {
                switch(args.Event)
                {
                case ConnectionEventArgs.ConnectionEvent
                    .NEW_INCOMING_CONNECTION:
                    if(events.Length > 0)
                        throw new Exception("too much incomings...");
                    events.Append("incoming ");
                    break;
                case ConnectionEventArgs.ConnectionEvent
                    .NEW_OUTGOING_CONNECTION:
                    events.Append("outgoing ");
                    break;
                case ConnectionEventArgs.ConnectionEvent
                    .CONNECTION_CLOSED:
                    events.Append("closed ");
                    break;
                }
            }
        }

        /// <summary> 
        /// connection event notifications 
        /// </summary>
        [Test]
        public void testConnectionEvents()
        {
            EventCallback serverCallback = new EventCallback();
            serverAgent.ConnectionsChanged += serverCallback.Report;

            string serverAddr = serverAgent.AddListener("tcp://*:*");

            EventCallback clientCallback = new EventCallback();
            clientAgent.ConnectionsChanged += clientCallback.Report;

            // no communication yet -> no connection
            Assert.AreEqual("", serverCallback.Events);
            Assert.AreEqual("", clientCallback.Events);

            OutgoingMessage msg = clientAgent.Send(
                serverAddr, "no_such_object", "hello", null);
            msg.WaitForCompletion();

            // one connection open
            Assert.AreEqual("incoming ", serverCallback.Events);
            Assert.AreEqual("outgoing ", clientCallback.Events);

            clientAgent.CloseConnection(serverAddr);

#if WAIT_FOR_CLOSING
            // yield to ensure about closing connections both sides
            Thread.Sleep(200);

            Assert.AreEqual("incoming closed ", serverCallback.Events);
            Assert.AreEqual("outgoing closed ", clientCallback.Events);
#else
            // one connection open and one closed,
            // but it is a race for both the client and the server
            Assert.IsTrue(serverCallback.Events == "incoming " 
                || serverCallback.Events == "incoming closed ");
            Assert.IsTrue(clientCallback.Events == "outgoing " 
                || clientCallback.Events == "outgoing closed ");
#endif
        }
    }

}