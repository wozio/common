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

package com.inspirel.yami;

import java.util.Arrays;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

public class AgentTest {
    
    private static final String objectName = "object";
    private static final String messageName = "message";

    private Agent serverAgent;
    private String serverAddress;
    
    private Agent clientAgent;
    
    // helper for passing ackowledgements
    private static class BooleanHolder {
        boolean value;

        BooleanHolder(boolean value) {
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
    
    @Before
    public void setUp() throws YAMIIOException {
        serverAgent = new Agent();
        serverAddress = serverAgent.addListener("tcp://*:*");
        
        clientAgent = new Agent();
    }
    
    @After
    public void tearDown() {
        serverAgent.close();
        clientAgent.close();
        
        // there should be no YAMI threads at this point
        
        final int maxThreads = 20; // arbitrary
        Thread[] threads = new Thread[maxThreads];
        int numOfThreads = Thread.enumerate(threads);
        for (int i = 0; i != numOfThreads; ++i) {
            String threadName = threads[i].getName();
            assertFalse(threadName.contains("YAMI"));
        }
    }
    
    /**
     * attempt to send a message to non-existing agent
     */
    @Test
    public void testNoReceiver() {
        try {
            clientAgent.sendOneWay("tcp://nosuchaddress:12345",
                    "nosuchobject", "badmessage", null);
            fail("should not reach this point");
        } catch (YAMIIOException ex) {
            assertTrue(ex.getMessage().contains("Unresolved address"));
        }

        try {
            // a bit dodgy, but 4 is an unassigned port in the list
            // of well-known services, so there is a chance that
            // no existing process uses it on the machine where this test
            // is executed
            // - if this test fails then it is a sign that some process
            // has a listening socket on port 4 - pick another dummy number
            
            clientAgent.sendOneWay("tcp://localhost:4",
                    "nosuchobject", "badmessage", null);
            fail("should not reach this point");
        } catch (YAMIIOException ex) {
            assertTrue(ex.getMessage().contains("Connection refused"));
        }
    }
    
    /**
     * message sent to nonexisting object
     */
    @Test
    public void testNoSuchObjectOneWay() throws YAMIIOException {

        // one-way message does not report any error
        clientAgent.sendOneWay(serverAddress,
                "nosuchobject", "badmessage", null);
        
        // two-way message is rejected
        OutgoingMessage message = clientAgent.send(serverAddress,
                "nosuchobject", "badmessage", null);
        message.waitForCompletion();
        assertTrue(message.getState() ==
                OutgoingMessage.MessageState.REJECTED);
        assertTrue(message.getExceptionMsg().equals(
                "Unknown destination object."));
        
        message.close();
    }
    
    /**
     * message sent to nonexisting object, explicit connection management
     */
    @Test
    public void testNoSuchObjectOneWayExplicitConn() throws YAMIIOException {

        boolean autoConnect = false;
        
        // message fails if there is no channel and no auto-connect
        try {
            clientAgent.send(serverAddress,
                    "nosuchobject", "badmessage", null, 0, autoConnect);
            fail("should not reach this point");
        } catch (YAMIIOException ex) {
            assertTrue(ex.getMessage().contains("I/O Error"));
        }
        
        // explicitly open the channel
        
        clientAgent.openConnection(serverAddress);
        
        // message is successfully sent over existing channel,
        // but later rejected by server
        
        OutgoingMessage message = clientAgent.send(serverAddress,
                "nosuchobject", "badmessage", null, 0, autoConnect);
        message.waitForCompletion();
        assertTrue(message.getState() ==
                OutgoingMessage.MessageState.REJECTED);
        assertTrue(message.getExceptionMsg().equals(
                "Unknown destination object."));
        
        message.close();
    }
    
    /**
     * message sent and replied to
     */
    @Test
    public void testSendReply() throws YAMIIOException {

        final BooleanHolder gotMessage = new BooleanHolder(false);
        
        serverAgent.registerObject(objectName,
                new IncomingMessageCallback() {

            @Override
            public void call(IncomingMessage message) throws Exception {
                gotMessage.value = true;
                
                assertTrue(message.getObjectName().equals(objectName));
                assertTrue(message.getMessageName().equals(messageName));
                
                Parameters content = message.getParameters();
                assertTrue(content.size() == 1);
                assertTrue(content.getString("value").equals("ping"));
                
                content.setString("value", "pong");
                message.reply(content);
            }
        });
        
        Parameters content = new Parameters();
        content.setString("value", "ping");
        
        OutgoingMessage message = clientAgent.send(serverAddress,
                objectName, messageName, content);
        
        message.waitForTransmission();

        // after transmission the whole message is pushed out
        OutgoingMessage.MessageStateInfo messageInfo =
                message.getStateInfo();
        assertTrue(messageInfo.sentBytes == messageInfo.totalByteCount);

        message.waitForCompletion();
        
        assertTrue(gotMessage.value);

        assertTrue(message.getState() ==
                OutgoingMessage.MessageState.REPLIED);
        content = message.getReply();
        assertTrue(content.getString("value").equals("pong"));
        
        message.close();
    }

    /**
     * message rejected by server
     */
    @Test
    public void testSendReject() throws YAMIIOException {

        final BooleanHolder gotMessage = new BooleanHolder(false);
        
        serverAgent.registerObject(objectName,
                new IncomingMessageCallback() {

            @Override
            public void call(IncomingMessage message) throws Exception {
                gotMessage.value = true;
                
                // expect empty parameters if null is sent
                Parameters content = message.getParameters();
                assertTrue(content.size() == 0);

                message.reject("some reason");
            }
        });
        
        OutgoingMessage message = clientAgent.send(serverAddress,
                objectName, messageName, null);
        
        message.waitForCompletion();
        
        assertTrue(gotMessage.value);

        assertTrue(message.getState() ==
                OutgoingMessage.MessageState.REJECTED);
        assertTrue(message.getExceptionMsg().equals("some reason"));
        
        message.close();
    }

    /**
     * message rejected due to exception in user code at the server side
     */
    @Test
    public void testSendRejectDueToException() throws YAMIIOException {

        final BooleanHolder gotMessage = new BooleanHolder(false);
        
        serverAgent.registerObject(objectName,
                new IncomingMessageCallback() {

            @Override
            public void call(IncomingMessage message) throws Exception {
                gotMessage.value = true;

                throw new Exception("something bad happened");
            }
        });
        
        OutgoingMessage message = clientAgent.send(serverAddress,
                objectName, messageName, null);
        
        message.waitForCompletion();
        
        assertTrue(gotMessage.value);

        assertTrue(message.getState() ==
                OutgoingMessage.MessageState.REJECTED);
        assertTrue(message.getExceptionMsg().contains(
                "something bad happened"));
        
        message.close();
    }
    
    /**
     * big messages sent with different priorities
     */
    @Test
    public void testBigMessages() throws YAMIIOException {
        
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
        final boolean[] gotMessages = new boolean[numOfMessages];
        
        int sizeOfBigString = 1000000;
        char[] bigChars = new char[sizeOfBigString];
        for (int i = 0; i != sizeOfBigString; ++i) {
            bigChars[i] = 'x';
        }
        final String bigString = String.copyValueOf(bigChars);
        
        serverAgent.registerObject(objectName,
                new IncomingMessageCallback() {

            @Override
            public void call(IncomingMessage message) throws Exception {
                
                Parameters content = message.getParameters();

                int id = content.getInteger("id");
                gotMessages[id] = true;

                // uncomment it to see how the messages get reordered
                //System.out.println("received message " + id);

                // verify the big value
                assertTrue(content.getString("big").equals(bigString));
                
                message.reply(null);
            }
        });
        
        Parameters content = new Parameters();
        content.setString("big", bigString);
        
        OutgoingMessage[] messages = new OutgoingMessage[numOfMessages];
        
        // send all messages with different ids and priorities
        for (int i = 0; i != numOfMessages; ++i) {
            int id = i;
            int priority = i; // increasing priority
            
            content.setInteger("id", id);
            messages[i] = clientAgent.send(serverAddress,
                objectName, messageName, content, priority);
        }
        
        // wait for all messages to complete
        for (int i = 0; i != numOfMessages; ++i) {
            messages[i].waitForCompletion();
            messages[i].close();
        }
                
        for (int i = 0; i != numOfMessages; ++i) {
            assertTrue(gotMessages[i]);
        }
    }

    /**
     * message sent to load-balanced pair of destinations
     */
    @Test
    public void testLoadBalancing() throws YAMIIOException {

        Agent serverAgent1 = new Agent();
        String serverAddress1 = serverAgent1.addListener("tcp://*:*");

        Agent serverAgent2 = new Agent();
        String serverAddress2 = serverAgent2.addListener("tcp://*:*");
        
        String loadBalancedTarget = "failover:(" + serverAddress1 + "|" +
                serverAddress2 + ")";

        final BooleanHolder gotMessage1 = new BooleanHolder(false);
        final BooleanHolder gotMessage2 = new BooleanHolder(false);
        
        serverAgent1.registerObject(objectName,
                new IncomingMessageCallback() {

            @Override
            public void call(IncomingMessage message) throws Exception {
                gotMessage1.value = true;
                message.reply(null);
            }
        });
        
        serverAgent2.registerObject(objectName,
                new IncomingMessageCallback() {

            @Override
            public void call(IncomingMessage message) throws Exception {
                gotMessage2.value = true;
                message.reply(null);
            }
        });
        
        OutgoingMessage message = clientAgent.send(loadBalancedTarget,
                objectName, messageName, null);

        // since this is a load-balanced (and failover) target,
        // the message is implicitly waited for completion

        assertTrue(message.getState() ==
                OutgoingMessage.MessageState.REPLIED);

        // exactly one of two servers got the message
        assertTrue((gotMessage1.value && gotMessage2.value == false) ||
                (gotMessage1.value == false && gotMessage2.value));
        
        message.close();
        
        serverAgent1.close();
        serverAgent2.close();
    }

    /**
     * message sent to failover pair of destinations
     */
    @Test
    public void testFailOver() throws YAMIIOException {

        // the failover pair consists of one proper address and one
        // that is certainly not working
        
        String brokenTarget = "tcp://nosuchhost:4";
        String failoverTarget = "failover:(" + serverAddress + "|" +
                brokenTarget + ")";

        final BooleanHolder gotMessage = new BooleanHolder(false);
                
        serverAgent.registerObject(objectName,
                new IncomingMessageCallback() {

            @Override
            public void call(IncomingMessage message) throws Exception {
                gotMessage.value = true;
                message.reply(null);
            }
        });
        
        OutgoingMessage message = clientAgent.send(failoverTarget,
                objectName, messageName, null);

        // since this is a failover target,
        // the message is implicitly waited for completion

        assertTrue(message.getState() ==
                OutgoingMessage.MessageState.REPLIED);

        // the working server in the failover pair got the message
        assertTrue(gotMessage.value);
        
        message.close();
    }
    
    /**
     * empty failover group is an error
     */
    @Test
    public void testEmptyFailover() {
        
        try {
            clientAgent.sendOneWay("failover:()",
                    objectName, messageName, null);
            fail("should not reach this point");
        } catch (Exception ex) {
            assertTrue(ex.getMessage().contains(
                    "Empty failover group is not allowed."));
        }
    }

    /**
     * raw binary message and reply
     */
    @Test
    public void testRawBinary() throws YAMIIOException {

        final BooleanHolder gotMessage = new BooleanHolder(false);
        
        final byte[] messageContent =
            new String("Hello this is raw binary message").getBytes();
        final byte[] replyContent =
            new String("Hi and this is raw binary response !").getBytes();

        Parameters options = new Parameters();
        options.setBoolean(OptionNames.DELIVER_AS_RAW_BINARY, true);
        
        Agent rawServerAgent = new Agent(options);
        String rawServerAddress = rawServerAgent.addListener("tcp://*:*");
        
        Agent rawClientAgent = new Agent(options);
        
        rawServerAgent.registerObject(objectName,
                new IncomingMessageCallback() {

            @Override
            public void call(IncomingMessage message) throws Exception {
                gotMessage.value = true;
                
                byte[] content = message.getRawContent();
                assertTrue(Arrays.equals(content, messageContent));
                
                message.reply(new RawBinaryDataSource(replyContent));
            }
        });

        OutgoingMessage message = rawClientAgent.send(rawServerAddress,
                objectName, messageName,
                new RawBinaryDataSource(messageContent));

        message.waitForCompletion();
        assertTrue(gotMessage.value);

        assertTrue(message.getState() ==
                OutgoingMessage.MessageState.REPLIED);

        byte[] reply = message.getRawReply();
        assertTrue(Arrays.equals(reply, replyContent));
        
        message.close();
        
        rawClientAgent.close();
        rawServerAgent.close();
    }
    
    /**
     * connection event notifications
     */
    @Test
    public void testConnectionEvents() throws YAMIIOException {
        
        class EventCallback implements ConnectionEventCallback {

            private final StringBuilder events;
            
            public EventCallback() {
                events = new StringBuilder();
            }
            
            String getEvents() {
                return events.toString();
            }
            
            @Override
            public void report(String name, ConnectionEvent event)
                throws Exception {
                switch (event) {
                case NEW_INCOMING_CONNECTION:
                    events.append("incoming ");
                    break;
                case NEW_OUTGOING_CONNECTION:
                    events.append("outgoing ");
                    break;
                case CONNECTION_CLOSED:
                    events.append("closed ");
                    break;
                }
            }
        }
        
        EventCallback serverCallback = new EventCallback();
        Agent server = new Agent(null, serverCallback, null, null);
        
        String serverAddr = server.addListener("tcp://*:*");

        EventCallback clientCallback = new EventCallback();
        Agent client = new Agent(null, clientCallback, null, null);
        
        // no communication yet -> no connection
        assertEquals(serverCallback.getEvents(), "");
        assertEquals(clientCallback.getEvents(), "");
        
        OutgoingMessage msg = client.send(serverAddr,
                "no_such_object", "hello", null);
        msg.waitForCompletion();
        
        // one connection open
        assertEquals(serverCallback.getEvents(), "incoming ");
        assertEquals(clientCallback.getEvents(), "outgoing ");
        
        client.closeConnection(serverAddr);
        
        // one connection open and one closed,
        // but it is a race for both the client and the server
        assertTrue(serverCallback.getEvents().equals("incoming ") ||
                serverCallback.getEvents().equals("incoming closed "));
        assertTrue(clientCallback.getEvents().equals("outgoing ") ||
                clientCallback.getEvents().equals("outgoing closed "));
        
        client.close();
        server.close();
    }

    /**
     * frame size border conditions - messages with all possible lengths
     */
    @Test
    public void testFrameSizes() throws YAMIIOException {
        
        final int maxStringSize = 10000;

        for (int stringSize = 1; stringSize != maxStringSize; ++stringSize) {
            StringBuilder sb = new StringBuilder();
            sb.ensureCapacity(stringSize);
            for (int i = 0; i != stringSize; ++i) {
                sb.append('x');
            }
            String s = sb.toString();
            assert s.length() == stringSize;
            
            Parameters params = new Parameters();
            params.setString("value", s);
        
            OutgoingMessage message = clientAgent.send(serverAddress,
                    "nosuchobject", "hello", params);
            message.waitForCompletion();
            assertTrue(message.getState() ==
                OutgoingMessage.MessageState.REJECTED);
        
            message.close();
        }
    }
}
