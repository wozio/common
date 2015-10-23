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

import com.inspirel.yami.BadProtocolException;
import com.inspirel.yami.LogCallback;
import com.inspirel.yami.Parameters;

import org.junit.Test;
import static org.junit.Assert.*;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.List;
import java.util.ArrayList;

public class ChannelTest {

    public ChannelTest() {
    }

    @Test
    public void testForBadProtocol() {
        try {
            new Channel("abc", new Options(null), null, null,
                    null, LogCallback.LogLevel.LOW);
            fail("should never reach this point");
        } catch (com.inspirel.yami.BadProtocolException ex) {
            assertTrue(ex.getMessage().equals(
                    "The protocol 'abc' is not supported."));
        } catch (IOException ex) {
            fail("should never reach this point");
        }
    }

    @Test
    public void testForNodeInsertion() {
        Channel ch = null;
        try {
            ch = new Channel("null", new Options(null), null, null,
                    null, LogCallback.LogLevel.LOW);
        } catch (IOException ex) {
            fail("should never reach this point");
        }

        assertTrue(ch.getOutgoingFrames().isEmpty());

        // first node

        byte[] buf = new byte[123];
        ArrayList<byte[]> buffers = new ArrayList<byte[]>();
        buffers.add(buf);

        boolean firstFrame = ch.post(0, 0, buffers, 0, null);
        assertTrue(firstFrame);

        List<OutgoingFrame> frames = ch.getOutgoingFrames();
        assertTrue(frames.size() == 1);
        assertTrue(frames.get(0).payload.length == 123);

        // second node, added after 1st

        buf = new byte[456];
        buffers.clear();
        buffers.add(buf);

        firstFrame = ch.post(1, 0, buffers, 0, null);
        assertFalse(firstFrame);

        frames = ch.getOutgoingFrames();
        assertTrue(frames.size() == 2);
        assertTrue(frames.get(0).payload.length == 123);
        assertTrue(frames.get(1).payload.length == 456);
        
        // third node, at the end
        
        buf = new byte[789];
        buffers.clear();
        buffers.add(buf);

        firstFrame = ch.post(2, 0, buffers, 0, null);
        assertFalse(firstFrame);

        frames = ch.getOutgoingFrames();
        assertTrue(frames.size() == 3);
        assertTrue(frames.get(0).payload.length == 123);
        assertTrue(frames.get(1).payload.length == 456);
        assertTrue(frames.get(2).payload.length == 789);
        
        // three buffers inserted after first node
        
        buffers.clear();
        buffers.add(new byte[1]);
        buffers.add(new byte[2]);
        buffers.add(new byte[3]);
        
        firstFrame = ch.post(3, 3, buffers, 0, null);
        assertFalse(firstFrame);

        frames = ch.getOutgoingFrames();
        assertTrue(frames.size() == 6);
        assertTrue(frames.get(0).payload.length == 123);
        assertTrue(frames.get(1).payload.length == 1);
        assertTrue(frames.get(2).payload.length == 2);
        assertTrue(frames.get(3).payload.length == 3);
        assertTrue(frames.get(4).payload.length == 456);
        assertTrue(frames.get(5).payload.length == 789);
        
        // two new nodes after the previous three
        
        buffers.clear();
        buffers.add(new byte[4]);
        buffers.add(new byte[5]);
        
        firstFrame = ch.post(4, 1, buffers, 0, null);
        assertFalse(firstFrame);

        frames = ch.getOutgoingFrames();
        assertTrue(frames.size() == 8);
        assertTrue(frames.get(0).payload.length == 123);
        assertTrue(frames.get(1).payload.length == 1);
        assertTrue(frames.get(2).payload.length == 2);
        assertTrue(frames.get(3).payload.length == 3);
        assertTrue(frames.get(4).payload.length == 4);
        assertTrue(frames.get(5).payload.length == 5);
        assertTrue(frames.get(6).payload.length == 456);
        assertTrue(frames.get(7).payload.length == 789);
                
        // poison pill added between 3 and 4
        
        boolean closeMe = ch.postClose(2);
        assertFalse(closeMe);

        frames = ch.getOutgoingFrames();
        assertTrue(frames.size() == 5);
        assertTrue(frames.get(0).payload.length == 123);
        assertTrue(frames.get(1).payload.length == 1);
        assertTrue(frames.get(2).payload.length == 2);
        assertTrue(frames.get(3).payload.length == 3);
        assertTrue(frames.get(4).closeFlag);
    }

    @Test
    public void testForImmediateClose() {
        Channel ch = null;
        try {
            ch = new Channel("null", new Options(null), null, null,
                    null, LogCallback.LogLevel.LOW);
        } catch (IOException ex) {
            fail("should never reach this point");
        }

        assertTrue(ch.getOutgoingFrames().isEmpty());

        boolean closeMe = ch.postClose(0);
        assertTrue(closeMe);
        assertTrue(ch.getOutgoingFrames().isEmpty());
    }
    
    @Test
    public void testForInvalidConnection() {
        try {
            new Channel("tcp://invalid", new Options(null), null, null,
                    null, LogCallback.LogLevel.LOW);
            fail("should never reach this point");
        } catch (IOException ex) {
            fail("should never reach this point");
        } catch (BadProtocolException ex) {
            // this is the right exception,
            // because the port number was not provided
        }

        try {
            new Channel("tcp://invalidhostname:12345",
                    new Options(null), null, null,
                    null, LogCallback.LogLevel.LOW);
            fail("should never reach this point");
        } catch (IOException ex) {
            // this is the right exception,
            // because the target host does not exist
        } catch (BadProtocolException ex) {
            fail("should never reach this point");
        }
    }
    
    @Test
    public void testForEstablishedConnection() {
        try {
            // artificial listening socket for this test
            ServerSocket server = new ServerSocket();
            server.bind(null); // system-assigned port
        
            String target = "tcp://127.0.0.1:" + server.getLocalPort();
                    
            try {
                Channel ch = new Channel(target, new Options(null), null, null,
                    null, LogCallback.LogLevel.LOW);
            
                Socket accepted = server.accept();
            
                ch.close();
                accepted.close();
                server.close();
            } catch (Exception ex) {
                fail("should never reach this point");
            }
        } catch (Exception ex) {
            fail("should never reach this point");
        }
    }
    
    @Test
    public void testForSimpleOutput() {
        try {
            // artificial listening socket for this test
            ServerSocket server = new ServerSocket();
            server.bind(null); // system-assigned port
        
            String target = "tcp://127.0.0.1:" + server.getLocalPort();
                    
            try {
                Channel ch = new Channel(target, new Options(null), null, null,
                    null, LogCallback.LogLevel.LOW);
            
                Socket accepted = server.accept();
                
                List<byte[]> buffers = new ArrayList<byte[]>();
                buffers.add(new byte[] {10, 11, 12, 13});
                
                ProgressCallback callback = new ProgressCallback();
                
                int messageHeaderSize = 4;
                boolean firstFrame =
                    ch.post(123, 0, buffers, messageHeaderSize, callback);
                assertTrue(firstFrame);
                
                boolean queueBecameEmpty = ch.doSomeWork(false, true);
                
                // the buffers are short and the whole can be expected
                // to be pushed in a single operation
                
                assertTrue(queueBecameEmpty);
                assertTrue(callback.progressCalled);
                assertTrue(callback.sentBytes == callback.totalByteCount);
                assertFalse(callback.cancelledCalled);

                // read the data on the receiving end
                
                byte[] buf = new byte[1024];
                int readBytes = accepted.getInputStream().read(buf);
                
                assertTrue(readBytes == 20);
                
                // message id
                assertTrue(buf[0] == 123);
                assertTrue(buf[1] == 0);
                assertTrue(buf[2] == 0);
                assertTrue(buf[3] == 0);
                
                // frame number (-1 -> it is the only one)
                assertTrue(buf[4] == -1);
                assertTrue(buf[5] == -1);
                assertTrue(buf[6] == -1);
                assertTrue(buf[7] == -1);
                
                // size of message header
                assertTrue(buf[8] == 4);
                assertTrue(buf[9] == 0);
                assertTrue(buf[10] == 0);
                assertTrue(buf[11] == 0);
                
                // frame payload
                assertTrue(buf[12] == 4);
                assertTrue(buf[13] == 0);
                assertTrue(buf[14] == 0);
                assertTrue(buf[15] == 0);
                
                // payload
                assertTrue(buf[16] == 10);
                assertTrue(buf[17] == 11);
                assertTrue(buf[18] == 12);
                assertTrue(buf[19] == 13);

                ch.close();
                accepted.close();
                server.close();
            } catch (Exception ex) {
                fail("should never reach this point");
            }
        } catch (Exception ex) {
            fail("should never reach this point");
        }
    }

    private static class ProgressCallback
            implements MessageProgressCallback {

        int sentBytes;
        int totalByteCount;
        boolean progressCalled = false;
        boolean cancelledCalled = false;
        
        @Override
        public void progress(int progrSentBytes, int progrTotalByteCount) {
            this.sentBytes = progrSentBytes;
            this.totalByteCount = progrTotalByteCount;
            progressCalled = true;
            if (sentBytes == totalByteCount && sentBytes == 0) {
                cancelledCalled = true;
            }
        }

        @Override
        public void replied(Parameters body, byte[] rawBody) {
            // not needed in this test
        }

        @Override
        public void rejected(String reason) {
            // not needed in this test
        }
    }
}
