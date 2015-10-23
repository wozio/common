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

import com.inspirel.yami.details.MessageProgressCallback;
import com.inspirel.yami.details.OutgoingMessageManager;
import com.inspirel.yami.details.WaterFlowManager;

/**
 * Outgoing message.
 * 
 * <p>
 * The handler (proxy) allowing to track the progress of outgoing message,
 * inspect its state and to obtain the reply content.
 * </p>
 * <p>
 * <b>Note:</b> The objects of this class can be safely used
 * from multiple threads.
 * </p>
 */
public final class OutgoingMessage {

    /**
     * Enumeration defining all possible message states.
     */
    public enum MessageState {
        
        /**
         * Message was posted for transmission.
         */
        POSTED,
        /**
         * Message was fully transmitted.
         */
        TRANSMITTED,
        /**
         * Message was abandoned due to error or channel closing.
         */
        ABANDONED,
        /**
         * The reply was received for the given message.
         */
        REPLIED,
        /**
         * Message was rejected.
         */
        REJECTED
    }
    
    /**
     * Helper class for reporting full message state
     */
    public static final class MessageStateInfo {
        /**
         * Current state of the message.
         */
        public final MessageState state;
        /**
         * Number of bytes that were transmitted.
         */
        public final int sentBytes;
        /**
         * Total length of the message.
         */
        public final int totalByteCount;
        /**
         * Reply body (valid only when state is REPLIED)
         */
        public final Parameters replyBody;
        /**
         * Reply raw binary body (valid only when state is REPLIED
         * and the agent was configured for raw delivery).
         */
        public final byte[] rawReplyBody;
        /**
         * Rejection reson (valid only when state is REJECTED)
         */
        public final String rejectionReason;

        MessageStateInfo(MessageState state,
                int sentBytes, int totalByteCount,
                Parameters replyBody, byte[] rawReplyBody,
                String rejectionReason) {
            this.state = state;
            this.sentBytes = sentBytes;
            this.totalByteCount = totalByteCount;
            this.replyBody = replyBody;
            this.rawReplyBody = rawReplyBody;
            this.rejectionReason = rejectionReason;
        }
    }
    
    private class ProgressCallback implements MessageProgressCallback {

        @Override
        public void progress(int progrSentBytes, int progrTotalByteCount) {
            
            OutgoingMessage message = OutgoingMessage.this;
            
            boolean lastNotification = false;
            synchronized (message) {
                message.sentBytes = progrSentBytes;
                message.totalByteCount = progrTotalByteCount;
                
                if (progrSentBytes == progrTotalByteCount) {
                    // there will be no more progress notifications
                    // for this message
                    
                    lastNotification = true;
                    
                    if (progrSentBytes != 0) {
                        // the transmission of the whole message
                        // was successful
                        
                        if (message.state == MessageState.POSTED) {
                            message.state = MessageState.TRANSMITTED;
                            
                            message.transmitted = true;
                            message.notifyAll();
                        }
                        
                    } else {
                        // the message was abandoned
                        // before it was fully transmitted
                        
                        message.state = MessageState.ABANDONED;

                        message.transmitted = true;
                        message.completed = true;
                        message.notifyAll();
                    }
                }
            }
            
            if (lastNotification) {
                // the message is treated as leaving the output queue
                
                message.outgoingFlowManager.decrease();

                message.updateUserCallback();
            }
        }

        @Override
        public void replied(Parameters body, byte[] rawBody) {

            OutgoingMessage message = OutgoingMessage.this;

            synchronized (message) {
                if (state == MessageState.POSTED ||
                        state == MessageState.TRANSMITTED) {
            
                    MessageState previousState = state;
            
                    state = MessageState.REPLIED;
                    reply = body;
                    rawReply = rawBody;
            
                    if (previousState == MessageState.POSTED) {
                        transmitted = true;
                    }

                    completed = true;
                    message.notifyAll();
                }
            }

            message.updateUserCallback();
        }

        @Override
        public void rejected(String reason) {

            OutgoingMessage message = OutgoingMessage.this;

            synchronized (message) {
                if (state == MessageState.POSTED ||
                        state == MessageState.TRANSMITTED) {
            
                    MessageState previousState = state;
            
                    state = MessageState.REJECTED;
                    rejectionReason = reason;
            
                    if (previousState == MessageState.POSTED) {
                        transmitted = true;
                    }

                    completed = true;
                    message.notifyAll();
                }
            }

            message.updateUserCallback();
        }
    }
    
    private final long messageId;
    private final OutgoingMessageCallback updateCallback;
    private final OutgoingMessageManager messageManager;
    private final WaterFlowManager outgoingFlowManager;
    private final ProgressCallback progressCallback;
    private MessageState state = MessageState.POSTED;
    private int sentBytes = 0;
    private int totalByteCount = 0;
    private Parameters reply = null;
    private byte[] rawReply = null;
    private String rejectionReason = null;
    
    private boolean transmitted = false;
    private boolean completed = false;

    OutgoingMessage(long messageId,
    		OutgoingMessageCallback updateCallback,
            OutgoingMessageManager messageManager,
            WaterFlowManager outgoingFlowManager) {
        this.messageId = messageId;
        this.updateCallback = updateCallback;
        this.messageManager = messageManager;
        this.outgoingFlowManager = outgoingFlowManager;
        this.progressCallback = new ProgressCallback();
    }

    /**
     * Gets the current state of the message.
     * @return current state
     */
    public MessageState getState() {
        synchronized (this) {
            return state;
        }
    }
    
    /**
     * Gets the current state and information about the message progress.
     * 
     * @return current state info
     */
    public MessageStateInfo getStateInfo() {
        synchronized (this) {
            return new MessageStateInfo(state, sentBytes, totalByteCount,
                    reply, rawReply, rejectionReason);
        }
    }
    
    /**
     * Gets the reply body.
     * 
     * @return reply body if the current state is REPLIED
     * @throws BadStateException if the current state is not REPLIED
     */
    public Parameters getReply() {
        synchronized (this) {
            if (state == MessageState.REPLIED && reply != null) {
                return reply;
            }

            throw new BadStateException(
                    "Cannot return the reply body for this message" +
                    " (no reply arrived yet).");
        }
    }
    
    /**
     * Gets the raw reply body.
     * 
     * @return reply body if the current state is REPLIED
     * @throws BadStateException if the current state is not REPLIED or
     *         if the agent was not configured for raw delivery
     */
    public byte[] getRawReply() {
        synchronized (this) {
            if (state == MessageState.REPLIED && rawReply != null) {
                return rawReply;
            }

            throw new BadStateException(
                    "Cannot return the reply body for this message" +
                    " (no reply arrived yet).");
        }
    }
    
    /**
     * Gets the rejection reason for this message
     * 
     * @return rejection reason (or exception message)
     *         if the current state is REJECTED
     * @throws BadStateException if the current state is not REJECTED
     */
    public String getExceptionMsg() {
        synchronized (this) {
            if (state == MessageState.REJECTED) {
                return rejectionReason;
            }

            throw new BadStateException(
                    "Cannot return the rejection reason" +
                    " (this message has not been rejected).");
        }
    }

    /**
     * Waits for the transmission of this message.
     * 
     * <p>
     * <b>Note:</b> The wait is terminated either when the transmission
     * is successful or when the message is cancelled due to error
     * or channel closing request.
     * </p>
     */
    public void waitForTransmission() {
        synchronized (this) {
            while (transmitted == false) {
                try {
                    wait();
                } catch (InterruptedException ex) {
                    // ignore, will never happen
                }
            }
        }
    }
        
    /**
     * Waits for the transmission of this message.
     * 
     * <p>
     * <b>Note:</b> The wait is terminated either when the transmission
     * is successful or when the message is cancelled due to error
     * or channel closing request or when the timeout expires.
     * </p>
     * 
     * @param relativeTimeout relative timeout in milliseconds
     *
     * @return false if the timeout has expired
     */
    public boolean waitForTransmission(long relativeTimeout) {
        long now = System.currentTimeMillis();
        long deadline = now + relativeTimeout;
        return waitForTransmissionAbsolute(deadline);
    }
        
    /**
     * Waits for the transmission of this message.
     * 
     * <p>
     * <b>Note:</b> The wait is terminated either when the transmission
     * is successful or when the message is cancelled due to error
     * or channel closing request or when the timeout expires.
     * </p>
     * 
     * @param absoluteTimeout absolute timeout in milliseconds,
     *        counted from the beginning of the epoch (UTC)
     *
     * @return false if the timeout has expired
     */
    public boolean waitForTransmissionAbsolute(long absoluteTimeout) {
        synchronized (this) {
            while (transmitted == false) {
                try {
                    long now = System.currentTimeMillis();
                    long delay = absoluteTimeout - now;
                    if (delay <= 0L) {
                        return false;
                    }
                    
                    wait(delay);
                } catch (InterruptedException ex) {
                    // ignore, will never happen
                }
            }
        }
        
        return true;
    }
        
    /**
     * Waits for the completion of this message.
     * 
     * <p>
     * <b>Note:</b> The wait is terminated either when the message
     * is completed - which means that there was a reply or rejection
     * received for it - or when the message is cancelled due to error
     * or channel closing request.
     * </p>
     */
    public void waitForCompletion() {
        synchronized (this) {
            while (completed == false) {
                try {
                    wait();
                } catch (InterruptedException ex) {
                    // ignore, will never happen
                }
            }
        }
    }
        
    /**
     * Waits for the completion of this message.
     * 
     * <p>
     * <b>Note:</b> The wait is terminated either when the message
     * is completed - which means that there was a reply or rejection
     * received for it - or when the message is cancelled due to error
     * or channel closing request or when the timeout expires.
     * </p>
     * 
     * @param relativeTimeout relative timeout in milliseconds
     *
     * @return false if the timeout has expired
     */
    public boolean waitForCompletion(long relativeTimeout) {
        long now = System.currentTimeMillis();
        long deadline = now + relativeTimeout;
        return waitForCompletionAbsolute(deadline);
    }
        
    /**
     * Waits for the completion of this message.
     * 
     * <p>
     * <b>Note:</b> The wait is terminated either when the message
     * is completed - which means that there was a reply or rejection
     * received for it - or when the message is cancelled due to error
     * or channel closing request or when the timeout expires.
     * </p>
     * 
     * @param absoluteTimeout absolute timeout in milliseconds,
     *        counted from the beginning of the epoch (UTC)
     *
     * @return false if the timeout has expired
     */
    public boolean waitForCompletionAbsolute(long absoluteTimeout) {
        synchronized (this) {
            while (completed == false) {
                try {
                    long now = System.currentTimeMillis();
                    long delay = absoluteTimeout - now;
                    if (delay <= 0L) {
                        return false;
                    }
                    wait(delay);
                } catch (InterruptedException ex) {
                    // ignore, will never happen
                }
            }
        }
        
        return true;
    }
    
    /**
     * Cleans up internal resources related to this message.
     */
    public void close() {
        messageManager.unregisterMessage(messageId);
    }
        
    MessageProgressCallback getProgressCallback() {
        return progressCallback;
    }
    
    void updateUserCallback() {
        if (updateCallback != null) {
        	try {
        		updateCallback.update(this);
        	} catch (Exception e) {
        		// ignore user exceptions
        	}
        }
    }
}
