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

/**
 * Incoming message.
 *
 * <p>
 * The handler allowing to inspect the details of the incoming message
 * and sent back replies or rejection notifications.
 * </p>
 * <p>
 * The user code interacts with objects of this type mainly in
 * callbacks that are provided during object registration and that are later
 * called back when the incoming message arrives. The handler objects
 * can be stored aside for further processing even after the callback
 * returns, but should no be used after the agent itself is closed.
 * </p>
 * <p>
 * <b>Note:</b>
 * The objects of this class are not supposed to be used
 * from multiple threads.
 * <p>
 */
public class IncomingMessage {

    private final Agent agent;
    private final long messageId;
    private final String sourceName;
    private final String objectName;
    private final String messageName;
    private final Parameters body;
    private final byte[] rawBody;
    private boolean alreadyUsed;

    IncomingMessage(Agent agent, long messageId,
            String sourceName, String objectName,
            String messageName, Parameters body, byte[] rawBody) {
        this.agent = agent;
        this.messageId = messageId;
        this.sourceName = sourceName;
        this.objectName = objectName;
        this.messageName = messageName;
        this.body = body;
        this.rawBody = rawBody;
        alreadyUsed = false;
    }

    /**
     * Get the target name of the message sender (the message source).
     * 
     * @return source name
     */
    public String getSource() {
        return sourceName;
    }
    
    /**
     * Get the destination object name.
     * 
     * @return destination object name
     */
    public String getObjectName() {
        return objectName;
    }
    
    /**
     * Get the message name.
     * 
     * @return message name
     */
    public String getMessageName() {
        return messageName;
    }
    
    /**
     * Get the message parameters (message body).
     * 
     * @return message parameters
     */
    public Parameters getParameters() {
        if (body == null) {
            throw new BadStateException("incoming message");
        }
        
        return body;
    }
    
    /**
     * Get the message raw (binary) content.
     * 
     * @return message raw content
     * @throws BadStateException if the agent was not configured
     *         for raw content delivery
     */
    public byte[] getRawContent() {
        if (rawBody == null) {
            throw new BadStateException("incoming message");
        }
        
        return rawBody;
    }
    
    /**
     * Send back the reply with default (lowest) priority.
     * 
     * @param replyBody the content of the reply
     *        (it can be null, in which case the empty object is sent)
     * @throws YAMIIOException if the reply channel cannot be established
     */
    public void reply(YAMISerializable replyBody) throws YAMIIOException {
        reply(replyBody, 0);
    }

    /**
     * Send back the reply with given priority.
     * 
     * @param replyBody the content of the reply
     *        (it can be null, in which case the empty object is sent)
     * @param priority the priority of reply
     * @throws YAMIIOException if the reply channel cannot be established
     */
    public void reply(YAMISerializable replyBody, int priority)
        throws YAMIIOException {
        
        if (alreadyUsed) {
            throw new BadStateException("incoming message");
        }
        agent.doSendReply(sourceName, messageId, replyBody, priority);
        alreadyUsed = true;
    }

    /**
     * Send back the rejection (exception) notification
     * with default (lowest) priority.
     * 
     * @param reason arbitrary text that will be visible by the
     *        original sender as a reason for rejection
     * @throws YAMIIOException if the reply channel cannot be established
     */
    public void reject(String reason) throws YAMIIOException {
        reject(reason, 0);
    }

    /**
     * Send back the rejection (exception) notification
     * with given priority.
     * 
     * @param reason arbitrary text that will be visible by the
     *        original sender as a reason for rejection
     * @param priority the priority of rejection
     * @throws YAMIIOException if the reply channel cannot be established
     */
    public void reject(String reason, int priority) throws YAMIIOException {
        if (alreadyUsed) {
            throw new BadStateException("incoming message");
        }
        agent.doSendReject(sourceName, messageId, reason, priority);
        alreadyUsed = true;
    }
}
