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

import com.inspirel.yami.ConnectionEventCallback;
import com.inspirel.yami.details.Channel;
import com.inspirel.yami.details.DispatchManager;
import com.inspirel.yami.details.Frame;
import com.inspirel.yami.details.IOWorker;
import com.inspirel.yami.details.IncomingMessageDispatchCallback;
import com.inspirel.yami.details.Listener;
import com.inspirel.yami.details.MessageProgressCallback;
import com.inspirel.yami.details.NetworkUtils;
import com.inspirel.yami.details.Options;
import com.inspirel.yami.details.OutgoingMessageManager;
import com.inspirel.yami.details.WaterFlowManager;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Message broker.
 *
 * <p>
 * The message broker that encapsulates physical channel management,
 * incoming and outgoing message queues, listeners and resource
 * management.
 * </p>
 * <p>
 * A single agent object can manage many listeners, which are responsible
 * for accepting remote connections, and many incoming and outgoing
 * connections.
 * </p>
 * <p>
 * The objects of this class can be safely used by multiple threads,
 * including the threads that execute incoming message callbacks.
 * </p>
 */
public final class Agent {
    
    private static final Parameters emptyParameters = new Parameters();
    
    private static final String failoverPrefix = "failover:(";
    private static final String failoverSuffix = ")";
    private static final String failoverSeparator = "\\|";

    private final Options options;
    
    private final LogCallback logCallback;
    private final LogCallback.LogLevel logLevel;

    private final Map<String, Channel> channels;

    private final Map<String, Listener> listeners;

    private final IncomingMessageDispatchCallback
            incomingMessageDispatchCallback;
    
    private final MessageProgressCallback oneWayMessageProgressCallback;
    
    private final OutgoingMessageManager outgoingMessageManager;
    
    private final DispatchManager dispatchManager;
    
    private final WaterFlowManager outgoingFlowManager;
    private final WaterFlowManager incomingFlowManager;
    
    private final IOWorker ioWorker;
    private final Thread ioWorkerThread;
    
    private final AtomicLong messageIdGenerator;
    
    private final ConnectionEventCallback connectionEventCallback;

    // this inner class is responsible for properly handling the outgoing
    // flow with one-way messages
    // when the message is completely transmitted (or cancelled),
    // the flow manager needs to be notified that the message
    // is no longer in the outgoing queue of any channel
    private class OneWayMessageProgressCallback
            implements MessageProgressCallback {

        @Override
        public void progress(int sentBytes, int totalByteCount) {
            if (sentBytes == totalByteCount) {
                // some one-way message has been either
                // transmitted or abandoned
                
                outgoingFlowManager.decrease();
            }
        }

        @Override
        public void replied(Parameters body, byte[] rawBody) {
            // ignore, this is meaningless for one-way messages
        }

        @Override
        public void rejected(String reason) {
            // ignore, this is meaningless for one-way messages
        }
    }
    
    // this inner class is responsible for dispatching incoming messages
    // - including real messages, replies, rejections, etc.
    private class DispatchCallback
            implements IncomingMessageDispatchCallback {

        @Override
        public void dispatch(String sourceName,
                Parameters header, Parameters body, byte[] rawBody) {
            
            String messageType = header.getString("type");

            if (messageType.equals("message")) {

                long messageId = header.getLong("message_id");
                String objectName = header.getString("object_name");
                String messageName = header.getString("message_name");

                IncomingMessage message = new IncomingMessage(
                        Agent.this, messageId, sourceName,
                        objectName, messageName, body, rawBody);

                if (logCallback != null) {
                    if (logLevel == LogCallback.LogLevel.MEDIUM ||
                            logLevel == LogCallback.LogLevel.HIGH) {

                        logCallback.log(LogCallback.LogLevel.MEDIUM,
                                "Message received:" +
                                " from: " + sourceName +
                                " object: " + objectName +
                                " message: " + messageName +
                                " mid: " + messageId);
                    }
                }

                incomingFlowManager.increase();
                dispatchManager.push(message);
                    
            } else if (messageType.equals("reply")) {
                // this is a response to the message that was sent
                // from this agent

                long messageId = header.getLong("message_id");

                if (logCallback != null) {
                    if (logLevel == LogCallback.LogLevel.MEDIUM ||
                            logLevel == LogCallback.LogLevel.HIGH) {

                        logCallback.log(LogCallback.LogLevel.MEDIUM,
                                "Reply received:" +
                                " from: " + sourceName +
                                " mid: " + messageId);
                    }
                }

                outgoingMessageManager.reportReplied(
                        messageId, body, rawBody);

            } else if (messageType.equals("exception")) {
                // this is a rejection/exception for the message
                // that was sent from this agent

                long messageId = header.getLong("message_id");

                String reason = header.getString("reason");

                if (logCallback != null) {
                    if (logLevel == LogCallback.LogLevel.MEDIUM ||
                            logLevel == LogCallback.LogLevel.HIGH) {

                        logCallback.log(LogCallback.LogLevel.MEDIUM,
                                "Reject received:" +
                                " from: " + sourceName +
                                " mid: " + messageId);
                    }
                }

                outgoingMessageManager.reportRejected(messageId, reason);
            }
        }
    }
    
    /**
     * Default constructor, creates an agent object without
     * any active listener and with default option values.
     * 
     * @throws YAMIIOException if the network resources cannot be obtained
     */
    public Agent() throws YAMIIOException {
        this((Parameters) null);
    }
    
    /**
     * Constructor, creates an agent object without any active listener
     * and uses the provided configuration options.
     * 
     * @param options set of configuration options
     * @throws YAMIIOException if the network resources cannot be obtained
     */
    public Agent(Parameters options) throws YAMIIOException {
        this(options, null, null, LogCallback.LogLevel.LOW);
    }
        
    /**
     * Constructor, creates an agent object without any active listener
     * and uses the provided configuration options and log callback.
     * 
     * @param options set of configuration options
     * @param logCallback callback object for log events
     * @param logLevel requested log level
     * @throws YAMIIOException if the network resources cannot be obtained
     */
    public Agent(Parameters options,
            LogCallback logCallback, LogCallback.LogLevel logLevel)
            throws YAMIIOException {
        this(options, null, logCallback, logLevel);
    }

    /**
     * Constructor, creates an agent object without any active listener
     * and uses the provided configuration options and log callback.
     * 
     * @param options set of configuration options
     * @param connectionEventCallback monitor for reporting connection events
     * @param logCallback callback object for log events
     * @param logLevel requested log level
     * @throws YAMIIOException if the network resources cannot be obtained
     */
    public Agent(Parameters options,
            ConnectionEventCallback connectionEventCallback,
            LogCallback logCallback, LogCallback.LogLevel logLevel)
            throws YAMIIOException {
        
        this.options = new Options(options);

        this.logCallback = logCallback;
        this.logLevel = logLevel;
        
        channels = new HashMap<String, Channel>();
        listeners = new HashMap<String, Listener>();
        
        outgoingFlowManager = new WaterFlowManager(
                this.options.outgoingHighWaterMark,
                this.options.outgoingLowWaterMark);
        
        incomingFlowManager = new WaterFlowManager(
                this.options.incomingHighWaterMark,
                this.options.incomingLowWaterMark);

        incomingMessageDispatchCallback = new DispatchCallback();
        
        oneWayMessageProgressCallback = new OneWayMessageProgressCallback();
        
        outgoingMessageManager = new OutgoingMessageManager();
        
        this.connectionEventCallback = connectionEventCallback; 
        
        messageIdGenerator = new AtomicLong();
        
        ioWorker = new IOWorker(
                channels, listeners, incomingFlowManager,
                this.options, incomingMessageDispatchCallback,
                connectionEventCallback,
                logCallback, logLevel);
        ioWorkerThread = new Thread(ioWorker, "YAMI4 I/O worker");
        ioWorkerThread.setDaemon(true);
        ioWorkerThread.start();

        dispatchManager =
                new DispatchManager(this.options, incomingFlowManager,
                ioWorker, logCallback, logLevel);
        
        if (logCallback != null) {
            logCallback.log(LogCallback.LogLevel.LOW,
                    "Agent created.");
        }
    }
    
    /**
     * Adds a new listener for the given target address.
     * 
     * <p>
     * The supported target formats are:
     *  - "tcp://host:port" for TCP/IP connections, where <code>host</code>
     *    can be provided in the symbolic or numeric form
     *  - "tcp://*:port" for TCP/IP connections, for "any" local address
     *  - "tcp://port" for TCP/IP connections, for "any" local address
     *  - "udp://host:port" for UDP communication, with rules as for TCP/IP
     * </p>
     * <p>
     * The port for TCP/IP protocol can be <code>0</code>, in which case
     * the actual port number is assigned by the system.
     * </p>
     * 
     * @param target the target name for the new listener
     * @return the locally resolved target name - this name can be used
     *         by clients to create new connections and by local code
     *         to remove the listener
     * @throws YAMIIOException if the network resources cannot be obtained
     */
    public String addListener(String target) throws YAMIIOException {
        
        try {
            Listener listener = NetworkUtils.prepareServer(target,
                    incomingMessageDispatchCallback, options,
                    logCallback, logLevel);
            String resolvedTarget = listener.getResolvedTarget();
            
            synchronized (listeners) {
                listeners.put(resolvedTarget, listener);
            }
            
            ioWorker.wakeup();
            
            if (logCallback != null) {
                logCallback.log(LogCallback.LogLevel.LOW,
                    "Added listener for " + resolvedTarget);
            }
            
            return resolvedTarget;
            
        } catch (IOException ex) {
            throw new YAMIIOException(ex.getMessage());
        }
    }
    
    /**
     * Removes existing listener.
     *
     * <p>
     * Removes the listener denoted by its actual target name.
     * Note that the actual target name might be different from the name
     * provided when the listener was created, due to target resolution.
     * The name which should be used for listener removal is the name
     * that is returned by the <code>addListener</code> function.
     * </p>
     * 
     * @param target the (resolved) target name of the listener to be removed
     */
    public void removeListener(String target) {
        synchronized (listeners) {
            Listener listener = listeners.get(target);
            if (listener != null) {
                listener.close();
                listeners.remove(target);

                if (logCallback != null) {
                    logCallback.log(LogCallback.LogLevel.LOW,
                        "Removed listener for " + target);
                }
            }
        }
    }

    /**
     * Registers a new logical destination object.
     *
     * <p>
     * Registers the new "object" that can be a logical destination
     * for incoming messages. The incoming messages that are recognized
     * as being addressed to this new object will be delivered through
     * the <code>IncomingMessageCallback</code> interface, which is
     * therefore supposed to be implemented by the user.
     * </p>
     * <p>
     * If an object with the given name is already registered,
     * the registration data is replaced.
     * </p>
     *
     * @param objectName name of the newly registered object
     * @param callback the callback implementation
     */
    public void registerObject(String objectName,
            IncomingMessageCallback callback) {
        dispatchManager.registerObject(objectName, callback);
        
        if (logCallback != null) {
            if (logLevel == LogCallback.LogLevel.MEDIUM ||
                    logLevel == LogCallback.LogLevel.HIGH) {
                logCallback.log(LogCallback.LogLevel.MEDIUM,
                        "Registered object " + objectName);
            }
        }
    }
    
    /**
     * Registers the value publisher as a new logical object.
     *
     * <p>
     * If an object with the given name is already registered,
     * the registration data is replaced.
     * </p>
     *
     * @param objectName name of the newly registered object
     * @param publisher value publisher to be registered
     */
    public void registerValuePublisher(String objectName,
            ValuePublisher publisher) {
        publisher.registerAt(this, objectName);
    }

    /**
     * Unregisters a given destination object.
     * 
     * <p>
     * If an object with the given name is not registered, this
     * operation does nothing.
     * </p>
     * <p>
     * <b>Note:</b>
     * Due to performance and design tradeoffs it is <b>not</b> guaranteed
     * that no more messages will be ever dispatched to the given object
     * when this function returns.
     * In fact, some of the messages that have been received by agent and not
     * yet dispatched might be still dispatched shortly after
     * this function returns.
     * Only those messages that are received by agent after
     * this function returns are guaranteed not to be dispatched to the
     * unregistered object.
     * </p>
     * 
     * @param objectName name of the object to unregister
     */
    public void unregisterObject(String objectName) {
        dispatchManager.unregisterObject(objectName);

        if (logCallback != null) {
            if (logLevel == LogCallback.LogLevel.MEDIUM ||
                    logLevel == LogCallback.LogLevel.HIGH) {
                logCallback.log(LogCallback.LogLevel.MEDIUM,
                        "Unregistered object " + objectName);
            }
        }
    }

    /**
     * Opens the new connection.
     *
     * Opens the new channel or does nothing if the channel already exists.
     *
     * This function is not necessary with automatic connection
     * recovery option in <code>send</code> and <code>sendOneWay</code>.
     *
     * @param target The name of the target endpoint.
     *               This name should correspond to the listener name
     *               in some target agent object.
     * @throws YAMIIOException if the channel cannot be established
     */
    public void openConnection(String target) throws YAMIIOException {
        makeSureChannelExists(target, true);
    }

    /**
     * Sends the new outgoing message to the given destination.
     * 
     * <p>
     * The content object is serialized and is no longer referenced
     * after this function returns. If the content is null, an empty
     * <code>Parameters</code> object is sent.
     * </p>
     * <p><b>Note:</b>
     * This function implicitly opens a new communication channel
     * if it is not already open. This channel is kept open until
     * it is explicitly closed
     * (see the <code>closeConnection</code> function)
     * or until the whole agent is closed or the communication error
     * is detected.
     * </p>
     * 
     * @param target the name of the target endpoint -
     *               this name should correspond to the listener name
     *               in some target agent object
     * @param objectName the name of the logical destination object
     *                   in the target agent
     * @param messageName the name of the message
     * @param content the content of the message
     * @return the handler to the outgoing message object
     * @throws YAMIIOException if the channel cannot be established
     */
    public OutgoingMessage send(String target,
            String objectName, String messageName,
            YAMISerializable content) throws YAMIIOException {
        
        return doSend(null, target, objectName, messageName, content,
                0, true, false);
    }
    
    /**
     * Sends the new outgoing message to the given destination.
     * 
     * <p>
     * The content object is serialized and is no longer referenced
     * after this function returns. If the content is null, an empty
     * <code>Parameters</code> object is sent.
     * </p>
     * <p><b>Note:</b>
     * This function implicitly opens a new communication channel
     * if it is not already open. This channel is kept open until
     * it is explicitly closed
     * (see the <code>closeConnection</code> function)
     * or until the whole agent is closed or the communication error
     * is detected.
     * </p>
     * 
     * @param target the name of the target endpoint -
     *               this name should correspond to the listener name
     *               in some target agent object
     * @param objectName the name of the logical destination object
     *                   in the target agent
     * @param messageName the name of the message
     * @param content the content of the message
     * @param priority the priority of the message
     * @return the handler to the outgoing message object
     * @throws YAMIIOException if the channel cannot be established
     */
    public OutgoingMessage send(String target,
            String objectName, String messageName,
            YAMISerializable content, int priority) throws YAMIIOException {
        
        return doSend(null, target, objectName, messageName,
                content, priority, true, false);
    }
    
    /**
     * Sends the new outgoing message to the given destination.
     * 
     * <p>
     * The content object is serialized and is no longer referenced
     * after this function returns. If the content is null, an empty
     * <code>Parameters</code> object is sent.
     * </p>
     * <p><b>Note:</b>
     * This function implicitly opens a new communication channel
     * if it is not already open. This channel is kept open until
     * it is explicitly closed
     * (see the <code>closeConnection</code> function)
     * or until the whole agent is closed or the communication error
     * is detected.
     * </p>
     * 
     * @param target the name of the target endpoint -
     *               this name should correspond to the listener name
     *               in some target agent object
     * @param objectName the name of the logical destination object
     *                   in the target agent
     * @param messageName the name of the message
     * @param content the content of the message
     * @param priority the priority of the message
     * @param autoConnect the flag controlling automatic (re)connection
     * @return the handler to the outgoing message object
     * @throws YAMIIOException if the channel cannot be established
     */
    public OutgoingMessage send(String target,
            String objectName, String messageName,
            YAMISerializable content, int priority, boolean autoConnect)
        throws YAMIIOException {
        
        return doSend(null, target, objectName, messageName,
                content, priority, autoConnect, false);
    }
    
    /**
     * Sends the new outgoing message to the given destination.
     * 
     * <p>
     * The content object is serialized and is no longer referenced
     * after this function returns. If the content is null, an empty
     * <code>Parameters</code> object is sent.
     * </p>
     * <p><b>Note:</b>
     * This function implicitly opens a new communication channel
     * if it is not already open. This channel is kept open until
     * it is explicitly closed
     * (see the <code>closeConnection</code> function)
     * or until the whole agent is closed or the communication error
     * is detected.
     * </p>
     * 
     * @param updateCallback object that will be called
     *                       when the state of outgoing message changes 
     * @param target the name of the target endpoint -
     *               this name should correspond to the listener name
     *               in some target agent object
     * @param objectName the name of the logical destination object
     *                   in the target agent
     * @param messageName the name of the message
     * @param content the content of the message
     * @return the handler to the outgoing message object
     * @throws YAMIIOException if the channel cannot be established
     */
    public OutgoingMessage send(OutgoingMessageCallback updateCallback,
    		String target, String objectName, String messageName,
            YAMISerializable content) throws YAMIIOException {
        
        return doSend(updateCallback, target, objectName, messageName,
        		content, 0, true, false);
    }
    
    /**
     * Sends the new outgoing message to the given destination.
     * 
     * <p>
     * The content object is serialized and is no longer referenced
     * after this function returns. If the content is null, an empty
     * <code>Parameters</code> object is sent.
     * </p>
     * <p><b>Note:</b>
     * This function implicitly opens a new communication channel
     * if it is not already open. This channel is kept open until
     * it is explicitly closed
     * (see the <code>closeConnection</code> function)
     * or until the whole agent is closed or the communication error
     * is detected.
     * </p>
     * 
     * @param updateCallback object that will be called
     *                       when the state of outgoing message changes 
     * @param target the name of the target endpoint -
     *               this name should correspond to the listener name
     *               in some target agent object
     * @param objectName the name of the logical destination object
     *                   in the target agent
     * @param messageName the name of the message
     * @param content the content of the message
     * @param priority the priority of the message
     * @return the handler to the outgoing message object
     * @throws YAMIIOException if the channel cannot be established
     */
    public OutgoingMessage send(OutgoingMessageCallback updateCallback,
    		String target, String objectName, String messageName,
            YAMISerializable content, int priority) throws YAMIIOException {
        
        return doSend(updateCallback, target, objectName, messageName,
                content, priority, true, false);
    }
    
    /**
     * Sends the new outgoing message to the given destination.
     * 
     * <p>
     * The content object is serialized and is no longer referenced
     * after this function returns. If the content is null, an empty
     * <code>Parameters</code> object is sent.
     * </p>
     * <p><b>Note:</b>
     * This function implicitly opens a new communication channel
     * if it is not already open. This channel is kept open until
     * it is explicitly closed
     * (see the <code>closeConnection</code> function)
     * or until the whole agent is closed or the communication error
     * is detected.
     * </p>
     * 
     * @param updateCallback object that will be called
     *                       when the state of outgoing message changes 
     * @param target the name of the target endpoint -
     *               this name should correspond to the listener name
     *               in some target agent object
     * @param objectName the name of the logical destination object
     *                   in the target agent
     * @param messageName the name of the message
     * @param content the content of the message
     * @param priority the priority of the message
     * @param autoConnect the flag controlling automatic (re)connection
     * @return the handler to the outgoing message object
     * @throws YAMIIOException if the channel cannot be established
     */
    public OutgoingMessage send(OutgoingMessageCallback updateCallback,
    		String target, String objectName, String messageName,
            YAMISerializable content, int priority, boolean autoConnect)
        throws YAMIIOException {
        
        return doSend(updateCallback, target, objectName, messageName,
                content, priority, autoConnect, false);
    }
    
    /**
     * Sends the new outgoing message to the given destination, without
     * the possibility to track its progress.
     * 
     * <p>
     * The content object is serialized and is no longer referenced
     * after this function returns. If the content is null, an empty
     * <code>Parameters</code> object is sent.
     * </p>
     * <p><b>Note:</b>
     * This function implicitly opens a new communication channel
     * if it is not already open. This channel is kept open until
     * it is explicitly closed
     * (see the <code>closeConnection</code> function)
     * or until the whole agent is closed or the communication error
     * is detected.
     * </p>
     * 
     * @param target the name of the target endpoint -
     *               this name should correspond to the listener name
     *               in some target agent object
     * @param objectName the name of the logical destination object
     *                   in the target agent
     * @param messageName the name of the message
     * @param content the content of the message
     * @throws YAMIIOException if the channel cannot be established
     */
    public void sendOneWay(String target,
            String objectName, String messageName,
            YAMISerializable content) throws YAMIIOException {
        doSend(null, target, objectName, messageName,
        		content, 0, true, true);
    }
    
    /**
     * Sends the new outgoing message to the given destination, without
     * the possibility to track its progress.
     * 
     * <p>
     * The content object is serialized and is no longer referenced
     * after this function returns. If the content is null, an empty
     * <code>Parameters</code> object is sent.
     * </p>
     * <p><b>Note:</b>
     * This function implicitly opens a new communication channel
     * if it is not already open. This channel is kept open until
     * it is explicitly closed
     * (see the <code>closeConnection</code> function)
     * or until the agent is closed or the communication error
     * is detected.
     * </p>
     * 
     * @param target the name of the target endpoint -
     *               this name should correspond to the listener name
     *               in some target agent object
     * @param objectName the name of the logical destination object
     *                   in the target agent
     * @param messageName the name of the message
     * @param content the content of the message
     * @param priority the priority of the message
     * @throws YAMIIOException if the channel cannot be established
     */
    public void sendOneWay(String target,
            String objectName, String messageName,
            YAMISerializable content, int priority) throws YAMIIOException {

        // the reply is not expected for this message
        // - the message is just posted
        // to the outgoing queue and forgotten, except that the progress
        // callback is installed that can properly manage the output
        // flow control semaphore

        doSend(null, target, objectName, messageName, content,
                priority, true, true);
    }

    /**
     * Sends the new outgoing message to the given destination, without
     * the possibility to track its progress.
     * 
     * <p>
     * The content object is serialized and is no longer referenced
     * after this function returns. If the content is null, an empty
     * <code>Parameters</code> object is sent.
     * </p>
     * <p><b>Note:</b>
     * This function implicitly opens a new communication channel
     * if it is not already open. This channel is kept open until
     * it is explicitly closed
     * (see the <code>closeConnection</code> function)
     * or until the agent is closed or the communication error
     * is detected.
     * </p>
     * 
     * @param target the name of the target endpoint -
     *               this name should correspond to the listener name
     *               in some target agent object
     * @param objectName the name of the logical destination object
     *                   in the target agent
     * @param messageName the name of the message
     * @param content the content of the message
     * @param priority the priority of the message
     * @param autoConnect the flag controlling automatic (re)connection
     * @throws YAMIIOException if the channel cannot be established
     */
    public void sendOneWay(String target,
            String objectName, String messageName,
            YAMISerializable content, int priority, boolean autoConnect)
        throws YAMIIOException {

        // the reply is not expected for this message
        // - the message is just posted
        // to the outgoing queue and forgotten, except that the progress
        // callback is installed that can properly manage the output
        // flow control semaphore

        doSend(null, target, objectName, messageName, content,
                priority, autoConnect, true);
    }

    /**
     * Closes the given connection with default (lowest) priority.
     * 
     * @param target the name of connection to close
     */
    public void closeConnection(String target) {
        closeConnection(target, 0);
    }
    
    /**
     * Closes the given connection.
     * 
     * <p>
     * The priority allows to properly handle the existing outgoing
     * messages that are waiting in the outgoing queue for transmission.
     * </p>
     * <p>
     * The existing messages with lower priority are
     * abandoned, whereas the existing messages with priority equal
     * or higher to the one provided as parameter are retained in the
     * outgoing queue and are properly pushed for transmission
     * before the channel is physically closed.
     * </p>
     * <p>
     * The channel is closed immediately only if there are no
     * messages waiting in its outgoing queue.
     * </p>
     * <p>
     * If the failover group is given then all targets from the group
     * are closed.
     * </p>
     * <p>
     * Closing the channel that does not exist is a no-op.
     * </p>
     * 
     * @param target the name of connection to close
     * @param priority proprity of the request, respects existing
     *        messages in the outgoing queue
     */
    public void closeConnection(String target, int priority) {
        if (isTargetFailover(target)) {
            // close all targets from the failover group
            doCloseConnections(splitFailoverTargets(target), false, priority);
        } else {
            // this is a single target
            List<String> targets = new ArrayList<String>();
            targets.add(target);
            doCloseConnections(targets, false, priority);
        }
    }
   
    /**
     * Immediately closes the given connection.
     * 
     * <p>
     * The channel is closed immediately and those messages that are
     * waiting in its outgoing queue are abandoned. Integrity of the
     * message that was already partly transmitted is not guaranteed.
     * </p>
     * <p>
     * If the failover group is given then all targets from the group
     * are closed.
     * </p>
     * <p>
     * Closing the channel that does not exist is a no-op.
     * </p>
     * 
     * @param target the name of connection to close
     */
    public void hardCloseConnection(String target) {
        if (isTargetFailover(target)) {
            // close all targets from the failover group
            doCloseConnections(splitFailoverTargets(target), true, 0);
        } else {
            // this is a single target
            List<String> targets = new ArrayList<String>();
            targets.add(target);
            doCloseConnections(targets, true, 0);
        }
    }
   
    /**
     * Helper class for returning the state of outgoing flow.
     */
    public static class OutgoingFlowState {
        public int currentLevel;
        public int highWaterMark;
        public int lowWaterMark;
    }
    
    /**
     * Obtains the state of overall outgoing flow.
     *
     * Obtains the state of overall outgoing flow.
     *
     * <b>Note:</b>
     * The outgoing flow is a combination of all outgoing traffic,
     * and is not tied to any particular communication channel.
     */
    public OutgoingFlowState getOutgoingFlowState() {
        return outgoingFlowManager.getFlowState();
    }
    
    /**
     * Cleans up all resources and stops internal threads.
     * 
     * <p><b>Note:</b> After this function is called the agent should
     * not be used any longer.</p>
     */
    public void close() {

        // stop the I/O worker thread
        
        ioWorker.requestStop();
        try {
            ioWorkerThread.join();
        } catch (InterruptedException ex) {
            // ignore
        }

        // stop the dispatcher threads
        
        dispatchManager.close();
        
        // physically close all existing listeners
        
        for (Map.Entry<String, Listener> e : listeners.entrySet()) {
            Listener lst = e.getValue();
            lst.close();
        }
        
        // physically close all existing channels
        
        for (Map.Entry<String, Channel> e : channels.entrySet()) {
            Channel ch = e.getValue();
            ch.close();
            reportClosedChannel(e.getKey());
        }

        if (logCallback != null) {
            logCallback.log(LogCallback.LogLevel.LOW,
                    "Agent closed.");
        }
    }
    
    private OutgoingMessage doSend(
    		OutgoingMessageCallback updateCallback,
    		String target, String objectName, String messageName,
            YAMISerializable content, int priority,
            boolean autoConnect, boolean oneWay)
            throws YAMIIOException {

        boolean waitForTransmission = false;
        boolean waitForReplyOrReject = false;

        OutgoingMessage message = null;
        YAMIIOException lastException = null;

        if (isTargetFailover(target)) {
            // the given target represents a group of separate destinations
            // that should be tried in random order until success
            
            List<String> targets = splitFailoverTargets(target);
            if (targets.isEmpty()) {
                throw new UnexpectedValueException(
                        "Empty failover group is not allowed.");
            }

            Collections.shuffle(targets);
            
            if (oneWay) {
                // with failover the one-way messages are implicitly
                // forced to wait for transmission
                
                waitForTransmission = true;

            } else {
                // with failover the two-way messages are implicitly
                // forced to wait for completion
                
                waitForReplyOrReject = true;
            }

            // try failover targets one by one until success
            for (String t : targets) {
                try {
                    message = doSendToSingleTarget(updateCallback,
                    		t, objectName, messageName,
                            content, priority, autoConnect, oneWay,
                            waitForTransmission,
                            waitForReplyOrReject);

                    // in the absence of exceptions
                    // consider this message to be successfully sent
                    
                    lastException = null;
                    break;
                        
                } catch (YAMIIOException ex) {
                    lastException = ex;
                }
            }
                
        } else {
            // this is a single-target message
            // do not force any waiting
            
            message = doSendToSingleTarget(updateCallback,
            		target, objectName, messageName,
                    content, priority, autoConnect, oneWay,
                    waitForTransmission,
                    waitForReplyOrReject);
        }
        
        if (lastException != null) {
            throw lastException;
        }

        return message;
    }
    
    private OutgoingMessage doSendToSingleTarget(
    		OutgoingMessageCallback updateCallback,
    		String target, String objectName, String messageName,
            YAMISerializable content, int priority, boolean autoConnect,
            boolean oneWay,
            boolean waitForTransmission, boolean waitForReplyOrReject)
            throws YAMIIOException {

        YAMISerializable contentToUse;
        if (content != null) {
            contentToUse = content;
        } else {
            // use empty object
            contentToUse = emptyParameters;
        }
        
        // flow control
        outgoingFlowManager.waitForPermission();
        
        // prepare the message header
        
        final long messageId = getNextId();
        
        Parameters header = new Parameters();
        header.setString("type", "message");
        header.setLong("message_id", messageId);
        header.setString("object_name", objectName);
        header.setString("message_name", messageName);
        
        // serialize everything together and post to the outgoing
        // queue of the channel that is relevant for the given target
        
        final int preferredFrameSize =
            NetworkUtils.getPreferredFrameSize(options, target);
        
        ArrayList<byte[]> allBuffers = new ArrayList<byte[]>();
        int messageHeaderSize =
            serialize(header, contentToUse, preferredFrameSize, allBuffers);
        
        Channel ch = makeSureChannelExists(target, autoConnect);
        
        OutgoingMessage message = null;
        MessageProgressCallback messageProgressCallback;
        
        if (oneWay == false ||
                waitForTransmission || waitForReplyOrReject) {
            
            // the OutgoingMessage needs to be created, because this is
            // either the two-way message or a failover message
            // with forced wait on transmission or completion

            message = new OutgoingMessage(messageId, updateCallback,
                    outgoingMessageManager, outgoingFlowManager);

            messageProgressCallback = message.getProgressCallback();
            outgoingMessageManager.registerNewMessage(messageId,
                    messageProgressCallback);

        } else {
            // this is purely one-way message with no need
            // for any (explicit or implicit) progress feedback
            
            messageProgressCallback = oneWayMessageProgressCallback;
        }
        
        // flow control
        // to avoid hazards increase before posting
        outgoingFlowManager.increase();

        int transportId = (int) messageId;
        
        boolean firstFrame = ch.post(transportId, priority,
                allBuffers, messageHeaderSize,
                messageProgressCallback);

        if (firstFrame) {
            ioWorker.wakeup();
        }
        
        if (waitForTransmission) {
            message.waitForTransmission();
        }
        if (waitForReplyOrReject) {
            if (options.defaultFailoverTimeout > 0) {
                message.waitForCompletion((long)options.defaultFailoverTimeout);
            } else {
                message.waitForCompletion();
            }

            if (message.getState() ==
                    OutgoingMessage.MessageState.ABANDONED) {
                throw new YAMIIOException("The message has been abandoned.");
            }
        }
        
        if (oneWay) {
            if (message != null) {
                // the OutgoingMessage object has been artificially created
                // it is of no interest to the user,
                // so needs to be cleaned up
                
                message.close();
                message = null;
            }
        }
        
        if (logCallback != null) {
            if (logLevel == LogCallback.LogLevel.MEDIUM ||
                    logLevel == LogCallback.LogLevel.HIGH) {
                
                logCallback.log(LogCallback.LogLevel.MEDIUM,
                        "Message posted:" +
                        " target: " + target +
                        " object: " + objectName +
                        " name: " + messageName +
                        " priority: " + priority +
                        " mid: " + messageId +
                        " tid: " + (int) messageId);
            }
        }
        
        return message;
    }

    void doSendReply(String target, long messageId,
            YAMISerializable content, int priority)
            throws YAMIIOException {

        YAMISerializable contentToUse;
        if (content != null) {
            contentToUse = content;
        } else {
            // use empty object
            contentToUse = emptyParameters;
        }
        
        // there is no flow control for replies
        
        // prepare the message header
                
        Parameters header = new Parameters();
        header.setString("type", "reply");
        header.setLong("message_id", messageId);
        
        int transportId =
                doSendReplyOrReject(target, header, contentToUse, priority);

        if (logCallback != null) {
            if (logLevel == LogCallback.LogLevel.MEDIUM ||
                    logLevel == LogCallback.LogLevel.HIGH) {
                
                logCallback.log(LogCallback.LogLevel.MEDIUM,
                        "Reply posted:" +
                        " target: " + target +
                        " priority: " + priority +
                        " mid: " + messageId +
                        " tid: " + transportId);
            }
        }
    }
    
    void doSendReject(String target, long messageId,
            String reason, int priority)
            throws YAMIIOException {

        // there is no flow control for rejections
        
        // prepare the message header
                
        Parameters header = new Parameters();
        header.setString("type", "exception");
        header.setLong("message_id", messageId);
        header.setString("reason", reason);
        
        int transportId = doSendReplyOrReject(
                target, header, emptyParameters, priority);

        if (logCallback != null) {
            if (logLevel == LogCallback.LogLevel.MEDIUM ||
                    logLevel == LogCallback.LogLevel.HIGH) {
                
                logCallback.log(LogCallback.LogLevel.MEDIUM,
                        "Reject posted:" +
                        " target: " + target +
                        " priority: " + priority +
                        " mid: " + messageId +
                        " tid: " + transportId);
            }
        }
    }
    
    private int doSendReplyOrReject(String target,
            Parameters header, YAMISerializable content, int priority)
            throws YAMIIOException {
        
        // serialize everything together and post to the outgoing
        // queue of the relevant channel
        
        final int preferredFrameSize =
            NetworkUtils.getPreferredFrameSize(options, target);
        
        ArrayList<byte[]> allBuffers = new ArrayList<byte[]>();
        int messageHeaderSize =
            serialize(header, content, preferredFrameSize, allBuffers);
        
        Channel ch = makeSureChannelExists(target, false);

        int transportId = (int) getNextId();
        boolean firstFrame = ch.post(transportId, priority,
                allBuffers, messageHeaderSize,
                null);

        if (firstFrame) {
            ioWorker.wakeup();
        }
        
        return transportId;
    }
    
    private int serialize(Parameters header, YAMISerializable content,
            int preferredFrameSize, ArrayList<byte[]> allBuffers) {
        
        final int frameHeaderSize = Frame.FRAME_HEADER_SIZE;
        final int chunkSize = preferredFrameSize - frameHeaderSize;
        
        List<byte[]> headerBuffers = header.serialize(chunkSize);
        List<byte[]> contentBuffers = content.serialize(chunkSize);

        int messageHeaderSize = Channel.groupBuffers(
                allBuffers, headerBuffers, contentBuffers, chunkSize);
        
        return messageHeaderSize;
    }

    private void reportChannelEvent(String target,
            ConnectionEventCallback.ConnectionEvent event) {
        if (connectionEventCallback != null) {
            try {
                connectionEventCallback.report(target, event);
            } catch (Exception e) {
                // ignore exceptions from user code
            }
        }                                
    }
    
    private void reportClosedChannel(String target) {
        reportChannelEvent(target,
                ConnectionEventCallback.ConnectionEvent.CONNECTION_CLOSED);
    }
    
    private Channel makeSureChannelExists(String target, boolean autoConnect)
            throws YAMIIOException {
        
        // note:
        // This function can be called by multiple threads.
        // In order to allow for higher concurrency it can lead to
        // multiple connections being created for the same target,
        // although only one such connection is ultimately kept open.
        
        String exceptionMsg = "I/O Error";
        
        Channel ch;
        synchronized (channels) {
            ch = channels.get(target);
        }
        
        if (ch == null && autoConnect) {
            // smart retry when connection fails is implemented
            // by bounded retry with randomized sleep between retries to
            // spread out initialization bursts in bigger systems
        
            final int connectionRetries = options.connectionRetries;
            final int connectionRetryDelaySpread =
                options.connectionRetryDelaySpread;
    
            for (int i = 0; i != connectionRetries; ++i) {
                try {
                    ch = new Channel(target, options,
                            incomingMessageDispatchCallback,
                            ioWorker,
                            logCallback, logLevel);
                    
                    reportChannelEvent(target,
                            ConnectionEventCallback.
                            ConnectionEvent.NEW_OUTGOING_CONNECTION);
                    
                    Channel existing = null;
                    synchronized (channels) {
                        existing = channels.get(target);
                        if (existing == null) {
                            channels.put(target, ch);
                        }
                    }
                    
                    if (existing != null) {
                        ch.close();
                        ch = existing;

                        reportClosedChannel(target);
                    }
                
                    ioWorker.prepareForChanges();
                    
                    break;
                } catch (IOException ex) {
                    exceptionMsg = ex.getMessage();
                }
    
                if (i < connectionRetries - 1) {
                    final long pause =
                        (long)(Math.random() * connectionRetryDelaySpread);
            
                    try {
                        Thread.sleep(pause);
                    } catch (InterruptedException ex) {
                        // ignore
                    }
                }
            }
        }
        
        if (ch == null) {
            throw new YAMIIOException(exceptionMsg);
        }
        
        return ch;
    }
    
    private void doCloseConnections(List<String> targets, boolean hard, int priority) {
        synchronized (channels) {
            for (String t : targets) {
                Channel ch = channels.get(t);
                if (ch != null) {
                	if (hard) {
                		doClose(ch, t);
                	} else {
                		boolean closeMeImmediately = ch.postClose(priority);
                		if (closeMeImmediately) {
                			doClose(ch, t);
                		}
                
                		// if the channel is not closed immediately,
	                    // it will be kept alive until all messages before
	                    // the close reuest are transmitted
	                    // after that the close event is discovered by the worker
	                    // and the channel closing and removal is performed
	                    // by the worker thread
                	}
                }
            }
        }
    }
    
    // synchronized by caller
    private void doClose(Channel ch, String target) {
    	ch.close();
        channels.remove(target);
        reportClosedChannel(target);
        
        ioWorker.prepareForChanges();
    }

    private boolean isTargetFailover(String target) {
        return target.startsWith(failoverPrefix) &&
                target.endsWith(failoverSuffix);
    }
    
    private List<String> splitFailoverTargets(String failoverTarget) {
        final String failoverContent = failoverTarget.substring(
                failoverPrefix.length(),
                failoverTarget.length() - failoverSuffix.length());
        String[] targets = failoverContent.split(failoverSeparator);
        
        List<String> result = new ArrayList<String>();
        for (String t : targets) {
            if (t.isEmpty() == false) {
                result.add(t);
            }
        }
        
        return result;
    }
    
    private long getNextId() {
        return messageIdGenerator.incrementAndGet();
    }
}
