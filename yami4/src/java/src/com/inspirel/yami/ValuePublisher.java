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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.inspirel.yami.ValuePublisherOverflowCallback.OverflowHandling;

/**
 * Simple subscription publisher.
 *
 * The subscription publisher that notifies remote listeners
 * with published value updates.
 *
 * Remote listeners can subscribe and unsubscribe at any time.
 */
public class ValuePublisher {
    
    private Agent controllingAgent;
    private String objectName;
    
    private static class SubscriptionState {
        String destinationObject;
        List<OutgoingMessage> lastSentMessages;

        SubscriptionState(String destinationObject) {
            this.destinationObject = destinationObject;
            this.lastSentMessages = new ArrayList<OutgoingMessage>();
        }
        
        void close() {
            for (OutgoingMessage msg : lastSentMessages) {
                msg.close();
            }
            lastSentMessages.clear();
        }
    }
    
    // destination target ->
    //      {destination object, previously sent message (or null)}
    private final Map<String, SubscriptionState> subscriptions;
    
    private final IncomingMessageCallback userCommandCallback;
    
    private int maxQueueLength;
    private ValuePublisherOverflowCallback overflowCallback;
    
    // the command callback understands "subscribe" and "unsubscribe"
    // commands from remote agents and delegates any other command
    // to the external handler provided when the PublishedValue is contructed
    private class CommandCallback implements IncomingMessageCallback {

        @Override
        public void call(IncomingMessage message) throws Exception {
            
            String messageName = message.getMessageName();
            
            if (messageName.equals("subscribe") ||
                    messageName.equals("unsubscribe")) {
                
                // extract the destination target
                
                Parameters content = message.getParameters();
                
                String destinationTarget = null;
                
                Parameters.Entry entry = content.find("destination_target");
                if (entry != null) {
                    if (entry.type() == Parameters.EntryType.STRING) {
                        destinationTarget = entry.getString();
                    }
                }
                
                if (destinationTarget == null) {
                    // if the destination target is not specified
                    // in the subscription message, use the
                    // message source as a default

                    destinationTarget = message.getSource();
                }
                
                if (messageName.equals("subscribe")) {
                    // extract the destination object name
                    
                    String destinationObject = null;
                    
                    entry = content.find("destination_object");
                    if (entry != null) {
                        if (entry.type() == Parameters.EntryType.STRING) {
                            destinationObject = entry.getString();
                        }
                    }
                    
                    if (destinationObject == null) {
                        // if the destination object is not specified
                        // in the subscription message, use the
                        // local object name as a default

                        destinationObject = message.getObjectName();
                    }
                    
                    subscribe(destinationTarget, destinationObject);
                    
                } else { // "unsubscribe"

                    unsubscribe(destinationTarget);
                }
            }
            
            // any message - delegate to external handler
                
            if (userCommandCallback != null) {
                userCommandCallback.call(message);
            } else {
                // in the absence of user callback,
                // just confirm this operation

                message.reply(null);
            }
        }
    }
    
    private final CommandCallback commandCallback;
    
    /**
     * Constructor.
     * 
     * Creates the subscription publisher
     * that is not registered at any agent.
     * The new publisher will use queues of length 1 for each subscriber.
     */
    public ValuePublisher() {
        this(null);
    }
     
    /**
     * Constructor.
     *
     * <p>
     * Creates the subscription publisher
     * that is not registered at any agent and that
     * delegates arbitrary remote commands to the given callback.
     * The new publisher will use queues of length 1 for each subscriber.
     * </p>
     * <p>
     * <b>Note:</b>
     * The "subscribe" and "unsubscribe" messages are also forwarded
     * to the user-provided callback, but these two messages are already
     * processed by the published value's implementation.
     * </p>
     * 
     * @param callback the callback implementation for unknown commands
     */
    public ValuePublisher(IncomingMessageCallback callback) {
        this(callback, 1, null);
    }
     
    /**
     * Constructor.
     *
     * <p>
     * Creates the subscription publisher
     * that is not registered at any agent and that
     * delegates arbitrary remote commands to the given callback
     * as well as reports queue overflow conditions.
     * </p>
     * <p>
     * <b>Note:</b>
     * The "subscribe" and "unsubscribe" messages are also forwarded
     * to the user-provided callback, but these two messages are already
     * "replied-to" by the published value's implementation.
     * </p>
     * 
     * @param callback the callback implementation for unknown commands
     * @param maxQueueLength length of message queue for each subscriber
     * @param overflowCallback the callback implementation
     *        for overflow condition
     */
    public ValuePublisher(IncomingMessageCallback callback,
            int maxQueueLength,
            ValuePublisherOverflowCallback overflowCallback) {
        
        controllingAgent = null;
        subscriptions = new HashMap<String, SubscriptionState>();
        userCommandCallback = callback;
        commandCallback = new CommandCallback();
        this.maxQueueLength = maxQueueLength;
        this.overflowCallback = overflowCallback;
    }
     
    /**
     * Subscribe new listener.
     * 
     * This function is usually called internally as a result of
     * processing the remote "subscribe" message, but can be also
     * used locally if the listener's location is obtained via
     * other means.
     *
     * @param destinationTarget target of the remote listener
     * @param destinationObject name of the remote listener's object
     * @throws YAMIIOException if the connection to the destination
     *                         cannot be established
     */
    public void subscribe(String destinationTarget,
            String destinationObject) throws YAMIIOException {
        synchronized (this) {
            SubscriptionState subState =
                subscriptions.get(destinationTarget);
            if (subState == null) {
                // this is a new subscription
                 
                // make sure the channel exists,
                // so that further sends will not have to create it
                 
                controllingAgent.openConnection(destinationTarget);
                 
                subscriptions.put(destinationTarget,
                        new SubscriptionState(destinationObject));
            } else {
                // there is already a subscription for this target
                // -> refresh it
                 
                subState.destinationObject = destinationObject;
            }
        }
    }
     
    /**
     * Unsubscribe the given listener.
     * 
     * @param destinationTarget target of the remote listener
     */
    public void unsubscribe(String destinationTarget) {
        synchronized (this) {
            SubscriptionState subState =
                subscriptions.get(destinationTarget);
            subState.close();
            subscriptions.remove(destinationTarget);
        }
    }
     
    /**
     * Publishe the new value with default priority.
     *
     * @param value new value that is to be sent as update to all listeners
     */
    public void publish(Parameters value) {
        publish(value, 0);
    }
     
    /**
     * Publishe the new value.
     * Sends the update message to all active listeners
     * with the given value.
     * In case of any errors or communication problems, the problematic
     * listener is automatically unsubscribed.
     *
     * @param value new value that is to be sent as update to all listeners
     * @param priority priority of the update message
     */
    public void publish(YAMISerializable value, int priority) {
        if (controllingAgent == null) {
            throw new BadStateException("published value");
        }
         
        synchronized (this) {
            Iterator<Map.Entry<String, SubscriptionState>> it =
                subscriptions.entrySet().iterator();
            while (it.hasNext()) {
                Map.Entry<String, SubscriptionState> entry = it.next();
                 
                String destinationTarget = entry.getKey();
                SubscriptionState subState = entry.getValue();
                
                // check all previous messages that were still not processed
                // by this subscriber
                
                boolean abandonSubscription = false;
                
                Iterator<OutgoingMessage> mit =
                    subState.lastSentMessages.iterator();
                while (mit.hasNext()) {
                    OutgoingMessage msg = mit.next();
                    OutgoingMessage.MessageState state = msg.getState();
                    switch (state)
                    {
                    case TRANSMITTED:
                    case REPLIED:
                    case REJECTED:
                        
                        // this previous message has been successfully sent
                        
                        msg.close();
                        mit.remove();
                        break;
                        
                    case ABANDONED:
                        
                        // the whole channel is broken
                        // - abandon the subscription

                        abandonSubscription = true;
                        break;
                        
                    default:
                        break;
                    }
                    
                    if (abandonSubscription) {
                        break;
                    }
                }
                
                if (abandonSubscription) {
                    subState.close();
                    it.remove();
                    continue;
                }

                // check if there is a place in the queue
                
                if (subState.lastSentMessages.size() >= maxQueueLength) {
                    
                    // the queue is full - ask user for decision
                    
                    OverflowHandling decision =
                        OverflowHandling.WAIT_FOR_PREVIOUS_MESSAGE;
                    if (overflowCallback != null) {
                        try {
                            decision = overflowCallback.subscriptionOverflow(
                                    destinationTarget,
                                    subState.destinationObject,
                                    value);
                        } catch (Exception e) {
                            // threat user exceptions as "abandon message"
                            
                            decision = OverflowHandling.ABANDON_MESSAGE;
                        }
                    }

                    if (decision ==
                        OverflowHandling.WAIT_FOR_PREVIOUS_MESSAGE) {
                        
                        OutgoingMessage msg =
                            subState.lastSentMessages.get(0);
                        msg.waitForTransmission();
                        msg.close();
                        subState.lastSentMessages.remove(0);
                        
                    } else if (decision ==
                        OverflowHandling.ABANDON_MESSAGE) {
                        
                        continue;
                    } else {
                        // decision == OverflowHandling.ABANDON_SUBSCRIPTION
                        
                        subState.close();
                        it.remove();
                        continue;
                    }
                }
                
                // send the message
                    
                try {
                    final boolean autoConnect = false;
                     
                    OutgoingMessage message =
                        controllingAgent.send(destinationTarget,
                                subState.destinationObject,
                                "subscription_update", value, priority,
                                autoConnect);
                     
                    subState.lastSentMessages.add(message);
                } catch (Exception ex) {
                    // in case of any error drop this subscription
                     
                    it.remove();
                }
            }
        }
    }

    /**
     * Returns the number of current subscribes.
     *
     * @return number of current subscribers
     */
    public int getNumberOfSubscribers() {
        synchronized (this) {
            return subscriptions.size();
        }
    }

    /**
     * Helper class for holding destination target and object
     * for any given subscriber.
     */
    public static class SubscriberInfo {
        public final String destinationTarget;
        public final String destinationObject;
        public final int queueLength;

        public SubscriberInfo(String destinationTarget,
                String destinationObject, int queueLength) {
            this.destinationTarget = destinationTarget;
            this.destinationObject = destinationObject;
            this.queueLength = queueLength;
        }
    }

    /**
     * Returns the list of current subscribers.
     * @return list of subscriber info objects, one for each subscriber
     */
    public List<SubscriberInfo> getSubscribers() {
        List<SubscriberInfo> result = new ArrayList<SubscriberInfo>();
        synchronized (this) {
            for (Map.Entry<String, SubscriptionState> e :
                subscriptions.entrySet()) {

                final String destinationTarget = e.getKey();
                
                SubscriptionState subState = e.getValue();
               
                final String destinationObject =
                    subState.destinationObject;
                 
                result.add(new SubscriberInfo(
                        destinationTarget, destinationObject,
                        subState.lastSentMessages.size()));
            }
        }
        return result;
    }

    /**
     * Unregisters from the controlling agent and cleans up resources.
     */
    public void close() {
        if (controllingAgent == null) {
            throw new BadStateException("published value");
        }

        controllingAgent.unregisterObject(objectName);
        controllingAgent = null;

        for (SubscriptionState subState : subscriptions.values()) {
            subState.close();
        }
    }

    void registerAt(Agent agent, String objName) {
        if (this.controllingAgent != null) {
            throw new BadStateException("published value");
        }

        agent.registerObject(objName, commandCallback);

        this.controllingAgent = agent;
        this.objectName = objName;
    }
}
