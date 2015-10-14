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

namespace Inspirel.YAMI
{
    /// <summary> 
    /// Simple subscription publisher.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The subscription publisher that notifies remote listeners
    /// with published value updates.
    /// </para>
    /// <para>
    /// Remote listeners can subscribe and unsubscribe at any time. 
    /// </para>
    /// </remarks>
    public class ValuePublisher
    {
        private Agent controllingAgent;
        private string objectName;

        private class SubscriptionState
        {
            internal string destinationObject;
            internal OutgoingMessage lastSentMessage;

            internal SubscriptionState(string destinationObject)
            {
                this.destinationObject = destinationObject;
                this.lastSentMessage = null;
            }
        }

        // destination target ->
        //      {destination object, previously sent message (or null)}
        private readonly IDictionary<string, SubscriptionState> 
            subscriptions;

        private readonly IncomingMessageHandler userCommandCallback;

        // the command callback understands "subscribe" and "unsubscribe"
        // commands from remote agents and delegates any other command
        // to the external handler provided when the PublishedValue 
        // is contructed

        private void commandCallback(object sender, IncomingMessageArgs args)
        {
            IncomingMessage message = args.Message;
            string messageName = message.MessageName;

            if (messageName.Equals("subscribe") 
                || messageName.Equals("unsubscribe"))
            {
                // extract the destination target
                Parameters content = message.Parameters;

                string destinationTarget = null;

                Parameters.Entry entry = 
                    content.Find("destination_target");
                if (entry != null)
                {
                    if (entry.Type == Parameters.EntryType.STRING)
                    {
                        destinationTarget = entry.GetString();
                    }
                }

                if (destinationTarget == null)
                {
                    // if the destination target is not specified
                    // in the subscription message, use the
                    // message source as a default
                    destinationTarget = message.Source;
                }

                if (messageName.Equals("subscribe"))
                {
                // extract the destination object name

                    string destinationObject = null;

                    entry = content.Find("destination_object");
                    if (entry != null)
                    {
                        if (entry.Type == Parameters.EntryType.STRING)
                        {
                            destinationObject = entry.GetString();
                        }
                    }

                    if (destinationObject == null)
                    {
                    // if the destination object is not specified
                    // in the subscription message, use the
                    // local object name as a default

                        destinationObject = message.ObjectName;
                    }

                    Subscribe(destinationTarget, destinationObject);

                } // "unsubscribe"
                else
                {
                    Unsubscribe(destinationTarget);
                }
            }

            // any message - delegate to external handler
            IncomingMessageHandler temp = userCommandCallback;
            if(temp != null)
            {
                temp(sender, args);
            }
            else
            {
                // in the absence of user command, just confirm this message
                message.Reply(null);
            }
        }
        

        /// <summary> 
        /// Initializes a new instance of <see cref="ValuePublisher"/>
        /// </summary>
        /// <remarks>
        /// Creates the subscription publisher that is not registered 
        /// at any agent. 
        /// </remarks>
        public ValuePublisher() : this(null)
        {
        }

        /// <summary> 
        /// Initializes a new instance of <see cref="ValuePublisher"/>
        /// </summary>
        /// <remarks>
        /// <para>
        /// Creates the subscription publisher
        /// that is not registered at any agent and that
        /// delegates arbitrary remote commands to the given callback.
        /// </para>
        /// <para>
        /// <b>Note:</b>
        /// The "subscribe" and "unsubscribe" messages are also forwarded
        /// to the user-provided callback, but these two messages are already
        /// "replied-to" by the published value's implementation.
        /// </para>
        /// </remarks>
        /// 
        /// <param name="callback"> the callback implementation for unknown 
        /// commands </param>

        public ValuePublisher(IncomingMessageHandler callback)
        {
            controllingAgent = null;
            subscriptions = new Dictionary<string, SubscriptionState>();
            userCommandCallback = callback;
        }

        /// 
        /// <summary> Subscribe new listener.</summary>
        /// <remarks>
        /// This function is usually called internally as a result of
        /// processing the remote "subscribe" message, but can be also
        /// used locally if the listener's location is obtained via
        /// other means.
        /// </remarks>
        /// <param name="destinationTarget"> 
        /// target of the remote listener 
        /// </param>
        /// <param name="destinationObject"> 
        /// name of the remote listener's object 
        /// </param>
        /// <exception cref="YAMIIOException"> 
        /// if the connection to the destination cannot be established 
        /// </exception>
        public virtual void Subscribe(
            string destinationTarget, string destinationObject)
        {
            lock (this)
            {
                SubscriptionState subState = null;
                if(!subscriptions.ContainsKey(destinationTarget))
                {
                    // this is a new subscription

                    // make sure the channel exists,
                    // so that further sends will not have to create it
                    controllingAgent.OpenConnection(destinationTarget);

                    subscriptions.Add(destinationTarget, 
                        new SubscriptionState(destinationObject));
                }
                else
                {
                    // there is already a subscription for this target
                    // -> refresh it

                    releaseLastMessage(subState);

                    subState.destinationObject = destinationObject;
                }
            }
        }

        /// <summary> 
        /// Unsubscribe the given listener.
        /// </summary>
        /// <param name="destinationTarget"> 
        /// target of the remote listener 
        /// </param>
        public virtual void Unsubscribe(string destinationTarget)
        {
            lock (this)
            {
                SubscriptionState subState = 
                    subscriptions[destinationTarget];
                releaseLastMessage(subState);
                subscriptions.Remove(destinationTarget);
            }
        }

        /// <summary> 
        /// Publishe the new value with default priority.
        /// </summary>
        /// <remarks>
        /// In case of any errors or communication problems, the problematic
        /// listener is automatically unsubscribed.
        /// </remarks>
        /// <param name="value"> 
        /// new value that is to be sent as update to all listeners 
        /// </param>
        public virtual void Publish(Parameters value)
        {
            Publish(value, 0);
        }

        /// <summary> Publishe the new value.</summary>
        /// <remarks>
        /// <para>
        /// Sends the update message to all active listeners
        /// with the given value.
        /// </para>
        /// <para>
        /// In case of any errors or communication problems, the problematic
        /// listener is automatically unsubscribed.
        /// </para>
        /// </remarks>
        /// <param name="value"> 
        /// new value that is to be sent as update to all listeners 
        /// </param>
        /// <param name="priority"> 
        /// priority of the update message 
        /// </param>
        public virtual void Publish(Parameters value, int priority)
        {
            if (controllingAgent == null)
            {
                throw new BadStateException("published value");
            }

            lock (this)
            {
                List<string> targetsToRemove = new List<string>();
                
                foreach(KeyValuePair<string, SubscriptionState> entry 
                    in subscriptions)
                {
                    string destinationTarget = entry.Key;
                    SubscriptionState subState = entry.Value;

                    OutgoingMessage lastSentMessage = 
                        subState.lastSentMessage;
                    if (lastSentMessage != null)
                    {
                    // check (lazily) if the last message
                    // has been properly sent

                        lastSentMessage.WaitForTransmission();

                        if(lastSentMessage.State 
                            == OutgoingMessage.MessageState.ABANDONED)
                        {

                        // the previously sent message has been abandoned,
                        // which means that the channel has been closed
                        // -> deactivate this subscription

                            releaseLastMessage(subState);

                            targetsToRemove.Add(destinationTarget);

                            continue;
                        }

                    // the previously sent message was successful

                        releaseLastMessage(subState);
                    }

                    try
                    {
                        const bool autoConnect = false;

                        OutgoingMessage message = controllingAgent.Send(
                            destinationTarget, subState.destinationObject, 
                            "subscription_update", value, 
                            priority, autoConnect);

                        subState.lastSentMessage = message;
                    }
                    catch (Exception)
                    {
                    // in case of any error drop this subscription

                        targetsToRemove.Add(destinationTarget);
                    }
                }

                foreach(string targetToRemove in targetsToRemove)
                {
                    subscriptions.Remove(targetToRemove);
                }
            }
        }

        /// <summary> 
        /// Gets the number of current subscribes.
        /// </summary>
        public virtual int NumberOfSubscribers
        {
            get
            {
                lock (this)
                {
                    return subscriptions.Count;
                }
            }
        }

        /// <summary> 
        /// Helper class for holding destination target and object
        /// for any given subscriber. 
        /// </summary>
        public class SubscriberInfo
        {
            private readonly string destinationTarget;
            
            /// <summary>
            /// Gets address of the destination listener
            /// </summary>
            public string DestinationTarget
            {
                get
                {
                    return destinationTarget;
                }
            }

            private readonly string destinationObject;

            /// <summary>
            /// Gets name of the destination object
            /// </summary>
            public string DestinationObject
            {
                get
                {
                    return destinationObject;
                }
            }

            /// <summary>
            /// Initializes a new instace of the 
            /// <see cref="SubscriberInfo"/> class
            /// </summary>
            /// <param name="destinationTarget">address of the desitnation 
            /// listener</param>
            /// <param name="destinationObject">name of the desitnation
            /// object</param>
            public SubscriberInfo(
                string destinationTarget, string destinationObject)
            {
               this.destinationTarget = destinationTarget;
               this.destinationObject = destinationObject;
            }
        }

    
        /// <summary>
        /// Gets list of subscriber info objects, one for each subscriber
        /// </summary>
        public virtual IList<SubscriberInfo> Subscribers
        {
            get
            {
                IList<SubscriberInfo> result = new List<SubscriberInfo>();
                lock (this)
                {
                    foreach (KeyValuePair<string, SubscriptionState> e 
                        in subscriptions)
                    {
                        string destinationTarget = e.Key;
                        string destinationObject = 
                            e.Value.destinationObject;
   
                        result.Add(new SubscriberInfo(destinationTarget, 
                            destinationObject));
                    }
                }
                return result;
            }
        }

        /// <summary> 
        /// Unregisters from the controlling agent and cleans up resources. 
        /// </summary>
        public virtual void Close()
        {
            if (controllingAgent == null)
            {
                throw new BadStateException("published value");
            }

            controllingAgent.UnregisterObject(objectName);
            controllingAgent = null;

            foreach (SubscriptionState subState in subscriptions.Values)
            {
                if (subState.lastSentMessage != null)
                {
                    subState.lastSentMessage.Close();
                }
            }
        }

        internal virtual void registerAt(Agent agent, string objName)
        {
            if (this.controllingAgent != null)
            {
                throw new BadStateException("published value");
            }

            agent.RegisterObject(objName, commandCallback);

            this.controllingAgent = agent;
            this.objectName = objName;
        }

        private void releaseLastMessage(SubscriptionState subState)
        {
            if (subState.lastSentMessage != null)
            {
                subState.lastSentMessage.Close();
                subState.lastSentMessage = null;
            }
        }
    }
}
