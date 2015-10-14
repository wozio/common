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

using System.Collections.Generic;
using System.Threading;
using Inspirel.YAMI.details;

///<summary>Namespace devoted for everything related to YAMI4</summary>
namespace Inspirel.YAMI
{
    /// <summary>
    /// Message broker. 
    /// </summary>
    /// <remarks>
    /// <para> 
    /// The message broker that encapsulates physical channel 
    /// management, incoming and outgoing message queues, listeners 
    /// and resource management. </para> 
    /// <para> A single agent object can manage many listeners, which 
    /// are responsible for accepting remote connections, and many 
    /// incoming and outgoing connections. </para> 
    /// <para> The objects of this class can be safely used by multiple 
    /// threads, including the threads that execute incoming message 
    /// callbacks. </para>
    /// </remarks>
    public sealed class Agent 
        : LogCallback
        , MessageProgressCallback
        , IncomingMessageDispatchCallback
        , ConnectionEventCallback
    {

        private static readonly Parameters emptyParameters = 
            new Parameters();

        private static readonly string failoverPrefix = "failover:(";
        private static readonly string failoverSuffix = ")";
        private static readonly string failoverSeparator = "|";

        private readonly Options options;

        /// <summary>
        /// Occurs when YAMI library wants to log anything on currently set
        /// logging level
        /// </summary>
        /// <seealso cref="LogEventArgs"/>
        /// <seealso cref="LogEventArgs.LogLevel"/>
        /// <seealso cref="LogLevel"/>
        public event LogHandler Log;

        private LogEventArgs.LogLevel logLevel = LogEventArgs.LogLevel.LOW;

        /// <summary>
        /// Gets or sets logging level for which <see cref="Log"/> event
        /// will be raised
        /// </summary>
        public LogEventArgs.LogLevel LogLevel
        {
            get
            {
                return logLevel;
            }
            set
            {
                logLevel = value;
            }
        }

        private readonly Dictionary<string, Channel> channels;

        private readonly Dictionary<string, Listener> listeners;

        private readonly IncomingMessageDispatchCallback
                incomingMessageDispatchCallback;

        private readonly MessageProgressCallback 
            oneWayMessageProgressCallback;

        private readonly OutgoingMessageManager outgoingMessageManager;

        private readonly DispatchManager dispatchManager;

        private readonly WaterFlowManager outgoingFlowManager;
        private readonly WaterFlowManager incomingFlowManager;

        private readonly IOWorker ioWorker;
        private readonly Thread ioWorkerThread;

        private long messageIdGenerator;

        /// <summary>
        /// Occurs when state of connections was changed
        /// </summary>
        /// <seealso cref="ConnectionEventArgs"/>
        public event ConnectionEventHandler ConnectionsChanged;

        // this implementation of MessageProgressCallback interface 
        // is responsible for properly handling the outgoing
        // flow with one-way messages
        // when the message is completely transmitted (or cancelled),
        // the flow manager needs to be notified that the message
        // is no longer in the outgoing queue of any channel
        #region MessageProgressCallback Members

        void MessageProgressCallback.progress(int sentBytes, 
            int totalByteCount)
        {
            if(sentBytes == totalByteCount)
            {
                // some one-way message has been either
                // transmitted or abandoned
                outgoingFlowManager.decrease();
            }
        }

        void MessageProgressCallback.replied(Parameters body, 
            byte[] rawBody)
        {
            // ignore, this is meaningless for one-way messages
        }

        void MessageProgressCallback.rejected(string reason)
        {
            // ignore, this is meaningless for one-way message
        }

        #endregion


        // this implementation of IncomingMessageDispatchCallback
        // is responsible for dispatching incoming messages
        // - including real messages, replies, rejections, etc.
        #region IncomingMessageDispatchCallback Members

        void IncomingMessageDispatchCallback.dispatch(string sourceName, 
            Parameters header, Parameters body, byte[] rawBody)
        {
            string messageType = header.GetString("type");

            switch(messageType)
            {
            case "message":
                {
                    long messageId = header.GetLong("message_id");
                    string objectName = header.GetString("object_name");
                    string messageName = header.GetString("message_name");

                    IncomingMessage message = new IncomingMessage(
                            this, messageId, sourceName,
                            objectName, messageName, body, rawBody);

                    ((LogCallback)this).Log(LogEventArgs.LogLevel.MEDIUM,
                                    "Message received:" +
                                    " from: " + sourceName +
                                    " object: " + objectName +
                                    " message: " + messageName +
                                    " mid: " + messageId);

                    incomingFlowManager.increase();
                    dispatchManager.push(message);
                }
                break;

            case "reply":
                {
                    // this is a response to the message that was sent
                    // from this agent

                    long messageId = header.GetLong("message_id");

                    ((LogCallback)this).Log(LogEventArgs.LogLevel.MEDIUM,
                                    "Reply received:" +
                                    " from: " + sourceName +
                                    " mid: " + messageId);

                    outgoingMessageManager.reportReplied(
                            messageId, body, rawBody);
                }
                break;

            case "exception":
                {
                    // this is a rejection/exception for the message
                    // that was sent from this agent

                    long messageId = header.GetLong("message_id");

                    string reason = header.GetString("reason");

                    ((LogCallback)this).Log(LogEventArgs.LogLevel.MEDIUM,
                                   "Reject received:" +
                                   " from: " + sourceName +
                                   " mid: " + messageId);

                    outgoingMessageManager.reportRejected(messageId, reason);
                }
                break;
            }
        }

        #endregion

        /// <summary>
        /// Default constructor, creates new instance of the 
        /// <see cref="Agent"/> class with default option values.
        /// </summary>
        /// if the network resources cannot be obtained
        public Agent()
            : this(null, null, LogEventArgs.LogLevel.LOW)
        {
        }

        /// <summary>
        /// Creates new instance of the <see cref="Agent"/> class
        /// using provided configuration options.
        /// </summary>
        /// <param name="options">set of configuration options</param>
        public Agent(Parameters options)
            : this(options, null, LogEventArgs.LogLevel.LOW)
        {
        }

        /// <summary>
        /// Creates new instance of the <see cref="Agent"/> class
        /// with default options values, appends handler for 
        /// <see cref="Log"/> event.
        /// </summary>
        /// <param name="logHandler">Handler for <see cref="Log"/>event
        /// </param>
        /// <param name="logLevel">Initial log level</param>
        public Agent(
            LogHandler logHandler, LogEventArgs.LogLevel logLevel)
            : this(null, logHandler, logLevel)
        {
        }

        /// <summary>
        /// Creates new instance of the <see cref="Agent"/> class,
        /// appends handler for <see cref="Log"/> event
        /// and uses the provided configuration options.
        /// </summary>
        /// <param name="options">set of configuration options</param>
        /// <param name="logHandler">Handler for <see cref="Log"/>event
        /// </param>
        /// <param name="logLevel">Initial log level</param>
        public Agent(Parameters options,
            LogHandler logHandler, LogEventArgs.LogLevel logLevel)
        {
            LogLevel = logLevel;
            Log += logHandler;
            this.options = new Options(options);

            channels = new Dictionary<string, Channel>();
            listeners = new Dictionary<string, Listener>();

            outgoingFlowManager = new WaterFlowManager(
                    this.options.outgoingHighWaterMark,
                    this.options.outgoingLowWaterMark);

            incomingFlowManager = new WaterFlowManager(
                    this.options.incomingHighWaterMark,
                    this.options.incomingLowWaterMark);

            incomingMessageDispatchCallback = 
                (IncomingMessageDispatchCallback)this;

            oneWayMessageProgressCallback = (MessageProgressCallback)this;

            outgoingMessageManager = new OutgoingMessageManager();

            ioWorker = new IOWorker(
                    channels, listeners, incomingFlowManager,
                    this.options, incomingMessageDispatchCallback,
                    this,
                    this, logLevel);
            ioWorkerThread = new Thread(ioWorker.run);
            ioWorkerThread.Name = "YAMI4 I/O worker";
            ioWorkerThread.IsBackground = true;
            ioWorkerThread.Start();

            dispatchManager = new DispatchManager(
                this, this.options, incomingFlowManager, 
                ioWorker, this, logLevel);

            log(LogEventArgs.LogLevel.LOW, "Agent created.");
        }

        /// 
        /// <summary> Adds a new listener for the given target address.
        /// </summary>
        /// <remarks>
        /// <para>
        /// <list type="table">
        /// <listheader>
        /// <term>format</term>
        /// <description></description>
        /// </listheader>
        /// <item>
        /// <term>tcp://host:port</term>
        /// <description>for TCP/IP connections, where <c>host</c>
        /// can be provided in the symbolic or numeric form</description>
        /// </item>
        /// <item>
        /// <term>tcp://*:port</term>
        /// <description>for TCP/IP connections, for "any" local address
        /// </description>
        /// </item>
        /// <item>
        /// <term>tcp://port</term>
        /// <description>for TCP/IP connections, for "any" local address
        /// </description>
        /// </item>
        /// <item>
        /// <term>udp://host:port</term>
        /// <description>for UDP communication, with rules as for TCP/IP
        /// </description>
        /// </item>
        /// </list>
        /// </para>
        /// 
        /// <para>
        /// The port for TCP/IP protocol can be <c>0</c>, in which case
        /// the actual port number is assigned by the system.
        /// </para>
        /// </remarks>
        /// 
        /// <param name="target"> the target name for the new listener 
        /// </param>
        /// <returns> the locally resolved target name - this name can be 
        /// used by clients to create new connections and by local code
        /// to remove the listener </returns>
        /// <exception cref="YAMIIOException"> if the network resources 
        /// cannot be obtained </exception>
        /// 
        public string AddListener(string target)
        {
            try
            {
                Listener listener = NetworkUtils.prepareServer(
                    target, incomingMessageDispatchCallback, options, 
                    this, logLevel);
                string resolvedTarget = listener.ResolvedTarget;

                lock(listeners)
                {
                    listeners.Add(resolvedTarget, listener);
                }

                ioWorker.wakeup();

                log(LogEventArgs.LogLevel.LOW, 
                    "Added listener for " + resolvedTarget);

                return resolvedTarget;

            }
            catch(System.Exception ex)
            {
                throw new YAMIIOException(ex.Message, ex);
            }
        }

        /// <summary> Removes existing listener.</summary>
        /// 
        /// <remarks>
        /// Removes the listener denoted by its actual target name.
        /// Note that the actual target name might be different from the name
        /// provided when the listener was created, due to target resolution.
        /// The name which should be used for listener removal is the name
        /// that is returned by the <see cref="AddListener"/> function.
        /// </remarks>
        /// 
        /// <param name="target"> the (resolved) target name 
        /// of the listener to be removed </param>
        public void RemoveListener(string target)
        {
            lock(listeners)
            {
                if(listeners.ContainsKey(target))
                {
                    Listener listener = listeners[target];
                    listener.close();
                    listeners.Remove(target);

                    log(LogEventArgs.LogLevel.LOW,
                        "Removed listener for " + target);
                }
            }
        }

        /// <summary> Registers a new logical destination object.</summary>
        /// <remarks>
        /// <para>
        /// Registers the new "object" that can be a logical destination
        /// for incoming messages. The incoming messages that are recognized
        /// as being addressed to this new object will be delivered through
        /// the <see cref="IncomingMessageHandler"/> implementation
        /// which is therefore supposed to be implemented by the user.
        /// </para>
        /// <para>
        /// If an object with the given name is already registered,
        /// the registration data is replaced.
        /// </para>
        /// </remarks>
        /// <param name="objectName"> name of the newly registered object 
        /// </param>
        /// <param name="callback"> the callback handler implementation 
        /// </param>
        /// <seealso cref="IncomingMessageArgs"/>
        public void RegisterObject(
            string objectName, IncomingMessageHandler callback)
        {
            dispatchManager.registerObject(objectName, callback);

            log(LogEventArgs.LogLevel.MEDIUM, 
                "Registered object " + objectName);
        }

        /// 
        /// <summary> Registers the value publisher as a new logical object.
        /// </summary>
        /// <remarks>
        /// If an object with the given name is already registered,
        /// the registration data is replaced.
        /// </remarks>
        /// 
        /// <param name="objectName"> name of the newly registered object 
        /// </param>
        /// <param name="publisher"> value publisher to be registered 
        /// </param>
        public void RegisterValuePublisher(string objectName, 
            ValuePublisher publisher)
        {
            publisher.registerAt(this, objectName);
        }

        /// <summary> Unregisters a given destination object.</summary>
        /// <remarks>
        /// <para>
        /// If an object with the given name is not registered, this
        /// operation does nothing.
        /// </para>
        /// <para>
        /// <b>Note:</b>
        /// Due to performance and design tradeoffs it is <b>not</b> 
        /// guaranteed that no more messages will be ever dispatched to the 
        /// given object when this function returns.
        /// In fact, some of the messages that have been received by agent 
        /// and not yet dispatched might be still dispatched shortly after
        /// this function returns.
        /// Only those messages that are received by agent after
        /// this function returns are guaranteed not to be dispatched to the
        /// unregistered object.
        /// </para>
        /// </remarks>
        /// <param name="objectName"> name of the object to unregister 
        /// </param>
        public void UnregisterObject(string objectName)
        {
            dispatchManager.unregisterObject(objectName);

            log(LogEventArgs.LogLevel.MEDIUM, 
                "Unregistered object " + objectName);
       }

        /// <summary> Opens the new connection.</summary>
        /// <remarks>
        /// <para>
        /// Opens the new channel or does nothing if the channel already 
        /// exists.
        /// </para>
        /// <para>
        /// This function is not necessary with automatic connection
        /// recovery option in <see cref="Send(string, string, string, 
        /// Inspirel.YAMI.YAMISerializable, int, bool)"/> and 
        /// <see cref="SendOneWay(string, string, string, 
        /// Inspirel.YAMI.YAMISerializable, int, bool)"/>.
        /// </para>
        /// </remarks>
        /// <param name="target"> The name of the target endpoint.
        /// This name should correspond to the listener name
        /// in some target agent object. </param>
        /// <exception cref="YAMIIOException"> if the channel cannot be 
        /// established </exception>
        public void OpenConnection(string target)
        {
            makeSureChannelExists(target, true);
        }

        /// <summary> 
        /// Sends the new outgoing message to the given destination.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The content object is serialized and is no longer referenced
        /// after this function returns. If the content is null, an empty
        /// <see cref="Parameters"/> object is sent.
        /// </para>
        /// <para><b>Note:</b>
        /// This function implicitly opens a new communication channel
        /// if it is not already open. This channel is kept open until
        /// it is explicitly closed
        /// (see the <see cref="CloseConnection(string, int)"/> function)
        /// or until the whole agent is closed or the communication error
        /// is detected.
        /// </para>
        /// </remarks>
        /// <param name="target"> the name of the target endpoint -
        /// this name should correspond to the listener name
        /// in some target agent object </param>
        /// <param name="objectName"> the name of the logical destination 
        /// object in the target agent </param>
        /// <param name="messageName"> the name of the message </param>
        /// <param name="content"> the content of the message </param>
        /// <returns> the handler to the outgoing message object </returns>
        /// <exception cref="YAMIIOException"> if the channel cannot be 
        /// established </exception>
        public OutgoingMessage Send(string target, string objectName, 
            string messageName, YAMISerializable content)
        {
            return doSend(null, target, objectName, messageName, content,
                0, true, false);
        }

        /// 
        /// <summary> 
        /// Sends the new outgoing message to the given destination.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The content object is serialized and is no longer referenced
        /// after this function returns. If the content is null, an empty
        /// <see cref="Parameters"/> object is sent.
        /// </para>
        /// <para><b>Note:</b>
        /// This function implicitly opens a new communication channel
        /// if it is not already open. This channel is kept open until
        /// it is explicitly closed
        /// (see the <see cref="CloseConnection(string, int)"/> function)
        /// or until the whole agent is closed or the communication error
        /// is detected.
        /// </para>
        /// </remarks>
        /// <param name="target"> the name of the target endpoint -
        /// this name should correspond to the listener name
        /// in some target agent object </param>
        /// <param name="objectName"> the name of the logical destination 
        /// object in the target agent </param>
        /// <param name="messageName"> the name of the message </param>
        /// <param name="content"> the content of the message </param>
        /// <param name="priority"> the priority of the message </param>
        /// <returns> the handler to the outgoing message object </returns>
        /// <exception cref="YAMIIOException"> if the channel cannot be 
        /// established </exception>
        public OutgoingMessage Send(string target, string objectName, 
            string messageName, YAMISerializable content, int priority)
        {
            return doSend(null, target, objectName, messageName, content,
                priority, true, false);
        }

        /// <summary> 
        /// Sends the new outgoing message to the given destination.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The content object is serialized and is no longer referenced
        /// after this function returns. If the content is null, an empty
        /// <see cref="Parameters"/> object is sent.
        /// </para>
        /// <para><b>Note:</b>
        /// This function implicitly opens a new communication channel
        /// if it is not already open. This channel is kept open until
        /// it is explicitly closed
        /// (see the <see cref="CloseConnection(string, int)"/> function)
        /// or until the whole agent is closed or the communication error
        /// is detected.
        /// </para>
        /// </remarks>
        /// <param name="target"> the name of the target endpoint -
        /// this name should correspond to the listener name
        /// in some target agent object </param>
        /// <param name="objectName"> the name of the logical destination 
        /// object in the target agent </param>
        /// <param name="messageName"> the name of the message </param>
        /// <param name="content"> the content of the message </param>
        /// <param name="priority"> the priority of the message </param>
        /// <param name="autoConnect"> the flag controlling automatic 
        /// (re)connection </param>
        /// <returns> the handler to the outgoing message object </returns>
        /// <exception cref="YAMIIOException"> if the channel cannot be 
        /// established </exception>
        public OutgoingMessage Send(string target, string objectName, 
            string messageName, YAMISerializable content, 
            int priority, bool autoConnect)
        {
            return doSend(null, target, objectName, messageName, content,
                priority, autoConnect, false);
        }

        /// <summary>
        /// Sends the new outgoing message to the given destination.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The content object is serialized and is no longer referenced
        /// after this function returns. If the content is null, an empty
        /// <see cref="Parameters"/> object is sent.
        /// </para>
        /// <para><b>Note:</b>
        /// This function implicitly opens a new communication channel
        /// if it is not already open. This channel is kept open until
        /// it is explicitly closed
        /// (see the <see cref="CloseConnection(string, int)"/> function)
        /// or until the whole agent is closed or the communication error
        /// is detected.
        /// </para>
        /// </remarks>
        /// <param name="updateCallback">
        /// method that will be called when the state of outgoing message
        /// changes
        /// </param>
        /// <param name="target"> the name of the target endpoint -
        /// this name should correspond to the listener name
        /// in some target agent object </param>
        /// <param name="objectName"> the name of the logical destination 
        /// object in the target agent </param>
        /// <param name="messageName"> the name of the message </param>
        /// <param name="content"> the content of the message </param>
        /// <returns> the handler to the outgoing message object </returns>
        /// <exception cref="YAMIIOException"> if the channel cannot be 
        /// established </exception>
        public OutgoingMessage Send(OutgoingMessageHandler updateCallback,
            string target, string objectName,
            string messageName, YAMISerializable content)
        {
            return doSend(updateCallback, target, objectName, messageName, content,
                0, true, false);
        }

        /// 
        /// <summary>
        /// Sends the new outgoing message to the given destination.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The content object is serialized and is no longer referenced
        /// after this function returns. If the content is null, an empty
        /// <see cref="Parameters"/> object is sent.
        /// </para>
        /// <para><b>Note:</b>
        /// This function implicitly opens a new communication channel
        /// if it is not already open. This channel is kept open until
        /// it is explicitly closed
        /// (see the <see cref="CloseConnection(string, int)"/> function)
        /// or until the whole agent is closed or the communication error
        /// is detected.
        /// </para>
        /// </remarks>
        /// <param name="updateCallback">
        /// method that will be called when the state of outgoing message
        /// changes
        /// </param>
        /// <param name="target"> the name of the target endpoint -
        /// this name should correspond to the listener name
        /// in some target agent object </param>
        /// <param name="objectName"> the name of the logical destination 
        /// object in the target agent </param>
        /// <param name="messageName"> the name of the message </param>
        /// <param name="content"> the content of the message </param>
        /// <param name="priority"> the priority of the message </param>
        /// <returns> the handler to the outgoing message object </returns>
        /// <exception cref="YAMIIOException"> if the channel cannot be 
        /// established </exception>
        public OutgoingMessage Send(OutgoingMessageHandler updateCallback,
            string target, string objectName,
            string messageName, YAMISerializable content, int priority)
        {
            return doSend(updateCallback, target, objectName, messageName, content,
                priority, true, false);
        }

        /// <summary> 
        /// Sends the new outgoing message to the given destination.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The content object is serialized and is no longer referenced
        /// after this function returns. If the content is null, an empty
        /// <see cref="Parameters"/> object is sent.
        /// </para>
        /// <para><b>Note:</b>
        /// This function implicitly opens a new communication channel
        /// if it is not already open. This channel is kept open until
        /// it is explicitly closed
        /// (see the <see cref="CloseConnection(string, int)"/> function)
        /// or until the whole agent is closed or the communication error
        /// is detected.
        /// </para>
        /// </remarks>
        /// <param name="updateCallback">
        /// method that will be called when the state of outgoing message 
        /// changes
        /// </param>
        /// <param name="target"> the name of the target endpoint -
        /// this name should correspond to the listener name
        /// in some target agent object </param>
        /// <param name="objectName"> the name of the logical destination 
        /// object in the target agent </param>
        /// <param name="messageName"> the name of the message </param>
        /// <param name="content"> the content of the message </param>
        /// <param name="priority"> the priority of the message </param>
        /// <param name="autoConnect"> the flag controlling automatic 
        /// (re)connection </param>
        /// <returns> the handler to the outgoing message object </returns>
        /// <exception cref="YAMIIOException"> if the channel cannot be 
        /// established </exception>
        public OutgoingMessage Send(OutgoingMessageHandler updateCallback,
            string target, string objectName,
            string messageName, YAMISerializable content,
            int priority, bool autoConnect)
        {
            return doSend(updateCallback, target, objectName, messageName, content,
                priority, autoConnect, false);
        }

        /// <summary> Sends the new outgoing message to the given 
        /// destination, without the possibility to track its progress.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The content object is serialized and is no longer referenced
        /// after this function returns. If the content is null, an empty
        /// <see cref="Parameters"/> object is sent.
        /// </para>
        /// <para><b>Note:</b>
        /// This function implicitly opens a new communication channel
        /// if it is not already open. This channel is kept open until
        /// it is explicitly closed
        /// (see the <see cref="CloseConnection(string, int)"/> function)
        /// or until the whole agent is closed or the communication error
        /// is detected.
        /// </para>
        /// </remarks>
        /// <param name="target"> the name of the target endpoint -
        /// this name should correspond to the listener name
        /// in some target agent object </param>
        /// <param name="objectName"> the name of the logical destination 
        /// object in the target agent </param>
        /// <param name="messageName"> the name of the message </param>
        /// <param name="content"> the content of the message </param>
        /// <exception cref="YAMIIOException"> if the channel cannot be 
        /// established </exception>
        public void SendOneWay(string target, string objectName, 
            string messageName, YAMISerializable content)
        {
            doSend(null, target, objectName, messageName, content,
                0, true, true);
        }

        /// <summary> Sends the new outgoing message to the given 
        /// destination, without the possibility to track its progress.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The content object is serialized and is no longer referenced
        /// after this function returns. If the content is null, an empty
        /// <see cref="Parameters"/> object is sent.
        /// </para>
        /// <para><b>Note:</b>
        /// This function implicitly opens a new communication channel
        /// if it is not already open. This channel is kept open until
        /// it is explicitly closed
        /// (see the <see cref="CloseConnection(string, int)"/> function)
        /// or until the whole agent is closed or the communication error
        /// is detected.
        /// </para>
        /// </remarks>
        /// <param name="target"> the name of the target endpoint -
        /// this name should correspond to the listener name
        /// in some target agent object </param>
        /// <param name="objectName"> the name of the logical destination 
        /// object in the target agent </param>
        /// <param name="messageName"> the name of the message </param>
        /// <param name="content"> the content of the message </param>
        /// <param name="priority"> the priority of the message </param>
        /// <exception cref="YAMIIOException"> if the channel cannot be 
        /// established </exception>
        public void SendOneWay(string target, string objectName, 
            string messageName, YAMISerializable content, int priority)
        {
            // the reply is not expected for this message
            // - the message is just posted
            // to the outgoing queue and forgotten, except that the progress
            // callback is installed that can properly manage the output
            // flow control semaphore

            doSend(null, target, objectName, messageName, content, priority,
                true, true);
        }

        /// <summary> Sends the new outgoing message to the given 
        /// destination, without the possibility to track its progress.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The content object is serialized and is no longer referenced
        /// after this function returns. If the content is null, an empty
        /// <see cref="Parameters"/> object is sent.
        /// </para>
        /// <para><b>Note:</b>
        /// This function implicitly opens a new communication channel
        /// if it is not already open. This channel is kept open until
        /// it is explicitly closed
        /// (see the <see cref="CloseConnection(string, int)"/> function)
        /// or until the whole agent is closed or the communication error
        /// is detected.
        /// </para>
        /// </remarks>
        /// <param name="target"> the name of the target endpoint -
        /// this name should correspond to the listener name
        /// in some target agent object </param>
        /// <param name="objectName"> the name of the logical destination 
        /// object in the target agent </param>
        /// <param name="messageName"> the name of the message </param>
        /// <param name="content"> the content of the message </param>
        /// <param name="priority"> the priority of the message </param>
        /// <param name="autoConnect"> the flag controlling automatic 
        /// (re)connection </param>
        /// <exception cref="YAMIIOException"> if the channel cannot be 
        /// established </exception>
        public void SendOneWay(string target, string objectName, 
            string messageName, YAMISerializable content, int priority,
            bool autoConnect)
        {
            // the reply is not expected for this message
            // - the message is just posted
            // to the outgoing queue and forgotten, except that the progress
            // callback is installed that can properly manage the output
            // flow control semaphore

            doSend(null, target, objectName, messageName, content, priority,
                autoConnect, true);
        }

        /// <summary> Closes the given connection with default (lowest) 
        /// priority. </summary>
        /// <remarks>See <see cref="CloseConnection(string, int)"/>
        /// for details.</remarks>
        /// <param name="target"> the name of connection to close </param>
        public void CloseConnection(string target)
        {
            CloseConnection(target, 0);
        }

        /// <summary> Closes the given connection.</summary>
        /// <remarks>
        /// <para>
        /// The priority allows to properly handle the existing outgoing
        /// messages that are waiting in the outgoing queue for transmission.
        /// </para>
        /// <para>
        /// The existing messages with lower priority are
        /// abandoned, whereas the existing messages with priority equal
        /// or higher to the one provided as parameter are retained in the
        /// outgoing queue and are properly pushed for transmission
        /// before the channel is physically closed.
        /// </para>
        /// <para>
        /// The channel is closed immediately only if there are no
        /// messages waiting in its outgoing queue.
        /// </para>
        /// <para>
        /// If the failover group is given then all targets from the group
        /// are closed.
        /// </para>
        /// <para>
        /// Closing the channel that does not exist is a no-op.
        /// </para>
        /// </remarks>
        /// <param name="target"> the name of connection to close </param>
        /// <param name="priority"> proprity of the request, respects 
        /// existing messages in the outgoing queue </param>
        public void CloseConnection(string target, int priority)
        {
            if(isTargetFailover(target))
            {
                // close all targets from the failover group
                doCloseConnections(splitFailoverTargets(target), priority);
            }
            else
            {
                // this is a single target
                IList<string> targets = new List<string>();
                targets.Add(target);
                doCloseConnections(targets, priority);
            }
        }

        /// <summary> 
        /// Helper class for returning the state of outgoing flow. 
        /// </summary>
        public class OutgoingFlowState
        {
            internal int currentLevel;

            /// <summary>
            /// Gets number of currently queued outgoing messages waiting 
            /// for send.
            /// </summary>
            public int CurrentLevel
            {
                get
                {
                    return currentLevel;
                }
            }

            internal int highWaterMark;
            /// <summary>
            /// Gets upper limit of queued outgoing messages. 
            /// </summary>
            /// <remarks>If this limit is reached, further attempts to 
            /// send the message are suppressed (that is, the user code is 
            /// blocked) until the queues are consumed to the level defined 
            /// by <see cref="LowWaterMark"/>.</remarks>
            public int HighWaterMark
            {
                get
                {
                    return highWaterMark;
                }
            }

            internal int lowWaterMark;
            /// <summary>
            /// Gets the "low water mark" limit.
            /// </summary>
            /// <remarks><see cref="CurrentLevel"/> should be
            /// lower than <c>LowWaterMark</c> to resume user code
            /// execution after reaching <see cref="HighWaterMark"/>
            /// limit.</remarks>
            public int LowWaterMark
            {
                get
                {
                    return lowWaterMark;
                }
            }
        }

        /// <summary> Obtains the state of overall outgoing flow.</summary>
        /// <returns>Current outgoing flow state.</returns>
        /// <remarks>
        /// The outgoing flow is a combination of all outgoing traffic,
        /// and is not tied to any particular communication channel. 
        /// </remarks>
        public OutgoingFlowState GetOutgoingFlowState()
        {
            return outgoingFlowManager.getFlowState();
        }

        /// <summary> Cleans up all resources and stops internal threads.
        /// </summary>
        /// <remarks>After this function is called the agent should
        /// not be used any longer.</remarks>
        public void Close()
        {
            // stop the I/O worker thread
            ioWorker.requestStop();
            try
            {
                ioWorkerThread.Join();
            }
            catch(ThreadInterruptedException)
            {
                // ignore
            }

            // stop the dispatcher threads
            dispatchManager.close();

            // physically close all existing listeners
            foreach(Listener lst in listeners.Values)
            {
                lst.close();
            }

            // physically close all existing channels
            foreach(KeyValuePair<string, Channel> e in channels)
            {
                Channel ch = e.Value;
                ch.close();
                reportClosedChannel(e.Key);
            }

            log(LogEventArgs.LogLevel.LOW, "Agent closed.");
        }

        private static class Util<T>
        {
            public static void shuffle(IList<T> list)
            {
                System.Random rand = new System.Random();
                for(int i = list.Count - 1; i > 0; --i)
                {
                    int toSwap = rand.Next(0, i);
                    if(toSwap != i)
                    {
                        T temp = list[i];
                        list[i] = list[toSwap];
                        list[toSwap] = temp;
                    }
                }
            }
        }

        private OutgoingMessage doSend(
            OutgoingMessageHandler updateCallback,
            string target, string objectName, string messageName, 
            YAMISerializable content, int priority, 
            bool autoConnect, bool oneWay)
        {
            bool waitForTransmission = false;
            bool waitForReplyOrReject = false;

            OutgoingMessage message = null;
            YAMIIOException lastException = null;

            if(isTargetFailover(target))
            {
                // the given target represents a group of separate 
                // destinations that should be tried in random order 
                // until success

                IList<string> targets = splitFailoverTargets(target);
                if(targets.Count == 0)
                {
                    throw new UnexpectedValueException(
                        "Empty failover group is not allowed.");
                }

                Util<string>.shuffle(targets);

                if(oneWay)
                {
                    // with failover the one-way messages are implicitly
                    // forced to wait for transmission

                    waitForTransmission = true;

                }
                else
                {
                    // with failover the two-way messages are implicitly
                    // forced to wait for completion

                    waitForReplyOrReject = true;
                }

                // try failover targets one by one until success
                foreach(string t in targets)
                {
                    try
                    {
                        message = doSendToSingleTarget(
                            updateCallback, t, objectName, messageName, 
                            content, priority, autoConnect, oneWay, 
                            waitForTransmission, waitForReplyOrReject);

                        // in the absence of exceptions
                        // consider this message to be successfully sent

                        lastException = null;
                        break;

                    }
                    catch(YAMIIOException ex)
                    {
                        lastException = ex;
                    }
                }

            }
            else
            {
                // this is a single-target message
                // do not force any waiting

                message = doSendToSingleTarget(
                    updateCallback, target, objectName, messageName, 
                    content, priority, autoConnect, oneWay, 
                    waitForTransmission, waitForReplyOrReject);
            }

            if(lastException != null)
            {
                throw lastException;
            }

            return message;
        }

        private OutgoingMessage doSendToSingleTarget(
            OutgoingMessageHandler updateCallback,
            string target, string objectName, string messageName, 
            YAMISerializable content, int priority, 
            bool autoConnect, bool oneWay, 
            bool waitForTransmission, bool waitForReplyOrReject)
        {
            YAMISerializable contentToUse;
            if(content != null)
            {
                contentToUse = content;
            }
            else
            {
                // use empty object
                contentToUse = emptyParameters;
            }

            // flow control
            outgoingFlowManager.waitForPermission();

            // prepare the message header

            long messageId = getNextId();

            Parameters header = new Parameters();
            header.SetString("type", "message");
            header.SetLong("message_id", messageId);
            header.SetString("object_name", objectName);
            header.SetString("message_name", messageName);

            // serialize everything together and post to the outgoing
            // queue of the channel that is relevant for the given target

            int preferredFrameSize = 
                NetworkUtils.getPreferredFrameSize(options, target);

            List<byte[]> allBuffers = new List<byte[]>();
            int messageHeaderSize = serialize(
                header, contentToUse, preferredFrameSize, allBuffers);

            OutgoingMessage message = null;
            MessageProgressCallback messageProgressCallback;

            if(oneWay == false 
                || waitForTransmission || waitForReplyOrReject)
            {

                // the OutgoingMessage needs to be created, because this is
                // either the two-way message or a failover message
                // with forced wait on transmission or completion

                message = new OutgoingMessage(messageId, updateCallback, 
                    outgoingMessageManager, outgoingFlowManager);

                messageProgressCallback = message.ProgressCallback;
                outgoingMessageManager.registerNewMessage(
                    messageId, messageProgressCallback);

            }
            else
            {
                // this is purely one-way message with no need
                // for any (explicit or implicit) progress feedback

                messageProgressCallback = oneWayMessageProgressCallback;
            }

            Channel ch = makeSureChannelExists(target, autoConnect);

            // flow control
            // to avoid hazards increase before posting
            outgoingFlowManager.increase();

            int transportId = (int)messageId;

            ch.post(transportId, priority, allBuffers, messageHeaderSize, 
                messageProgressCallback);

            ioWorker.wakeup();

            if(waitForTransmission)
            {
                message.WaitForTransmission();
            }
            if(waitForReplyOrReject)
            {
                if(options.default_failover_timeout == 0)
                {
                    message.WaitForCompletion();
                }
                else
                {
                    message.WaitForCompletion(options.default_failover_timeout);
                }

                if(message.State == OutgoingMessage.MessageState.ABANDONED)
                {
                    throw new 
                        YAMIIOException("The message has been abandoned.");
                }
            }

            if(oneWay)
            {
                if(message != null)
                {
                    // the OutgoingMessage object has been 
                    // artificially created
                    // it is of no interest to the user,
                    // so needs to be cleaned up

                    message.Close();
                    message = null;
                }
            }

            log(LogEventArgs.LogLevel.MEDIUM, 
                "Message posted:" 
                + " target: " + target 
                + " object: " + objectName 
                + " name: " + messageName 
                + " priority: " + priority 
                + " mid: " + messageId 
                + " tid: " + (int)messageId);

            return message;
        }

        internal void doSendReply(
            string target, long messageId, YAMISerializable content, 
            int priority)
        {
            YAMISerializable contentToUse;
            if(content != null)
            {
                contentToUse = content;
            }
            else
            {
                // use empty object
                contentToUse = emptyParameters;
            }

            // there is no flow control for replies

            // prepare the message header

            Parameters header = new Parameters();
            header.SetString("type", "reply");
            header.SetLong("message_id", messageId);

            int transportId = doSendReplyOrReject(
                target, header, contentToUse, priority);

            ioWorker.wakeup();

            log(LogEventArgs.LogLevel.MEDIUM,
                "Reply posted:" 
                + " target: " + target 
                + " priority: " + priority 
                + " mid: " + messageId 
                + " tid: " + transportId);
        }

        internal void doSendReject(
            string target, long messageId, string reason, int priority)
        {
            // there is no flow control for rejections

            // prepare the message header

            Parameters header = new Parameters();
            header.SetString("type", "exception");
            header.SetLong("message_id", messageId);
            header.SetString("reason", reason);

            int transportId = doSendReplyOrReject(
                target, header, emptyParameters, priority);

            log(LogEventArgs.LogLevel.MEDIUM,
                "Reject posted:" 
                + " target: " + target 
                + " priority: " + priority 
                + " mid: " + messageId 
                + " tid: " + transportId);
        }

        private int doSendReplyOrReject(
            string target, Parameters header, YAMISerializable content, 
            int priority)
        {
            // serialize everything together and post to the outgoing
            // queue of the relevant channel

            int preferredFrameSize = 
                NetworkUtils.getPreferredFrameSize(options, target);

            List<byte[]> allBuffers = new List<byte[]>();
            int messageHeaderSize = 
                serialize(header, content, preferredFrameSize, allBuffers);

            Channel ch = makeSureChannelExists(target, false);

            int transportId = (int)getNextId();
            ch.post(
                transportId, priority, allBuffers, messageHeaderSize, null);

            ioWorker.wakeup();

            return transportId;
        }

        private int serialize(Parameters header, YAMISerializable content, 
            int preferredFrameSize, List<byte[]> allBuffers)
        {
            int frameHeaderSize = Frame.FRAME_HEADER_SIZE;
            int chunkSize = preferredFrameSize - frameHeaderSize;

            IList<byte[]> headerBuffers = header.Serialize(chunkSize);
            IList<byte[]> contentBuffers = content.Serialize(chunkSize);

            int messageHeaderSize = Channel.groupBuffers(
                allBuffers, headerBuffers, contentBuffers, chunkSize);

            return messageHeaderSize;
        }

        private void reportChannelEvent(string target, 
            ConnectionEventArgs.ConnectionEvent eventType)
        {
            ConnectionEventHandler temp = ConnectionsChanged;
            if(temp != null)
            {
                try
                {
                    temp(this, new ConnectionEventArgs(target, eventType));
                }
                catch(System.Exception)
                {
                    // ignore exceptions from user code
                }
            }
        }

        private void reportClosedChannel(string target)
        {
            reportChannelEvent(target, 
                ConnectionEventArgs.ConnectionEvent.CONNECTION_CLOSED);
        }

        private Channel makeSureChannelExists(
            string target, bool autoConnect)
        {
            // note:
            // This function can be called by multiple threads.
            // In order to allow for higher concurrency it can lead to
            // multiple connections being created for the same target,
            // although only one such connection is ultimately kept open.

            string exceptionMsg = "I/O Error";
            System.Exception innerEx = null;

            Channel ch = null;
            lock(channels)
            {
                if(channels.ContainsKey(target))
                    ch = channels[target];
            }

            if(ch == null && autoConnect)
            {
                // smart retry when connection fails is implemented
                // by bounded retry with randomized sleep between retries to
                // spread out initialization bursts in bigger systems

                int connectionRetries = options.connectionRetries;
                int connectionRetryDelaySpread = 
                    options.connectionRetryDelaySpread;

                for(int i = 0; i != connectionRetries; ++i)
                {
                    try
                    {
                        ch = new Channel(target, options, 
                            incomingMessageDispatchCallback,
                            ioWorker,
                            (LogCallback)this, logLevel);

                        reportChannelEvent(target, 
                            ConnectionEventArgs.ConnectionEvent
                            .NEW_OUTGOING_CONNECTION);

                        Channel existing = null;
                        lock(channels)
                        {
                            if(channels.ContainsKey(target))
                                existing = channels[target];
                            else
                                channels.Add(target, ch);
                        }

                        if(existing != null)
                        {
                            ch.close();
                            ch = existing;

                            reportClosedChannel(target);
                        }

                        break;
                    }
                    catch(System.Exception ex)
                    {
                        innerEx = ex;
                    }

                    int pause = 
                        (int)(new System.Random().NextDouble() 
                        * connectionRetryDelaySpread);

                    try
                    {
                        System.Threading.Thread.Sleep(pause);
                    }
                    catch(ThreadInterruptedException)
                    {
                        // ignore
                    }
                }
            }

            if(ch == null)
            {
                if(innerEx == null)
                    throw new YAMIIOException(exceptionMsg);
                else
                    throw new YAMIIOException(exceptionMsg, innerEx);
            }

            return ch;
        }

        private void doCloseConnections(IList<string> targets, int priority)
        {
            lock(channels)
            {
                foreach(string t in targets)
                {
                    Channel ch = channels[t];
                    if(ch != null)
                    {
                        bool closeMeImmediately = ch.postClose(priority);
                        if(closeMeImmediately)
                        {
                            ch.close();
                            channels.Remove(t);
                            reportClosedChannel(t);
                        }

                        // if the channel is not closed immediately,
                        // it will be kept alive until all messages before
                        // the close reuest are transmitted
                        // after that the close event is discovered by the 
                        // worker and the channel closing and removal 
                        // is performed by the worker thread
                    }
                }
            }
        }

        private bool isTargetFailover(string target)
        {
            return target.StartsWith(failoverPrefix) 
                && target.EndsWith(failoverSuffix);
        }

        private IList<string> splitFailoverTargets(string failoverTarget)
        {
            string failoverContent = failoverTarget.Substring(
                failoverPrefix.Length, 
                failoverTarget.Length 
                - (failoverSuffix.Length + failoverPrefix.Length)
            );

            string[] targets = failoverContent.Split(
                new string[] { failoverSeparator }, 
                System.StringSplitOptions.RemoveEmptyEntries
            );

            return new List<string>(targets);
        }

        private long getNextId()
        {
            return Interlocked.Increment(ref messageIdGenerator);
        }

        #region LogCallback Members

        void LogCallback.Log(LogEventArgs.LogLevel logLevel, string message)
        {
            LogHandler temp = Log;
            if(temp != null && logLevel <= this.logLevel)
                temp(this, new LogEventArgs(logLevel, message));
        }

        #endregion

        private void log(LogEventArgs.LogLevel logLevel, string message)
        {
            ((LogCallback)this).Log(logLevel, message);
        }

        #region ConnectionCallback Members

        void ConnectionEventCallback.Report(
            string name, ConnectionEventArgs.ConnectionEvent eventType)
        {
            ConnectionEventHandler temp = ConnectionsChanged;
            if(temp != null)
            {
                temp(this, new ConnectionEventArgs(name, eventType));
            }
        }

        #endregion
    }
}	// end namespace
