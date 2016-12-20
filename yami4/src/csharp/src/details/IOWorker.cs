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
using System.Net.Sockets;
using System.Threading;

namespace Inspirel.YAMI.details
{
    internal class IOWorker
    {

        private readonly IDictionary<string, Channel> channels;
        private readonly IDictionary<string, Listener> listeners;
        private readonly WaterFlowManager incomingFlowManager;
        private readonly Options options;
        private readonly IncomingMessageDispatchCallback 
            incomingMessageDispatchCallback;
        private readonly ConnectionEventCallback connectionEventCallback;
        private readonly LogCallback logCallback;
        private readonly LogEventArgs.LogLevel logLevel;

        private readonly IDictionary<Socket, Listener> listenersForSelection;

        private readonly IDictionary<Socket, Channel> channelsForSelection;

        private readonly IList<Channel> blockingChannelsReadyForReading;
        private readonly IList<Channel> blockingChannelsReadyForWriting;

        private bool stopRequest;

        private Selector selector;

        private bool keepWaiting = false;
        private bool useBlockingOnly = false;

        public IOWorker(
            IDictionary<string, Channel> channels, 
            IDictionary<string, Listener> listeners, 
            WaterFlowManager incomingFlowManager, 
            Options options, 
            IncomingMessageDispatchCallback incomingMessageDispatchCallback,
            ConnectionEventCallback connectionEventCallback, 
            LogCallback logCallback, LogEventArgs.LogLevel logLevel)
        {

            this.channels = channels;
            this.listeners = listeners;
            this.incomingFlowManager = incomingFlowManager;
            this.options = options;
            this.incomingMessageDispatchCallback = 
                incomingMessageDispatchCallback;
            this.connectionEventCallback = connectionEventCallback;
            this.logCallback = logCallback;
            this.logLevel = logLevel;

            listenersForSelection = new Dictionary<Socket, Listener>();
            channelsForSelection = new Dictionary<Socket, Channel>();

            blockingChannelsReadyForReading = new List<Channel>();
            blockingChannelsReadyForWriting = new List<Channel>();

            stopRequest = false;
        }

        public virtual void wakeup()
        {
            lock (this)
            {
                if (selector != null)
                {
                    selector.Wakeup();
                }

                keepWaiting = false;

                Monitor.Pulse(this);
            }
        }

        public virtual void requestStop()
        {
            lock (this)
            {
                stopRequest = true;
                if (selector != null)
                {
                    selector.Wakeup();
                }

                keepWaiting = false;

                Monitor.Pulse(this);
            }
        }

        public void UseBlockingChannelsOnly() {
        lock (this)
            {
        useBlockingOnly = true;
        keepWaiting = false;

                Monitor.Pulse(this);
        }
        }

        private void openSelector()
        {
            lock (this)
            {
                if (useBlockingOnly)
                {
                    selector = null;
                }
                else
                {
                    try
                    {
                        selector = new Selector();
                        // Selector.open();
                        if (stopRequest)
                        {
                            selector.Wakeup();
                        }
                    }
                    catch (SocketException)
                    {
                        // ignore
                    }
                }
            }
        }

        private void closeSelector()
        {
            lock (this)
            {
                if (useBlockingOnly == false)
                {
                    try
                    {
                        //foreach (Socket key in selector.keys())
                        //{
                        //    key.cancel();
                        //}
                        // selector.SelectNow();
                        selector.Close();
                        selector = null;
                    }
                    catch (SocketException)
                    {
                        // ignore, will never happen
                    }
                }
            }
        }

        private void registerListenersAndChannels()
        {
            blockingChannelsReadyForReading.Clear();
            blockingChannelsReadyForWriting.Clear();

            selector.Clear();
            listenersForSelection.Clear();
            channelsForSelection.Clear();

        // flow control:
        // the outgoing traffic is controlled at the level of
        // send/send_one_way functions - that is, even before the message
        // reaches the transport layer and so the output at the transport
        // layer is always enabled
        // the incoming traffic is controlled at the transport level,
        // depending on the length of the incoming message queue
        // which is managed by the agent and its dispatch manager

            const bool allowOutput = true;
            bool allowInput = incomingFlowManager.isAllowed();

            bool onlyBlocking;
            lock (this)
            {
            onlyBlocking = useBlockingOnly;
            }

            if (onlyBlocking == false)
            {
                lock (listeners)
                {
                    foreach (KeyValuePair<string, Listener> e in listeners)
                    {
                        Listener lst = e.Value;

                        Socket key = lst.registerForSelection(selector);
                        listenersForSelection.Add(key, lst);
                    }
                }
            }

            lock (channels)
            {
                foreach (KeyValuePair<string, Channel> e in channels)
                {
                    Channel ch = e.Value;

                    Channel.SelectionKeys keys = ch.registerForSelection(
                        selector, allowInput, allowOutput, onlyBlocking);

                    if (keys.key != null)
                    {
                        channelsForSelection.Add(keys.key, ch);
                    }
                    else if (keys.blockingChannelReadyForReading)
                    {
                        blockingChannelsReadyForReading.Add(ch);
                    }
                    else if (keys.blockingChannelReadyForWriting)
                    {
                        blockingChannelsReadyForWriting.Add(ch);
                    }
                }
            }
        }

        private void waitUntilReady()
        {
            if (blockingChannelsReadyForReading.Count != 0 || blockingChannelsReadyForWriting.Count != 0)
            {
                // there are some blocking channels that are immediately available,
                // no need to wait on selector

                return;
            }

            lock (this)
            {
                if (useBlockingOnly)
                {
                    keepWaiting = true;
                    while (keepWaiting)
                    {
                        try
                        {
                            Monitor.Wait(this);
                        }
                        catch (System.Exception)
                        {
                            // ignore
                        }
                    }

                    return;
                }
            }

            try
            {
                selector.Select();
            }
            catch (SocketException)
            {
            // ignore, will never happen
            }
        }

        private void reportChannelEvent(
            string name, ConnectionEventArgs.ConnectionEvent connectionEvent)
        {
            if (connectionEventCallback != null)
            {
                try
                {
                    connectionEventCallback.Report(name, connectionEvent);
                }
                catch (Exception)
                {
                // ignore exceptions from user code
                }
            }
        }

        private void reportNewIncomingChannel(string name)
        {
            reportChannelEvent(name, 
                ConnectionEventArgs.ConnectionEvent.NEW_INCOMING_CONNECTION);
        }

        private void useReadyListener(Listener lst)
        {
            try
            {
                Listener.ListeningResult listenResult = lst.accept();
                string target;
                if (listenResult.channel != null)
                {
                // a new channel was accepted by the listener

                    Channel acceptedChannel = listenResult.channel;
                    target = acceptedChannel.Target;

                    lock (channels)
                    {
                        channels.Add(target, acceptedChannel);
                    }

                    reportNewIncomingChannel(target);

                }
                else
                {
                // a full frame was accepted by the listener

                    target = listenResult.target;
                    System.IO.MemoryStream buffer = listenResult.buffer;
                    Channel ch;
                    lock (channels)
                    {
                        if (!channels.ContainsKey(target))
                        {
                        // no such channel, create it
                            ch = new Channel(target, options, 
                                incomingMessageDispatchCallback,
                                null, // IOWorker not needed there
                                logCallback, logLevel);
                            channels.Add(target, ch);
                        }
                        ch = channels[target];
                    }

                // appropriate channel already exists
                // -> inject the frame there

                    ch.injectFullFrame(buffer.GetBuffer());
                }
            }
            catch (Exception)
            {

            // close the listener

                lock (listeners)
                {
                    lst.close();

                    string resolvedTarget = lst.ResolvedTarget;

                    listeners.Remove(resolvedTarget);
                }
            }
        }

        private void useReadyChannel(Channel ch, bool doInput, bool doOutput)
        {
            try
            {
                ch.doSomeWork(doInput, doOutput);
            }
            catch (Exception)
            {
            // in case of error during I/O operation
            // on any channel, close it and abandon
            // all incoming and outgoing messages
            // that are in this channel's queues

                lock (channels)
                {
                    ch.close();

                    string target = ch.Target;

                    channels.Remove(target);

                    reportChannelEvent(target, 
                        ConnectionEventArgs.ConnectionEvent
                        .CONNECTION_CLOSED);
                }
            }
        }

        private void useReadyListenersAndChannels()
        {
            if (blockingChannelsReadyForReading.Count != 0)
            {
                foreach (Channel ch in blockingChannelsReadyForReading)
                {
                    useReadyChannel(ch, true, false);
                }
            }
    
            if (blockingChannelsReadyForWriting.Count != 0)
            {
                foreach (Channel ch in blockingChannelsReadyForWriting)
                {
                    useReadyChannel(ch, false, true);
                }
            }

            if (selector != null)
            {
                foreach (Socket key in selector.SelectedKeys)
                {
                    bool doAccept = selector.ReadyForAccept(key);
                    if (doAccept)
                    {
                        Listener lst = listenersForSelection[key];
                        useReadyListener(lst);
                    }

                    bool doInput = selector.ReadyForRead(key);
                    bool doOutput = selector.ReadyForWrite(key);
                    if (doInput || doOutput)
                    {
                        if (channelsForSelection.ContainsKey(key))
                        {
                            useReadyChannel(channelsForSelection[key],
                                doInput, doOutput);
                        }
                        else
                        {
                            // if the channel was not found in the channel set,
                            // it might be because it was actually a UDP channel
                            // working as a listener - in which case its operation
                            // is OP_READ, but it was added to the listener set

                            Listener lst = listenersForSelection[key];
                            useReadyListener(lst);
                        }
                    }
                }
            }
        }

        public void run()
        {
            bool finished = false;

            openSelector();

            while (finished == false)
            {
                registerListenersAndChannels();

                waitUntilReady();

                useReadyListenersAndChannels();

                lock (this)
                {
                    finished = stopRequest;
                }
            }

            closeSelector();
        }
    }

}
