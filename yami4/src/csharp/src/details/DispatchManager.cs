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
using System.Threading;

namespace Inspirel.YAMI.details
{
    internal sealed class DispatchManager
    {

        private readonly IList<Thread> dispatchers;

        private readonly WaterFlowManager incomingFlowManager;

        private readonly LinkedList<IncomingMessage> messageQueue;

        private readonly 
            IDictionary<string, IncomingMessageHandler> objectMap;
        private IncomingMessageHandler anyObjectCallback;

        private readonly object sender;

        private readonly IOWorker ioWorker;

        private readonly LogCallback logCallback;
        private readonly LogEventArgs.LogLevel logLevel;

        private class Dispatcher
        {
            private DispatchManager outer;

            public Dispatcher(DispatchManager outer)
            {
                this.outer = outer;
            }

            private void call(IncomingMessageHandler callback, 
                IncomingMessage message, string objectName)
            {
                callback(outer.sender, 
                    new IncomingMessageArgs(objectName, message));

                outer.logCallback.Log(LogEventArgs.LogLevel.MEDIUM,
                    "Dispatched:" + " object: " + message.ObjectName 
                    + " message: " + message.MessageName);
            }

            public void run()
            {
                bool finished = false;
                while (finished == false)
                {
                    IncomingMessage message = outer.takeNext();
                    if (message != null)
                    {
                        string objectName = message.ObjectName;

                        IncomingMessageHandler callback = 
                            outer.anyObjectCallback;
                        lock (outer.objectMap)
                        {
                            if(outer.objectMap.ContainsKey(objectName))
                                callback = outer.objectMap[objectName];
                        }

                        if (callback != null)
                        {
                            try
                            {
                                call(callback, message, objectName);
                            }
                            catch (Exception ex)
                            {
                            // all exceptions in the user code
                            // should be treated as rejections

                                try
                                {
                                    message.Reject(ex.ToString());
                                }
                                catch (Exception)
                                {
                                // ignore all errors here
                                }
                            }
                        }
                        else
                        {
                        // the message was sent to the unknown object
                        // attempt to send back the rejection

                            try
                            {
                                message.Reject(
                                    "Unknown destination object.");
                            }
                            catch (Exception)
                            {
                            // ignore all errors here
                            }
                        }

                        bool changeToAllowFlow = 
                            outer.incomingFlowManager.decrease();

                        if (changeToAllowFlow)
                        {
                        // if the incoming flow allow flag changes from
                        // false to true it is necessary to wake up
                        // the worker thread, which might be in the
                        // (otherwise) permanent sleep
                        // this can happen if the worker thread was
                        // informed in its last cycle that input is not
                        // allowed - now it is allowed and the selector
                        // should be reconfigured to take this into account

                            outer.ioWorker.wakeup();
                        }

                    }
                    else
                    {
                    // this is a poison pill, finish the thread
                        finished = true;
                    }
                }
            }
        }

        public DispatchManager(
            object sender, // Agent reference for callbacks
            Options options, WaterFlowManager incomingFlowManager, 
            IOWorker ioWorker, 
            LogCallback logCallback, LogEventArgs.LogLevel logLevel)
        {
            this.sender = sender;
            this.incomingFlowManager = incomingFlowManager;
            this.ioWorker = ioWorker;

            this.logCallback = logCallback;
            this.logLevel = logLevel;

            messageQueue = new LinkedList<IncomingMessage>();

            objectMap = new Dictionary<string, IncomingMessageHandler>();
            anyObjectCallback = null;

            int numOfThreads = options.dispatcherThreads;

            dispatchers = new List<Thread>();
            for (int i = 0; i != numOfThreads; ++i)
            {

                Thread th = new Thread((new Dispatcher(this)).run);
                th.Name = "YAMI4 message dispatcher";
                th.IsBackground = true;
                th.Start();
                dispatchers.Add(th);
            }
        }

        public void registerObject(string objectName, 
            IncomingMessageHandler callback)
        {
            lock (objectMap)
            {
                if (objectName.Equals("*"))
                {
                    anyObjectCallback = callback;
                }
                else
                {
                    if(objectMap.ContainsKey(objectName))
                        objectMap[objectName] = callback;
                    else
                        objectMap.Add(objectName, callback);
                }
            }
        }

        public void unregisterObject(string objectName)
        {
            lock (objectMap)
            {
                if (objectName.Equals("*"))
                {
                    anyObjectCallback = null;
                }
                else
                {
                    objectMap.Remove(objectName);
                }
            }
        }

        public void push(IncomingMessage message)
        {
            lock (messageQueue)
            {
                messageQueue.AddLast(message);
                Monitor.Pulse(messageQueue);
            }
        }

        public void close()
        {

            lock (messageQueue)
            {
                messageQueue.Clear();

            // inject poison pills - one for each dispatcher thread
                for (int i = 0; i != dispatchers.Count; ++i)
                {
                    messageQueue.AddLast((IncomingMessage)null);
                }

                Monitor.PulseAll(messageQueue);
            }

        // wait for all dispatchers to finish
            foreach (Thread th in dispatchers)
            {
                try
                {
                    th.Join();
                }
                catch (ThreadInterruptedException)
                {
                // ignore, will never happen
                }
            }
        }

        internal IncomingMessage takeNext()
        {
            lock (messageQueue)
            {
                while (messageQueue.Count == 0)
                {
                    try
                    {
                        Monitor.Wait(messageQueue);
                    }
                    catch (ThreadInterruptedException)
                    {
                    // ignore, will never happen
                    }
                }

                IncomingMessage rv = messageQueue.First.Value;
                messageQueue.RemoveFirst();
                return rv;
            }
        }
    }

}