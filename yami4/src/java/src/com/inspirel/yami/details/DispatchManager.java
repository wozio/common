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

import com.inspirel.yami.IncomingMessageCallback;
import com.inspirel.yami.IncomingMessage;
import com.inspirel.yami.LogCallback;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.LinkedList;
import java.util.Map;

public final class DispatchManager {
    
    private final List<Thread> dispatchers;
    
    private final WaterFlowManager incomingFlowManager;
    
    private final LinkedList<IncomingMessage> messageQueue;
    
    private final Map<String, IncomingMessageCallback> objectMap;
    private IncomingMessageCallback anyObjectCallback;
    
    private final IOWorker ioWorker;
    
    private final LogCallback logCallback;
    private final LogCallback.LogLevel logLevel;

    private class Dispatcher implements Runnable {

        private void call(IncomingMessageCallback callback,
                IncomingMessage message) throws Exception {

            callback.call(message);

            if (logCallback != null) {
                if (logLevel == LogCallback.LogLevel.MEDIUM ||
                        logLevel == LogCallback.LogLevel.HIGH) {

                    logCallback.log(LogCallback.LogLevel.MEDIUM,
                            "Dispatched:" +
                            " object: " + message.getObjectName() +
                            " message: " + message.getMessageName());
                }
            }
        }
        
        @Override
        public void run() {
            boolean finished = false;
            while (finished == false) {
                IncomingMessage message = takeNext();
                if (message != null) {
                    
                    String objectName = message.getObjectName();
                    
                    IncomingMessageCallback callback;
                    synchronized (objectMap) {
                        callback = objectMap.get(objectName);
                        if (callback == null) {
                            callback = anyObjectCallback;
                        }
                    }
                        
                    if (callback != null) {
                        try {
                            call(callback, message);
                        } catch (Exception ex) {
                            // all exceptions in the user code
                            // should be treated as rejections
                                
                            try {
                                message.reject(ex.toString());
                            } catch (Exception e) {
                                // ignore all errors here
                            }
                        }
                    } else {
                        // the message was sent to the unknown object
                        // attempt to send back the rejection
                        
                        try {
                            message.reject("Unknown destination object.");
                        } catch (Exception e) {
                            // ignore all errors here
                        }
                    }
                        
                    boolean changeToAllowFlow =
                            incomingFlowManager.decrease();
                    
                    if (changeToAllowFlow) {
                        // if the incoming flow allow flag changes from
                        // false to true it is necessary to wake up
                        // the worker thread, which might be in the
                        // (otherwise) permanent sleep
                        // this can happen if the worker thread was
                        // informed in its last cycle that input is not
                        // allowed - now it is allowed and the selector
                        // should be reconfigured to take this into account
                        
                        ioWorker.wakeup();
                    }
                                    
                } else {
                    // this is a poison pill, finish the thread
                    finished = true;                        
                }
            }
        }
    }
    
    public DispatchManager(Options options,
            WaterFlowManager incomingFlowManager,
            IOWorker ioWorker,
            LogCallback logCallback, LogCallback.LogLevel logLevel) {
        
        this.incomingFlowManager = incomingFlowManager;
        this.ioWorker = ioWorker;
        
        this.logCallback = logCallback;
        this.logLevel = logLevel;
        
        messageQueue = new LinkedList<IncomingMessage>();
        
        objectMap = new HashMap<String, IncomingMessageCallback>();
        anyObjectCallback = null;
        
        int numOfThreads = options.dispatcherThreads;
        
        dispatchers = new ArrayList<Thread>();
        for (int i = 0; i != numOfThreads; ++i) {
            
            Thread th = new Thread(
                    new Dispatcher(), "YAMI4 message dispatcher");
            th.setDaemon(true);
            th.start();
            dispatchers.add(th);
        }
    }

    public void registerObject(String objectName,
            IncomingMessageCallback callback) {
        synchronized (objectMap) {
            if (objectName.equals("*")) {
                anyObjectCallback = callback;
            } else {
                objectMap.put(objectName, callback);
            }
        }
    }
    
    public void unregisterObject(String objectName) {
        synchronized (objectMap) {
            if (objectName.equals("*")) {
                anyObjectCallback = null;
            } else {
                objectMap.remove(objectName);
            }
        }
    }
    
    public void push(IncomingMessage message) {
        synchronized (messageQueue) {
            messageQueue.add(message);
            messageQueue.notify();
        }
    }
    
    public void close() {
        
        synchronized (messageQueue) {
            messageQueue.clear();
        
            // inject poison pills - one for each dispatcher thread
            for (int i = 0; i != dispatchers.size(); ++i) {
                messageQueue.add(null);
            }
            
            messageQueue.notifyAll();
        }
        
        // wait for all dispatchers to finish
        for (Thread th : dispatchers) {
            try {
                th.join();
            } catch (InterruptedException ex) {
                // ignore, will never happen
            }
        }
    }
    
    IncomingMessage takeNext() {
        synchronized (messageQueue) {
            while (messageQueue.isEmpty()) {
                try {
                    messageQueue.wait();
                } catch (InterruptedException ex) {
                    // ignore, will never happen
                }
            }
            return messageQueue.poll();
        }
    }
}
