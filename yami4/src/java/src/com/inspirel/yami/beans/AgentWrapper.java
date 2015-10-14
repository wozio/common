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

package com.inspirel.yami.beans;

import com.inspirel.yami.Agent;
import com.inspirel.yami.IncomingMessageCallback;
import com.inspirel.yami.OptionNames;
import com.inspirel.yami.Parameters;
import com.inspirel.yami.YAMIIOException;

/**
 * Bean wrapper for the YAMI agent.
 *
 * The wrapper allows the basic agent management
 * within the container-like environment.
 * The Spring framework is a particular target for this wrapper.
 */
public class AgentWrapper {

    private Agent agent;
    private final Parameters options;
    private String listener;
    private IncomingMessageCallback callback;
    private String objectName;

    /**
     * Default constructor.
     */
    public AgentWrapper() {
        this.options = new Parameters();
    }

    /**
     * The property equivalent to the tcp_listen_backlog option.
     */
    public void setTcpListenBacklog(int backlog) {
        options.setInteger(OptionNames.TCP_LISTEN_BACKLOG, backlog);
    }

    /**
     * The property equivalent to the tcp_reuseaddr option.
     */
    public void setTcpReuseAddr(boolean reuseAddr) {
        options.setBoolean(OptionNames.TCP_REUSEADDR, reuseAddr);
    }

    /**
     * The property equivalent to the tcp_nodelay option.
     */
    public void setTcpNoDelay(boolean noDelay) {
        options.setBoolean(OptionNames.TCP_NODELAY, noDelay);
    }

    /**
     * The property equivalent to the tcp_frame_size option.
     */
    public void setTcpFrameSize(int frameSize) {
        options.setInteger(OptionNames.TCP_FRAME_SIZE, frameSize);
    }

    /**
     * The property equivalent to the udp_frame_size option.
     */
    public void setUdpFrameSize(int frameSize) {
        options.setInteger(OptionNames.UDP_FRAME_SIZE, frameSize);
    }

    /**
     * The property equivalent to the dispatcher_threads option.
     */
    public void setDispatcherThreads(int dispatcherThreads) {
        options.setInteger(OptionNames.DISPATCHER_THREADS,
                dispatcherThreads);
    }

    /**
     * The property equivalent to the connection_retries option.
     */
    public void setConnectionRetries(int connectionRetries) {
        options.setInteger(OptionNames.CONNECTION_RETRIES,
                connectionRetries);
    }

    /**
     * The property equivalent to the connection_retry_delay_spread option.
     */
    public void setConnectionRetryDelaySpread(
            int connectionRetryDelaySpread) {
        options.setInteger(OptionNames.CONNECTION_RETRY_DELAY_SPREAD,
                connectionRetryDelaySpread);
    }

    /**
     * The property equivalent to the outgoing_high_water_mark option.
     */
    public void setOutgoingHighWaterMark(int outgoingHighWaterMark) {
        options.setInteger(OptionNames.OUTGOING_HIGH_WATER_MARK,
                outgoingHighWaterMark);
    }

    /**
     * The property equivalent to the outgoing_low_water_mark option.
     */
    public void setOutgoingLowWaterMark(int outgoingLowWaterMark) {
        options.setInteger(OptionNames.OUTGOING_LOW_WATER_MARK,
                outgoingLowWaterMark);
    }

    /**
     * The property equivalent to the incoming_high_water_mark option.
     */
    public void setIncomingHighWaterMark(int incomingHighWaterMark) {
        options.setInteger(OptionNames.INCOMING_HIGH_WATER_MARK,
                incomingHighWaterMark);
    }

    /**
     * The property equivalent to the incoming_low_water_mark option.
     */
    public void setIncomingLowWaterMark(int incomingLowWaterMark) {
        options.setInteger(OptionNames.INCOMING_LOW_WATER_MARK,
                incomingLowWaterMark);
    }

    /**
     * The property representing the listener address.
     */
    public void setListener(String listener) {
        this.listener = listener;
    }

    /**
     * Convenience property for defining single service.
     */
    public void setObject(IncomingMessageCallback callback) {
        this.callback = callback;
    }

    /**
     * Convenience property for defining single service name.
     */
    public void setObjectName(String name) {
        this.objectName = name;
    }

    /**
     * Initialization - creates and starts the agent.
     */
    public void init() {
        try {
            agent = new Agent(options);

            if (listener != null) {
                agent.addListener(listener);
            }

            // register single object, if defined
            if (objectName != null && callback != null) {
                agent.registerObject(objectName, callback);
            }
            
        } catch (YAMIIOException e) {
            throw new BeanInstantiationException(
                    "Cannot instantiate the agent bean: " +
                    e.getMessage());
        }
    }

    /**
     * Closes the agent object and cleans its internal resources.
     */
    public void close() {
        if (agent != null) {
            agent.close();
            agent = null;
        }
    }

    /**
     * Retrieves the delegated agent object.
     */
    public Agent getAgent() {
        return agent;
    }
}
