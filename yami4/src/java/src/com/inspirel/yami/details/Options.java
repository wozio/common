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

import com.inspirel.yami.OptionNames;
import com.inspirel.yami.Parameters;
import com.inspirel.yami.Parameters.Entry;

public class Options {

    public int tcpListenBacklog;
    public boolean tcpReuseAddress;
    public int tcpConnectTimeout;
    public boolean tcpNoDelay;
    public boolean tcpKeepAlive;
    public int tcpFrameSize;
    
    public int udpFrameSize;

    public int dispatcherThreads;
    
    public int connectionRetries;
    public int connectionRetryDelaySpread;
    
    public int outgoingHighWaterMark;
    public int outgoingLowWaterMark;
    public int incomingHighWaterMark;
    public int incomingLowWaterMark;
    
    public int incomingFramePayloadLimit;
    
    public boolean deliverAsRawBinary;

    public int defaultFailoverTimeout;
    
    public Options(Parameters params) {

        tcpListenBacklog = 10;
        tcpReuseAddress = true;
        tcpConnectTimeout = 0;
        tcpNoDelay = true;
        tcpKeepAlive = false;
        tcpFrameSize = 4096;
    
        udpFrameSize = 512;

        dispatcherThreads = 1;
    
        connectionRetries = 5;
        connectionRetryDelaySpread = 100;
    
        outgoingHighWaterMark = 100;
        outgoingLowWaterMark = 20;
        incomingHighWaterMark = 100;
        incomingLowWaterMark = 20;
        
        incomingFramePayloadLimit = Integer.MAX_VALUE;
        
        deliverAsRawBinary = false;

        defaultFailoverTimeout = 0;
    
        if (params != null) {
            // override default settings
            
            Entry e = params.find(OptionNames.TCP_LISTEN_BACKLOG);
            if (e != null && e.type() == Parameters.EntryType.INTEGER) {
                tcpListenBacklog = e.getInteger();
            }
            
            e = params.find(OptionNames.TCP_REUSEADDR);
            if (e != null && e.type() == Parameters.EntryType.BOOLEAN) {
                tcpReuseAddress = e.getBoolean();
            }

            e = params.find(OptionNames.TCP_CONNECT_TIMEOUT);
            if (e != null && e.type() == Parameters.EntryType.INTEGER) {
                tcpConnectTimeout = e.getInteger();
            }

            e = params.find(OptionNames.TCP_NODELAY);
            if (e != null && e.type() == Parameters.EntryType.BOOLEAN) {
                tcpNoDelay = e.getBoolean();
            }

            e = params.find(OptionNames.TCP_KEEPALIVE);
            if (e != null && e.type() == Parameters.EntryType.BOOLEAN) {
                tcpKeepAlive = e.getBoolean();
            }

            e = params.find(OptionNames.TCP_FRAME_SIZE);
            if (e != null && e.type() == Parameters.EntryType.INTEGER) {
                tcpFrameSize = e.getInteger();
            }

            e = params.find(OptionNames.UDP_FRAME_SIZE);
            if (e != null && e.type() == Parameters.EntryType.INTEGER) {
                udpFrameSize = e.getInteger();
            }

            e = params.find(OptionNames.DISPATCHER_THREADS);
            if (e != null && e.type() == Parameters.EntryType.INTEGER) {
                dispatcherThreads = e.getInteger();
            }

            e = params.find(OptionNames.CONNECTION_RETRIES);
            if (e != null && e.type() == Parameters.EntryType.INTEGER) {
                connectionRetries = e.getInteger();
            }

            e = params.find(OptionNames.CONNECTION_RETRY_DELAY_SPREAD);
            if (e != null && e.type() == Parameters.EntryType.INTEGER) {
                connectionRetryDelaySpread = e.getInteger();
            }

            e = params.find(OptionNames.OUTGOING_HIGH_WATER_MARK);
            if (e != null && e.type() == Parameters.EntryType.INTEGER) {
                outgoingHighWaterMark = e.getInteger();
            }

            e = params.find(OptionNames.OUTGOING_LOW_WATER_MARK);
            if (e != null && e.type() == Parameters.EntryType.INTEGER) {
                outgoingLowWaterMark = e.getInteger();
            }
            
            if (outgoingLowWaterMark > outgoingHighWaterMark) {
                outgoingLowWaterMark = outgoingHighWaterMark;
            }

            e = params.find(OptionNames.INCOMING_HIGH_WATER_MARK);
            if (e != null && e.type() == Parameters.EntryType.INTEGER) {
                incomingHighWaterMark = e.getInteger();
            }

            e = params.find(OptionNames.INCOMING_LOW_WATER_MARK);
            if (e != null && e.type() == Parameters.EntryType.INTEGER) {
                incomingLowWaterMark = e.getInteger();
            }
            
            if (incomingLowWaterMark > incomingHighWaterMark) {
                incomingLowWaterMark = incomingHighWaterMark;
            }

            e = params.find(OptionNames.INCOMING_FRAME_PAYLOAD_LIMIT);
            if (e != null && e.type() == Parameters.EntryType.INTEGER) {
                incomingFramePayloadLimit = e.getInteger();
            }
            
            e = params.find(OptionNames.DELIVER_AS_RAW_BINARY);
            if (e != null && e.type() == Parameters.EntryType.BOOLEAN) {
                deliverAsRawBinary = e.getBoolean();
            }

            e = params.find(OptionNames.DEFAULT_FAILOVER_TIMEOUT);
            if (e != null && e.type() == Parameters.EntryType.INTEGER) {
                defaultFailoverTimeout = e.getInteger();
            }
        }
    }
}
