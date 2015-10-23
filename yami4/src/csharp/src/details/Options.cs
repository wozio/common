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

namespace Inspirel.YAMI.details
{
    internal class Options
    {
        public int tcpListenBacklog;
        public bool tcpReuseAddress;
        public int tcpConnectTimeout;
        public bool tcpNoDelay;
        public bool tcpKeepAlive;
        public int tcpFrameSize;

        public int udpFrameSize;

        public int dispatcherThreads;

        public int connectionRetries;
        public int connectionRetryDelaySpread;

        public int outgoingHighWaterMark;
        public int outgoingLowWaterMark;
        public int incomingHighWaterMark;
        public int incomingLowWaterMark;

        public bool deliverAsRawBinary;

        public int default_failover_timeout;

        public Options(Parameters parameters)
        {
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

            deliverAsRawBinary = false;

            default_failover_timeout = 0;

            if(parameters != null)
            {
                // override default settings

                Parameters.Entry e = 
                    parameters.Find(OptionNames.TCP_LISTEN_BACKLOG);
                if(e != null && e.Type == Parameters.EntryType.INTEGER)
                {
                    tcpListenBacklog = e.GetInteger();
                }

                e = parameters.Find(OptionNames.TCP_REUSEADDR);
                if(e != null && e.Type == Parameters.EntryType.BOOLEAN)
                {
                    tcpReuseAddress = e.GetBoolean();
                }

                e = parameters.Find(OptionNames.TCP_CONNECT_TIMEOUT);
                if (e != null && e.Type == Parameters.EntryType.INTEGER) 
                {
                    tcpConnectTimeout = e.GetInteger();
                }

                e = parameters.Find(OptionNames.TCP_NODELAY);
                if(e != null && e.Type == Parameters.EntryType.BOOLEAN)
                {
                    tcpNoDelay = e.GetBoolean();
                }

                e = parameters.Find(OptionNames.TCP_KEEPALIVE);
                if(e != null && e.Type == Parameters.EntryType.BOOLEAN)
                {
                    tcpKeepAlive = e.GetBoolean();
                }

                e = parameters.Find(OptionNames.TCP_FRAME_SIZE);
                if(e != null && e.Type == Parameters.EntryType.INTEGER)
                {
                    tcpFrameSize = e.GetInteger();
                }

                e = parameters.Find(OptionNames.UDP_FRAME_SIZE);
                if(e != null && e.Type == Parameters.EntryType.INTEGER)
                {
                    udpFrameSize = e.GetInteger();
                }

                e = parameters.Find(OptionNames.DISPATCHER_THREADS);
                if(e != null && e.Type == Parameters.EntryType.INTEGER)
                {
                    dispatcherThreads = e.GetInteger();
                }

                e = parameters.Find(OptionNames.CONNECTION_RETRIES);
                if(e != null && e.Type == Parameters.EntryType.INTEGER)
                {
                    connectionRetries = e.GetInteger();
                }

                e = parameters.Find(
                    OptionNames.CONNECTION_RETRY_DELAY_SPREAD);
                if(e != null && e.Type == Parameters.EntryType.INTEGER)
                {
                    connectionRetryDelaySpread = e.GetInteger();
                }

                e = parameters.Find(OptionNames.OUTGOING_HIGH_WATER_MARK);
                if(e != null && e.Type == Parameters.EntryType.INTEGER)
                {
                    outgoingHighWaterMark = e.GetInteger();
                }

                e = parameters.Find(OptionNames.OUTGOING_LOW_WATER_MARK);
                if(e != null && e.Type == Parameters.EntryType.INTEGER)
                {
                    outgoingLowWaterMark = e.GetInteger();
                }

                if(outgoingLowWaterMark > outgoingHighWaterMark)
                {
                    outgoingLowWaterMark = outgoingHighWaterMark;
                }

                e = parameters.Find(OptionNames.INCOMING_HIGH_WATER_MARK);
                if(e != null && e.Type == Parameters.EntryType.INTEGER)
                {
                    incomingHighWaterMark = e.GetInteger();
                }

                e = parameters.Find(OptionNames.INCOMING_LOW_WATER_MARK);
                if(e != null && e.Type == Parameters.EntryType.INTEGER)
                {
                    incomingLowWaterMark = e.GetInteger();
                }

                if(incomingLowWaterMark > incomingHighWaterMark)
                {
                    incomingLowWaterMark = incomingHighWaterMark;
                }

                e = parameters.Find(OptionNames.DELIVER_AS_RAW_BINARY);
                if(e != null && e.Type == Parameters.EntryType.BOOLEAN)
                {
                    deliverAsRawBinary = e.GetBoolean();
                }

                e = parameters.Find(OptionNames.DEFAULT_FAILOVER_TIMEOUT);
                if(e != null && e.Type == Parameters.EntryType.INTEGER)
                {
                    default_failover_timeout = e.GetInteger();
                }
            }
        }
    }
}