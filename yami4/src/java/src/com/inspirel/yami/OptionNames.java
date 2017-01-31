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

/**
 * This class groups constant values for option names.
 * These constants are recommended instead of literal values for safety.
 */
public class OptionNames {

    public static final String TCP_LISTEN_BACKLOG =
            "tcp_listen_backlog";

    public static final String TCP_REUSEADDR =
            "tcp_reuseaddr";

    public static final String TCP_CONNECT_TIMEOUT =
            "tcp_connect_timeout";

    public static final String TCP_NODELAY =
            "tcp_nodelay";

    public static final String TCP_KEEPALIVE =
            "tcp_keepalive";

    public static final String TCP_FRAME_SIZE =
            "tcp_frame_size";

    public static final String UDP_FRAME_SIZE =
            "udp_frame_size";

    public static final String DISPATCHER_THREADS =
            "dispatcher_threads";

    public static final String CONNECTION_RETRIES =
            "connection_retries";

    public static final String CONNECTION_RETRY_DELAY_SPREAD =
            "connection_retry_delay_spread";

    public static final String OUTGOING_HIGH_WATER_MARK =
            "outgoing_high_water_mark";

    public static final String OUTGOING_LOW_WATER_MARK =
            "outgoing_low_water_mark";

    public static final String INCOMING_HIGH_WATER_MARK =
            "incoming_high_water_mark";

    public static final String INCOMING_LOW_WATER_MARK =
            "incoming_low_water_mark";
    
    public static final String INCOMING_FRAME_PAYLOAD_LIMIT =
    	    "incoming_frame_payload_limit";

    public static final String DELIVER_AS_RAW_BINARY =
            "deliver_as_raw_binary";
    
    public static final String DEFAULT_FAILOVER_TIMEOUT =
            "default_failover_timeout";
}
