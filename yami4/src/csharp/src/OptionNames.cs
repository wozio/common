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

namespace Inspirel.YAMI
{

    /// <summary> This class groups constant values for option names.
    /// These constants are recommended instead of literal values 
    /// for safety. 
    /// </summary>
    public static class OptionNames
    {
        /// <summary>
        /// literal for "tcp_listen_backlog" property
        /// </summary>
        public const string TCP_LISTEN_BACKLOG = "tcp_listen_backlog";

        /// <summary>
        /// literal for "tcp_reuseaddr" property
        /// </summary>
        public const string TCP_REUSEADDR = "tcp_reuseaddr";

        /// <summary>
        /// literal for "tcp_connect_timeout" property
        /// </summary>
        public const string TCP_CONNECT_TIMEOUT = "tcp_connect_timeout";

        /// <summary>
        /// literal for "tcp_nodelay" property
        /// </summary>
        public const string TCP_NODELAY = "tcp_nodelay";

        /// <summary>
        /// literal for "tcp_keepalive" property
        /// </summary>
        public const string TCP_KEEPALIVE = "tcp_keepalive";

        /// <summary>
        /// literal for "tcp_frame_size" property
        /// </summary>
        public const string TCP_FRAME_SIZE = "tcp_frame_size";

        /// <summary>
        /// literal for "udp_frame_size" property
        /// </summary>
        public const string UDP_FRAME_SIZE = "udp_frame_size";

        /// <summary>
        /// literal for "dispatcher_threads" property
        /// </summary>
        public const string DISPATCHER_THREADS = "dispatcher_threads";

        /// <summary>
        /// literal for "connection_retries" property
        /// </summary>
        public const string CONNECTION_RETRIES = "connection_retries";

        /// <summary>
        /// literal for "connection_retry_delay_spread" property
        /// </summary>
        public const string CONNECTION_RETRY_DELAY_SPREAD =
            "connection_retry_delay_spread";

        /// <summary>
        /// literal for "outgoing_high_water_mark" property
        /// </summary>
        public const string OUTGOING_HIGH_WATER_MARK =
            "outgoing_high_water_mark";

        /// <summary>
        /// literal for "outgoing_low_water_mark" property
        /// </summary>
        public const string OUTGOING_LOW_WATER_MARK =
            "outgoing_low_water_mark";

        /// <summary>
        /// literal for "incoming_high_water_mark" property
        /// </summary>
        public const string INCOMING_HIGH_WATER_MARK =
            "incoming_high_water_mark";

        /// <summary>
        /// literal for "incoming_low_water_mark" property
        /// </summary>
        public const string INCOMING_LOW_WATER_MARK =
            "incoming_low_water_mark";

        /// <summary>
        /// literal for "deliver_as_raw_binary" property
        /// </summary>
        public const string DELIVER_AS_RAW_BINARY =
            "deliver_as_raw_binary";

        /// <summary>
        /// literal for "default_failover_timeout" property
        /// </summary>
        public const string DEFAULT_FAILOVER_TIMEOUT =
            "default_failover_timeout";
    }

}
