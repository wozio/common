--  Copyright Maciej Sobczak 2008-2015.
--  This file is part of YAMI4.
--
--  YAMI4 is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  YAMI4 is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

with YAMI.Core.Option_Names;

package YAMI.Option_Names is

   --
   --  These names are convenience renamings for the same constants in Core.
   --

   Tcp_Listen_Backlog : String renames Core.Option_Names.Tcp_Listen_Backlog;

   Tcp_Reuseaddr : String renames Core.Option_Names.Tcp_Reuseaddr;

   Tcp_Nonblocking : String renames Core.Option_Names.Tcp_Nonblocking;

   Tcp_Connect_Timeout : String renames Core.Option_Names.Tcp_Connect_Timeout;

   Tcp_Nodelay : String renames Core.Option_Names.Tcp_Nodelay;

   Tcp_Keepalive : String renames Core.Option_Names.Tcp_Keepalive;

   Tcp_Frame_Size : String renames Core.Option_Names.Tcp_Frame_Size;

   Udp_Frame_Size : String renames Core.Option_Names.Udp_Frame_Size;

   Unix_Listen_Backlog : String renames Core.Option_Names.Unix_Listen_Backlog;

   Unix_Nonblocking : String renames Core.Option_Names.Unix_Nonblocking;

   Unix_Frame_Size : String renames Core.Option_Names.Unix_Frame_Size;

   File_Nonblocking : String renames Core.Option_Names.File_Nonblocking;

   File_Frame_Size : String renames Core.Option_Names.File_Frame_Size;

   SSL_Certificate_File : String renames Core.Option_Names.SSL_Certificate_File;
   
   SSL_Private_Key_File : String renames Core.Option_Names.SSL_Private_Key_File;
   
   --
   --  Additional option names for the general-purpose library.
   --

   Dispatcher_Threads : constant String := "dispatcher_threads";

   Connection_Retries : constant String := "connection_retries";

   Connection_Retry_Delay_Spread : constant String :=
     "connection_retry_delay_spread";

   Outgoing_High_Water_Mark : constant String := "outgoing_high_water_mark";

   Outgoing_Low_Water_Mark : constant String := "outgoing_low_water_mark";

   Incoming_High_Water_Mark : constant String := "incoming_high_water_mark";

   Incoming_Low_Water_Mark : constant String := "incoming_low_water_mark";

   Deliver_As_Raw_Binary : constant String := "deliver_as_raw_binary";
   
   Default_Failover_Timeout : constant String := "default_failover_timeout";

end YAMI.Option_Names;
