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

package YAMI.Core.Option_Names is

   --
   --  Option names for the Core library.
   --
   Tcp_Listen_Backlog : constant String :=  "tcp_listen_backlog";
   Tcp_Reuseaddr : constant String :=       "tcp_reuseaddr";
   Tcp_Nonblocking : constant String :=     "tcp_nonblocking";
   Tcp_Connect_Timeout : constant String := "tcp_connect_timeout";
   Tcp_Nodelay : constant String :=         "tcp_nodelay";
   Tcp_Keepalive : constant String :=       "tcp_keepalive";
   Tcp_Frame_Size : constant String :=      "tcp_frame_size";
   Udp_Frame_Size : constant String :=      "udp_frame_size";
   Unix_Listen_Backlog : constant String := "unix_listen_backlog";
   Unix_Nonblocking : constant String :=    "unix_nonblocking";
   Unix_Frame_Size : constant String :=     "unix_frame_size";
   File_Nonblocking : constant String :=    "file_nonblocking";
   File_Frame_Size : constant String :=     "file_frame_size";
   SSL_Certificate_File : constant String := "ssl_certificate_file";
   SSL_Private_Key_File : constant String := "ssl_private_key_file";
   
end YAMI.Core.Option_Names;
