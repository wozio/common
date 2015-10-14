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

with Ada.Streams;

package YAMI.Core.Event_Notification_Handlers is

   --
   --  Callback interface for processing event notifications.
   --
   type Handler is limited interface;
   type Handler_Access is access all Handler'Class;

   --
   --  Operations of the Handler interface.
   --

   procedure Agent_Closed (H : in out Handler) is abstract;
   
   procedure Listener_Added (H : in out Handler; Target : in String)
      is abstract;
     
   procedure Listener_Removed (H : in out Handler; Target : in String)
      is abstract;
     
   procedure Incoming_Connection_Open (H : in out Handler; Target : in String)
      is abstract;
     
   procedure Outgoing_Connection_Open (H : in out Handler; Target : in String)
      is abstract;
     
   procedure Connection_Closed (H : in out Handler; Target : in String)
      is abstract;
     
   procedure Connection_Error (H : in out Handler; Target : in String)
      is abstract;
     
   procedure Message_Sent
     (H : in out Handler;
      Target : in String;
      Size : in Ada.Streams.Stream_Element_Count)
      is abstract;

   procedure Message_Received
     (H : in out Handler;
      Target : in String;
      Size : in Ada.Streams.Stream_Element_Count)
      is abstract;

end YAMI.Core.Event_Notification_Handlers;
