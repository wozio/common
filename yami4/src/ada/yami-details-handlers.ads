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

with YAMI.Core.Agents;
with YAMI.Core.Incoming_Message_Handlers;
with YAMI.Core.Message_Progress_Handlers;
with YAMI.Details.Core_Connection_Handlers;
with YAMI.Details.Dispatch_Managers;
with YAMI.Details.Outgoing_Message_Managers;
with YAMI.Details.Water_Flow_Managers;

with Ada.Streams;

package YAMI.Details.Handlers is

   --  Core handler for incoming messages.
   --  Note: "incoming message" at the core level are either
   --  new incoming messages or replies to messages
   --  that were sent out from this agent.

   type Core_Incoming_Message_Handler is
     new Core.Incoming_Message_Handlers.Handler with record
        Core_Agent : Core.Agents.Agent_Access;
        Outgoing_Manager : Details.Outgoing_Message_Managers.Manager_Access;
        Dispatcher : Details.Dispatch_Managers.Dispatch_Manager_Access;
        Connection_Event_Handler : Core_Connection_Handlers.Handler_Access;
        Deliver_As_Raw_Binary : Boolean;
   end record;

   overriding
   procedure Call
     (H : in out Core_Incoming_Message_Handler;
      Source : in String;
      Header_Buffers : in Core.Serialization_Buffers_Descriptor;
      Body_Buffers : in Core.Serialization_Buffers_Descriptor);

   procedure Init
     (H : in out Core_Incoming_Message_Handler;
      Core_Agent : in Core.Agents.Agent_Access;
      Outgoing_Manager : in Details.Outgoing_Message_Managers.Manager_Access;
      Dispatcher : in Details.Dispatch_Managers.Dispatch_Manager_Access;
      Connection_Event_Handler : Core_Connection_Handlers.Handler_Access;
      Deliver_As_Raw_Binary : in Boolean);

   --  Core progress handler for managing one-way messages

   type One_Way_Message_Progress_Handler is
     new Core.Message_Progress_Handlers.Handler with record
        Outgoing_Flow : Details.Water_Flow_Managers.Manager_Access;
   end record;

   overriding
   procedure Progress
     (H : in out One_Way_Message_Progress_Handler;
      Sent_Bytes : in Ada.Streams.Stream_Element_Count;
      Total_Byte_Count : in Ada.Streams.Stream_Element_Count);

   procedure Init
     (H : in out One_Way_Message_Progress_Handler;
      Outgoing_Flow : Details.Water_Flow_Managers.Manager_Access);

end YAMI.Details.Handlers;
