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
with YAMI.Details.Core_Connection_Handlers;
with YAMI.Parameters;
with YAMI.Serializables;

with Ada.Finalization;
with Ada.Strings.Unbounded;

package YAMI.Incoming_Messages is

   --
   --  Type encapsulating all information about the incoming message.
   --
   type Incoming_Message is tagged limited private;
   type Incoming_Message_Access is access all Incoming_Message;

   --
   --  Operations of the Incoming_Message type.
   --

   --
   --  Gets the message source name.
   --
   function Source (Msg : in Incoming_Message) return String;

   --
   --  Gets the destination object name.
   --
   function Object_Name (Msg : in Incoming_Message) return String;

   --
   --  Gets the message name.
   --
   function Message_Name (Msg : in Incoming_Message) return String;

   --
   --  Queries the body content.
   --
   procedure Process_Content
     (Msg : in Incoming_Message;
      Process : not null access procedure
      (Content : in out Parameters.Parameters_Collection));

   --
   --  Queries the raw (binary) body content.
   --
   --  This operation can be executed only if the agent is configured
   --  for raw content delivery.
   --
   procedure Process_Raw_Content
     (Msg : in Incoming_Message;
      Process : not null access procedure
      (Raw_Content : in Serializables.Serialization_Buffer_List));

   --
   --  Stores the content of Incoming_Message in other object.
   --
   --  Note: the destination object should be empty
   --  and the source object becomes empty after this operation.
   --
   procedure Move (What : in out Incoming_Message;
                   Where : in out Incoming_Message);

   --
   --  Sends back the reply.
   --
   procedure Reply (Msg : in Incoming_Message;
                    Priority : in Natural := 0);
   procedure Reply (Msg : in Incoming_Message;
                    Reply_Content : in Serializables.Serializable'Class;
                    Priority : in Natural := 0);

   --
   --  Sends back the rejection notification.
   --
   procedure Reject (Msg : in Incoming_Message;
                     Reason : in String := "";
                     Priority : in Natural := 0);

   --  used internally
   function New_Message
     (Core_Agent : in Core.Agents.Agent_Access;
      Connection_Event_Handler :
        in Details.Core_Connection_Handlers.Handler_Access;
      Message_Id : in Parameters.YAMI_Long_Long_Integer;
      Source : in String;
      Object_Name : in String;
      Message_Name : in String;
      Content : in Parameters.Parameters_Collection_Access;
      Raw_Content : in Serializables.Serialization_Buffer_List)
     return Incoming_Message_Access;

   --
   --  Callback interface for processing incoming messages.
   --
   type Message_Handler is limited interface;
   type Message_Handler_Access is access all Message_Handler'Class;

   --
   --  Operations of the Handler interface.
   --

   --
   --  Processes the new incoming message.
   --
   procedure Call (H : in out Message_Handler;
                   Message : in out Incoming_Message'Class)
      is abstract;

private

   type Incoming_Message is
     new Ada.Finalization.Limited_Controlled with record
      Core_Agent : Core.Agents.Agent_Access;
      Connection_Event_Handler :
        Details.Core_Connection_Handlers.Handler_Access;
      Message_Id : Parameters.YAMI_Long_Long_Integer;
      Source : Ada.Strings.Unbounded.Unbounded_String;
      Object_Name : Ada.Strings.Unbounded.Unbounded_String;
      Message_Name : Ada.Strings.Unbounded.Unbounded_String;

      --  exactly one of these two contains valid data
      --  Content is used when messages are delivered in the serialized form
      --  and Raw_Content is used (that is, its only element is not null) when
      --  the raw content delivery is configured in the agent
      Content : Parameters.Parameters_Collection_Access;
      Raw_Content : Serializables.Serialization_Buffer_List (1 .. 1);

      Empty : Boolean := True;
   end record;

   overriding
   procedure Finalize (Message : in out Incoming_Message);

end YAMI.Incoming_Messages;
