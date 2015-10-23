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

with YAMI.Outgoing_Messages;
with YAMI.Parameters;

package YAMI.Agents.Helpers is

   --
   --  Helper objects.
   --

   Empty_Parameters : constant Parameters.Parameters_Collection;

   --
   --  Helper operations.
   --

   procedure Common_Make_Agent
     (Event_Handler : in Event_Notification_Handlers.Handler_Access;
      Options : in Parameters.Parameters_Collection;
      The_Agent : out Agent);

   procedure Finalize_Agent (The_Agent : in out Agent);

   procedure Make_Sure_Channel_Exists
     (The_Agent : in out Agent;
      Target : in String;
      Auto_Connect : in Boolean;
      Channel : out Core.Channel_Descriptor);

   procedure Send
     (The_Agent : in out Agent;
      Target : in String;
      Object_Name : in String;
      Message_Name : in String;
      Content : in Serializables.Serializable'Class;
      Message_Handler : in Outgoing_Messages.Outgoing_Message_Access;
      Priority : in Natural;
      Auto_Connect : in Boolean);

   procedure Send_Reply
     (Core_Agent : in out Core.Agents.Agent;
      Core_Connection_Handler :
        in Details.Core_Connection_Handlers.Handler_Access;
      Source : in String;
      Message_Id : in Parameters.YAMI_Long_Long_Integer;
      Reply_Content : in Serializables.Serializable'Class;
      Priority : Natural);

   procedure Send_Reject
     (Core_Agent : in out Core.Agents.Agent;
      Core_Connection_Handler :
        in Details.Core_Connection_Handlers.Handler_Access;
      Source : in String;
      Message_Id : in Parameters.YAMI_Long_Long_Integer;
      Reason : in String;
      Priority : Natural);

private

   Empty_Parameters : constant Parameters.Parameters_Collection :=
     Parameters.Make_Parameters;

end YAMI.Agents.Helpers;
