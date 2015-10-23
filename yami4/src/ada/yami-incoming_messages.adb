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

with YAMI.Agents.Helpers;

package body YAMI.Incoming_Messages is

   --  helper subprograms

   procedure Raise_Wrong_State is
   begin
      raise Logic_Error with "Object is in the wrong state.";
   end Raise_Wrong_State;

   procedure Assert_Not_Empty (Msg : in Incoming_Message) is
   begin
      if Msg.Empty then
         Raise_Wrong_State;
      end if;
   end Assert_Not_Empty;

   --
   --  Operations of the Incoming_Message type.
   --

   function Source (Msg : in Incoming_Message) return String is
   begin
      Assert_Not_Empty (Msg);
      return Ada.Strings.Unbounded.To_String (Msg.Source);
   end Source;

   function Object_Name (Msg : in Incoming_Message) return String is
   begin
      Assert_Not_Empty (Msg);
      return Ada.Strings.Unbounded.To_String (Msg.Object_Name);
   end Object_Name;

   function Message_Name (Msg : in Incoming_Message) return String is
   begin
      Assert_Not_Empty (Msg);
      return Ada.Strings.Unbounded.To_String (Msg.Message_Name);
   end Message_Name;

   procedure Process_Content
     (Msg : in Incoming_Message;
      Process : not null access procedure
      (Content : in out Parameters.Parameters_Collection)) is

      use type Parameters.Parameters_Collection_Access;

   begin
      Assert_Not_Empty (Msg);
      if Msg.Content = null then
         Raise_Wrong_State;
      end if;

      Process (Msg.Content.all);
   end Process_Content;

   procedure Process_Raw_Content
     (Msg : in Incoming_Message;
      Process : not null access procedure
      (Raw_Content : in Serializables.Serialization_Buffer_List)) is

      use type Serializables.Serialization_Buffer_Access;

   begin
      Assert_Not_Empty (Msg);
      if Msg.Raw_Content (1) = null then
         Raise_Wrong_State;
      end if;

      Process (Msg.Raw_Content);
   end Process_Raw_Content;

   procedure Move (What : in out Incoming_Message;
                   Where : in out Incoming_Message) is
   begin
      if What.Empty then
         raise Logic_Error with "Source object is in the wrong state.";
      end if;
      if not Where.Empty then
         raise Logic_Error with "Destination object is in the wrong state.";
      end if;

      Where.Core_Agent := What.Core_Agent;
      Where.Message_Id := What.Message_Id;
      Where.Source := What.Source;
      Where.Object_Name := What.Object_Name;
      Where.Message_Name := What.Message_Name;
      Where.Content := What.Content;
      What.Content := null;

      Where.Empty := False;
      What.Empty := True;
   end Move;

   procedure Reply (Msg : in Incoming_Message;
                    Priority : in Natural := 0) is
   begin
      Assert_Not_Empty (Msg);
      Agents.Helpers.Send_Reply
        (Msg.Core_Agent.all,
         Msg.Connection_Event_Handler,
         Ada.Strings.Unbounded.To_String (Msg.Source),
         Msg.Message_Id,
         Agents.Helpers.Empty_Parameters,
         Priority);
   end Reply;

   procedure Reply (Msg : in Incoming_Message;
                    Reply_Content : in Serializables.Serializable'Class;
                    Priority : in Natural := 0) is
   begin
      Assert_Not_Empty (Msg);
      Agents.Helpers.Send_Reply
        (Msg.Core_Agent.all,
         Msg.Connection_Event_Handler,
         Ada.Strings.Unbounded.To_String (Msg.Source),
         Msg.Message_Id,
         Reply_Content,
         Priority);
   end Reply;

   procedure Reject (Msg : in Incoming_Message;
                     Reason : in String := "";
                     Priority : in Natural := 0) is
   begin
      Assert_Not_Empty (Msg);
      Agents.Helpers.Send_Reject
        (Msg.Core_Agent.all,
         Msg.Connection_Event_Handler,
         Ada.Strings.Unbounded.To_String (Msg.Source),
         Msg.Message_Id,
         Reason,
         Priority);
   end Reject;

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
     return Incoming_Message_Access is

      Message : Incoming_Message_Access := new Incoming_Message;

   begin
      Message.all.Core_Agent := Core_Agent;
      Message.all.Connection_Event_Handler := Connection_Event_Handler;
      Message.all.Message_Id := Message_Id;
      Message.all.Source :=
        Ada.Strings.Unbounded.To_Unbounded_String (Source);
      Message.all.Object_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Object_Name);
      Message.all.Message_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Message_Name);
      Message.all.Content := Content;
      Message.all.Raw_Content := Raw_Content;

      Message.Empty := False;

      return Message;
   end New_Message;

   procedure Finalize (Message : in out Incoming_Message) is
   begin
      Parameters.Free (Message.Content);
      Serializables.Free (Message.Raw_Content (1));
   end Finalize;

end YAMI.Incoming_Messages;
