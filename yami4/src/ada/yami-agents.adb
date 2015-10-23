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
with YAMI.Core.Message_Progress_Handlers;
with YAMI.Details.Options;

with Ada.Unchecked_Deallocation;

package body YAMI.Agents is

   --
   --  Operations of the Agent type.
   --

   procedure Do_Free is new Ada.Unchecked_Deallocation
     (Object => Agent, Name => Agent_Access);

   function Make_Agent return Agent is
   begin
      return The_Agent : Agent do
         Helpers.Common_Make_Agent
           (null, Helpers.Empty_Parameters, The_Agent);
      end return;
   end Make_Agent;

   function New_Agent return Agent_Access is
      The_Agent : Agent_Access := new Agent;
   begin
      Helpers.Common_Make_Agent
        (null, Helpers.Empty_Parameters, The_Agent.all);
      return The_Agent;
   exception
      when others =>
         Do_Free (The_Agent);
         raise;
   end New_Agent;

   function Make_Agent (Options : in Parameters.Parameters_Collection)
                       return Agent is
   begin
      return The_Agent : Agent do
         Helpers.Common_Make_Agent (null, Options, The_Agent);
      end return;
   end Make_Agent;

   function New_Agent (Options : in Parameters.Parameters_Collection)
                      return Agent_Access is
      The_Agent : Agent_Access := new Agent;
   begin
      Helpers.Common_Make_Agent (null, Options, The_Agent.all);
      return The_Agent;
   exception
      when others =>
         Do_Free (The_Agent);
         raise;
   end New_Agent;

   function Make_Agent
     (Event_Handler : in Event_Notification_Handlers.Handler_Access;
      Options : in Parameters.Parameters_Collection)
     return Agent is
   begin
      return The_Agent : Agent do
         Helpers.Common_Make_Agent (Event_Handler, Options, The_Agent);
      end return;
   end Make_Agent;
   
   function New_Agent
     (Event_Handler : in Event_Notification_Handlers.Handler_Access;
      Options : in Parameters.Parameters_Collection)
     return Agent_Access is
      The_Agent : Agent_Access := new Agent;
   begin
      Helpers.Common_Make_Agent (Event_Handler, Options, The_Agent.all);
      return The_Agent;
   exception
      when others =>
         Do_Free (The_Agent);
         raise;
   end New_Agent;
   
   procedure Finalize (The_Agent : in out Agent) is
   begin
      Helpers.Finalize_Agent (The_Agent);
   end Finalize;

   procedure Free (The_Agent : in out Agent_Access) is
   begin
      Do_Free (The_Agent);
   end Free;

   procedure Add_Listener
     (The_Agent : in out Agent;
      Target : in String;
      Resolved_Target : out String;
      Resolved_Target_Last : out Natural) is
   begin
      The_Agent.Core_Agent.all.Add_Listener
        (Target, Resolved_Target, Resolved_Target_Last,
         The_Agent.Core_Connection_Handler'Unchecked_Access);
   end Add_Listener;

   procedure Add_Listener (The_Agent : in out Agent; Target : in String) is
      Dummy_Resolved_Target : String (1 .. Max_Target_Length);
      Dummy_Resolved_Target_Last : Natural;
   begin
      The_Agent.Core_Agent.all.Add_Listener
        (Target, Dummy_Resolved_Target, Dummy_Resolved_Target_Last);
   end Add_Listener;

   procedure Remove_Listener (The_Agent : in out Agent; Target : in String) is
   begin
      The_Agent.Core_Agent.all.Remove_Listener (Target);
   end Remove_Listener;

   procedure Register_Object
     (The_Agent : in out Agent;
      Object_Name : in String;
      Handler : in Incoming_Messages.Message_Handler_Access) is
      
      use type Event_Notification_Handlers.Handler_Access;
      
   begin
      The_Agent.Object_Map.Register_Object (Object_Name, Handler);
      
      if The_Agent.Event_Handler /= null then
         begin
            The_Agent.Event_Handler.all.Object_Registered (Object_Name);
         exception
            when others =>
               null;
         end;
      end if;
   end Register_Object;

   procedure Unregister_Object (The_Agent : in out Agent;
                                Object_Name : in String) is
      
      use type Event_Notification_Handlers.Handler_Access;
      
   begin
      The_Agent.Object_Map.Unregister_Object (Object_Name);
      
      if The_Agent.Event_Handler /= null then
         begin
            The_Agent.Event_Handler.all.Object_Unregistered (Object_Name);
         exception
            when others =>
               null;
         end;
      end if;
   end Unregister_Object;

   procedure Open_Connection (The_Agent : in out Agent;
                              Target : in String) is
      Dummy_Channel : Core.Channel_Descriptor;
      Auto_Connect : constant Boolean := True;
   begin
      Helpers.Make_Sure_Channel_Exists
        (The_Agent, Target, Auto_Connect, Dummy_Channel);
   end Open_Connection;

   procedure Send_One_Way (The_Agent : in out Agent;
                           Target : in String;
                           Object_Name : in String;
                           Message_Name : in String;
                           Priority : in Natural := 0;
                           Auto_Connect : in Boolean := True) is
   begin
      Helpers.Send (The_Agent, Target, Object_Name, Message_Name,
                    Helpers.Empty_Parameters, null, Priority, Auto_Connect);
   end Send_One_Way;

   procedure Send_One_Way (The_Agent : in out Agent;
                           Target : in String;
                           Object_Name : in String;
                           Message_Name : in String;
                           Content : in Serializables.Serializable'Class;
                           Priority : in Natural := 0;
                           Auto_Connect : in Boolean := True) is
   begin
      Helpers.Send (The_Agent, Target, Object_Name, Message_Name,
                    Content, null, Priority, Auto_Connect);
   end Send_One_Way;

   procedure Send
     (The_Agent : in out Agent;
      Target : in String;
      Object_Name : in String;
      Message_Name : in String;
      Message_Handler : in Outgoing_Messages.Outgoing_Message_Access;
      Priority : in Natural := 0;
      Auto_Connect : in Boolean := True) is
   begin
      Helpers.Send (The_Agent, Target, Object_Name, Message_Name,
                    Helpers.Empty_Parameters, Message_Handler,
                    Priority, Auto_Connect);
   end Send;

   procedure Send
     (The_Agent : in out Agent;
      Target : in String;
      Object_Name : in String;
      Message_Name : in String;
      Content : in Serializables.Serializable'Class;
      Message_Handler : in Outgoing_Messages.Outgoing_Message_Access;
      Priority : in Natural := 0;
      Auto_Connect : in Boolean := True) is
   begin
      Helpers.Send (The_Agent, Target, Object_Name, Message_Name,
                    Content, Message_Handler, Priority, Auto_Connect);
   end Send;

   procedure Close_Connection (The_Agent : in out Agent;
                               Target : in String;
                               Priority : in Natural := 0) is
   begin
      The_Agent.Core_Agent.all.Close (Target, Priority);
   end Close_Connection;

   procedure Hard_Close_Connection (The_Agent : in out Agent;
                                    Target : in String) is
   begin
      The_Agent.Core_Agent.all.Hard_Close (Target);
   end Hard_Close_Connection;

   procedure Register_Connection_Event_Monitor
     (The_Agent : in out Agent;
      User_Event_Handler : Connection_Event_Handlers.Handler_Access) is
   begin
      The_Agent.Core_Connection_Handler.Set_User_Event_Handler
        (User_Event_Handler);
   end Register_Connection_Event_Monitor;

   procedure Get_Outgoing_Flow_State
     (The_Agent : in out Agent;
      Current_Level : out Parameters.Count_Type;
      High_Water_Mark : out Parameters.Count_Type;
      Low_Water_Mark : out Parameters.Count_Type) is
   begin
      The_Agent.Outgoing_Flow.Get_State
        (Current_Level, High_Water_Mark, Low_Water_Mark);
   end Get_Outgoing_Flow_State;

end YAMI.Agents;
