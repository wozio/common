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

with YAMI.Core.Event_Notification_Handlers;
with YAMI.Core.Message_Progress_Handlers;
with YAMI.Details.Outgoing_Message_Notifications;
with YAMI.Outgoing_Messages;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

package body YAMI.Agents.Helpers is

   procedure Common_Make_Agent
     (Event_Handler : in Event_Notification_Handlers.Handler_Access;
      Options : in Parameters.Parameters_Collection;
      The_Agent : out Agent) is
      
      use type Event_Notification_Handlers.Handler_Access;
      
   begin
      --  decode option values for future use

      The_Agent.Options := Details.Options.Parse_Parameters (Options);

      --  create the core agent object

      --  NOTE: Unchecked_Access is justified here
      --  because all objects referencing each other
      --  will be destroyed together.

      The_Agent.Core_Agent := Core.Agents.New_Agent
        (Options, The_Agent.Core_Message_Handler'Unchecked_Access,
         The_Agent.Core_Connection_Handler'Unchecked_Access);

      --  initialize handlers

      Details.Handlers.Init
        (The_Agent.Core_Message_Handler,
         The_Agent.Core_Agent,
         The_Agent.Outgoing_Msg_Manager'Unchecked_Access,
         The_Agent.Dispatcher'Unchecked_Access,
         The_Agent.Core_Connection_Handler'Unchecked_Access,
         The_Agent.Options.Deliver_As_Raw_Binary);

      Details.Handlers.Init
        (The_Agent.Core_One_Way_Progress_Handler,
         The_Agent.Outgoing_Flow'Unchecked_Access);

      --  initialize water flow managers

      The_Agent.Outgoing_Flow.Set_Limits
        (The_Agent.Options.Outgoing_High_Water_Mark,
         The_Agent.Options.Outgoing_Low_Water_Mark);

      The_Agent.Incoming_Flow.Set_Limits
        (The_Agent.Options.Incoming_High_Water_Mark,
         The_Agent.Options.Incoming_Low_Water_Mark);

      --  create and initialize the worker task

      The_Agent.Worker := new Details.Worker_Tasks.Worker_Type;

      Details.Worker_Tasks.Init (The_Agent.Worker.all,
                                 The_Agent.Core_Agent,
                                 The_Agent.Incoming_Flow'Unchecked_Access);

      --  initialize the dispatch manager and its internal tasks
      Details.Dispatch_Managers.Init
        (The_Agent.Dispatcher,
         The_Agent.Object_Map'Unchecked_Access,
         The_Agent.Incoming_Flow'Unchecked_Access,
         The_Agent.Core_Agent,
         The_Agent.Options);
      
      if Event_Handler /= null then
         The_Agent.Core_Agent.all.Install_Event_Notification_Handler
           (Core.Event_Notification_Handlers.Handler_Access (Event_Handler));
         
         The_Agent.Event_Handler := Event_Handler;
         
         begin
            Event_Handler.all.Agent_Created;
         exception
            when others =>
               null;
         end;
      end if;
      
   end Common_Make_Agent;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Details.Worker_Tasks.Worker_Type,
      Name => Details.Worker_Tasks.Worker_Access);

   procedure Finalize_Agent (The_Agent : in out Agent) is
   begin
      --  stop and deallocate the worker thread
      Details.Worker_Tasks.Stop (The_Agent.Worker.all);
      Free (The_Agent.Worker);

      --  stop the dispatch manager tasks
      Details.Dispatch_Managers.Stop_Tasks (The_Agent.Dispatcher);

      --  deallocate the core agent
      Core.Agents.Free (The_Agent.Core_Agent);
   end Finalize_Agent;

   --  the random delay might be needed from many different tasks
   --  and there is no provision in AARM for standard random generator
   --  to be task safe

   protected Random_Duration is
      procedure Get_Next (Upper : in Duration; D : out Duration);
   private
      G : Ada.Numerics.Float_Random.Generator;
   end Random_Duration;

   protected body Random_Duration is
      procedure Get_Next (Upper : in Duration; D : out Duration) is
      begin
         D := Duration (Float (Upper) * Ada.Numerics.Float_Random.Random (G));
      end Get_Next;
   end Random_Duration;

   procedure Make_Sure_Channel_Exists
     (The_Agent : in out Agent;
      Target : in String;
      Auto_Connect : in Boolean;
      Channel : out Core.Channel_Descriptor) is

      Succeeded : Boolean := False;
      Last_Exception : Ada.Exceptions.Exception_Occurrence;

      procedure Random_Pause is
         D : Duration;
      begin
         Random_Duration.Get_Next
           (The_Agent.Options.Connection_Retry_Delay_Spread, D);

         delay D;

      end Random_Pause;

      use type Parameters.YAMI_Integer;

   begin
      if Auto_Connect then
         --  Smart retry when connection fails is implemented
         --  by bounded retry with randomized sleep between retries to
         --  spread out initialization bursts in bigger systems.

         for I in 1 .. The_Agent.Options.Connection_Retries loop
            declare
               Created_New : Boolean;
            begin
               The_Agent.Core_Agent.all.Open (Target, Channel, Created_New);

               if Created_New then
                  The_Agent.Core_Connection_Handler.
                    Report_New_Outgoing_Connection (Target);
               end if;

               --  if the above call was successful, finish
               Succeeded := True;
               exit;

            exception
               when E : others =>
                  --  the connection was not successful - try again

                  Ada.Exceptions.Save_Occurrence (Last_Exception, E);

                  if I /= The_Agent.Options.Connection_Retries then
                     Random_Pause;
                  end if;
            end;
         end loop;

         if not Succeeded then
            Ada.Exceptions.Reraise_Occurrence (Last_Exception);
         end if;
      else
         --  do not attempt any reconnection
         The_Agent.Core_Agent.all.Is_Open (Target, Succeeded, Channel);
         if not Succeeded then
            --  the given channel does not exist, treat it as an error
            Details.Check_Result (Details.IO_Error);
         end if;
      end if;
   end Make_Sure_Channel_Exists;

   --  helpers for splitting failover targets into
   --  randomized lists of single targets

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Element_Type => String);

   package Random_Boolean is new Ada.Numerics.Discrete_Random (Boolean);

   Random_Boolean_Generator : Random_Boolean.Generator;

   Failover_Prefix : constant String := "failover:(";
   Failover_Suffix : constant String := ")";
   Failover_Separator : constant String := "|";

   function Is_Target_Failover (Target : in String) return Boolean is
   begin
      return Ada.Strings.Fixed.Index (Target, Failover_Prefix) = Target'First
        and Ada.Strings.Fixed.Index (Target, Failover_Suffix) = Target'Last;
   end Is_Target_Failover;

   procedure Split_Failover_Targets
     (Target : in String;
      Single_Targets : out String_Lists.List) is

      Failover_Content : constant String :=
        Target (Target'First + Failover_Prefix'Length ..
                  Target'Last - Failover_Suffix'Length);

      From : Natural := Failover_Content'First;
      Found : Natural := Failover_Content'First;

      procedure Add_Single_Target (T : in String) is
      begin
         if Random_Boolean.Random (Random_Boolean_Generator) then
            Single_Targets.Append (T);
         else
            Single_Targets.Prepend (T);
         end if;
      end Add_Single_Target;

   begin
      if Failover_Content'Length /= 0 then
         Random_Boolean.Reset (Random_Boolean_Generator);

         while Found /= 0 loop
            Found := Ada.Strings.Fixed.Index
              (Failover_Content, Failover_Separator, From);
            if Found /= 0 then
               --  separator was found
               Add_Single_Target (Failover_Content (From .. Found - 1));

               From := Found + Failover_Separator'Length;
            else
               --  separator was not found
               Add_Single_Target
                 (Failover_Content (From .. Failover_Content'Last));
            end if;
         end loop;
      end if;
   end Split_Failover_Targets;

   --  TODO: this is a workaround for the compiler bug,
   --  remove it when Outgoing_Message will be able to inherit directly
   --  from the notification interface
   package Outgoing_Message_Access_Conversions is
      new System.Address_To_Access_Conversions
     (Outgoing_Messages.Outgoing_Message);

   procedure Send_To_Single_Target
     (The_Agent : in out Agent;
      Target : in String;
      Header : in Serializables.Serializable'Class;
      Content : in Serializables.Serializable'Class;
      Message_Id : in Parameters.YAMI_Long_Long_Integer;
      Message_Handler : in Outgoing_Messages.Outgoing_Message_Access;
      Priority : in Natural;
      Auto_Connect : in Boolean;
      Wait_For_Transmission : in Boolean := False;
      Wait_For_Reply_Or_Reject : in Boolean := False) is

      Channel : Core.Channel_Descriptor;

      Message_Proxy :
        Details.Outgoing_Message_Notifications.Message_Proxy_Access;

      --  TODO: workaround, to be removed
      OM_A : System.Address :=
        Outgoing_Message_Access_Conversions.To_Address
        (Outgoing_Message_Access_Conversions.Object_Pointer (Message_Handler));
      Notif_Handler :
        Details.Outgoing_Message_Notifications.Notification_Handler_Access;

      Progress_Handler : Core.Message_Progress_Handlers.Handler_Access :=
        The_Agent.Core_One_Way_Progress_Handler'Unchecked_Access;

      use type Outgoing_Messages.Outgoing_Message_Access;

   begin
      The_Agent.Outgoing_Flow.Wait_For_Permission;
      The_Agent.Outgoing_Flow.Increase;

      begin

         if Message_Handler /= null then
            --  this message has a handler (either provided by user
            --  or an artificial one for failover handling),
            --  which needs to be wired with a proxy
            --  within the outgoing message manager

            Message_Handler.all.Get_Notification_Handler (Notif_Handler);
            Message_Proxy :=
              new Details.Outgoing_Message_Notifications.Message_Proxy
              (Notif_Handler);
            Message_Proxy.all.Message_Id := Message_Id;
            Message_Proxy.all.Outgoing_Flow :=
              The_Agent.Outgoing_Flow'Unchecked_Access;

            Message_Handler.all.Init
              (OM_A,
               Message_Id,
               The_Agent.Outgoing_Msg_Manager'Unchecked_Access,
               Message_Proxy,
               The_Agent.Options.Deliver_As_Raw_Binary);

            --  the handler's proxy should be used as a progress handler,
            --  so that progress notifications can be redirected to
            --  the message handler and therefore processed
            --  on the per-message basis
            Progress_Handler := Message_Proxy.all'Unchecked_Access;

            --  register the proxy object in the message manager
            The_Agent.Outgoing_Msg_Manager.Put (Message_Id, Message_Proxy);

         end if;

         Helpers.Make_Sure_Channel_Exists
           (The_Agent, Target, Auto_Connect, Channel);

         --  NOTE: Unchecked_Access is justified,
         --  because the core agent object will never live longer than
         --  the referenced handler.

         The_Agent.Core_Agent.all.Post
           (Channel, Header, Content, Priority, Progress_Handler);

         if Message_Handler /= null then
            if Wait_For_Transmission then
               Message_Handler.all.Wait_For_Transmission;
            end if;
            if Wait_For_Reply_Or_Reject then
               if The_Agent.Options.Default_Failover_Timeout /= 0.0 then
                  select
                     Message_Handler.all.Wait_For_Completion;
                  or
                     delay The_Agent.Options.Default_Failover_Timeout;
                  end select;
               else
                  Message_Handler.all.Wait_For_Completion;
               end if;
            end if;
         end if;

      exception
         when others =>
            --  flow control: due to the error this message conceptually
            --  disappears from the pipeline and the flow manager
            --  should no longer take account of it

            The_Agent.Outgoing_Flow.Decrease;

            if Message_Handler /= null then
               The_Agent.Outgoing_Msg_Manager.Remove (Message_Id);
            end if;

            raise;
      end;

   end Send_To_Single_Target;

   procedure Send
     (The_Agent : in out Agent;
      Target : in String;
      Object_Name : in String;
      Message_Name : in String;
      Content : in Serializables.Serializable'Class;
      Message_Handler : in Outgoing_Messages.Outgoing_Message_Access;
      Priority : in Natural;
      Auto_Connect : in Boolean) is

      Header : Parameters.Parameters_Collection := Parameters.Make_Parameters;
      Message_Id : Parameters.YAMI_Long_Long_Integer;

      Wait_For_Transmission : Boolean := False;
      Wait_For_Reply_Or_Reject : Boolean := False;

      Last_Exception : Ada.Exceptions.Exception_Occurrence;

      use type Outgoing_Messages.Outgoing_Message_Access;

   begin
      Header.Set_String ("type", "message");
      Header.Set_String ("object_name", Object_Name);
      Header.Set_String ("message_name", Message_Name);

      The_Agent.Id_Generator.Get_Next (Message_Id);
      Header.Set_Long_Long ("message_id", Message_Id);

      if Is_Target_Failover (Target) then
         --  the given target represents a group of separate destinations
         --  that should be tried in random order until success

         declare
            Single_Targets : String_Lists.List;
            C : String_Lists.Cursor;

            Dummy_Outgoing_Message : aliased
              Outgoing_Messages.Outgoing_Message;
            Effective_Message_Handler :
              Outgoing_Messages.Outgoing_Message_Access;

            Success : Boolean := False;
         begin
            Split_Failover_Targets (Target, Single_Targets);
            if Single_Targets.Is_Empty then
               raise Logic_Error with "Empty failover group is not allowed.";
            end if;

            if Message_Handler = null then
               --  this is a one-way message, but with failover
               --  the one-way messages are implicitly
               --  forced to wait for transmission

               --  Dummy_Outgoing_Message is an artificial message object
               --  used to wait for transmission,
               --  since one-way message have no such possibility on their own

               Effective_Message_Handler :=
                 Dummy_Outgoing_Message'Unchecked_Access;

               Wait_For_Transmission := True;
            else
               --  with failover the two-way messages are implicitly
               --  forced to wait for completion

               Effective_Message_Handler := Message_Handler;

               Wait_For_Reply_Or_Reject := True;
            end if;

            --  try failover targets one by one until success

            C := Single_Targets.First;
            while String_Lists.Has_Element (C) loop
               begin
                  Send_To_Single_Target
                    (The_Agent,
                     String_Lists.Element (C), --  subsequent target
                     Header,
                     Content,
                     Message_Id,
                     Effective_Message_Handler,
                     Priority,
                     Auto_Connect,
                     Wait_For_Transmission,
                     Wait_For_Reply_Or_Reject);

                  --  in the absence of exceptions
                  --  consider this message to be successfully sent

                  Success := True;
                  exit;

               exception
                  when E : others =>
                     Ada.Exceptions.Save_Occurrence (Last_Exception, E);
               end;

               String_Lists.Next (C);
            end loop;

            if not Success then
               Ada.Exceptions.Reraise_Occurrence (Last_Exception);
            end if;

         end;
      else
         --  the target is of a single type (not failover)
         Send_To_Single_Target
           (The_Agent,
            Target,
            Header,
            Content,
            Message_Id,
            Message_Handler,
            Priority, Auto_Connect);
      end if;

   end Send;

   procedure Send_Reply
     (Core_Agent : in out Core.Agents.Agent;
      Core_Connection_Handler :
        in Details.Core_Connection_Handlers.Handler_Access;
      Source : in String;
      Message_Id : in Parameters.YAMI_Long_Long_Integer;
      Reply_Content : in Serializables.Serializable'Class;
      Priority : Natural) is

      Header : Parameters.Parameters_Collection := Parameters.Make_Parameters;
      Channel : Core.Channel_Descriptor;
      Created_New : Boolean;

   begin
      Header.Set_String ("type", "reply");
      Header.Set_Long_Long ("message_id", Message_Id);

      Core_Agent.Open (Source, Channel, Created_New);

      if Created_New then
         Core_Connection_Handler.all.Report_New_Outgoing_Connection (Source);
      end if;

      Core_Agent.Post (Channel, Header, Reply_Content, Priority);
   end Send_Reply;

   procedure Send_Reject
     (Core_Agent : in out Core.Agents.Agent;
      Core_Connection_Handler :
        in Details.Core_Connection_Handlers.Handler_Access;
      Source : in String;
      Message_Id : in Parameters.YAMI_Long_Long_Integer;
      Reason : in String;
      Priority : Natural) is

      Header : Parameters.Parameters_Collection := Parameters.Make_Parameters;
      Channel : Core.Channel_Descriptor;
      Created_New : Boolean;

   begin
      Header.Set_String ("type", "exception");
      Header.Set_Long_Long ("message_id", Message_Id);
      Header.Set_String ("reason", Reason);

      Core_Agent.Open (Source, Channel, Created_New);

      if Created_New then
         Core_Connection_Handler.all.Report_New_Outgoing_Connection (Source);
      end if;

      Core_Agent.Post (Channel, Header, Empty_Parameters, Priority);
   end Send_Reject;

end YAMI.Agents.Helpers;
