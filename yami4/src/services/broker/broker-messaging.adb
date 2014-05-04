--  Copyright Maciej Sobczak 2008-2014.
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

with Broker.Ids;
with Broker.Routing;
with IAL.Tasking.Starter;
with Log;
with YAMI.Core.Agents;
with YAMI.Core.Incoming_Message_Handlers;
with YAMI.Core.Message_Progress_Handlers;
with YAMI.Parameters;
with YAMI.Raw_Buffer_Data_Sources;

with Ada.Exceptions;

pragma Elaborate_All (IAL.Tasking.Starter);
pragma Elaborate_All (YAMI.Core.Agents);

package body Broker.Messaging is

   Agent : YAMI.Core.Agents.Agent;
   Empty_Parameters : YAMI.Parameters.Parameters_Collection;

   Incoming_Counter : YAMI.Parameters.YAMI_Long_Long_Integer := 0;
   Outgoing_Counter : YAMI.Parameters.YAMI_Long_Long_Integer := 0;

   type Incoming_Handler is
     new YAMI.Core.Incoming_Message_Handlers.Handler with record
        Subscription_Overflow_Policy : Overflow_Policy;
        Warmed : Boolean := False;
        pragma Atomic (Warmed);
   end record;

   overriding
   procedure Call
     (H : in out Incoming_Handler;
      Source : in String;
      Header_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor;
      Body_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor);

   Msg_Handler : aliased Incoming_Handler;

   overriding
   procedure Call
     (H : in out Incoming_Handler;
      Source : in String;
      Header_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor;
      Body_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor) is

     Header : YAMI.Parameters.Parameters_Collection;

     Incoming_Msg_Id  : YAMI.Parameters.YAMI_Long_Long_Integer;

     procedure Confirm (Module : in Broker_Module) is
        Confirmation_Header : YAMI.Parameters.Parameters_Collection;
     begin
        YAMI.Parameters.Init_Parameters (Confirmation_Header);

        Confirmation_Header.Set_String ("type", "reply");
        Confirmation_Header.Set_Long_Long ("message_id", Incoming_Msg_Id);

        Agent.Post (Source, Confirmation_Header, Empty_Parameters);

        Log.Put (Module, "    confirmed to " & Source);
     end Confirm;

     procedure Reject (Reason : in String) is
        Reject_Header : YAMI.Parameters.Parameters_Collection;
     begin
        Log.Put (Messages, "rejected message from " & Source);

        YAMI.Parameters.Init_Parameters (Reject_Header);

        Reject_Header.Set_String ("type", "exception");
        Reject_Header.Set_Long_Long ("message_id", Incoming_Msg_Id);
        Reject_Header.Set_String ("reason", Reason);

        Agent.Post (Source, Reject_Header, Empty_Parameters);
     end Reject;

     procedure Process_Publish (Tags : in String;
                                Requires_Confirmation : in Boolean) is

        Update_Header : YAMI.Parameters.Parameters_Collection;

        Raw_Body_Source :
          YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source (0);

        use type YAMI.Parameters.YAMI_Long_Long_Integer;

        procedure Forward_To_Single_Subscriber
          (Target_Object : in String;
           Target_Location : in String;
           Progress_Handler : in
           YAMI.Core.Message_Progress_Handlers.Handler_Access) is
        begin
           Log.Put (Messages, "    routing to " & Target_Location);

           Outgoing_Counter := Outgoing_Counter + 1;

           if Target_Object = "*" then
              --  this is a forwarding subscription
              --  use original object name (list of targets)
              --  and "publish" as the message name

              Update_Header.Set_String ("object_name", Tags);
              Update_Header.Set_String ("message_name", "publish");

              --  also, make sure that the forwarding channel exists

              Agent.Open (Target_Location);

           else
              --  this is a regular subscription

              Update_Header.Set_String ("object_name", Target_Object);
              Update_Header.Set_String ("message_name", "update " & Tags);
           end if;

           Update_Header.Set_Long_Long ("message_id", Ids.Next_Message_Id);

           Agent.Post_Raw_Body
             (Target_Location, Update_Header, Raw_Body_Source,
              0, Progress_Handler);
        end Forward_To_Single_Subscriber;

        Overflow : Boolean;

     begin
        Log.Put (Messages,
                 "processing " & Tags & " from " & Source);

        Incoming_Counter := Incoming_Counter + 1;

        YAMI.Parameters.Init_Parameters (Update_Header);

        Update_Header.Set_String ("type", "message");

        YAMI.Raw_Buffer_Data_Sources.Init_Raw_Buffer_Data_Source
          (Raw_Body_Source, Body_Buffers);

        Routing.Iterate_Matching_Subscriptions
          (Tags,
           Forward_To_Single_Subscriber'Access,
           Overflow);

        if Requires_Confirmation then
           if Overflow and
             H.Subscription_Overflow_Policy = Reject_Message then

              Reject ("Message queue overflow");
           else
              Confirm (Messages);
           end if;
        end if;
     end Process_Publish;

     procedure Process_Subscribe (Tags : in String) is
        Params : YAMI.Parameters.Parameters_Collection;
     begin
        YAMI.Parameters.Init_Parameters (Params);
        Params.Deserialize (Body_Buffers);

        Routing.Subscribe (Tags,
                           Params.Get_String ("destination_object"),
                           Source);

        Confirm (Broker.Subscriptions);
     end Process_Subscribe;

     procedure Process_Stats (Include_Details : in Boolean) is
        Stats_Header : YAMI.Parameters.Parameters_Collection;
        Params : YAMI.Parameters.Parameters_Collection;
     begin
        YAMI.Parameters.Init_Parameters (Stats_Header);
        YAMI.Parameters.Init_Parameters (Params);

        Stats_Header.Set_String ("type", "reply");
        Stats_Header.Set_Long_Long ("message_id", Incoming_Msg_Id);

        Params.Set_Long_Long ("total_incoming", Incoming_Counter);
        Params.Set_Long_Long ("total_outgoing", Outgoing_Counter);

        if Include_Details then
           Routing.Fill_Detailed_Stats (Params);
        end if;

        Agent.Post (Source, Stats_Header, Params);
     end Process_Stats;

     use type YAMI.Parameters.Parameter_Type;

   begin
      YAMI.Parameters.Init_Parameters (Header);
      Header.Deserialize (Header_Buffers);

      declare
         Msg_Type : constant String := Header.Get_String ("type");
         Requires_Confirmation : Boolean := False;
      begin
         if Msg_Type = "message" then
            declare
               Object_Name : constant String :=
                 Header.Get_String ("object_name");
               Message_Name : constant String :=
                 Header.Get_String ("message_name");
            begin
               Incoming_Msg_Id := Header.Get_Long_Long ("message_id");

               if Message_Name = "publish" then
                  if H.Warmed then
                     --  treat object name as a list of tags
                     --  this message does not require confirmation

                     Process_Publish (Object_Name, Requires_Confirmation);
                  end if;
               elsif Message_Name = "publish_confirm" then
                  if H.Warmed then
                     --  treat object name as a list of tags
                     --  this message requires confirmation

                     Requires_Confirmation := True;
                     Process_Publish (Object_Name, Requires_Confirmation);
                  else
                     Reject ("Broker warming up.");
                  end if;
               elsif Message_Name = "subscribe" then
                  --  treat object name as a list of tags

                  Requires_Confirmation := True;
                  Process_Subscribe (Object_Name);
               elsif Object_Name = "stats" then
                  if Message_Name = "get" then
                     Process_Stats (False);
                  elsif Message_Name = "get_details" then
                     Process_Stats (True);
                  end if;
               end if;
            end;
         end if;
      exception
         when E : others =>
            Log.Put (Messages,
                     "    error: " & Ada.Exceptions.Exception_Message (E));
            if Requires_Confirmation then
               Reject (Ada.Exceptions.Exception_Message (E));
            end if;
      end;
   end Call;

   --
   --  The worker task, which is a main driver of the broker's activity.
   --
   task Worker;
   task body Worker is
      Dummy_Unit_Timeout : constant Duration := 10.0; --  arbitrary
      Dummy_Timed_Out : Boolean;
   begin
      IAL.Tasking.Starter.Wait_Until_Started;

      loop
         begin
            Agent.Do_Some_Work (Dummy_Unit_Timeout, Dummy_Timed_Out);
         exception
            when E : YAMI.Runtime_Error =>
               --  I/O errors can be ignored here as they are normal
               --  when sending data to failing targets
               --  otherwise the exception from the worker thread
               --  indicates something serious

               if Ada.Exceptions.Exception_Message (E) /= "I/O error." then
                  raise;
               end if;
         end;
      end loop;
   exception
      when E : others =>
         Log.Put (Broker.Main, "exception in worker " &
                    Ada.Exceptions.Exception_Message (E));
   end Worker;

   procedure Init (Listener : in String;
                   Subscription_Overflow_Policy : in Overflow_Policy) is

      Resolved_Target : String (1 .. YAMI.Core.Agents.Max_Target_Length);
      Resolved_Target_Last : Natural;
   begin
      YAMI.Parameters.Init_Parameters (Empty_Parameters);

      YAMI.Core.Agents.Init_Agent (Agent, Msg_Handler'Access);

      Agent.Add_Listener (Listener, Resolved_Target, Resolved_Target_Last);
      Log.Put (Broker.Main, "listening on " &
                 Resolved_Target (1 .. Resolved_Target_Last));

      Msg_Handler.Subscription_Overflow_Policy :=
        Subscription_Overflow_Policy;

      IAL.Tasking.Starter.Start;
      Log.Put (Broker.Main, "started in the warm-up mode");
   end Init;

   procedure Allow_Incoming is
   begin
      Msg_Handler.Warmed := True;
      Log.Put (Broker.Main, "fully initialized");
   end Allow_Incoming;

end Broker.Messaging;
