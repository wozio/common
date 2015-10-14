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

with YAMI.Agents;
with YAMI.Incoming_Messages;
with YAMI.Outgoing_Messages;
with YAMI.Parameters;
with YAMI.Value_Publishers;

with Ada.Strings.Unbounded;

--  Note: this test cannot be easily implemented "properly",
--  because it also checks that the message was *not* received.
--  This is done with artificial sleep calls with the assumption
--  that the interaction between agents awlays takes place
--  within the period of 1s.

procedure Subscription_Test is

   Local_Listener : constant String := "tcp://*:*";

   --  simple subscribe and unsubscribe
   --  (this test corresponds to test1 in C++)
   package Test_1 is

      type Incoming_Message_Handler is
        new YAMI.Incoming_Messages.Message_Handler with record
           Got_Update : Boolean := False;
      end record;

      overriding
      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class);

      procedure Run;

   end Test_1;

   package body Test_1 is
      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class) is
      begin
         H.Got_Update := True;

         pragma Assert (Message.Message_Name = "subscription_update");
      end Call;

      My_Update_Handler : aliased Incoming_Message_Handler;

      procedure Run is
         Publisher_Agent : aliased YAMI.Agents.Agent :=
           YAMI.Agents.Make_Agent;
         Subscriber_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Publisher_Address : String (1 .. YAMI.Agents.Max_Target_Length);
         Publisher_Address_Last : Natural;

         Value : YAMI.Value_Publishers.Value_Publisher :=
           YAMI.Value_Publishers.Make_Value_Publisher;

         Params : YAMI.Parameters.Parameters_Collection :=
           YAMI.Parameters.Make_Parameters;
         Dummy : YAMI.Parameters.Parameters_Collection :=
           YAMI.Parameters.Make_Parameters;

         Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

         Num : Natural;

         Subscribers_Processed : Boolean;
         Destination : Ada.Strings.Unbounded.Unbounded_String;

         procedure Process_Subscribers
           (Destination_Target : in String;
            Destination_Object : in String) is
         begin
            Subscribers_Processed := True;
            Destination :=
              Ada.Strings.Unbounded.To_Unbounded_String (Destination_Object);
         end Process_Subscribers;

         use type Ada.Strings.Unbounded.Unbounded_String;

      begin
         --  set up the publisher side

         Publisher_Agent.Add_Listener
           (Local_Listener, Publisher_Address, Publisher_Address_Last);

         Value.Register_At (Publisher_Agent'Unchecked_Access, "my_value");

         --  no subscribers yet
         Value.Number_Of_Subscribers (Num);
         pragma Assert (Num = 0);

         Subscribers_Processed := False;
         Value.Iterate_Subscribers_Info (Process_Subscribers'Access);
         pragma Assert (not Subscribers_Processed);

         --  set up the subscriber side

         Subscriber_Agent.Register_Object
           ("my_update_handler", My_Update_Handler'Unchecked_Access);

         --  subscribe

         Params.Set_String ("destination_object", "my_update_handler");

         Subscriber_Agent.Send
           (Publisher_Address (1 .. Publisher_Address_Last),
            "my_value", "subscribe", Params, Msg'Unchecked_Access);

         Msg.Wait_For_Completion;

         --  there should be one subscriber, as seen at the publisher side

         Value.Number_Of_Subscribers (Num);
         pragma Assert (Num = 1);

         Subscribers_Processed := False;
         Value.Iterate_Subscribers_Info (Process_Subscribers'Access);
         pragma Assert (Subscribers_Processed);
         pragma Assert (Destination = "my_update_handler");

         --  publish some value

         Value.Publish (Dummy);

         --  check if the listener got it

         delay 1.0;

         pragma Assert (My_Update_Handler.Got_Update);

         --  unsubscribe

         Subscriber_Agent.Send
           (Publisher_Address (1 .. Publisher_Address_Last),
            "my_value", "unsubscribe", Msg'Unchecked_Access);

         Msg.Wait_For_Completion;

         --  there should be no subscribers

         Value.Number_Of_Subscribers (Num);
         pragma Assert (Num = 0);

         Subscribers_Processed := False;
         Value.Iterate_Subscribers_Info (Process_Subscribers'Access);
         pragma Assert (not Subscribers_Processed);

         --  check that the updates do not arrive any longer

         My_Update_Handler.Got_Update := False;

         Value.Publish (Dummy);

         delay 1.0;

         pragma Assert (not My_Update_Handler.Got_Update);
      end Run;
   end Test_1;

   --  dispatch of unknown commands
   --  (this test corresponds to test2 in C++)
   package Test_2 is

      type Unknown_Command_Handler is
        new YAMI.Incoming_Messages.Message_Handler with record
           Got_Unknown : Boolean := False;
      end record;

      overriding
      procedure Call
        (H : in out Unknown_Command_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class);

      procedure Run;

   end Test_2;

   package body Test_2 is
      procedure Call
        (H : in out Unknown_Command_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class) is
      begin
         H.Got_Unknown := True;

         pragma Assert (Message.Message_Name = "unknown");

         Message.Reply;
      end Call;

      My_Unknown_Handler : aliased Unknown_Command_Handler;

      procedure Run is
         Publisher_Agent : aliased YAMI.Agents.Agent :=
           YAMI.Agents.Make_Agent;
         Subscriber_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Publisher_Address : String (1 .. YAMI.Agents.Max_Target_Length);
         Publisher_Address_Last : Natural;

         Value : YAMI.Value_Publishers.Value_Publisher :=
           YAMI.Value_Publishers.Make_Value_Publisher
           (Command_Handler => My_Unknown_Handler'Unchecked_Access,
            Max_Queue_Length => 1,
            Overflow_Handler => null);

         Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

      begin
         --  set up the publisher side

         Publisher_Agent.Add_Listener
           (Local_Listener, Publisher_Address, Publisher_Address_Last);

         Value.Register_At (Publisher_Agent'Unchecked_Access, "my_value");

         --  send unknown command

         Subscriber_Agent.Send
           (Publisher_Address (1 .. Publisher_Address_Last),
            "my_value", "unknown", Msg'Unchecked_Access);

         Msg.Wait_For_Completion;

         pragma Assert (My_Unknown_Handler.Got_Unknown);
      end Run;
   end Test_2;

begin
   Test_1.Run;
   Test_2.Run;
end Subscription_Test;
