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
with YAMI.Connection_Event_Handlers;
with YAMI.Incoming_Messages;
with YAMI.Option_Names;
with YAMI.Outgoing_Messages;
with YAMI.Parameters;
with YAMI.Raw_Buffer_Data_Sources;
with YAMI.Serializables;

with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings.Unbounded;

procedure Agent_Test is

   Local_Listener : constant String := "tcp://*:*";

   --  attempt to send a message to non-existing agent
   --  (this test corresponds to test1 in C++)
   package Test_1 is
      procedure Run;
   end Test_1;

   package body Test_1 is
      procedure Run is
         Client_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
      begin
         begin
            Client_Agent.Send_One_Way ("tcp://nosuchaddress:12345",
                                       "nosuchobject", "badmessage");
            pragma Assert (False);
         exception
            when E : others =>
               pragma Assert
                 (Ada.Exceptions.Exception_Message (E) = "I/O error.");
               null;
         end;

         begin
            --  a bit dodgy, but 4 is an unassigned port in the list
            --  of well-known services, so there is a chance that
            --  no existing process uses it on the machine where this test
            --  is executed
            --  - if this test fails then it is a sign that some process
            --  has a listening socket on port 4 - pick another dummy number

            Client_Agent.Send_One_Way ("tcp://localhost:4",
                                       "nosuchobject", "badmessage");
         exception
            when E : others =>
               pragma Assert
                 (Ada.Exceptions.Exception_Message (E) = "I/O error.");
               null;
         end;
      end Run;
   end Test_1;

   --  message sent to nonexisting object
   --  (this test corresponds to test2 in C++)
   package Test_2 is
      procedure Run;
   end Test_2;

   package body Test_2 is
      procedure Run is
         Server_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Client_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Server_Address : String (1 .. YAMI.Agents.Max_Target_Length);
         Server_Address_Last : Natural;

         Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

         use type YAMI.Outgoing_Messages.Message_State;
      begin
         Server_Agent.Add_Listener
           (Local_Listener, Server_Address, Server_Address_Last);

         --  one-way message does not report any error
         Client_Agent.Send_One_Way
           (Server_Address (1 .. Server_Address_Last),
            "nosuchobject", "badmessage");

         --  two-way message is rejected
         Client_Agent.Send
           (Server_Address (1 .. Server_Address_Last),
            "nosuchobject", "badmessage",
            Msg'Unchecked_Access);

         Msg.Wait_For_Completion;

         pragma Assert (Msg.State = YAMI.Outgoing_Messages.Rejected);
         pragma Assert
           (Msg.Exception_Message = "Unknown destination object.");
      end Run;
   end Test_2;

   --  message sent to nonexisting object, explicit connection management
   --  (this test corresponds to test2a in C++)
   package Test_2a is
      procedure Run;
   end Test_2a;

   package body Test_2a is
      procedure Run is
         Server_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Client_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Server_Address : String (1 .. YAMI.Agents.Max_Target_Length);
         Server_Address_Last : Natural;

         Auto_Connect : constant Boolean := False;

         Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

         use type YAMI.Outgoing_Messages.Message_State;
      begin
         Server_Agent.Add_Listener
           (Local_Listener, Server_Address, Server_Address_Last);

         --  message fails if there is no channel and no auto-connect
         begin
            Client_Agent.Send
              (Server_Address (1 .. Server_Address_Last),
               "nosuchobject", "badmessage",
               Msg'Unchecked_Access, 0, Auto_Connect);

            pragma Assert (False);
         exception
            when E : others =>
               pragma Assert
                 (Ada.Exceptions.Exception_Message (E) = "I/O error.");
               null;
         end;

         --  explicitly open the channel

         Client_Agent.Open_Connection (Server_Address (1 .. Server_Address_Last));

         --  message is successfully sent over existing channel,
         --  but later rejected by server

         Client_Agent.Send
           (Server_Address (1 .. Server_Address_Last),
            "nosuchobject", "badmessage",
            Msg'Unchecked_Access, 0, Auto_Connect);

         Msg.Wait_For_Completion;

         pragma Assert (Msg.State = YAMI.Outgoing_Messages.Rejected);
         pragma Assert
           (Msg.Exception_Message = "Unknown destination object.");
      end Run;
   end Test_2a;

   Object_Name : constant String := "object";
   Message_Name : constant String := "message";

   --  message sent and replied to
   --  (this test corresponds to test3 in C++)
   package Test_3 is

      type Incoming_Message_Handler is
        new YAMI.Incoming_Messages.Message_Handler with record
           Got_Message : Boolean := False;
      end record;

      overriding
      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class);

      procedure Run;

   end Test_3;

   package body Test_3 is
      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class) is

         --  server-side processing of message content
         procedure Process
           (Content : in out YAMI.Parameters.Parameters_Collection) is
            use type YAMI.Parameters.Count_Type;
         begin
            pragma Assert (Content.Length = 1);
            pragma Assert (Content.Get_String ("value") = "ping");

            Content.Set_String ("value", "pong");
            Message.Reply (Content);
         end Process;
      begin
         H.Got_Message := True;

         pragma Assert (Message.Object_Name = Object_Name);
         pragma Assert (Message.Message_Name = Message_Name);

         Message.Process_Content (Process'Access);
      end Call;

      My_Incoming_Message_Handler : aliased Incoming_Message_Handler;

      procedure Run is
         Server_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Client_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Server_Address : String (1 .. YAMI.Agents.Max_Target_Length);
         Server_Address_Last : Natural;

         Content : YAMI.Parameters.Parameters_Collection :=
           YAMI.Parameters.Make_Parameters;
         Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

         State : YAMI.Outgoing_Messages.Message_State;
         Sent_Bytes : Ada.Streams.Stream_Element_Count;
         Total_Byte_Count : Ada.Streams.Stream_Element_Count;

         --  client-side processing of reply content
         procedure Process
           (Content : in out YAMI.Parameters.Parameters_Collection) is
         begin
            pragma Assert (Content.Get_String ("value") = "pong");
            null;
         end Process;

         use type YAMI.Outgoing_Messages.Message_State;
         use type Ada.Streams.Stream_Element_Count;
      begin
         Server_Agent.Add_Listener
           (Local_Listener, Server_Address, Server_Address_Last);
         Server_Agent.Register_Object
           (Object_Name, My_Incoming_Message_Handler'Unchecked_Access);

         Content.Set_String ("value", "ping");

         Client_Agent.Send
           (Server_Address (1 .. Server_Address_Last),
            Object_Name, Message_Name, Content, Msg'Unchecked_Access);

         Msg.Wait_For_Transmission;

         --  after transmission the whole message is pushed out
         Msg.Get_State (State, Sent_Bytes, Total_Byte_Count);

         pragma Assert (Sent_Bytes = Total_Byte_Count);

         Msg.Wait_For_Completion;

         pragma Assert (My_Incoming_Message_Handler.Got_Message);

         pragma Assert (Msg.State = YAMI.Outgoing_Messages.Replied);

         Msg.Process_Reply_Content (Process'Access);
      end Run;
   end Test_3;

   --  message rejected by server
   --  (this test corresponds to test4 in C++)
   package Test_4 is

      type Incoming_Message_Handler is
        new YAMI.Incoming_Messages.Message_Handler with record
           Got_Message : Boolean := False;
      end record;

      overriding
      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class);

      procedure Run;

   end Test_4;

   package body Test_4 is
      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class) is

         --  server-side processing of message content
         procedure Process
           (Content : in out YAMI.Parameters.Parameters_Collection) is
            use type YAMI.Parameters.Count_Type;
         begin
            pragma Assert (Content.Length = 0);

            Message.Reject ("some reason");
         end Process;
      begin
         H.Got_Message := True;
         Message.Process_Content (Process'Access);
      end Call;

      My_Incoming_Message_Handler : aliased Incoming_Message_Handler;

      procedure Run is
         Server_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Client_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Server_Address : String (1 .. YAMI.Agents.Max_Target_Length);
         Server_Address_Last : Natural;

         Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

         --  client-side processing of reply content
         procedure Process
           (Content : in out YAMI.Parameters.Parameters_Collection) is
         begin
            pragma Assert (Content.Get_String ("value") = "pong");
            null;
         end Process;

         use type YAMI.Outgoing_Messages.Message_State;
      begin
         Server_Agent.Add_Listener
           (Local_Listener, Server_Address, Server_Address_Last);
         Server_Agent.Register_Object
           (Object_Name, My_Incoming_Message_Handler'Unchecked_Access);

         Client_Agent.Send
           (Server_Address (1 .. Server_Address_Last),
            Object_Name, Message_Name, Msg'Unchecked_Access);

         Msg.Wait_For_Completion;

         pragma Assert (My_Incoming_Message_Handler.Got_Message);

         pragma Assert (Msg.State = YAMI.Outgoing_Messages.Rejected);
         pragma Assert (Msg.Exception_Message = "some reason");
      end Run;
   end Test_4;

   --  message rejected due to exception in user code at the server side
   --  (this test corresponds to test5 in C++)
   package Test_5 is

      type Incoming_Message_Handler is
        new YAMI.Incoming_Messages.Message_Handler with record
           Got_Message : Boolean := False;
      end record;

      overriding
      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class);

      procedure Run;

   end Test_5;

   package body Test_5 is
      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class) is

         Some_Exception : exception;
      begin
         H.Got_Message := True;

         raise Some_Exception with "something bad happened";
      end Call;

      My_Incoming_Message_Handler : aliased Incoming_Message_Handler;

      procedure Run is
         Server_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Client_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Server_Address : String (1 .. YAMI.Agents.Max_Target_Length);
         Server_Address_Last : Natural;

         Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

         use type YAMI.Outgoing_Messages.Message_State;
      begin
         Server_Agent.Add_Listener
           (Local_Listener, Server_Address, Server_Address_Last);
         Server_Agent.Register_Object
           (Object_Name, My_Incoming_Message_Handler'Unchecked_Access);

         Client_Agent.Send
           (Server_Address (1 .. Server_Address_Last),
            Object_Name, Message_Name, Msg'Unchecked_Access);

         Msg.Wait_For_Completion;

         pragma Assert (My_Incoming_Message_Handler.Got_Message);

         pragma Assert (Msg.State = YAMI.Outgoing_Messages.Rejected);
         pragma Assert (Msg.Exception_Message = "something bad happened");
      end Run;
   end Test_5;

   --  big messages sent with different priorities
   --  (this test corresponds to test5 in C++)
   package Test_6 is

      Num_Of_Messages : constant := 10;

      Size_Of_Big_String : constant := 1_000_000;

      type Got_Messages_Array is
        array (Positive range 1 .. Num_Of_Messages)
        of Boolean;

      type Incoming_Message_Handler is
        new YAMI.Incoming_Messages.Message_Handler with record
           Got_Messages : Got_Messages_Array := (others => False);
      end record;

      overriding
      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class);

      procedure Run;

   end Test_6;

   package body Test_6 is

      Big_String : access String :=
        new String'(1 .. Size_Of_Big_String => 'x');

      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class) is

         --  server-side processing of message content
         procedure Process
           (Content : in out YAMI.Parameters.Parameters_Collection) is
            Id : constant YAMI.Parameters.YAMI_Integer :=
              Content.Get_Integer ("id");
         begin
            H.Got_Messages (Natural (Id)) := True;

            --  verify the big value
            pragma Assert (Content.Get_String ("big") = Big_String.all);
         end Process;
      begin
         Message.Process_Content (Process'Access);
         Message.Reply;
      end Call;

      My_Incoming_Message_Handler : aliased Incoming_Message_Handler;

      --  Note:
      --  The messages are sent with different priorities, which means
      --  that they might complete in the order that is different from the
      --  order of posting them to the outgoing queue.
      --  The messages are posted with increasing priorities (first message
      --  is sent with lowest priority, last message with highest),
      --  so it is *very likely* that they will be received by server
      --  in the reversed order, but this cannot be guaranteed as there is
      --  no relation between the speed of posting and the speed
      --  of transmission.

      procedure Run is
         Server_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Client_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Server_Address : String (1 .. YAMI.Agents.Max_Target_Length);
         Server_Address_Last : Natural;

         Content : YAMI.Parameters.Parameters_Collection :=
           YAMI.Parameters.Make_Parameters;

         type Message_Array is
           array (Positive range 1 .. Num_Of_Messages)
           of aliased YAMI.Outgoing_Messages.Outgoing_Message;
         Msgs : Message_Array;

         use type YAMI.Outgoing_Messages.Message_State;
      begin
         Server_Agent.Add_Listener
           (Local_Listener, Server_Address, Server_Address_Last);
         Server_Agent.Register_Object
           (Object_Name, My_Incoming_Message_Handler'Unchecked_Access);

         Content.Set_String ("big", Big_String.all);

         --  send all messages with different ids and priorities
         for I in 1 .. Num_Of_Messages loop
            declare
               Id : constant YAMI.Parameters.YAMI_Integer :=
                 YAMI.Parameters.YAMI_Integer (I);
               Priority : constant Natural := I;
            begin
               Content.Set_Integer ("id", Id);

               Client_Agent.Send
                 (Server_Address (1 .. Server_Address_Last),
                  Object_Name, Message_Name, Content,
                  Msgs (I)'Unchecked_Access, Priority);
            end;
         end loop;

         --  wait for all messages to complete
         for I in 1 .. Num_Of_Messages loop
            Msgs (I).Wait_For_Completion;
            pragma Assert (Msgs (I).State = YAMI.Outgoing_Messages.Replied);
         end loop;

         for I in 1 .. Num_Of_Messages loop
            pragma Assert (My_Incoming_Message_Handler.Got_Messages (I));
            null;
         end loop;
      end Run;
   end Test_6;

   --  message sent to load-balanced pair of destinations
   --  (this test corresponds to test7 in C++)
   package Test_7 is

      type Incoming_Message_Handler is
        new YAMI.Incoming_Messages.Message_Handler with record
           Got_Message : Boolean := False;
      end record;

      overriding
      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class);

      procedure Run;

   end Test_7;

   package body Test_7 is
      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class) is
      begin
         H.Got_Message := True;
         Message.Reply;
      end Call;

      My_Incoming_Message_Handler_1 : aliased Incoming_Message_Handler;
      My_Incoming_Message_Handler_2 : aliased Incoming_Message_Handler;

      procedure Run is
         Server_Agent_1 : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Server_Agent_2 : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Server_Address_1 : String (1 .. YAMI.Agents.Max_Target_Length);
         Server_Address_Last_1 : Natural;
         Server_Address_2 : String (1 .. YAMI.Agents.Max_Target_Length);
         Server_Address_Last_2 : Natural;

         Client_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;

         Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

         use type YAMI.Outgoing_Messages.Message_State;
      begin
         Server_Agent_1.Add_Listener
           (Local_Listener, Server_Address_1, Server_Address_Last_1);
         Server_Agent_2.Add_Listener
           (Local_Listener, Server_Address_2, Server_Address_Last_2);

         declare
            Load_Balanced_Target : constant String :=
              "failover:(" &
              Server_Address_1 (1 .. Server_Address_Last_1) &
              "|" &
              Server_Address_2 (1 .. Server_Address_Last_2) &
              ")";
         begin

            Server_Agent_1.Register_Object
              (Object_Name, My_Incoming_Message_Handler_1'Unchecked_Access);
            Server_Agent_2.Register_Object
              (Object_Name, My_Incoming_Message_Handler_2'Unchecked_Access);

            Client_Agent.Send
              (Load_Balanced_Target,
               Object_Name, Message_Name, Msg'Unchecked_Access);

            --  since this is a load-balanced (and failover) target,
            --  the message is implicitly waited for completion

            pragma Assert (Msg.State = YAMI.Outgoing_Messages.Replied);

            --  exactly one of two servers got the message
            pragma Assert
              (My_Incoming_Message_Handler_1.Got_Message xor
               My_Incoming_Message_Handler_2.Got_Message);
         end;
      end Run;
   end Test_7;

   --  message sent to failover pair of destinations
   --  (this test corresponds to test8 in C++)
   package Test_8 is

      type Incoming_Message_Handler is
        new YAMI.Incoming_Messages.Message_Handler with record
           Got_Message : Boolean := False;
      end record;

      overriding
      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class);

      procedure Run;

   end Test_8;

   package body Test_8 is
      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class) is
      begin
         H.Got_Message := True;
         Message.Reply;
      end Call;

      My_Incoming_Message_Handler_1 : aliased Incoming_Message_Handler;

      procedure Run is
         Server_Agent_1 : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Server_Address_1 : String (1 .. YAMI.Agents.Max_Target_Length);
         Server_Address_Last_1 : Natural;

         Client_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;

         Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

         use type YAMI.Outgoing_Messages.Message_State;
      begin
         Server_Agent_1.Add_Listener
           (Local_Listener, Server_Address_1, Server_Address_Last_1);

         declare
            Failover_Target : constant String :=
              "failover:(" &
              Server_Address_1 (1 .. Server_Address_Last_1) &
              "|" &
              "tcp://nosuchhost:4" &
              ")";
         begin

            Server_Agent_1.Register_Object
              (Object_Name, My_Incoming_Message_Handler_1'Unchecked_Access);

            Client_Agent.Send
              (Failover_Target,
               Object_Name, Message_Name, Msg'Unchecked_Access);

            --  since this is a load-balanced (and failover) target,
            --  the message is implicitly waited for completion

            pragma Assert (Msg.State = YAMI.Outgoing_Messages.Replied);

            --  the working server in the failover pair got the message
            pragma Assert (My_Incoming_Message_Handler_1.Got_Message);
         end;
      end Run;
   end Test_8;

   --  empty failover group is an error
   --  (this test corresponds to test9 in C++)
   package Test_9 is
      procedure Run;
   end Test_9;

   package body Test_9 is

      procedure Run is
         Client_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
      begin
         Client_Agent.Send_One_Way ("failover:()", Object_Name, Message_Name);

         pragma Assert (False);
      exception
         when E : others =>
            pragma Assert (Ada.Exceptions.Exception_Message (E) =
                             "Empty failover group is not allowed.");
            null;
      end Run;
   end Test_9;

   --  raw binary message and reply
   --  (this test corresponds to test10 in C++)
   package Test_10 is

      type Incoming_Message_Handler is
        new YAMI.Incoming_Messages.Message_Handler with record
           Got_Message : Boolean := False;
      end record;

      overriding
      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class);

      procedure Run;

   end Test_10;

   package body Test_10 is

      Message_Content : aliased YAMI.Serializables.Serialization_Buffer :=
        (Size => 12, Buffer =>
           (Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('w')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('s')),
            Ada.Streams.Stream_Element (Character'Pos ('s')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('g')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('e'))));

      Message_Content_List :
        constant YAMI.Serializables.Serialization_Buffer_List :=
        (1 => Message_Content'Unchecked_Access);

      Reply_Content : aliased YAMI.Serializables.Serialization_Buffer :=
        (Size => 8, Buffer =>
           (Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('w')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('p')),
            Ada.Streams.Stream_Element (Character'Pos ('l'))));

      Reply_Content_List :
        constant YAMI.Serializables.Serialization_Buffer_List :=
        (1 => Reply_Content'Unchecked_Access);

      procedure Call
        (H : in out Incoming_Message_Handler;
         Message : in out YAMI.Incoming_Messages.Incoming_Message'Class) is

         --  server-side processing of message content
         procedure Process
           (Raw_Content : in YAMI.Serializables.Serialization_Buffer_List)
         is
            Reply_Wrapper :
              YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source :=
              YAMI.Raw_Buffer_Data_Sources.Make_Raw_Buffer_Data_Source
              (Reply_Content_List);

            use type YAMI.Serializables.Serialization_Buffer;

         begin
            pragma Assert (Raw_Content (1).all = Message_Content);

            Message.Reply (Reply_Wrapper);
         end Process;
      begin
         H.Got_Message := True;

         Message.Process_Raw_Content (Process'Access);
      end Call;

      My_Incoming_Message_Handler : aliased Incoming_Message_Handler;

      procedure Run is
         Options : YAMI.Parameters.Parameters_Collection :=
           YAMI.Parameters.Make_Parameters;
      begin
         Options.Set_Boolean (YAMI.Option_Names.Deliver_As_Raw_Binary, True);
         declare
            Server_Agent : YAMI.Agents.Agent :=
              YAMI.Agents.Make_Agent (Options);
            Client_Agent : YAMI.Agents.Agent :=
              YAMI.Agents.Make_Agent (Options);

            Server_Address : String (1 .. YAMI.Agents.Max_Target_Length);
            Server_Address_Last : Natural;

            Message_Wrapper :
              YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source :=
              YAMI.Raw_Buffer_Data_Sources.Make_Raw_Buffer_Data_Source
              (Message_Content_List);

            Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

            --  client-side processing of reply content
            procedure Process
              (Raw_Content : in YAMI.Serializables.Serialization_Buffer_List)
            is
               use type YAMI.Serializables.Serialization_Buffer;
            begin
               pragma Assert (Raw_Content (1).all = Reply_Content);
               null;
            end Process;

         begin
            Server_Agent.Add_Listener
              (Local_Listener, Server_Address, Server_Address_Last);
            Server_Agent.Register_Object
              (Object_Name, My_Incoming_Message_Handler'Unchecked_Access);

            Client_Agent.Send
              (Server_Address (1 .. Server_Address_Last),
               Object_Name, Message_Name, Message_Wrapper,
               Msg'Unchecked_Access);

            Msg.Wait_For_Completion;

            pragma Assert (My_Incoming_Message_Handler.Got_Message);

            Msg.Process_Raw_Reply_Content (Process'Access);
         end;
      end Run;
   end Test_10;

   --  connection event notifications
   --  (this test corresponds to test11 in C++)
   package Test_11 is

      --  user callback type
      type Connection_Event_Handler is
        new YAMI.Connection_Event_Handlers.Handler with record
           Events : Ada.Strings.Unbounded.Unbounded_String;
      end record;

      overriding
      procedure Report
        (H : in out Connection_Event_Handler;
         Name : in String;
         Event : in YAMI.Connection_Event_Handlers.Connection_Event);

      procedure Run;
   end Test_11;

   package body Test_11 is

      procedure Report
        (H : in out Connection_Event_Handler;
         Name : in String;
         Event : in YAMI.Connection_Event_Handlers.Connection_Event) is
      begin
         case Event is
            when YAMI.Connection_Event_Handlers.New_Incoming_Connection =>
               Ada.Strings.Unbounded.Append (H.Events, "incoming ");
            when YAMI.Connection_Event_Handlers.New_Outgoing_Connection =>
               Ada.Strings.Unbounded.Append (H.Events, "outgoing ");
            when YAMI.Connection_Event_Handlers.Connection_Closed =>
               Ada.Strings.Unbounded.Append (H.Events, "closed ");
         end case;
      end Report;

      procedure Run is
         Server_Event_Handler : aliased Connection_Event_Handler;
         Server_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;

         Client_Event_Handler : aliased Connection_Event_Handler;
         Client_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;

         Server_Address : String (1 .. YAMI.Agents.Max_Target_Length);
         Server_Address_Last : Natural;

         Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

         use type Ada.Strings.Unbounded.Unbounded_String;

      begin
         Server_Agent.Register_Connection_Event_Monitor
           (Server_Event_Handler'Unchecked_Access);
         Client_Agent.Register_Connection_Event_Monitor
           (Client_Event_Handler'Unchecked_Access);

         Server_Agent.Add_Listener
           (Local_Listener, Server_Address, Server_Address_Last);

         --  no communication yet -> no connection
         pragma Assert (Server_Event_Handler.Events = "");
         pragma Assert (Client_Event_Handler.Events = "");

         Client_Agent.Send
           (Server_Address (1 .. Server_Address_Last),
            "no_such_object", "hello",
            Msg'Unchecked_Access);
         Msg.Wait_For_Completion;

         --  one connection open
         pragma Assert (Server_Event_Handler.Events = "incoming ");
         pragma Assert (Client_Event_Handler.Events = "outgoing ");

         Client_Agent.Close_Connection
           (Server_Address (1 .. Server_Address_Last));

         --  one connection open and one closed
         --  but it is a race for both the client and the server
         pragma Assert (Server_Event_Handler.Events = "incoming " or
                        Server_Event_Handler.Events = "incoming closed ");
         pragma Assert (Client_Event_Handler.Events = "outgoing " or
                        Client_Event_Handler.Events = "outgoing closed ");
      end Run;
   end Test_11;

   --  frame size border conditions - messages with all possible lengths
   --  (this test corresponds to test12 in C++)
   package Test_12 is
      procedure Run;
   end Test_12;

   package body Test_12 is
      procedure Run is
         Server_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Client_Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;
         Server_Address : String (1 .. YAMI.Agents.Max_Target_Length);
         Server_Address_Last : Natural;

         Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

         Max_String_Size : constant := 10_000;

         Params : YAMI.Parameters.Parameters_Collection :=
           YAMI.Parameters.Make_Parameters;

         use type YAMI.Outgoing_Messages.Message_State;
      begin
         Server_Agent.Add_Listener
           (Local_Listener, Server_Address, Server_Address_Last);

         for String_Size in 1 .. Max_String_Size loop
            declare
               S : String (1 .. String_Size) := (others => 'x');
            begin
               pragma Assert (S'Length = String_Size);

               Params.Set_String ("value", S);

               Client_Agent.Send
                 (Server_Address (1 .. Server_Address_Last),
                  "nosuchobject", "hello", Params,
                  Msg'Unchecked_Access);

               Msg.Wait_For_Completion;

               pragma Assert (Msg.State = YAMI.Outgoing_Messages.Rejected);
            end;
         end loop;
      end Run;
   end Test_12;

begin
   Test_1.Run;
   Test_2.Run;
   Test_2a.Run;
   Test_3.Run;
   Test_4.Run;
   Test_5.Run;
   Test_6.Run;
   Test_7.Run;
   Test_8.Run;
   Test_9.Run;
   Test_10.Run;
   Test_11.Run;
   Test_12.Run;
end Agent_Test;
