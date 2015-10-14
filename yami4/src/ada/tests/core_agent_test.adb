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

--with Ada.Text_IO;
with Ada.Streams;
with Ada.Strings.Unbounded;
with YAMI.Core.Agents;
with YAMI.Core.Closed_Connection_Handlers;
with YAMI.Core.Incoming_Message_Handlers;
with YAMI.Core.Message_Progress_Handlers;
with YAMI.Core.New_Connection_Handlers;
with YAMI.Parameters;

procedure Core_Agent_Test is

   --  This is a simple messaging test
   --  with explicit close on the client side.
   --  This test corresponds to core/message_test1.cpp.
   package Test_1 is

      type New_Connection_Handler is
        new YAMI.Core.New_Connection_Handlers.Handler with null record;

      type Closed_Connection_Handler is
        new YAMI.Core.Closed_Connection_Handlers.Handler with null record;

      type Message_Progress_Handler is
        new YAMI.Core.Message_Progress_Handlers.Handler with null record;

      type Incoming_Message_Handler is
        new YAMI.Core.Incoming_Message_Handlers.Handler with null record;

      overriding
      procedure New_Connection (H : in out New_Connection_Handler;
                                Source : in String;
                                Channel : in YAMI.Core.Channel_Descriptor);

      overriding
      procedure Closed_Connection (H : in out Closed_Connection_Handler;
                                   Source : in String);

      overriding
      procedure Progress
        (H : in out Message_Progress_Handler;
         Sent_Bytes : in Ada.Streams.Stream_Element_Count;
         Total_Byte_Count : in Ada.Streams.Stream_Element_Count);

      overriding
      procedure Call
        (H : in out Incoming_Message_Handler;
         Source : in String;
         Header_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor;
         Body_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor);

      procedure Run;

   end Test_1;

   package body Test_1 is

      Client_Target : String (1 .. YAMI.Core.Agents.Max_Target_Length);
      Client_Target_Last : Natural;

      New_Connection_Called : Boolean := False;
      pragma Volatile (New_Connection_Called);

      procedure New_Connection (H : in out New_Connection_Handler;
                                Source : in String;
                                Channel : in YAMI.Core.Channel_Descriptor) is
      begin
         Client_Target_Last := Source'Length;
         Client_Target (1 .. Client_Target_Last) := Source;
         New_Connection_Called := True;
      end New_Connection;

      Closed_Connection_Called : Boolean := False;
      pragma Volatile (Closed_Connection_Called);

      procedure Closed_Connection (H : in out Closed_Connection_Handler;
                                   Source : in String) is
      begin
         pragma Assert (Source = Client_Target (1 .. Client_Target_Last));
         Closed_Connection_Called := True;
      end Closed_Connection;

      Progress_Called : Boolean := False;
      pragma Volatile (Progress_Called);

      procedure Progress
        (H : in out Message_Progress_Handler;
         Sent_Bytes : in Ada.Streams.Stream_Element_Count;
         Total_Byte_Count : in Ada.Streams.Stream_Element_Count) is

         use type Ada.Streams.Stream_Element_Count;

      begin
         --  assume that the whole message fits in a single frame
         --  and that the whole frame can be pushed as a single I/O operation
         pragma Assert (Sent_Bytes = Total_Byte_Count);
         pragma Assert (Sent_Bytes > 0);
         Progress_Called := True;
      end Progress;

      Incoming_Header : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;
      Incoming_Body : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;

      Call_Called : Boolean := False;
      pragma Volatile (Call_Called);

      procedure Call
        (H : in out Incoming_Message_Handler;
         Source : in String;
         Header_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor;
         Body_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor) is

      begin
         pragma Assert (Source = Client_Target (1 .. Client_Target_Last));
         Incoming_Header.Deserialize (Header_Buffers);
         Incoming_Body.Deserialize (Body_Buffers);
         Call_Called := True;
      end Call;

      My_New_Connection_Handler : aliased New_Connection_Handler;
      My_Closed_Connection_Handler : aliased Closed_Connection_Handler;
      My_Message_Progress_Handler : aliased Message_Progress_Handler;
      My_Incoming_Message_Handler : aliased Incoming_Message_Handler;

      procedure Run is
         Server_Agent : YAMI.Core.Agents.Agent := YAMI.Core.Agents.Make_Agent
           (My_Incoming_Message_Handler'Unchecked_Access,
            My_Closed_Connection_Handler'Unchecked_Access);

         Resolved_Target : String (1 .. YAMI.Core.Agents.Max_Target_Length);
         Resolved_Target_Last : Natural;

         Timed_Out : Boolean;

      begin
         --  there is nothing to do at the server side for the moment:
         Server_Agent.Do_Some_Work (0.0, Timed_Out);
         pragma Assert (Timed_Out);

         --  create listener

         Server_Agent.Add_Listener
           ("tcp://localhost:*", Resolved_Target, Resolved_Target_Last,
           My_New_Connection_Handler'Unchecked_Access);

         --  the listener has been added and the selector was interrupted
         --  so that the worker will report normal outcome
         Server_Agent.Do_Some_Work (0.0, Timed_Out);
         pragma Assert (not Timed_Out);

         --  create client agent
         declare
            Client_Agent : YAMI.Core.Agents.Agent :=
              YAMI.Core.Agents.Make_Agent
              (null,  --  no incoming message handler
               null); --  no disconnection handler

            Channel : YAMI.Core.Channel_Descriptor;

            Outgoing_Header : YAMI.Parameters.Parameters_Collection :=
              YAMI.Parameters.Make_Parameters;
            Outgoing_Body : YAMI.Parameters.Parameters_Collection :=
              YAMI.Parameters.Make_Parameters;

            Created_New : Boolean;

            use type YAMI.Parameters.Count_Type;

         begin
            --  open new connection
            Client_Agent.Open
              (Resolved_Target (1 .. Resolved_Target_Last),
               Channel, Created_New);

            pragma Assert (Created_New);

            --  the new connection should be visible at the server side
            pragma Assert (not New_Connection_Called);
            Server_Agent.Do_Some_Work (0.0, Timed_Out);
            pragma Assert (not Timed_Out);
            pragma Assert (New_Connection_Called);

            --  prepare the message header and body
            Outgoing_Body.Set_String ("greetings", "Hello YAMI4");

            --  post the message
            Client_Agent.Post
              (Channel, Outgoing_Header, Outgoing_Body, 0,
               My_Message_Progress_Handler'Unchecked_Access);

            --  do the work on the client side to actually push the frame
            pragma Assert (not Progress_Called);
            Client_Agent.Do_Some_Work (0.0, Timed_Out);
            pragma Assert (not Timed_Out);
            pragma Assert (Progress_Called);  -- progress has been reported

            --  do the work on the server side to receive the message
            pragma Assert (not Call_Called);
            while not Call_Called loop
               Server_Agent.Do_Some_Work (1000.0, Timed_Out);
               pragma Assert (not Timed_Out);
            end loop;
            --  message has been seen by server-side code at this point

            --  verify that the data arrived properly
            pragma Assert (Incoming_Header.Length = 0);
            pragma Assert (Incoming_Body.Length = 1);
            pragma Assert (Incoming_Body.Get_String ("greetings") =
                             "Hello YAMI4");

            --  explicit close of the client connection
            --  (this action is immediate)
            Client_Agent.Close (Channel);

            --  server needs to "do work"
            --  in order to discover closed connection
            pragma Assert (not Closed_Connection_Called);
            Server_Agent.Do_Some_Work (1000.0, Timed_Out);
            pragma Assert (not Timed_Out);
            pragma Assert (Closed_Connection_Called);

         end;
      end Run;
   end Test_1;

   --  This is a simple messaging test with implicit (stored in the queue)
   --  close request on the client side.
   --  Unnecessary callbacks are ommitted.
   --  Otherwise the test is identical to Test_1
   --  and corresponds to core/message_test2.cpp.
   package Test_2 is

      type Client_Closed_Connection_Handler is
        new YAMI.Core.Closed_Connection_Handlers.Handler with null record;

      type Server_Closed_Connection_Handler is
        new YAMI.Core.Closed_Connection_Handlers.Handler with null record;

      type Incoming_Message_Handler is
        new YAMI.Core.Incoming_Message_Handlers.Handler with null record;

      overriding
      procedure Closed_Connection
        (H : in out Client_Closed_Connection_Handler;
         Source : in String);

      overriding
      procedure Closed_Connection
        (H : in out Server_Closed_Connection_Handler;
         Source : in String);

      overriding
      procedure Call
        (H : in out Incoming_Message_Handler;
         Source : in String;
         Header_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor;
         Body_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor);

      procedure Run;

   end Test_2;

   package body Test_2 is

      Server_Target : String (1 .. YAMI.Core.Agents.Max_Target_Length);
      Server_Target_Last : Natural;

      Client_Closed_Connection_Called : Boolean := False;
      pragma Volatile (Client_Closed_Connection_Called);

      procedure Closed_Connection
        (H : in out Client_Closed_Connection_Handler;
         Source : in String) is
      begin
         pragma Assert (Source = Server_Target (1 .. Server_Target_Last));
         Client_Closed_Connection_Called := True;
      end Closed_Connection;

      Server_Closed_Connection_Called : Boolean := False;
      pragma Volatile (Server_Closed_Connection_Called);

      procedure Closed_Connection
        (H : in out Server_Closed_Connection_Handler;
         Source : in String) is
      begin
         Server_Closed_Connection_Called := True;
      end Closed_Connection;

      Incoming_Header : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;
      Incoming_Body : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;

      Call_Called : Boolean := False;
      pragma Volatile (Call_Called);

      procedure Call
        (H : in out Incoming_Message_Handler;
         Source : in String;
         Header_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor;
         Body_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor) is

      begin
         Incoming_Header.Deserialize (Header_Buffers);
         Incoming_Body.Deserialize (Body_Buffers);
         Call_Called := True;
      end Call;

      My_Client_Closed_Connection_Handler :
        aliased Client_Closed_Connection_Handler;
      My_Server_Closed_Connection_Handler :
        aliased Server_Closed_Connection_Handler;
      My_Incoming_Message_Handler :
        aliased Incoming_Message_Handler;

      procedure Run is
         Server_Agent : YAMI.Core.Agents.Agent := YAMI.Core.Agents.Make_Agent
           (My_Incoming_Message_Handler'Unchecked_Access,
            My_Server_Closed_Connection_Handler'Unchecked_Access);

         Timed_Out : Boolean;

      begin
         --  there is nothing to do at the server side for the moment:
         Server_Agent.Do_Some_Work (0.0, Timed_Out);
         pragma Assert (Timed_Out);

         --  create listener

         Server_Agent.Add_Listener
           ("tcp://localhost:*", Server_Target, Server_Target_Last);

         --  the listener has been added and the selector was interrupted
         --  so that the worker will report normal outcome
         Server_Agent.Do_Some_Work (0.0, Timed_Out);
         pragma Assert (not Timed_Out);

         --  create client agent
         declare
            Client_Agent : YAMI.Core.Agents.Agent :=
              YAMI.Core.Agents.Make_Agent
              (null,  --  no incoming message handler
               My_Client_Closed_Connection_Handler'Unchecked_Access);

            Channel : YAMI.Core.Channel_Descriptor;

            Outgoing_Header : YAMI.Parameters.Parameters_Collection :=
              YAMI.Parameters.Make_Parameters;
            Outgoing_Body : YAMI.Parameters.Parameters_Collection :=
              YAMI.Parameters.Make_Parameters;

            Created_New : Boolean;

            use type YAMI.Parameters.Count_Type;

         begin
            --  open new connection
            Client_Agent.Open
              (Server_Target (1 .. Server_Target_Last), Channel, Created_New);

            pragma Assert (Created_New);

            --  do the work to accept the connection at the server side
            Server_Agent.Do_Some_Work (0.0, Timed_Out);
            pragma Assert (not Timed_Out);

            --  prepare the message header and body
            Outgoing_Body.Set_String ("greetings", "Hello YAMI4");

            --  post the message
            Client_Agent.Post (Channel, Outgoing_Header, Outgoing_Body);

            --  post also the close request (it will be queued)
            Client_Agent.Close (Channel);

            --  do the work on the client side to actually push the frame
            Client_Agent.Do_Some_Work (0.0, Timed_Out);
            pragma Assert (not Timed_Out);

            --  do the work on the server side to receive the message
            pragma Assert (not Call_Called);
            while not Call_Called loop
               Server_Agent.Do_Some_Work (1000.0, Timed_Out);
               pragma Assert (not Timed_Out);
            end loop;
            --  message has been seen by server-side code at this point

            --  verify that the data arrived properly
            pragma Assert (Incoming_Header.Length = 0);
            pragma Assert (Incoming_Body.Length = 1);
            pragma Assert (Incoming_Body.Get_String ("greetings") =
                             "Hello YAMI4");

            --  no explicit close on the client side,
            --  "doing work" is necessary to actually process
            --  the pending close request
            --  (the close request will be taken from the queue
            --  and the channel will be closed)

            pragma Assert (not Client_Closed_Connection_Called);
            Client_Agent.Do_Some_Work (0.0, Timed_Out);
            pragma Assert (not Timed_Out);
            pragma Assert (Client_Closed_Connection_Called);

            --  server needs to "do work"
            --  in order to discover closed connection
            pragma Assert (not Server_Closed_Connection_Called);
            Server_Agent.Do_Some_Work (1000.0, Timed_Out);
            pragma Assert (not Timed_Out);
            pragma Assert (Server_Closed_Connection_Called);

         end;
      end Run;
   end Test_2;

   --  This is a test for message with the same priorities.
   --  One bigger message is posted then small messsage with the same priority
   --  is posted after that.
   --  Both client and server parts expect the long message to be
   --  pushed/arrived first.
   --  This test corresponds to core/message-test3.cpp.
   package Test_3 is

      type Short_Message_Progress_Handler is
        new YAMI.Core.Message_Progress_Handlers.Handler with null record;

      type Long_Message_Progress_Handler is
        new YAMI.Core.Message_Progress_Handlers.Handler with null record;

      type Incoming_Message_Handler is
        new YAMI.Core.Incoming_Message_Handlers.Handler with null record;

      overriding
      procedure Progress
        (H : in out Short_Message_Progress_Handler;
         Sent_Bytes : in Ada.Streams.Stream_Element_Count;
         Total_Byte_Count : in Ada.Streams.Stream_Element_Count);

      overriding
      procedure Progress
        (H : in out Long_Message_Progress_Handler;
         Sent_Bytes : in Ada.Streams.Stream_Element_Count;
         Total_Byte_Count : in Ada.Streams.Stream_Element_Count);

      overriding
      procedure Call
        (H : in out Incoming_Message_Handler;
         Source : in String;
         Header_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor;
         Body_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor);

      procedure Run;

   end Test_3;

   package body Test_3 is

      Server_Target : String (1 .. YAMI.Core.Agents.Max_Target_Length);
      Server_Target_Last : Natural;

      Short_Progress_Called : Boolean := False;
      Long_Progress_Called : Boolean := False;
      pragma Volatile (Short_Progress_Called);
      pragma Volatile (Long_Progress_Called);

      procedure Progress
        (H : in out Short_Message_Progress_Handler;
         Sent_Bytes : in Ada.Streams.Stream_Element_Count;
         Total_Byte_Count : in Ada.Streams.Stream_Element_Count) is

         use type Ada.Streams.Stream_Element_Count;

      begin
         if Sent_Bytes = Total_Byte_Count and Sent_Bytes > 0 then
            --  make sure that the long message is pushed first
            pragma Assert (Long_Progress_Called);
            Short_Progress_Called := True;
         end if;
      end Progress;

      procedure Progress
        (H : in out Long_Message_Progress_Handler;
         Sent_Bytes : in Ada.Streams.Stream_Element_Count;
         Total_Byte_Count : in Ada.Streams.Stream_Element_Count) is

         use type Ada.Streams.Stream_Element_Count;

      begin
         if Sent_Bytes = Total_Byte_Count and Sent_Bytes > 0 then
            --  make sure that the long message is pushed first
            pragma Assert (not Short_Progress_Called);
            Long_Progress_Called := True;
         end if;
      end Progress;

      Short_Message_Received : Boolean := False;
      Long_Message_Received : Boolean := False;
      pragma Volatile (Short_Message_Received);
      pragma Volatile (Long_Message_Received);

      Short_Content : Ada.Strings.Unbounded.Unbounded_String;
      Long_Content : Ada.Strings.Unbounded.Unbounded_String;

      procedure Call
        (H : in out Incoming_Message_Handler;
         Source : in String;
         Header_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor;
         Body_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor) is

         Message_Header : YAMI.Parameters.Parameters_Collection :=
           YAMI.Parameters.Make_Parameters;
         Message_Body : YAMI.Parameters.Parameters_Collection :=
           YAMI.Parameters.Make_Parameters;

         use type YAMI.Parameters.Count_Type;

      begin
         Message_Header.Deserialize (Header_Buffers);
         Message_Body.Deserialize (Body_Buffers);

         pragma Assert (Message_Header.Length = 1);

         declare
            Message_Type : String :=
              Message_Header.Get_String ("message-type");

            use type Ada.Strings.Unbounded.Unbounded_String;

         begin
            if Message_Type = "short" then
               --  this is a short message

               --  the long one was already received
               pragma Assert (Long_Message_Received);
               pragma Assert
                 (Message_Body.Get_String ("content") = Short_Content);

               Short_Message_Received := True;
            elsif Message_Type = "long" then
               --  this is a long message

               --  the short one was not yet received
               pragma Assert (not Short_Message_Received);
               pragma Assert
                 (Message_Body.Get_String ("content") = Long_Content);

               Long_Message_Received := True;
            else
               --  nothing else should ever arrive here
               pragma Assert (False);
               null;
            end if;
         end;
      end Call;

      My_Long_Message_Progress_Handler :
        aliased Long_Message_Progress_Handler;
      My_Short_Message_Progress_Handler :
        aliased Short_Message_Progress_Handler;
      My_Incoming_Message_Handler :
        aliased Incoming_Message_Handler;

      procedure Run is
         Server_Agent : YAMI.Core.Agents.Agent := YAMI.Core.Agents.Make_Agent
           (My_Incoming_Message_Handler'Unchecked_Access);

         Timed_Out : Boolean;

      begin

         --  prepare short and long content
         Short_Content := Ada.Strings.Unbounded.To_Unbounded_String ("abc");
         for I in 0 .. 10_000 loop
            Ada.Strings.Unbounded.Append (Long_Content, "abc");
         end loop;

         --  create listener

         Server_Agent.Add_Listener
           ("tcp://localhost:*", Server_Target, Server_Target_Last);

         --  create client agent
         declare
            Client_Agent : YAMI.Core.Agents.Agent :=
              YAMI.Core.Agents.Make_Agent (null);

            Channel : YAMI.Core.Channel_Descriptor;

            Short_Header : YAMI.Parameters.Parameters_Collection :=
              YAMI.Parameters.Make_Parameters;
            Short_Body : YAMI.Parameters.Parameters_Collection :=
              YAMI.Parameters.Make_Parameters;
            Long_Header : YAMI.Parameters.Parameters_Collection :=
              YAMI.Parameters.Make_Parameters;
            Long_Body : YAMI.Parameters.Parameters_Collection :=
              YAMI.Parameters.Make_Parameters;

            Created_New : Boolean;

         begin
            --  open new connection
            Client_Agent.Open
              (Server_Target (1 .. Server_Target_Last), Channel, Created_New);

            pragma Assert (Created_New);

            --  prepare the long message header and body
            --  and post it first

            Long_Header.Set_String ("message-type", "long");
            Long_Body.Set_String
              ("content", Ada.Strings.Unbounded.To_String (Long_Content));

            Client_Agent.Post
              (Channel, Long_Header, Long_Body,
               0, --  whatever priority
               My_Long_Message_Progress_Handler'Unchecked_Access);

            --  prepare the short message header and body
            --  and post it after the first one

            Short_Header.Set_String ("message-type", "short");
            Short_Body.Set_String
              ("content", Ada.Strings.Unbounded.To_String (Short_Content));

            Client_Agent.Post
              (Channel, Short_Header, Short_Body,
               0, --  the same priority
               My_Short_Message_Progress_Handler'Unchecked_Access);

            --  do the work on both client and server side so that they can
            --  pass the messages properly
            --  (the pending work at the server side related to
            --  listener registration or accepting new connection
            --  is also performed in this loop)
            while not Short_Message_Received or not Long_Message_Received loop
               Client_Agent.Do_Some_Work (0.001, Timed_Out);
               Server_Agent.Do_Some_Work (0.001, Timed_Out);
            end loop;

            pragma Assert (Short_Message_Received);
            pragma Assert (Long_Message_Received);

         end;
      end Run;
   end Test_3;

   --  This is a test for message priorities.
   --  One bigger message is posted then small messsage with higher priority
   --  is posted after that.
   --  Both client and server parts expect the short (high-priority)
   --  message to be pushed/arrived first.
   --  This test corresponds to core/message-test4.cpp.
   package Test_4 is

      type Short_Message_Progress_Handler is
        new YAMI.Core.Message_Progress_Handlers.Handler with null record;

      type Long_Message_Progress_Handler is
        new YAMI.Core.Message_Progress_Handlers.Handler with null record;

      type Incoming_Message_Handler is
        new YAMI.Core.Incoming_Message_Handlers.Handler with null record;

      overriding
      procedure Progress
        (H : in out Short_Message_Progress_Handler;
         Sent_Bytes : in Ada.Streams.Stream_Element_Count;
         Total_Byte_Count : in Ada.Streams.Stream_Element_Count);

      overriding
      procedure Progress
        (H : in out Long_Message_Progress_Handler;
         Sent_Bytes : in Ada.Streams.Stream_Element_Count;
         Total_Byte_Count : in Ada.Streams.Stream_Element_Count);

      overriding
      procedure Call
        (H : in out Incoming_Message_Handler;
         Source : in String;
         Header_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor;
         Body_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor);

      procedure Run;

   end Test_4;

   package body Test_4 is

      Server_Target : String (1 .. YAMI.Core.Agents.Max_Target_Length);
      Server_Target_Last : Natural;

      Short_Progress_Called : Boolean := False;
      Long_Progress_Called : Boolean := False;
      pragma Volatile (Short_Progress_Called);
      pragma Volatile (Long_Progress_Called);

      procedure Progress
        (H : in out Short_Message_Progress_Handler;
         Sent_Bytes : in Ada.Streams.Stream_Element_Count;
         Total_Byte_Count : in Ada.Streams.Stream_Element_Count) is

         use type Ada.Streams.Stream_Element_Count;

      begin
         if Sent_Bytes = Total_Byte_Count and Sent_Bytes > 0 then
            --  make sure that the short message is pushed first
            pragma Assert (not Long_Progress_Called);
            Short_Progress_Called := True;
         end if;
      end Progress;

      procedure Progress
        (H : in out Long_Message_Progress_Handler;
         Sent_Bytes : in Ada.Streams.Stream_Element_Count;
         Total_Byte_Count : in Ada.Streams.Stream_Element_Count) is

         use type Ada.Streams.Stream_Element_Count;

      begin
         if Sent_Bytes = Total_Byte_Count and Sent_Bytes > 0 then
            --  make sure that the short message is pushed first
            pragma Assert (Short_Progress_Called);
            Long_Progress_Called := True;
         end if;
      end Progress;

      Short_Message_Received : Boolean := False;
      Long_Message_Received : Boolean := False;
      pragma Volatile (Short_Message_Received);
      pragma Volatile (Long_Message_Received);

      Short_Content : Ada.Strings.Unbounded.Unbounded_String;
      Long_Content : Ada.Strings.Unbounded.Unbounded_String;

      procedure Call
        (H : in out Incoming_Message_Handler;
         Source : in String;
         Header_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor;
         Body_Buffers : in YAMI.Core.Serialization_Buffers_Descriptor) is

         Message_Header : YAMI.Parameters.Parameters_Collection :=
           YAMI.Parameters.Make_Parameters;
         Message_Body : YAMI.Parameters.Parameters_Collection :=
           YAMI.Parameters.Make_Parameters;

         use type YAMI.Parameters.Count_Type;

      begin
         Message_Header.Deserialize (Header_Buffers);
         Message_Body.Deserialize (Body_Buffers);

         pragma Assert (Message_Header.Length = 1);

         declare
            Message_Type : String :=
              Message_Header.Get_String ("message-type");

            use type Ada.Strings.Unbounded.Unbounded_String;

         begin
            if Message_Type = "short" then
               --  this is a short message

               --  the long one was not yet received
               pragma Assert (not Long_Message_Received);
               pragma Assert
                 (Message_Body.Get_String ("content") = Short_Content);

               Short_Message_Received := True;
            elsif Message_Type = "long" then
               --  this is a long message

               --  the short one was received before
               pragma Assert (Short_Message_Received);
               pragma Assert
                 (Message_Body.Get_String ("content") = Long_Content);

               Long_Message_Received := True;
            else
               --  nothing else should ever arrive here
               pragma Assert (False);
               null;
            end if;
         end;
      end Call;

      My_Long_Message_Progress_Handler :
        aliased Long_Message_Progress_Handler;
      My_Short_Message_Progress_Handler :
        aliased Short_Message_Progress_Handler;
      My_Incoming_Message_Handler :
        aliased Incoming_Message_Handler;

      procedure Run is
         Server_Agent : YAMI.Core.Agents.Agent := YAMI.Core.Agents.Make_Agent
           (My_Incoming_Message_Handler'Unchecked_Access);

         Timed_Out : Boolean;

      begin

         --  prepare short and long content
         Short_Content := Ada.Strings.Unbounded.To_Unbounded_String ("abc");
         for I in 0 .. 10_000 loop
            Ada.Strings.Unbounded.Append (Long_Content, "abc");
         end loop;

         --  create listener

         Server_Agent.Add_Listener
           ("tcp://localhost:*", Server_Target, Server_Target_Last);

         --  create client agent
         declare
            Client_Agent : YAMI.Core.Agents.Agent :=
              YAMI.Core.Agents.Make_Agent (null);

            Channel : YAMI.Core.Channel_Descriptor;

            Short_Header : YAMI.Parameters.Parameters_Collection :=
              YAMI.Parameters.Make_Parameters;
            Short_Body : YAMI.Parameters.Parameters_Collection :=
              YAMI.Parameters.Make_Parameters;
            Long_Header : YAMI.Parameters.Parameters_Collection :=
              YAMI.Parameters.Make_Parameters;
            Long_Body : YAMI.Parameters.Parameters_Collection :=
              YAMI.Parameters.Make_Parameters;

            Created_New : Boolean;

         begin
            --  open new connection
            Client_Agent.Open
              (Server_Target (1 .. Server_Target_Last), Channel, Created_New);

            pragma Assert (Created_New);

            --  prepare the long message header and body
            --  and post it first

            Long_Header.Set_String ("message-type", "long");
            Long_Body.Set_String
              ("content", Ada.Strings.Unbounded.To_String (Long_Content));

            Client_Agent.Post
              (Channel, Long_Header, Long_Body,
               0, --  low priority
               My_Long_Message_Progress_Handler'Unchecked_Access);

            --  prepare the short message header and body
            --  and post it after the first one

            Short_Header.Set_String ("message-type", "short");
            Short_Body.Set_String
              ("content", Ada.Strings.Unbounded.To_String (Short_Content));

            Client_Agent.Post
              (Channel, Short_Header, Short_Body,
               1, --  high priority
               My_Short_Message_Progress_Handler'Unchecked_Access);

            --  do the work on both client and server side so that they can
            --  pass the messages properly
            --  (the pending work at the server side related to
            --  listener registration or accepting new connection
            --  is also performed in this loop)
            while not Short_Message_Received or not Long_Message_Received loop
               Client_Agent.Do_Some_Work (0.001, Timed_Out);
               Server_Agent.Do_Some_Work (0.001, Timed_Out);
            end loop;

            pragma Assert (Short_Message_Received);
            pragma Assert (Long_Message_Received);

         end;
      end Run;
   end Test_4;

begin
   Test_1.Run;
   Test_2.Run;
   Test_3.Run;
   Test_4.Run;
end Core_Agent_Test;
