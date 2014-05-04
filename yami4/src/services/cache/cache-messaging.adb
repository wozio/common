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

with Cache.State;
with IAL.Tasking.Starter;
with Log;
with YAMI.Core.Agents;
with YAMI.Core.Incoming_Message_Handlers;
with YAMI.Parameters;
with YAMI.Serializables;

--  TODO: workaround for GNAT GPL 2010 bug, to be removed
with YAMI.Raw_Buffer_Data_Sources;

with Ada.Exceptions;

pragma Elaborate_All (IAL.Tasking.Starter);
pragma Elaborate_All (YAMI.Core.Agents);

package body Cache.Messaging is

   Agent : YAMI.Core.Agents.Agent;
   Empty_Parameters : YAMI.Parameters.Parameters_Collection;

   type Incoming_Handler is
     new YAMI.Core.Incoming_Message_Handlers.Handler with null record;

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
     Msg_Body : YAMI.Parameters.Parameters_Collection;

     Incoming_Msg_Id  : YAMI.Parameters.YAMI_Long_Long_Integer;

     procedure Reply (Reply_Body : in YAMI.Serializables.Serializable'Class)
     is
        Reply_Header : YAMI.Parameters.Parameters_Collection;
     begin
        YAMI.Parameters.Init_Parameters (Reply_Header);

        Reply_Header.Set_String ("type", "reply");
        Reply_Header.Set_Long_Long ("message_id", Incoming_Msg_Id);

        Agent.Post (Source, Reply_Header, Reply_Body);
     end Reply;

     --  TODO: workaround for GNAT GPL 2010 bug, to be removed
     procedure Reply_Raw
       (Reply_Body : in YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source)
     is
        Reply_Header : YAMI.Parameters.Parameters_Collection;
     begin
        YAMI.Parameters.Init_Parameters (Reply_Header);

        Reply_Header.Set_String ("type", "reply");
        Reply_Header.Set_Long_Long ("message_id", Incoming_Msg_Id);

        Agent.Post_Raw_Body (Source, Reply_Header, Reply_Body);
     end Reply_Raw;

     procedure Reject (Reason : in String) is
        Reject_Header : YAMI.Parameters.Parameters_Collection;
     begin
        YAMI.Parameters.Init_Parameters (Reject_Header);

        Log.Put (Messages, "    rejected message from " &
                   Source & " " & Reason);

        Reject_Header.Set_String ("type", "exception");
        Reject_Header.Set_Long_Long ("message_id", Incoming_Msg_Id);
        Reject_Header.Set_String ("reason", Reason);

        Agent.Post (Source, Reject_Header, Empty_Parameters);
     end Reject;

     procedure Process_Set (Key : in String) is
        Accepted : Boolean;
     begin
        Log.Put (Messages, "set from " & Source & " for " & Key);

        Cache.State.Set (Key, Body_Buffers, Accepted);

        if Accepted then
           Reply (Empty_Parameters);
        else
           Reject ("Space limit exceeded.");
        end if;
     end Process_Set;

     procedure Process_Get (Key : in String) is
        Found : Boolean;
     begin
        Log.Put (Messages, "get from " & Source & " for " & Key);

        --  TODO: workaround for GNAT GPL 2010 bug, to be removed
        --  original version:
        --  Cache.State.Find_And_Process (Key, Found, Reply'Access);
        Cache.State.Find_And_Process_Raw (Key, Found, Reply_Raw'Access);

        if not Found then
           Reject ("No such value.");
           Log.Put (Messages, "    value not found");
        end if;
     end Process_Get;

     procedure Process_Delete (Key : in String) is
     begin
        Log.Put (Messages, "delete from " & Source & " for " & Key);

        Cache.State.Delete (Key);

        Reply (Empty_Parameters);
     end Process_Delete;

   begin
      YAMI.Parameters.Init_Parameters (Header);
      Header.Deserialize (Header_Buffers);

      YAMI.Parameters.Init_Parameters (Msg_Body);
      Msg_Body.Deserialize (Body_Buffers);

      declare
         Msg_Type : constant String := Header.Get_String ("type");
      begin
         if Msg_Type = "message" then
            declare
               Object_Name : constant String :=
                 Header.Get_String ("object_name");
               Message_Name : constant String :=
                 Header.Get_String ("message_name");
            begin
               Incoming_Msg_Id := Header.Get_Long_Long ("message_id");

               if Message_Name = "set" then
                  Process_Set (Object_Name);
               elsif Message_Name = "get" then
                  Process_Get (Object_Name);
               elsif Message_Name = "delete" then
                  Process_Delete (Object_Name);
               end if;
            end;
         end if;
      exception
         when E : others =>
            Log.Put (Messages,
                     "    error: " & Ada.Exceptions.Exception_Message (E));
            Reject (Ada.Exceptions.Exception_Message (E));
      end;
   end Call;

   --
   --  The worker task, which is a main driver of the server's activity.
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
         Log.Put (Main, "exception in worker " &
                    Ada.Exceptions.Exception_Message (E));
   end Worker;

   procedure Init (Listener : in String) is

      Resolved_Target : String (1 .. YAMI.Core.Agents.Max_Target_Length);
      Resolved_Target_Last : Natural;
   begin
      YAMI.Parameters.Init_Parameters (Empty_Parameters);

      YAMI.Core.Agents.Init_Agent (Agent, Msg_Handler'Access);

      Agent.Add_Listener (Listener, Resolved_Target, Resolved_Target_Last);
      Log.Put (Main, "listening on " &
                 Resolved_Target (1 .. Resolved_Target_Last));

      IAL.Tasking.Starter.Start;
      Log.Put (Main, "cache started");
   end Init;

end Cache.Messaging;
