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

with Name_Server.Cache;
with Name_Server.Storage;
with IAL.Tasking.Starter;
with Log;
with YAMI.Core.Agents;
with YAMI.Core.Incoming_Message_Handlers;
with YAMI.Parameters;

with Ada.Exceptions;

pragma Elaborate_All (IAL.Tasking.Starter);
pragma Elaborate_All (YAMI.Core.Agents);

package body Name_Server.Messaging is

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

     Object_Field_Name : constant String := "object";
     Location_Field_Name : constant String := "location";

     Header : YAMI.Parameters.Parameters_Collection;
     Msg_Body : YAMI.Parameters.Parameters_Collection;

     Incoming_Msg_Id  : YAMI.Parameters.YAMI_Long_Long_Integer;

     procedure Reply (Reply_Body : in YAMI.Parameters.Parameters_Collection)
     is
        Reply_Header : YAMI.Parameters.Parameters_Collection;
     begin
        YAMI.Parameters.Init_Parameters (Reply_Header);

        Reply_Header.Set_String ("type", "reply");
        Reply_Header.Set_Long_Long ("message_id", Incoming_Msg_Id);

        Agent.Post (Source, Reply_Header, Reply_Body);
     end Reply;

     procedure Reject (Reason : in String) is
        Reject_Header : YAMI.Parameters.Parameters_Collection;
     begin
        YAMI.Parameters.Init_Parameters (Reject_Header);

        Log.Put (Messages, "    rejected message from " & Source);

        Reject_Header.Set_String ("type", "exception");
        Reject_Header.Set_Long_Long ("message_id", Incoming_Msg_Id);
        Reject_Header.Set_String ("reason", Reason);

        Agent.Post (Source, Reject_Header, Empty_Parameters);
     end Reject;

     procedure Process_Bind is

        Object_Entry : YAMI.Parameters.Parameter_Entry;
        Object_Entry_Type : YAMI.Parameters.Parameter_Type;
        Object_Found : Boolean;

        Success : Boolean := False;

        procedure Process_Single_Bind (Object_Name : in String;
                                       Location : in String) is
        begin
           Storage.Store (Object_Name, Location);
           Cache.Bind (Object_Name, Location);

           Log.Put (Messages,
                    "    " & Object_Name &
                      " bound to " & Location);
        end Process_Single_Bind;

        use type YAMI.Parameters.Parameter_Type;

     begin
        Msg_Body.Find (Object_Field_Name, Object_Entry, Object_Found);
        if Object_Found then
           Object_Entry_Type := YAMI.Parameters.Entry_Type (Object_Entry);

           if Object_Entry_Type = YAMI.Parameters.String_Type then

              --  single call

              Log.Put (Messages, "single bind from " & Source);

              Process_Single_Bind (YAMI.Parameters.Get_String (Object_Entry),
                                   Msg_Body.Get_String (Location_Field_Name));

              Reply (Empty_Parameters);
              Success := True;

           elsif Object_Entry_Type = YAMI.Parameters.String_Array_Type then

              --  array call

              Log.Put (Messages, "array bind from " & Source);

              declare
                 Location_Entry : YAMI.Parameters.Parameter_Entry;
                 Dummy_Found : Boolean;

                 Length : constant YAMI.Parameters.Count_Type :=
                   YAMI.Parameters.Get_String_Array_Length (Object_Entry);
              begin
                 Msg_Body.Find
                   (Location_Field_Name, Location_Entry, Dummy_Found);

                 for I in YAMI.Parameters.Index_Type range 1 .. Length loop
                    declare
                       Object_Name : constant String :=
                         YAMI.Parameters.Get_String_In_Array
                         (Object_Entry, I);
                       Location : constant String :=
                         YAMI.Parameters.Get_String_In_Array
                         (Location_Entry, I);
                    begin
                       Process_Single_Bind (Object_Name, Location);
                    end;
                 end loop;
              end;

              Reply (Empty_Parameters);
              Success := True;
           end if;
        end if;

        if not Success then
           Reject ("Unknown message");
        end if;
     end Process_Bind;

     procedure Process_Resolve is

        Reply_Body : YAMI.Parameters.Parameters_Collection;

        Object_Entry : YAMI.Parameters.Parameter_Entry;
        Object_Entry_Type : YAMI.Parameters.Parameter_Type;
        Object_Found : Boolean;

        Success : Boolean := False;

        function Single_Resolve (Object_Name : in String) return String is
        begin
           return Cache.Resolve (Object_Name);
        end Single_Resolve;

        use type YAMI.Parameters.Parameter_Type;

     begin
        YAMI.Parameters.Init_Parameters (Reply_Body);

        Msg_Body.Find (Object_Field_Name, Object_Entry, Object_Found);
        if Object_Found then
           Object_Entry_Type := YAMI.Parameters.Entry_Type (Object_Entry);

           if Object_Entry_Type = YAMI.Parameters.String_Type then

              --  single call

              Log.Put (Messages, "single resolve from " & Source);

              Reply_Body.Set_String
                (Location_Field_Name,
                 Single_Resolve (YAMI.Parameters.Get_String (Object_Entry)));

              Reply (Reply_Body);
              Success := True;

           elsif Object_Entry_Type = YAMI.Parameters.String_Array_Type then

              --  array call

              Log.Put (Messages, "array resolve from " & Source);

              declare
                 Length : constant YAMI.Parameters.Count_Type :=
                   YAMI.Parameters.Get_String_Array_Length (Object_Entry);
              begin
                 Reply_Body.Create_String_Array (Location_Field_Name, Length);

                 for I in YAMI.Parameters.Index_Type range 1 .. Length loop
                    Reply_Body.Set_String_In_Array
                      (Location_Field_Name, I,
                       Single_Resolve
                         (YAMI.Parameters.Get_String_In_Array
                            (Object_Entry, I)));
                 end loop;

                 Reply (Reply_Body);
                 Success := True;
              end;
           end if;
        end if;

        if not Success then
           Reject ("Unknown message");
        end if;
     end Process_Resolve;

     procedure Process_List is
        Reply_Body : YAMI.Parameters.Parameters_Collection;

        Length : constant YAMI.Parameters.Count_Type :=
          YAMI.Parameters.Count_Type (Cache.Length);

        I : YAMI.Parameters.Index_Type := 1;

        procedure Process_Element (Object_Name : in String;
                                   Location : in String) is

           use type YAMI.Parameters.Index_Type;
        begin
           Reply_Body.Set_String_In_Array
             (Object_Field_Name, I, Object_Name);
           Reply_Body.Set_String_In_Array
             (Location_Field_Name, I, Location);

           I := I + 1;
        end Process_Element;

     begin
        Log.Put (Messages, "list from " & Source);

        YAMI.Parameters.Init_Parameters (Reply_Body);

        Reply_Body.Create_String_Array (Object_Field_Name, Length);
        Reply_Body.Create_String_Array (Location_Field_Name, Length);

        Cache.Iterate (Process_Element'Access);

        Reply (Reply_Body);
     end Process_List;

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
               Message_Name : constant String :=
                 Header.Get_String ("message_name");
            begin
               Incoming_Msg_Id := Header.Get_Long_Long ("message_id");

               if Message_Name = "bind" then
                  Process_Bind;
               elsif Message_Name = "resolve" then
                  Process_Resolve;
               elsif Message_Name = "list" then
                  Process_List;
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

   procedure Init (Listener : in String;
                   Data_Directory : in string) is

      Resolved_Target : String (1 .. YAMI.Core.Agents.Max_Target_Length);
      Resolved_Target_Last : Natural;
   begin
      YAMI.Parameters.Init_Parameters (Empty_Parameters);

      YAMI.Core.Agents.Init_Agent (Agent, Msg_Handler'Access);

      Agent.Add_Listener (Listener, Resolved_Target, Resolved_Target_Last);
      Log.Put (Main, "listening on " &
                 Resolved_Target (1 .. Resolved_Target_Last));

      Storage.Init (Data_Directory);
      Storage.Iterate (Cache.Bind'Access);

      IAL.Tasking.Starter.Start;
      Log.Put (Main, "name server started");
   end Init;

end Name_Server.Messaging;
