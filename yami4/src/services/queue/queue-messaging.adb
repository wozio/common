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

with IAL.Tasking.Starter;
with Log;
with Queue.Routing;
with YAMI.Core.Agents;
with YAMI.Core.Incoming_Message_Handlers;
with YAMI.Parameters;
with YAMI.Raw_Buffer_Data_Sources;
with YAMI.Serializables;

with Ada.Exceptions;

pragma Elaborate_All (IAL.Tasking.Starter);
pragma Elaborate_All (YAMI.Core.Agents);

package body Queue.Messaging is

   Agent : YAMI.Core.Agents.Agent;
   Empty_Parameters : YAMI.Parameters.Parameters_Collection;
   
   Send_Response_Failed : Boolean;

   procedure Send_Reply
     (Target : in String;
      Message_Id : in YAMI.Parameters.YAMI_Long_Long_Integer;
      Body_Buffers : in YAMI.Serializables.Serializable'Class) is

      Reply_Header : YAMI.Parameters.Parameters_Collection;
   begin
      YAMI.Parameters.Init_Parameters (Reply_Header);

      Log.Put (Messages, "    sending response to " & Target);

      Reply_Header.Set_String ("type", "reply");
      Reply_Header.Set_Long_Long ("message_id", Message_Id);

      Agent.Post (Target, Reply_Header, Body_Buffers);

   exception
      when others =>
         Log.Put (Messages, "    send error for " & Target);
         
         --  intentionally swallow errors here:
         --  response failures are not handled in any meaningful way,
         --  but the put request that triggered the failue can be repeated
         --  at the highest level, so that other clients
         --  can have their chance to receive the same message
         --  (get requests are not repeated as their failures cannot be
         --  distinguished from successful response followed by client crash)
         
         --  Note:
         --  this mechanism does not take into account the possibility
         --  of the response to fail in the middle of transmission
         --  in such a case the response (and therefore the queued message)
         --  will be silently lost;
         --  in any way, this situation cannot be distinguished from
         --  successful transmission followed by client crash

         Send_Response_Failed := True;
         
   end Send_Reply;

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

      Incoming_Msg_Id  : YAMI.Parameters.YAMI_Long_Long_Integer;

      procedure Process_Put (Queue_Name : in String) is
         Raw_Body : YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source (0);
      begin
         YAMI.Raw_Buffer_Data_Sources.Init_Raw_Buffer_Data_Source
           (Raw_Body, Body_Buffers);
                       
         Routing.Put (Queue_Name, Raw_Body, Send_Reply'Access);
         
         --  if there was no exception in the routing module,
         --  the incoming request should be confirmed
         
         declare
            Confirm_Header : YAMI.Parameters.Parameters_Collection;
         begin
            YAMI.Parameters.Init_Parameters (Confirm_Header);

            Confirm_Header.Set_String ("type", "reply");
            Confirm_Header.Set_Long_Long ("message_id", Incoming_Msg_Id);

            Agent.Post (Source, Confirm_Header, Empty_Parameters);
         end;
      end Process_Put;
      
      procedure Process_Stats (Detailed : in Boolean) is
         Response_Header : YAMI.Parameters.Parameters_Collection;
         Response : YAMI.Parameters.Parameters_Collection;
      begin
         YAMI.Parameters.Init_Parameters (Response_Header);
         YAMI.Parameters.Init_Parameters (Response);
         
         Routing.Fill_Simple_Stats (Response);
         
         if Detailed then
            Routing.Fill_Detailed_Stats (Response);

            Log.Put (Messages, "request for detailed stats from " & Source);
         else
            Log.Put (Messages, "request for simple stats from " & Source);
         end if;

         Response_Header.Set_String ("type", "reply");
         Response_Header.Set_Long_Long ("message_id", Incoming_Msg_Id);

         Agent.Post (Source, Response_Header, Response);
      end Process_Stats;
         
      procedure Reject (Reason : in String) is
         Reject_Header : YAMI.Parameters.Parameters_Collection;
      begin
         YAMI.Parameters.Init_Parameters (Reject_Header);

         Log.Put (Messages, "    rejecting message from " &
                  Source & ", " & Reason);

         Reject_Header.Set_String ("type", "exception");
         Reject_Header.Set_Long_Long ("message_id", Incoming_Msg_Id);
         Reject_Header.Set_String ("reason", Reason);

         Agent.Post (Source, Reject_Header, Empty_Parameters);
      end Reject;

   begin
      YAMI.Parameters.Init_Parameters (Header);
      Header.Deserialize (Header_Buffers);

      declare
         Msg_Type : constant String := Header.Get_String ("type");
      begin
         if Msg_Type = "message" then
            declare
               Object_Name : constant String :=
                  Header.Get_String ("object_name");
               Message_Name : constant String :=
                  Header.Get_String ("message_name");
               
               First_Try : Boolean;
               
            begin
               Incoming_Msg_Id := Header.Get_Long_Long ("message_id");

               if Message_Name = "put" then
               
                  First_Try := True;
                  loop
                     if First_Try then
                        Log.Put (Messages,
                           "put requested for queue " & Object_Name);
                     else
                        Log.Put (Messages,
                           "put repeated for queue " & Object_Name);
                     end if;
                  
                     Send_Response_Failed := False;
                     Process_Put (Object_Name);
                     exit when not Send_Response_Failed;
                     
                     First_Try := False;
                  end loop;
                  
               elsif Message_Name = "get" then
               
                  Log.Put (Messages,
                     "get requested for queue " & Object_Name);
                  
                  Routing.Get (Object_Name,
                               Source,
                               Incoming_Msg_Id,
                               Send_Reply'Access);
                  
               elsif Message_Name = "try-get" then
               
                  Log.Put (Messages,
                     "try-get requested for queue " & Object_Name);
                  
                  Routing.Try_Get (Object_Name,
                                   Source,
                                   Incoming_Msg_Id,
                                   Send_Reply'Access);
                  
               elsif Object_Name = "stats" then
                  if Message_Name = "get-basic" then
                     Process_Stats (False);
                  elsif Message_Name = "get-details" then
                     Process_Stats (True);
                  end if;
               end if;
            end;
         end if;
      exception
         when E : others =>
            Reject (Ada.Exceptions.Exception_Message (E));
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
         Log.Put (Queue.Main, "exception in worker " &
                  Ada.Exceptions.Exception_Message (E));
   end Worker;

   procedure Init (Listener : in String) is

      Resolved_Target : String (1 .. YAMI.Core.Agents.Max_Target_Length);
      Resolved_Target_Last : Natural;
   begin
      YAMI.Parameters.Init_Parameters (Empty_Parameters);

      YAMI.Core.Agents.Init_Agent (Agent, Msg_Handler'Access);

      Agent.Add_Listener (Listener, Resolved_Target, Resolved_Target_Last);
      Log.Put (Queue.Main, "listening on " &
                 Resolved_Target (1 .. Resolved_Target_Last));

      IAL.Tasking.Starter.Start;
      Log.Put (Queue.Main, "started");
   end Init;

end Queue.Messaging;
