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
with YAMI.Parameters;
with YAMI.Raw_Buffer_Data_Sources;
with YAMI.Serializables;

package body YAMI.Details.Handlers is

   --
   --  Operations of the Core_Incoming_Message_Handler type.
   --

   procedure Call
     (H : in out Core_Incoming_Message_Handler;
      Source : in String;
      Header_Buffers : in Core.Serialization_Buffers_Descriptor;
      Body_Buffers : in Core.Serialization_Buffers_Descriptor) is

      Header_Content : Parameters.Parameters_Collection :=
        Parameters.Make_Parameters;

   begin
      Header_Content.Deserialize (Header_Buffers);

      declare
         Message_Type : constant String :=
           Header_Content.Get_String ("type");

         Message_Id : constant Parameters.YAMI_Long_Long_Integer :=
           Header_Content.Get_Long_Long ("message_id");

      begin
         if Message_Type = "message" then

            declare
               Object_Name : constant String :=
                 Header_Content.Get_String ("object_name");
               Message_Name : constant String :=
                 Header_Content.Get_String ("message_name");

               Body_Content : Parameters.Parameters_Collection_Access := null;
               Raw_Body_Content :
                 Serializables.Serialization_Buffer_List (1 .. 1) :=
                 (others => null);
            begin
               if H.Deliver_As_Raw_Binary then
                  --  the content is to be delivered in raw binary form
                  --  by convention the list of buffers contains only
                  --  one contiguous buffer

                  --  repackage the data buffer so that
                  --  the incoming message object can own it

                  declare
                     Buffer_Converter :
                       Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source :=
                       Raw_Buffer_Data_Sources.Make_Raw_Buffer_Data_Source
                       (Body_Buffers);

                     Target_Size :
                       constant Ada.Streams.Stream_Element_Count :=
                       Buffer_Converter.Serialize_Buffer_Size;

                     Target_Buffer :
                       Serializables.Serialization_Buffer_Access :=
                       new Serializables.Serialization_Buffer (Target_Size);
                  begin
                     Raw_Body_Content (1) := Target_Buffer;

                     Buffer_Converter.Serialize (Raw_Body_Content);
                  exception
                     when others =>
                        Serializables.Free (Target_Buffer);
                        raise;
                  end;

               else
                  --  the content is to be delivered in
                  --  the already deserialized form (parameters collection)

                  Body_Content := Parameters.New_Parameters;
                  Body_Content.all.Deserialize (Body_Buffers);
               end if;

               declare
                  Message : Incoming_Messages.Incoming_Message_Access :=
                    Incoming_Messages.New_Message
                    (H.Core_Agent,
                     H.Connection_Event_Handler,
                     Message_Id,
                     Source,
                     Object_Name,
                     Message_Name,
                     Body_Content,
                     Raw_Body_Content);
               begin
                  Details.Dispatch_Managers.Enqueue
                    (H.Dispatcher.all, Message);
               end;
            end;

         elsif Message_Type = "reply" then

            H.Outgoing_Manager.all.Report_Replied (Message_Id, Body_Buffers);

         elsif Message_Type = "exception" then

            H.Outgoing_Manager.all.Report_Rejected
              (Message_Id, Header_Content.Get_String ("reason"));

         end if;
      end;
   end Call;

   procedure Init
     (H : in out Core_Incoming_Message_Handler;
      Core_Agent : in Core.Agents.Agent_Access;
      Outgoing_Manager : in Details.Outgoing_Message_Managers.Manager_Access;
      Dispatcher : in Details.Dispatch_Managers.Dispatch_Manager_Access;
      Connection_Event_Handler : Core_Connection_Handlers.Handler_Access;
      Deliver_As_Raw_Binary : in Boolean) is
   begin
      H.Core_Agent := Core_Agent;
      H.Outgoing_Manager := Outgoing_Manager;
      H.Dispatcher := Dispatcher;
      H.Connection_Event_Handler := Connection_Event_Handler;
      H.Deliver_As_Raw_Binary := Deliver_As_Raw_Binary;
   end Init;

   --
   --  Operations of the One_Way_Message_Progress_Handler type.
   --

   procedure Progress
     (H : in out One_Way_Message_Progress_Handler;
      Sent_Bytes : in Ada.Streams.Stream_Element_Count;
      Total_Byte_Count : in Ada.Streams.Stream_Element_Count) is

      use type Ada.Streams.Stream_Element_Offset;

   begin
      if Sent_Bytes = Total_Byte_Count then

         --  this is the last progress notification for *some*
         --  one-way message - the transmission of this message
         --  is either finished or abandoned and therefore
         --  can be treated as leaving the output queue

         H.Outgoing_Flow.all.Decrease;

      end if;
   end Progress;

   procedure Init
     (H : in out One_Way_Message_Progress_Handler;
      Outgoing_Flow : Details.Water_Flow_Managers.Manager_Access) is
   begin
      H.Outgoing_Flow := Outgoing_Flow;
   end Init;

end YAMI.Details.Handlers;
