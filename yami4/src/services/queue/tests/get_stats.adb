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

with YAMI.Agents;
with YAMI.Outgoing_Messages;
with YAMI.Parameters;

with Ada.Command_Line;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;

procedure Get_Stats is
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("expecting 1 parameter:");
      Ada.Text_IO.Put_Line ("    queue address");
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Failure);
      return;
   end if;

   declare
      Queue_Address : constant String :=
        Ada.Command_Line.Argument (1);

      Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;

      Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

      procedure Process_Reply
        (Content : in out YAMI.Parameters.Parameters_Collection) is

         Queue_Names_Field_Name : constant String := "queue_names";
         Dummy_Entry : YAMI.Parameters.Parameter_Entry;
         Details_Found : Boolean;
         
         procedure Process_Details is

            Num_Of_Queues : constant YAMI.Parameters.Count_Type :=
              Content.Get_String_Array_Length (Queue_Names_Field_Name);

            type Counter_Array_Type is
               array (YAMI.Parameters.Index_Type range <>)
               of Ada.Containers.Count_Type;
      
            type Size_Array_Type is
               array (YAMI.Parameters.Index_Type range <>)
               of Ada.Streams.Stream_Element_Count;
      
            Num_Of_Waiting_Messages :
               Counter_Array_Type
                 (1 .. YAMI.Parameters.Index_Type (Num_Of_Queues));
            Total_Content_Sizes :
               Size_Array_Type
                 (1 .. YAMI.Parameters.Index_Type (Num_Of_Queues));
            Num_Of_Waiting_Clients :
               Counter_Array_Type
                 (1 .. YAMI.Parameters.Index_Type (Num_Of_Queues));
      
            procedure Get_Counters_Array is
               new YAMI.Parameters.Get_Integer_Array
                 (Index_Type => YAMI.Parameters.Index_Type,
                  Integer_Type => Ada.Containers.Count_Type,
                  Integer_Array_Type => Counter_Array_Type);
      
            procedure Get_Sizes_Array is
               new YAMI.Parameters.Get_Long_Long_Array
                 (Index_Type => YAMI.Parameters.Index_Type,
                  Long_Long_Integer_Type => Ada.Streams.Stream_Element_Count,
                  Long_Long_Integer_Array_Type => Size_Array_Type);

         begin
            Get_Counters_Array
              (Content, "num_of_waiting_messages", Num_Of_Waiting_Messages);
            Get_Sizes_Array
              (Content, "total_content_sizes", Total_Content_Sizes);
            Get_Counters_Array
              (Content, "num_of_waiting_clients", Num_Of_Waiting_Clients);

            Ada.Text_IO.Put_Line
              ("queue stats " &
                 "(queue name, num of waiting messages, " &
                 "total content size, num of waiting clients):");

            for I in 1 .. Num_Of_Queues loop
               Ada.Text_IO.Put
                 (YAMI.Parameters.Get_String_In_Array
                    (Content, Queue_Names_Field_Name, I));
               Ada.Text_IO.Put
                 (Ada.Containers.Count_Type'Image
                    (Num_Of_Waiting_Messages (I)));
               Ada.Text_IO.Put
                 (Ada.Streams.Stream_Element_Count'Image
                    (Total_Content_Sizes (I)));
               Ada.Text_IO.Put
                 (Ada.Containers.Count_Type'Image
                    (Num_Of_Waiting_Clients (I)));
               
               Ada.Text_IO.New_Line;
            end loop;
         end Process_Details;

      begin
         Ada.Text_IO.Put_Line
           ("number of queues:" &
              YAMI.Parameters.YAMI_Integer'Image
              (Content.Get_Integer ("num_of_queues")));
         Ada.Text_IO.Put_Line
           ("number of all waiting messages:" &
              YAMI.Parameters.YAMI_Integer'Image
              (Content.Get_Integer ("num_of_all_waiting_messages")));
         Ada.Text_IO.Put_Line
           ("total byte size of all queues:" &
              YAMI.Parameters.YAMI_Long_Long_Integer'Image
              (Content.Get_Long_Long ("total_of_all_content_sizes")));
         Ada.Text_IO.Put_Line
           ("number of all waiting clients:" &
              YAMI.Parameters.YAMI_Integer'Image
              (Content.Get_Integer ("num_of_all_waiting_clients")));

         Content.Find
           (Queue_Names_Field_Name, Dummy_Entry, Details_Found);

         if Details_Found then
            Ada.Text_IO.New_Line;
            Process_Details;
         end if;

      end Process_Reply;

      use type YAMI.Outgoing_Messages.Message_State;

   begin
      Agent.Send
        (Queue_Address, "stats", "get-details", Msg'Unchecked_Access);

      Msg.Wait_For_Completion;

      if Msg.State = YAMI.Outgoing_Messages.Replied then
         Msg.Process_Reply_Content (Process_Reply'Access);
      end if;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        ("error: " & Ada.Exceptions.Exception_Message (E));
end Get_Stats;
