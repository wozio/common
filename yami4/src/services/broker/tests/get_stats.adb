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
with Ada.Exceptions;
with Ada.Text_IO;

procedure Get_Stats is
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("expecting 1 parameter:");
      Ada.Text_IO.Put_Line ("    broker address");
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Failure);
      return;
   end if;

   declare
      Broker_Address : constant String :=
        Ada.Command_Line.Argument (1);

      Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;

      Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

      procedure Process_Reply
        (Content : in out YAMI.Parameters.Parameters_Collection) is

         Overflows_Field_Name : constant String := "overflows";

         Dummy_Entry : YAMI.Parameters.Parameter_Entry;
         Subscription_Details_Found : Boolean;

         procedure Process_Subscription_Details is

            Active_Subscription_Count : constant YAMI.Parameters.Count_Type :=
              Content.Get_Boolean_Array_Length (Overflows_Field_Name);

            type Overflow_Array_Type is
              array (YAMI.Parameters.Index_Type range <>) of Boolean;

            type Sent_Counters_Array_Type is
              array (YAMI.Parameters.Index_Type range <>) of
              YAMI.Parameters.YAMI_Long_Long_Integer;


            procedure Get_Boolean_Array is
               new YAMI.Parameters.Get_Boolean_Array
              (Index_Type => YAMI.Parameters.Index_Type,
               Boolean_Array_Type => Overflow_Array_Type);

            procedure Get_Long_Long_Array is
               new YAMI.Parameters.Get_Long_Long_Array
              (Index_Type => YAMI.Parameters.Index_Type,
               Long_Long_Integer_Type =>
                 YAMI.Parameters.YAMI_Long_Long_Integer,
               Long_Long_Integer_Array_Type => Sent_Counters_Array_Type);

            Overflow_Array : Overflow_Array_Type
              (1 .. Active_Subscription_Count);
            Sent_Messages_Array : Sent_Counters_Array_Type
              (1 .. Active_Subscription_Count);
            Sent_Bytes_Array : Sent_Counters_Array_Type
              (1 .. Active_Subscription_Count);

         begin
            Get_Boolean_Array
              (Content, Overflows_Field_Name, Overflow_Array);
            Get_Long_Long_Array
              (Content, "sent_messages", Sent_Messages_Array);
            Get_Long_Long_Array
              (Content, "sent_bytes", Sent_Bytes_Array);

            Ada.Text_IO.Put_Line
              ("subscriber stats " &
                 "(overflow flag, message count, byte count, location):");

            for I in 1 .. Active_Subscription_Count loop
               if Overflow_Array (I) then
                  Ada.Text_IO.Put ('#');
               else
                  Ada.Text_IO.Put (' ');
               end if;
               Ada.Text_IO.Put
                 (YAMI.Parameters.YAMI_Long_Long_Integer'Image
                    (Sent_Messages_Array (I)));
               Ada.Text_IO.Put
                 (YAMI.Parameters.YAMI_Long_Long_Integer'Image
                    (Sent_Bytes_Array (I)));
               Ada.Text_IO.Put (' ');
               Ada.Text_IO.Put_Line
                 (Content.Get_String_In_Array ("locations", I));
            end loop;
         end Process_Subscription_Details;

      begin
         Ada.Text_IO.Put_Line
           ("total incoming:" &
              YAMI.Parameters.YAMI_Long_Long_Integer'Image
              (Content.Get_Long_Long ("total_incoming")));
         Ada.Text_IO.Put_Line
           ("total outgoing:" &
              YAMI.Parameters.YAMI_Long_Long_Integer'Image
              (Content.Get_Long_Long ("total_outgoing")));

         Content.Find
           (Overflows_Field_Name, Dummy_Entry, Subscription_Details_Found);

         if Subscription_Details_Found then
            Process_Subscription_Details;
         else
            Ada.Text_IO.Put_Line ("there are no subscriptions");
         end if;

      end Process_Reply;

      use type YAMI.Outgoing_Messages.Message_State;

   begin
      Agent.Send
        (Broker_Address, "stats", "get_details", Msg'Unchecked_Access);

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
