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
with YAMI.Parameters;

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

procedure Subscriber is

   type Update_Handler is
     new YAMI.Incoming_Messages.Message_Handler
     with null record;

   overriding
   procedure Call
     (H : in out Update_Handler;
      Message : in out
      YAMI.Incoming_Messages.Incoming_Message'Class) is

     procedure Process
       (Content : in out YAMI.Parameters.Parameters_Collection)
     is
        Value : constant YAMI.Parameters.YAMI_Integer :=
          Content.Get_Integer ("value");
     begin
        Ada.Text_IO.Put_Line
          ("received update: " &
             YAMI.Parameters.YAMI_Integer'Image (Value));
     end Process;

   begin
      Message.Process_Content (Process'Access);
   end Call;

   My_Handler : aliased Update_Handler;

begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line
        ("expecting one parameter: publisher destination");
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Failure);
      return;
   end if;

   declare
      Publisher_Address : constant String :=
        Ada.Command_Line.Argument (1);

      Subscriber_Agent : YAMI.Agents.Agent :=
        YAMI.Agents.Make_Agent;

      Update_Object_Name : constant String :=
        "update_handler";

      Params : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;
   begin
      --  prepare subscription update callback

      Subscriber_Agent.Register_Object
        (Update_Object_Name, My_Handler'Unchecked_Access);

      --  subscribe to the producer

      Params.Set_String
        ("destination_object", Update_Object_Name);

      Subscriber_Agent.Send_One_Way
        (Publisher_Address,
         "random_number", "subscribe", Params);

      Ada.Text_IO.Put_Line
        ("subscribed, waiting for updates");

      loop
         delay 10.0;
      end loop;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Message (E));
end Subscriber;
