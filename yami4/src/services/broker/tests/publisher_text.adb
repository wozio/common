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

with Ada.Calendar.Formatting;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

procedure Publisher_Text is
begin
   if Ada.Command_Line.Argument_Count /= 4 then
      Ada.Text_IO.Put_Line ("expecting 4 parameters:");
      Ada.Text_IO.Put_Line ("    broker address");
      Ada.Text_IO.Put_Line ("    list of tags for publishing");
      Ada.Text_IO.Put_Line ("    message text");
      Ada.Text_IO.Put_Line ("    delay");
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Failure);
      return;
   end if;

   declare
      Broker_Address : constant String :=
        Ada.Command_Line.Argument (1);

      List_Of_Tags : constant String :=
        Ada.Command_Line.Argument (2);

      Text : constant String :=
        Ada.Command_Line.Argument (3);

      Wait_Time : constant Duration :=
        Duration'Value (Ada.Command_Line.Argument (4));

      Publisher_Agent : YAMI.Agents.Agent :=
        YAMI.Agents.Make_Agent;

      Params :
        YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;

      Counter : Positive := 1;
   begin
      Params.Set_String ("value", Text);

      loop
         Publisher_Agent.Send_One_Way
           (Broker_Address, List_Of_Tags, "publish", Params);

         Ada.Text_IO.Put_Line ("sent" & Positive'Image (Counter));
         Counter := Counter + 1;

         delay Wait_Time;
      end loop;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        ("error: " & Ada.Exceptions.Exception_Message (E));
end Publisher_Text;
