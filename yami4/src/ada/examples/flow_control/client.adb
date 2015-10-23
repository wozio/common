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
with YAMI.Outgoing_Messages;
with YAMI.Parameters;

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

procedure Client is
   Options : YAMI.Parameters.Parameters_Collection :=
     YAMI.Parameters.Make_Parameters;
   Num_Of_Iterations : YAMI.Parameters.YAMI_Integer :=
     YAMI.Parameters.YAMI_Integer'Last;
begin
   if Ada.Command_Line.Argument_Count /= 1 and
     Ada.Command_Line.Argument_Count /= 4 then
      Ada.Text_IO.Put_Line ("expecting 1 or 4 parameters:");
      Ada.Text_IO.Put_Line ("   - server address");
      Ada.Text_IO.Put_Line ("   - outgoing high water mark");
      Ada.Text_IO.Put_Line ("   - outgoing low water mark");
      Ada.Text_IO.Put_Line ("   - number of iterations");
      Ada.Text_IO.Put_Line
        ("If only server address is given," &
           " the limits will have default values" &
           " and the loop will be infinite");
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Failure);
      return;
   end if;

   if Ada.Command_Line.Argument_Count = 4 then
      begin
         declare
            Outgoing_High_Water_Mark :
              YAMI.Parameters.YAMI_Integer :=
              YAMI.Parameters.YAMI_Integer'Value
              (Ada.Command_Line.Argument (2));
            Outgoing_Low_Water_Mark :
              YAMI.Parameters.YAMI_Integer :=
              YAMI.Parameters.YAMI_Integer'Value
              (Ada.Command_Line.Argument (3));
         begin
            Num_Of_Iterations :=
              YAMI.Parameters.YAMI_Integer'Value
              (Ada.Command_Line.Argument (4));

            Options.Set_Integer
              ("outgoing_high_water_mark",
               Outgoing_High_Water_Mark);
            Options.Set_Integer
              ("outgoing_low_water_mark",
               Outgoing_Low_Water_Mark);
         end;
      exception
         when Constraint_Error =>
            Ada.Text_IO.Put_Line ("invalid arguments");
            Ada.Command_Line.Set_Exit_Status
              (Ada.Command_Line.Failure);
            return;
      end;
   end if;

   declare
      Server_Address : constant String :=
        Ada.Command_Line.Argument (1);

      Client_Agent : YAMI.Agents.Agent :=
        YAMI.Agents.Make_Agent (Options);

      Params : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;
   begin

      for Index in YAMI.Parameters.YAMI_Integer
        range 1 .. Num_Of_Iterations loop

         Params.Set_Integer ("index", Index);

         Client_Agent.Send_One_Way
           (Server_Address, "object", "message", Params);

         Ada.Text_IO.Put_Line
           ("posted message " &
              YAMI.Parameters.YAMI_Integer'Image (Index));
      end loop;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Message (E));
end Client;
