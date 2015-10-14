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
with YAMI.Parameters;
with YAMI.Serializables;
with YAMI.Value_Publishers;

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;

procedure Publisher is
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

      Publisher_Agent : aliased YAMI.Agents.Agent :=
        YAMI.Agents.Make_Agent;
      Resolved_Publisher_Address :
        String (1 .. YAMI.Agents.Max_Target_Length);
      Resolved_Publisher_Address_Last : Natural;

      Random_Value : YAMI.Value_Publishers.Value_Publisher :=
        YAMI.Value_Publishers.Make_Value_Publisher;

      Content : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;

      package My_Random_Numbers is
         new Ada.Numerics.Discrete_Random
        (YAMI.Parameters.YAMI_Integer);

      My_Random_Generator : My_Random_Numbers.Generator;

      Random : YAMI.Parameters.YAMI_Integer;

      use type YAMI.Parameters.YAMI_Integer;
   begin
      Publisher_Agent.Add_Listener
        (Publisher_Address,
         Resolved_Publisher_Address,
         Resolved_Publisher_Address_Last);

      Ada.Text_IO.Put_Line
        ("The publisher is listening on " &
           Resolved_Publisher_Address
           (1 .. Resolved_Publisher_Address_Last));

      Random_Value.Register_At
        (Publisher_Agent'Unchecked_Access, "random_number");

      --  publish random numbers forever
      loop
         Random :=
           My_Random_Numbers.Random (My_Random_Generator)
           mod 100;
         Content.Set_Integer ("value", Random);

         Ada.Text_IO.Put_Line
           ("publishing value " &
              YAMI.Parameters.YAMI_Integer'Image (Random));

         Random_Value.Publish (Content);

         delay 1.0;
      end loop;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Message (E));
end Publisher;
