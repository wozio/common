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

procedure Server is

   type Incoming_Message_Handler is
     new YAMI.Incoming_Messages.Message_Handler
     with null record;

   overriding
   procedure Call
     (H : in out Incoming_Message_Handler;
      Message : in out
      YAMI.Incoming_Messages.Incoming_Message'Class) is

     procedure Process
       (Content : in out YAMI.Parameters.Parameters_Collection)
     is
        Index : YAMI.Parameters.YAMI_Integer;
     begin

        Index := Content.Get_Integer ("index");

        Ada.Text_IO.Put_Line
          ("processing message " &
             YAMI.Parameters.YAMI_Integer'Image (Index) &
             " from " & Message.Source);
     end Process;

   begin
      Message.Process_Content (Process'Access);
   end Call;

   My_Handler : aliased Incoming_Message_Handler;

   Options : YAMI.Parameters.Parameters_Collection :=
     YAMI.Parameters.Make_Parameters;

begin
   if Ada.Command_Line.Argument_Count /= 1 and
     Ada.Command_Line.Argument_Count /= 3 then
      Ada.Text_IO.Put_Line ("expecting 1 or 3 parameters:");
      Ada.Text_IO.Put_Line ("   - server address");
      Ada.Text_IO.Put_Line ("   - incoming high water mark");
      Ada.Text_IO.Put_Line ("   - incoming low water mark");
      Ada.Text_IO.Put_Line
        ("If only server address is given," &
           " the limits will have default values");
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Failure);
      return;
   end if;

   if Ada.Command_Line.Argument_Count = 3 then
      begin
         declare
            Incoming_High_Water_Mark :
              YAMI.Parameters.YAMI_Integer :=
              YAMI.Parameters.YAMI_Integer'Value
              (Ada.Command_Line.Argument (2));
            Incoming_Low_Water_Mark :
              YAMI.Parameters.YAMI_Integer :=
              YAMI.Parameters.YAMI_Integer'Value
              (Ada.Command_Line.Argument (3));
         begin
            Options.Set_Integer
              ("incoming_high_water_mark",
               Incoming_High_Water_Mark);
            Options.Set_Integer
              ("incoming_low_water_mark",
               Incoming_Low_Water_Mark);
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

      Server_Agent : YAMI.Agents.Agent :=
        YAMI.Agents.Make_Agent;
      Resolved_Server_Address :
        String (1 .. YAMI.Agents.Max_Target_Length);
      Resolved_Server_Address_Last : Natural;
   begin
      Server_Agent.Add_Listener
        (Server_Address,
         Resolved_Server_Address,
         Resolved_Server_Address_Last);

      Ada.Text_IO.Put_Line
        ("The server is listening on " &
           Resolved_Server_Address
           (1 .. Resolved_Server_Address_Last));

      Server_Agent.Register_Object
        ("object", My_Handler'Unchecked_Access);

      loop
         delay 10.0;
      end loop;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Message (E));
end Server;
