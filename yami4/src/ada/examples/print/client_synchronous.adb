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

procedure Client_Synchronous is
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line
        ("expecting one parameter: server destination");
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Failure);
      return;
   end if;

   declare
      Server_Address : constant String :=
        Ada.Command_Line.Argument (1);

      Client_Agent : YAMI.Agents.Agent :=
        YAMI.Agents.Make_Agent;

   begin
      --  read lines of text from standard input
      --  and post each one for transmission

      while not Ada.Text_IO.End_Of_File loop
         declare
            Input_Line : constant String :=
              Ada.Text_IO.Get_Line;

            Params :
              YAMI.Parameters.Parameters_Collection :=
              YAMI.Parameters.Make_Parameters;

            Message :
              aliased YAMI.Outgoing_Messages.Outgoing_Message;
         begin
            --  the "content" field name is arbitrary,
            --  but needs to be recognized at the server side

            Params.Set_String ("content", Input_Line);

            Client_Agent.Send
              (Server_Address, "printer", "print", Params,
               Message'Unchecked_Access);

            Message.Wait_For_Transmission;
         end;
      end loop;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Message (E));
end Client_Synchronous;
