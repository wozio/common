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

procedure Try_Get is
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Ada.Text_IO.Put_Line ("expecting 2 parameters:");
      Ada.Text_IO.Put_Line ("    queue server address");
      Ada.Text_IO.Put_Line ("    queue name");
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Failure);
      return;
   end if;

   declare
      Queue_Address : constant String :=
        Ada.Command_Line.Argument (1);

      Queue_Name : constant String :=
        Ada.Command_Line.Argument (2);

      Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;

      Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

      use type YAMI.Outgoing_Messages.Message_State;

   begin
      Agent.Send
        (Queue_Address, Queue_Name, "try-get", Msg'Unchecked_Access);

      Msg.Wait_For_Completion;

      if Msg.State = YAMI.Outgoing_Messages.Replied then
         declare
            procedure Process
              (Content : in out YAMI.Parameters.Parameters_Collection) is
            begin
               Ada.Text_IO.Put_Line (Content.Get_String ("value"));
            end Process;
         begin
            Msg.Process_Reply_Content (Process'Access);
         end;
      elsif Msg.State = YAMI.Outgoing_Messages.Rejected then
         Ada.Text_IO.Put_Line ("try-get rejected: " & Msg.Exception_Message);
      end if;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        ("error: " & Ada.Exceptions.Exception_Message (E));
end Try_Get;
