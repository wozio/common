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

procedure Resolve is
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Ada.Text_IO.Put_Line ("expecting 2 parameters:");
      Ada.Text_IO.Put_Line ("    name server address");
      Ada.Text_IO.Put_Line ("    object name");
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Failure);
      return;
   end if;

   declare
      Name_Server_Address : constant String :=
        Ada.Command_Line.Argument (1);

      Object_Name : constant String :=
        Ada.Command_Line.Argument (2);

      Agent : YAMI.Agents.Agent := YAMI.Agents.Make_Agent;

      Params : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;

      Msg : aliased YAMI.Outgoing_Messages.Outgoing_Message;

      procedure Process_Reply
        (Content : in out YAMI.Parameters.Parameters_Collection) is
      begin
         Ada.Text_IO.Put_Line (Content.Get_String ("location"));
      end Process_Reply;

      use type YAMI.Outgoing_Messages.Message_State;

   begin
      Params.Set_String ("object", Object_Name);

      Agent.Send
        (Name_Server_Address, "names", "resolve",
         Params, Msg'Unchecked_Access);

      Msg.Wait_For_Completion;

      if Msg.State = YAMI.Outgoing_Messages.Replied then
         Msg.Process_Reply_Content (Process_Reply'Access);
      end if;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        ("error: " & Ada.Exceptions.Exception_Message (E));
end Resolve;
