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
   A : YAMI.Parameters.YAMI_Integer;
   B : YAMI.Parameters.YAMI_Integer;
begin
   if Ada.Command_Line.Argument_Count /= 3 then
      Ada.Text_IO.Put_Line
        ("expecting three parameters: " &
           "server destination and two integers");
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Failure);
      return;
   end if;

   begin
      A := YAMI.Parameters.YAMI_Integer'Value
        (Ada.Command_Line.Argument (2));
      B := YAMI.Parameters.YAMI_Integer'Value
        (Ada.Command_Line.Argument (3));
   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line
           ("cannot parse the second or third parameter");
         Ada.Command_Line.Set_Exit_Status
           (Ada.Command_Line.Failure);
         return;
   end;

   declare
      Server_Address : constant String :=
        Ada.Command_Line.Argument (1);

      Client_Agent : YAMI.Agents.Agent :=
        YAMI.Agents.Make_Agent;

      Params : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;

      Message :
        aliased YAMI.Outgoing_Messages.Outgoing_Message;
      State : YAMI.Outgoing_Messages.Message_State;

      procedure Process_Reply
        (Content : in out YAMI.Parameters.Parameters_Collection)
      is
         Sum : constant YAMI.Parameters.YAMI_Integer :=
           Content.Get_Integer ("sum");
         Difference :
           constant YAMI.Parameters.YAMI_Integer :=
           Content.Get_Integer ("difference");
         Product : constant YAMI.Parameters.YAMI_Integer :=
           Content.Get_Integer ("product");
         Ratio : YAMI.Parameters.YAMI_Integer;
         Ratio_Entry : YAMI.Parameters.Parameter_Entry;
         Ratio_Defined : Boolean;
      begin
         Content.Find
           ("ratio", Ratio_Entry, Ratio_Defined);
         if Ratio_Defined then
            Ratio :=
              YAMI.Parameters.Get_Integer (Ratio_Entry);
         end if;

         Ada.Text_IO.Put_Line
           ("sum        = " &
              YAMI.Parameters.YAMI_Integer'Image (Sum));
         Ada.Text_IO.Put_Line
           ("difference = " &
              YAMI.Parameters.YAMI_Integer'Image
              (Difference));
         Ada.Text_IO.Put_Line
           ("product    = " &
              YAMI.Parameters.YAMI_Integer'Image
              (Product));
         Ada.Text_IO.Put ("ratio      = ");
         if Ratio_Defined then
            Ada.Text_IO.Put_Line
              (YAMI.Parameters.YAMI_Integer'Image (Ratio));
         else
            Ada.Text_IO.Put_Line ("<undefined>");
         end if;
      end Process_Reply;

      use type YAMI.Outgoing_Messages.Message_State;
   begin

      Params.Set_Integer ("a", A);
      Params.Set_Integer ("b", B);

      Client_Agent.Send
        (Server_Address, "calculator", "calculate", Params,
         Message'Unchecked_Access);

      Message.Wait_For_Completion;

      State := Message.State;
      if State = YAMI.Outgoing_Messages.Replied then

         Message.Process_Reply_Content
           (Process_Reply'Access);

      elsif State = YAMI.Outgoing_Messages.Rejected then
         Ada.Text_IO.Put_Line
           ("The message has been rejected: " &
              Message.Exception_Message);
      else
         Ada.Text_IO.Put_Line
           ("The message has been abandoned.");
      end if;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Message (E));
end Client;
