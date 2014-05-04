with Print_Example;
with YAMI.Agents;

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

use Ada.Strings.Unbounded;

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
      
      My_Printer : Print_Example.Printer;
   begin
      My_Printer.Initialize_Printer
        (Client_Agent, Server_Address, "printer");
      
      --  read lines of text from standard input
      --  and post each one for transmission

      while not Ada.Text_IO.End_Of_File loop
         declare
            Input_Line : constant String :=
              Ada.Text_IO.Get_Line;

            Msg : Print_Example.Text;
         begin
            --  the "content" field name is arbitrary,
            --  but needs to be recognized at the server side

            Msg.Content := To_Unbounded_String (Input_Line);
            
            My_Printer.Print_Synchronously (Msg);
         end;
      end loop;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Message (E));
end Client_Synchronous;
