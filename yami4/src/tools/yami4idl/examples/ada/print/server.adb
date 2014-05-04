with Print_Example;
with YAMI.Agents;

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

use Ada.Strings.Unbounded;

procedure Server is

   type Printer_Impl is
     new Print_Example.Printer_Server with null record;

   overriding
   procedure Print
     (Server : in out Printer_Impl; T : in Print_Example.Text);

   overriding
   procedure Print_Synchronously
     (Server : in out Printer_Impl; T : in Print_Example.Text);

   overriding
   procedure Print
     (Server : in out Printer_Impl; T : in Print_Example.Text) is
   begin
      Ada.Text_IO.Put_Line (To_String (T.Content));
   end Print;

   overriding
   procedure Print_Synchronously
     (Server : in out Printer_Impl; T : in Print_Example.Text) is
   begin
      Ada.Text_IO.Put_Line (To_String (T.Content));
   end Print_Synchronously;
   
   My_Server : aliased Printer_Impl;

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
        ("printer", My_Server'Unchecked_Access);

      loop
         delay 10.0;
      end loop;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Message (E));
end Server;
