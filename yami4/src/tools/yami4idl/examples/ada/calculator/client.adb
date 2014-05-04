with Calculator;

with YAMI.Agents;
with YAMI.Parameters;

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

procedure Client is
   Op : Calculator.Operands;
   Res : Calculator.Results;
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
      Op.A := YAMI.Parameters.YAMI_Integer'Value
        (Ada.Command_Line.Argument (2));
      Op.B := YAMI.Parameters.YAMI_Integer'Value
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

      My_Calculator : Calculator.Operations;

   begin

      My_Calculator.Initialize_Operations
        (Client_Agent, Server_Address, "calculator");

      My_Calculator.Calculate (Op, Res);

      Ada.Text_IO.Put_Line ("sum        = " &
         YAMI.Parameters.YAMI_Integer'Image (Res.Sum));
      Ada.Text_IO.Put_Line ("difference = " &
         YAMI.Parameters.YAMI_Integer'Image (Res.Difference));
      Ada.Text_IO.Put_Line ("product    = " &
         YAMI.Parameters.YAMI_Integer'Image (Res.Product));
      Ada.Text_IO.Put ("ratio      = ");
      if Res.Ratio_Valid then
         Ada.Text_IO.Put_Line (YAMI.Parameters.YAMI_Integer'Image (Res.Ratio));
      else
         Ada.Text_IO.Put_Line ("<undefined>");
      end if;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Message (E));
end Client;
