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

with Ada.Calendar.Formatting;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

procedure Publisher is
begin
   if Ada.Command_Line.Argument_Count /= 4 then
      Ada.Text_IO.Put_Line ("expecting 4 parameters:");
      Ada.Text_IO.Put_Line ("    broker address");
      Ada.Text_IO.Put_Line ("    num of messages in a burst");
      Ada.Text_IO.Put_Line ("    num of bursts");
      Ada.Text_IO.Put_Line ("    delay between bursts");
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Failure);
      return;
   end if;

   declare
      Broker_Address : constant String :=
        Ada.Command_Line.Argument (1);

      Burst_Size : constant Positive :=
        Positive'Value (Ada.Command_Line.Argument (2));

      Num_Of_Bursts : constant Positive :=
        Positive'Value (Ada.Command_Line.Argument (3));

      Wait_Time : constant Duration :=
        Duration'Value (Ada.Command_Line.Argument (4));

      Shortest_Burst : Duration := Duration'Last;
      Longest_Burst : Duration := Duration'First;
      All_Bursts : Duration := 0.0;

      Publisher_Agent : YAMI.Agents.Agent :=
        YAMI.Agents.Make_Agent;

      Params :
        YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;

      --  last message in each burst is synchronized
      Last_In_Burst :
        aliased YAMI.Outgoing_Messages.Outgoing_Message;

      Lost_Count : Natural := 0;

      Start : Ada.Calendar.Time;
      Stop : Ada.Calendar.Time;
      Test_Duration : Duration;
      Burst_Start : Ada.Calendar.Time;
      Burst_Stop : Ada.Calendar.Time;
      Burst_Duration : Duration;

      use type Ada.Calendar.Time;
   begin
      Publisher_Agent.Open_Connection (Broker_Address);

      Start := Ada.Calendar.Clock;

      for I in 1 .. Num_Of_Bursts loop
         Burst_Start := Ada.Calendar.Clock;

         for J in 1 .. Burst_Size - 1 loop

            Params.Set_Integer ("value", YAMI.Parameters.YAMI_Integer (J));

            Publisher_Agent.Send_One_Way
              (Broker_Address, "abc", "publish", Params);

         end loop;

         --  finishing message in the burst
         Params.Set_Integer ("value", 0);

         Publisher_Agent.Send
           (Broker_Address, "abc", "publish_confirm", Params,
            Last_In_Burst'Unchecked_Access);

         Last_In_Burst.Wait_For_Completion;

         Burst_Stop := Ada.Calendar.Clock;
         Burst_Duration := Burst_Stop - Burst_Start;

         All_Bursts := All_Bursts + Burst_Duration;
         if Burst_Duration > Longest_Burst then
            Longest_Burst := Burst_Duration;
         end if;
         if Burst_Duration < Shortest_Burst then
            Shortest_Burst := Burst_Duration;
         end if;

         declare
            Last_State : YAMI.Outgoing_Messages.Message_State :=
              Last_In_Burst.State;

            use type YAMI.Outgoing_Messages.Message_State;
         begin
            if Last_State /= YAMI.Outgoing_Messages.Replied then
               Lost_Count := Lost_Count + 1;
            end if;
         end;

         delay Wait_Time;
      end loop;

      Stop := Ada.Calendar.Clock;
      Test_Duration := Stop - Start;

      Ada.Text_IO.Put
        ("all messages published in " &
           Ada.Calendar.Formatting.Image (Test_Duration, True) & " (");
      Ada.Text_IO.Put
        (Integer'Image
           (Integer (Float (Num_Of_Bursts * Burst_Size) /
                       Float (Test_Duration))));
      Ada.Text_IO.Put_Line (" msg/s )");

      Ada.Text_IO.Put
        ("shortest burst            " &
           Ada.Calendar.Formatting.Image (Shortest_Burst, True) & " (");
      Ada.Text_IO.Put
        (Integer'Image
           (Integer (Float (Burst_Size) / Float (Shortest_Burst))));
      Ada.Text_IO.Put_Line (" msg/s )");

      Ada.Text_IO.Put
        ("longest burst             " &
           Ada.Calendar.Formatting.Image (Longest_Burst, True) & " (");
      Ada.Text_IO.Put
        (Integer'Image
           (Integer (Float (Burst_Size) / Float (Longest_Burst))));
      Ada.Text_IO.Put_Line (" msg/s )");

      if Lost_Count /= 0 then
         Ada.Text_IO.Put_Line (Natural'Image (Lost_Count) &
                                 " bursts have losses");
      end if;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        ("error: " & Ada.Exceptions.Exception_Message (E));
end Publisher;
