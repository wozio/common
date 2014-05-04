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

with Queue;
with Queue.Configuration;
with Queue.Messaging;
with Queue.Routing;
with Log;

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;

procedure YAMI4Queue is
   Next_Wakeup : Ada.Calendar.Time;
   Success : Boolean;
   Queue_Index : Positive;

   use type Ada.Calendar.Time;

begin
   Queue.Configuration.Init (Success);

   if Success then

      for Module in Queue.Queue_Module'Range loop
         Log.Enable (Module,
                     Queue.Configuration.Log_Enabled (Module));
      end loop;

      Log.Put (Queue.Main, "initialized configuration settings");

      Queue.Routing.Init
        (Queue.Configuration.Max_Queue_Num_Of_Messages,
         Queue.Configuration.Max_Queue_Size,
         Queue.Configuration.Queue_Creation_Policy);

      --  install all forward channels

      Queue_Index := 1;
      loop
         declare
            Queue_Name : constant String :=
              Queue.Configuration.Initial_Queue_Name (Queue_Index);
         begin
            exit when Queue_Name'Length = 0;

            Queue.Routing.Create (Queue_Name);

            Queue_Index := Queue_Index + 1;
         end;
      end loop;

      Queue.Messaging.Init (Queue.Configuration.Listener);

   else
      Log.Put (Queue.Main, "Configuration not initialized properly");
   end if;

   Next_Wakeup := Ada.Calendar.Clock;
   
   loop
      Next_Wakeup := Next_Wakeup + 1000.0; --  arbitrary
      delay until Next_Wakeup;
   end loop;

exception
   when E : others =>
      Log.Put
        (Queue.Main, "Unknown exception in main task: " &
           Ada.Exceptions.Exception_Message (E));
end YAMI4Queue;
