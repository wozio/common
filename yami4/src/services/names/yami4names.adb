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

with Name_Server;
with Name_Server.Configuration;
with Name_Server.Messaging;
with Log;

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;

procedure YAMI4Names is
   Next_Wakeup : Ada.Calendar.Time;
   Success : Boolean;

   use type Ada.Calendar.Time;

begin
   Name_Server.Configuration.Init (Success);

   if Success then

      for Module in Name_Server.Name_Server_Module'Range loop
         Log.Enable (Module,
                     Name_Server.Configuration.Log_Enabled (Module));
      end loop;

      Log.Put (Name_Server.Main, "initialized configuration settings");

      Name_Server.Messaging.Init
        (Name_Server.Configuration.Listener,
         Name_Server.Configuration.Data_Directory);

   else
      Log.Put (Name_Server.Main, "Configuration not initialized properly");
   end if;

   Next_Wakeup := Ada.Calendar.Clock;
   loop
      Next_Wakeup := Next_Wakeup + 1000.0; --  arbitrary
      delay until Next_Wakeup;
   end loop;

exception
   when E : others =>
      Log.Put
        (Name_Server.Main, "Unknown exception in main task: " &
           Ada.Exceptions.Exception_Message (E));
end YAMI4Names;
