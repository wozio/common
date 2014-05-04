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

with Cache;
with Cache.Configuration;
with Cache.Messaging;
with Cache.State;
with Log;

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;

procedure YAMI4Cache is
   Next_Wakeup : Ada.Calendar.Time;
   Success : Boolean;

   Data_Eviction_Scan_Period : Duration;
   Perform_Data_Eviction : Boolean;

   use type Ada.Calendar.Time;

begin
   Cache.Configuration.Init (Success);

   if Success then

      for Module in Cache.Cache_Module'Range loop
         Log.Enable (Module,
                     Cache.Configuration.Log_Enabled (Module));
      end loop;

      Log.Put (Cache.Main, "initialized configuration settings");

      Data_Eviction_Scan_Period :=
        Cache.Configuration.Data_Eviction_Scan_Period;

      if Data_Eviction_Scan_Period > 0.0 then
         Perform_Data_Eviction := True;
      else
         Perform_Data_Eviction := False;

         Log.Put (Cache.Main, "data items will never expire");

         --  arbitrary value to avoid spinning in the following loop
         Data_Eviction_Scan_Period := 60.0;
      end if;

      Cache.State.Init (Cache.Configuration.Data_Max_Size,
                        Cache.Configuration.Data_Eviction_Time);
      Cache.Messaging.Init (Cache.Configuration.Listener);

   else
      Log.Put (Cache.Main, "Configuration not initialized properly");
   end if;

   Next_Wakeup := Ada.Calendar.Clock;
   loop
      Next_Wakeup := Next_Wakeup + Data_Eviction_Scan_Period;
      delay until Next_Wakeup;

      if Perform_Data_Eviction then
         Cache.State.Evict_Old_Values;
      end if;

   end loop;

exception
   when E : others =>
      Log.Put
        (Cache.Main, "Unknown exception in main task: " &
           Ada.Exceptions.Exception_Message (E));
end YAMI4Cache;
