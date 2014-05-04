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

with Broker;
with Broker.Configuration;
with Broker.Messaging;
with Broker.Routing;
with Log;

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;

procedure YAMI4Broker is
   Start_Time : Ada.Calendar.Time;
   Next_Wakeup : Ada.Calendar.Time;
   Success : Boolean;
   Forward_Index : Positive;

   use type Ada.Calendar.Time;

begin
   Broker.Configuration.Init (Success);

   if Success then

      for Module in Broker.Broker_Module'Range loop
         Log.Enable (Module,
                     Broker.Configuration.Log_Enabled (Module));
      end loop;

      Log.Put (Broker.Main, "initialized configuration settings");

      Broker.Routing.Init
        (Broker.Configuration.Max_Subscriptions,
         Broker.Configuration.Max_Client_Queue,
         Broker.Configuration.Subscription_Overflow_Policy);

      --  install all forward channels

      Forward_Index := 1;
      loop
         declare
            Target : constant String :=
              Broker.Configuration.Forward_Target (Forward_Index);
            Filter : constant String :=
              Broker.Configuration.Forward_Filter (Forward_Index);
         begin
            exit when Target'Length = 0;

            Broker.Routing.Forward (Filter, Target);

            Forward_Index := Forward_Index + 1;
         end;
      end loop;

      Broker.Messaging.Init
        (Broker.Configuration.Listener,
         Broker.Configuration.Subscription_Overflow_Policy);

      Start_Time := Ada.Calendar.Clock;
      Next_Wakeup := Start_Time + Broker.Configuration.Warmup_Time;

      delay until Next_Wakeup;

      Broker.Messaging.Allow_Incoming;
   else
      Log.Put (Broker.Main, "Configuration not initialized properly");
   end if;

   loop
      Next_Wakeup := Next_Wakeup + 1000.0; --  arbitrary
      delay until Next_Wakeup;
   end loop;

exception
   when E : others =>
      Log.Put
        (Broker.Main, "Unknown exception in main task: " &
           Ada.Exceptions.Exception_Message (E));
end YAMI4Broker;
