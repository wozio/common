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

with Subscription;

with YAMI.Agents;
with YAMI.Parameters;

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

use Ada.Strings.Unbounded;

procedure Subscriber is

   type Subscriber_Impl is
     new Subscription.Subscriber_Server with null record;

   overriding
   procedure Subscription_Update
     (Server : in out Subscriber_Impl;
      P : in Subscription.Payload) is
   begin
      Ada.Text_IO.Put_Line
          ("received update: " &
             YAMI.Parameters.YAMI_Integer'Image (P.Value));
   end Subscription_Update;

   My_Subscriber : aliased Subscriber_Impl;

begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line
        ("expecting one parameter: publisher destination");
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Failure);
      return;
   end if;

   declare
      Publisher_Address : constant String :=
        Ada.Command_Line.Argument (1);

      Subscriber_Agent : YAMI.Agents.Agent :=
        YAMI.Agents.Make_Agent;

      Update_Object_Name : constant String :=
        "update_handler";

      My_Publisher : Subscription.Publisher;
      
      S : Subscription.Subscription_Info;
   begin
      --  prepare subscription update callback

      Subscriber_Agent.Register_Object
        (Update_Object_Name, My_Subscriber'Unchecked_Access);

      --  subscribe to the producer

      My_Publisher.Initialize_Publisher
        (Subscriber_Agent, Publisher_Address, "random_number");
        
      S.Destination_Object :=
         To_Unbounded_String (Update_Object_Name);

      My_Publisher.Subscribe (S);

      Ada.Text_IO.Put_Line
        ("subscribed, waiting for updates");

      loop
         delay 10.0;
      end loop;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Message (E));
end Subscriber;
