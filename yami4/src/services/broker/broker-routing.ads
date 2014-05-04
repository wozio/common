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

with YAMI.Core.Message_Progress_Handlers;
with YAMI.Parameters;

package Broker.Routing is

   pragma Elaborate_Body;

   procedure Init (Max_Subscriptions : in Positive;
                   Max_Client_Queue : in Positive;
                   Subscription_Overflow_Policy : in Overflow_Policy);

   --
   --  Introduces new subscription or refreshes the existing one.
   --
   procedure Subscribe (Tags : in String;
                        Target_Object : in String;
                        Target_Location : in String);

   --
   --  Introduces new forward channel.
   --
   procedure Forward (Tags : in String;
                      Target_Location : in String);

   --
   --  Iterates the subscriptions that match given tags.
   --
   procedure Iterate_Matching_Subscriptions
     (Tags : in String;
      Process : not null access procedure
      (Target_Object : in String;
       Target_Location : in String;
       Progress_Handler : in
       YAMI.Core.Message_Progress_Handlers.Handler_Access);
      Overflow : out Boolean);

   --
   --  Fills the parameters object with detailed statistics
   --  for each active subscriber.
   --
   procedure Fill_Detailed_Stats
     (Params : in out YAMI.Parameters.Parameters_Collection);

end Broker.Routing;
