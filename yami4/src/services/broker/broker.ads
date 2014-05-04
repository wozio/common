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

package Broker is

   pragma Pure;

   type Broker_Module is (Main, Subscriptions, Messages);

   --
   --  Queue overflow policy.
   --
   type Overflow_Policy is (Reject_Message, Drop_Update, Unsubscribe);

   --
   --  Implementation limits.
   --
   Max_Pattern_Length : constant := 200;
   Max_Name_Length : constant := 50;

end Broker;
