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

package Broker.Configuration is

   --
   --  Initializes all configuration options from the config file
   --  that is named in the first command-line parameter or
   --  from the yami4broker.cfg file if no command-line argument is given.
   --
   procedure Init (Success : out Boolean);

   --
   --  Returns the listener endpoint for the messaging part.
   --
   function Listener return String;

   --
   --  Returns the warmup time.
   --
   function Warmup_Time return Duration;

   --
   --  Max size of subscription table.
   --
   function Max_Subscriptions return Positive;

   --
   --  Max queue length for any given subscriber.
   --
   function Max_Client_Queue return Positive;

   --
   --  Queue overflow policy.
   --
   function Subscription_Overflow_Policy return Overflow_Policy;

   --
   --  Initial log level.
   --
   function Log_Enabled (Module : in Broker_Module) return Boolean;

   --
   --  Push channels configuration
   --
   function Forward_Target (Index : in Positive) return String;
   function Forward_Filter (Index : in Positive) return String;

end Broker.Configuration;
