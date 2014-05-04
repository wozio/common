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

package Queue.Configuration is

   --
   --  Initializes all configuration options from the config file
   --  that is named in the first command-line parameter or
   --  from the yami4queue.cfg file if no command-line argument is given.
   --
   procedure Init (Success : out Boolean);

   --
   --  Returns the listener endpoint for the messaging part.
   --
   function Listener return String;

   --
   --  Max number of messages and total capacity of any given queue.
   --
   function Max_Queue_Num_Of_Messages return Positive;
   function Max_Queue_Size return Positive;

   --
   --  Max number of clients waiting on any given queue.
   --
   function Max_Waiting_Clients return Positive;

   --
   --  Queue creation policy.
   --
   function Queue_Creation_Policy return Creation_Policy;

   --
   --  Initial log level.
   --
   function Log_Enabled (Module : in Queue_Module) return Boolean;

   --
   --  Initial set of queues.
   --
   function Initial_Queue_Name (Index : in Positive) return String;

end Queue.Configuration;
