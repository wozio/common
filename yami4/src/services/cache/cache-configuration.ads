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

with Ada.Streams;

package Cache.Configuration is

   --
   --  Initializes all configuration options from the config file
   --  that is named in the first command-line parameter or
   --  from the yami4cache.cfg file if no command-line argument is given.
   --
   procedure Init (Success : out Boolean);

   --
   --  Returns the listener endpoint for the messaging part.
   --
   function Listener return String;

   --
   --  Returns the limit of total space used by data values.
   --
   function Data_Max_Size return Ada.Streams.Stream_Element_Count;

   --
   --  Returns the default data eviction time.
   --
   function Data_Eviction_Time return Duration;

   --
   --  Returns the default data eviction scan period.
   --
   function Data_Eviction_Scan_Period return Duration;

   --
   --  Initial log level.
   --
   function Log_Enabled (Module : in Cache_Module) return Boolean;

end Cache.Configuration;
