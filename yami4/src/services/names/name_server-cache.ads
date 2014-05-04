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

package Name_Server.Cache is

   pragma Elaborate_Body;

   --
   --  Stores new binding.
   --
   procedure Bind (Object_Name : in String;
                   Location : in String);

   --
   --  Resolves a given name. Returns empty string if name is not found.
   --
   function Resolve (Object_Name : in String) return String;

   --
   --  Returns the current size of cache.
   --
   function Length return Natural;

   --
   --  Iterates over the whole cache.
   --
   procedure Iterate (Process : not null access
                      procedure (Object_Name : in String;
                                 Location : in String));

end Name_Server.Cache;
