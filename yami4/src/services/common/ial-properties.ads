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

package IAL.Properties is

   --
   --  Loads the properties from a given file.
   --  The new mappings (name=value) will be applied onto the existing ones.
   --
   procedure Load_Properties (File_Name : in String);

   --
   --  Clears the properties that were read from file(s).
   --  Does not clear the properties set by command-line options.
   --
   procedure Clear;

   --
   --  Returns True is the given property is defined either in
   --  the regular property set or in the properties defined by
   --  command-line options.
   --
   function Is_Defined (Name : in String) return Boolean;

   --
   --  Returns the value for the given name, or Default if no
   --  such name is found.
   --  During the lookup, the command-line properties override those
   --  read from file(s).
   --
   function Get (Name : in String; Default : in String := "") return String;

   --
   -- Iterates over the whole set (first command-line properties then
   -- those from files).
   --
   procedure Iterate (Process : not null access
                      procedure (Name : in String;
                                 Value : in String)
                     );

end IAL.Properties;
