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

package YAMI is

   pragma Pure (YAMI);

   --
   --  General exception for reporting logic errors.
   --
   --  Logic errors represent misuses of the API
   --  like invalid arguments, out of range indexes or type mismatches.
   --
   Logic_Error : exception;

   --
   --  General exception for reporting run-time errors.
   --
   --  Runtime errors represent problems that might not result
   --  from incorrect library usage, but can be related to
   --  resource constraints or communication problems.
   --
   Runtime_Error : exception;

   --
   --  Library version information.
   --
   Version_Name : constant String := "1.10.1";
   Version_Number : constant := 11001;

end YAMI;
