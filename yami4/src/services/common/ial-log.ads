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

generic
   type Module is (<>);
   Max_Message_Size : Positive := 200;
   Max_Pending_Messages : Positive := 1000;
package IAL.Log is

   --
   --  Writes message associated with the given module to the log file.
   --
   procedure Put (M : in Module; Msg : in String);

   --
   --  Enables or disables output for the given module.
   --  By default all modules are enabled.
   --
   procedure Enable (M : in Module; Active : in Boolean);

end IAL.Log;
