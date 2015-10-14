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

with Ada.Unchecked_Deallocation;

package body YAMI.Serializables is

   --  convenience helpers

   procedure Do_Free is new Ada.Unchecked_Deallocation
     (Object => Serializables.Serialization_Buffer,
      Name => Serializables.Serialization_Buffer_Access);

   --
   --  Operations of Serializables
   --

   procedure Free (Buffer : in out Serialization_Buffer_Access) is
   begin
      Do_Free (Buffer);
   end Free;

end YAMI.Serializables;
