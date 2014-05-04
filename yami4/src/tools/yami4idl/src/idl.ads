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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package IDL is

   Invalid_Input : exception;
   Unsupported : exception;

   type Input_Mode is (Full, Import);
   
   type Casing_Mode is (Default, Ident, Lower_Case, Camel_Case);
   
   package Name_Lists is
      new Ada.Containers.Indefinite_Doubly_Linked_Lists
        (Element_Type => String);

end IDL;
