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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package body Name_Server.Cache is

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => String,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   Names : String_Maps.Map;

   procedure Bind (Object_Name : in String;
                   Location : in String) is
   begin
      Names.Include (Object_Name, Location);
   end Bind;

   function Resolve (Object_Name : in String) return String is
      C : String_Maps.Cursor;
   begin
      C := Names.Find (Object_Name);
      if String_Maps.Has_Element (C) then
         return String_Maps.Element (C);
      else
         return "";
      end if;
   end Resolve;

   function Length return Natural is
   begin
      return Natural (Names.Length);
   end Length;

   procedure Iterate (Process : not null access
                      procedure (Object_Name : in String;
                                 Location : in String)) is

      procedure Translate_Iteration (Position : in String_Maps.Cursor) is
      begin
         String_Maps.Query_Element (Position, Process);
      end Translate_Iteration;

   begin
      String_Maps.Iterate (Names, Translate_Iteration'Access);
   end Iterate;

end Name_Server.Cache;
