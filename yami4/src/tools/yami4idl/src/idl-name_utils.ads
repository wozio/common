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

package IDL.Name_Utils is

   function Package_To_File_Name (Package_Name : in String) return String;

   procedure Verify_Name_Is_Simple (Name : in String);
   
   function Name_Is_Qualified (Name : in String) return Boolean;

   function Last_Component
     (S : in String; Leave_If_Single : in Boolean := False) return String;

   function Trim_Last_Component
     (S : in String; Leave_If_Single : in Boolean := False) return String;
   
   function First_Component
     (S : in String; Leave_If_Single : in Boolean := False) return String;

   function Trim_First_Component
     (S : in String; Leave_If_Single : in Boolean := False) return String;
   
end IDL.Name_Utils;

