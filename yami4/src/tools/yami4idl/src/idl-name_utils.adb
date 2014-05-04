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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;

package body IDL.Name_Utils is

   function Package_To_File_Name (Package_Name : in String) return String is
      Base_Name : String := Package_Name;
      C : Character;
   begin
      for I in Base_Name'Range loop
         C := Base_Name (I);
         C := Ada.Characters.Handling.To_Lower (C);
         if C = Ada.Characters.Latin_1.Full_Stop then
            C := Ada.Characters.Latin_1.Minus_Sign;
         end if;
         
         Base_Name (I) := C;
      end loop;
      
      return Base_Name & ".ydl";
      
   end Package_To_File_Name;

   procedure Verify_Name_Is_Simple (Name : in String) is
   begin
      for I in Name'Range loop
         if Name (I) = Ada.Characters.Latin_1.Full_Stop then
            raise Invalid_Input with
               "'" & Name & "' - this must be a simple name";
         end if;
      end loop;
   end Verify_Name_Is_Simple;
   
   function Name_Is_Qualified (Name : in String) return Boolean is
   begin
      for I in Name'Range loop
         if Name (I) = Ada.Characters.Latin_1.Full_Stop then
            return True;
         end if;
      end loop;
      
      return False;
   end Name_Is_Qualified;

   function Last_Component
     (S : in String; Leave_If_Single : in Boolean := False) return String is
   begin
      for I in reverse S'Range loop
         if S (I) = '.' then
            return S (I + 1 .. S'Last);
         end if;
      end loop;
      
      if Leave_If_Single then
         return S;
      else
         return "";
      end if;
   end Last_Component;

   function Trim_Last_Component
     (S : in String; Leave_If_Single : in Boolean := False) return String is
   begin
      for I in reverse S'Range loop
         if S (I) = '.' then
            return S (S'First .. I - 1);
         end if;
      end loop;
      
      if Leave_If_Single then
         return S;
      else
         return "";
      end if;
   end Trim_Last_Component;
   
   function First_Component
     (S : in String; Leave_If_Single : in Boolean := False) return String is
   begin
      for I in S'Range loop
         if S (I) = '.' then
            return S (S'First .. I - 1);
         end if;
      end loop;
      
      if Leave_If_Single then
         return S;
      else
         return "";
      end if;
   end First_Component;

   function Trim_First_Component
     (S : in String; Leave_If_Single : in Boolean := False) return String is
   begin
      for I in S'Range loop
         if S (I) = '.' then
            return S (I + 1 .. S'Last);
         end if;
      end loop;
      
      if Leave_If_Single then
         return S;
      else
         return "";
      end if;
   end Trim_First_Component;
   
end IDL.Name_Utils;

