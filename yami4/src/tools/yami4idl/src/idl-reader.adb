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

with IDL.Name_Utils;
with IDL.Parser;
with IDL.Structures;
with IDL.Tokenizer;

with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

use Ada.Characters.Latin_1;
use Ada.Strings.Unbounded;

package body IDL.Reader is

   Line : Positive;
   Column : Positive;
   Current_File_Name : Unbounded_String;

   Import_Dirs : Name_Lists.List;

   function Trimmed_Image (V : in Positive) return String is
      Image : constant String := Positive'Image (V);
   begin
      return Image (2 .. Image'Last);
   end Trimmed_Image;

   function Last_Location return String is
   begin      
      return To_String (Current_File_Name) & ":" & Trimmed_Image (Line) & ":" &
         Trimmed_Image (Column - IDL.Tokenizer.Last_Accumulated_Length) & ": ";
   end Last_Location;

   procedure Add_Import_Directory (Import_Dir_Name : in String) is
      use type Ada.Directories.File_Kind;
   begin
      if Ada.Directories.Exists (Import_Dir_Name) and then
         Ada.Directories.Kind (Import_Dir_Name) = Ada.Directories.Directory then

         Import_Dirs.Append (Import_Dir_Name);
      else
         Ada.Text_IO.Put_Line ("'" & Import_Dir_Name & "' is not a directory");
      end if;
   end Add_Import_Directory;

   function Resolved_File_Name (File_Name : in String) return String is
      C : Name_Lists.Cursor;
      
      use type Name_Lists.Cursor;
   begin
      if Ada.Directories.Exists (File_Name) then
         return File_Name;
      end if;
      
      C := Import_Dirs.First;
      while C /= Name_Lists.No_Element loop
         declare
            Candidate_Name : constant String :=
               Ada.Directories.Compose (Name_Lists.Element (C), File_Name);
         begin
            if Ada.Directories.Exists (Candidate_Name) then
               return Candidate_Name;
            end if;
         end;
         
         Name_Lists.Next (C);
      end loop;
      
      return "";
      
   end Resolved_File_Name;
   
   procedure Read_File (File_Name : in String) is
   
      Res_File_Name : constant String := Resolved_File_Name (File_Name);
      File : Ada.Text_IO.File_Type;
      C : Character;
      
   begin
      if Res_File_Name = "" then
         Ada.Text_IO.Put_Line ("cannot find file '" & File_Name & "'");
         return;
      end if;
      
      Current_File_Name := To_Unbounded_String (Res_File_Name);
      Line := 1;
      Column := 1;
      
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Res_File_Name);
      while not Ada.Text_IO.End_Of_File (File) loop
         Ada.Text_IO.Get_Immediate (File, C);
      
         if C /= CR then
            IDL.Tokenizer.Tokenize (C);
         end if;

         if C = LF then
            Line := Line + 1;
            Column := 1;
         else
            Column := Column + 1;
         end if;
      
      end loop;
      
      --  ensure tokens from distinct files are not concatenated
      IDL.Tokenizer.Tokenize (LF);
      
      Ada.Text_IO.Close (File);

      if not IDL.Parser.Finished then
         Ada.Text_IO.Put_Line ("file '" & File_Name & "' is not complete");
      end if;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Ada.Text_IO.Put_Line ("cannot open file '" & File_Name & "'");
         
   end Read_File;

   procedure Import_Package (Package_Name : in String) is
      Tmp_Line : Positive := Line;
      Tmp_Column : Positive := Column;
      Tmp_Current_File_Name : Unbounded_String := Current_File_Name;
   begin
      IDL.Tokenizer.Reset;
      IDL.Parser.Reset;
      IDL.Parser.Set_Mode (Import);
      IDL.Structures.Set_Mode (Import);
      
      Read_File (Name_Utils.Package_To_File_Name (Package_Name));
      
      IDL.Parser.Set_Mode (Full);
      IDL.Structures.Set_Mode (Full);
      
      Line := Tmp_Line;
      Column := Tmp_Column;
      Current_File_Name := Tmp_Current_File_Name;
   end Import_Package;
   
end IDL.Reader;
