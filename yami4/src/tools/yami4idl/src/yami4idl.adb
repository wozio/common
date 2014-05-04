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

with IDL;
with IDL.Parser;
with IDL.Reader;
with IDL.Structures.Ada_Generator;
with IDL.Structures.CPP_Generator;
with IDL.Structures.IDL_Generator;
with IDL.Structures.Java_Generator;

with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

procedure YAMI4IDL is

   Errors_Found : Boolean := False;

   procedure Process_File (C : in IDL.Name_Lists.Cursor) is
      File_Name : constant String := IDL.Name_Lists.Element (C);
   begin
      IDL.Parser.Reset;
      IDL.Reader.Read_File (File_Name);
   exception
      when E : IDL.Invalid_Input =>
         Put_Line (IDL.Reader.Last_Location & Ada.Exceptions.Exception_Message (E));
         
         Errors_Found := True;
   end Process_File;
   
   YDL_File_Names : IDL.Name_Lists.List;
   
   Output_Dir : Unbounded_String;
   Next_Is_Output_Dir : Boolean := False;
   
   Casing_Style : IDL.Casing_Mode := IDL.Default;
   Next_Is_Casing : Boolean := False;
   
   Generate_Ada : Boolean := False;
   Generate_CPP : Boolean := False;
   Generate_IDL : Boolean := False;
   Generate_Java : Boolean := False;
   
begin
   if Ada.Command_Line.Argument_Count = 0 then
      Put_Line ("no input files");
      return;
   end if;
   
   --  collect command-line options

   for I in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Arg : constant String := Ada.Command_Line.Argument (I);
         
         use type Ada.Directories.File_Kind;
      begin
         if Next_Is_Output_Dir then
         
            if Ada.Directories.Exists (Arg) and then
               Ada.Directories.Kind (Arg) = Ada.Directories.Directory then
               
               Output_Dir := To_Unbounded_String (Arg);
               Next_Is_Output_Dir := False;
               
            else
               Put_Line ("output directory '" & Arg & "' does not exist");
               return;
            end if;
         
         elsif Next_Is_Casing then
         
            Casing_Style := IDL.Casing_Mode'Value (Arg);
            Next_Is_Casing := False;
         
         elsif Arg = "-help" or Arg = "--help" then
            Put_Line ("usage: yami4idl [options] [file1.ydl file2.ydl ...]");
            Put_Line ("options:");
            Put_Line ("    --help             : print this help");
            New_Line;
            Put_Line ("    --ada              : generate Ada code (default casing is ident)");
            Put_Line ("    --cpp              : generate C++ code (default casing is lower_case)");
            Put_Line ("    --idl              : generate IDL code (beware overwriting source files)");
            Put_Line ("    --java             : generate Java code (default casing is camel_case");
            New_Line;
            Put_Line ("    --casing           : ident, lower_case, camel_case or default");
            New_Line;
            Put_Line ("    -I<dir>            : search <dir> for imported packages");
            New_Line;
            Put_Line ("    --output-dir <dir> : directory where new files will be created");
            New_Line;
         elsif Arg = "-ada" or Arg = "--ada" then
            Generate_Ada := True;
         elsif Arg = "-cpp" or Arg = "--cpp" then
            Generate_CPP := True;
         elsif Arg = "-idl" or Arg = "--idl" then
            Generate_IDL := True;
         elsif Arg = "-java" or Arg = "--java" then
            Generate_Java := True;
         elsif Arg = "-casing" or Arg = "--casing" then
            Next_Is_Casing := True;
         elsif Arg (1 .. 2) = "-I" then
            IDL.Reader.Add_Import_Directory (Arg (3 .. Arg'Last));
         elsif Arg = "-output-dir" or Arg = "--output-dir" then
            Next_Is_Output_Dir := True;
         else
            YDL_File_Names.Append (Arg);
         end if;
      end;
   end loop;
   
   if Next_Is_Output_Dir then
      Put_Line ("missing output directory for -I option");
      return;
   end if;
   
   if Next_Is_Casing then
      Put_Line ("missing casing name for --casing option");
      return;
   end if;

   --  process all files
   
   YDL_File_Names.Iterate (Process_File'Access);
   
   if not Errors_Found then
      --  execute requested actions
   
      if Generate_Ada then
         IDL.Structures.Ada_Generator.Generate (To_String (Output_Dir));
      end if;

      if Generate_CPP then
         IDL.Structures.CPP_Generator.Generate (To_String (Output_Dir), Casing_Style);
      end if;
   
      if Generate_IDL then
         IDL.Structures.IDL_Generator.Generate (To_String (Output_Dir));
      end if;

      if Generate_Java then
         IDL.Structures.Java_Generator.Generate (To_String (Output_Dir), Casing_Style);
      end if;
   end if;

end YAMI4IDL;

