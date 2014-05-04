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
with Ada.Directories;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Text_IO;
use Ada.Text_IO;

package body IDL.Structures.IDL_Generator is

   function File_Name (Output_Dir : in String; Package_Name : in String) return String is
      Base_Name : String (1 .. Package_Name'Length);
      C : Character;
   begin
      for I in Package_Name'Range loop
         C := Package_Name (I);
         if C = Ada.Characters.Latin_1.Full_Stop then
            C := Ada.Characters.Latin_1.Minus_Sign;
         elsif Ada.Characters.Handling.Is_Upper (C) then
            C := Ada.Characters.Handling.To_Lower (C);
         end if;
         
         Base_Name (I) := C;
      end loop;
      
      return Ada.Directories.Compose (Output_Dir, Base_Name, "ydl");
   end File_Name;
   
   procedure Write (File : in File_Type; Package_Name : in String) is
   
      procedure Process_Package_Definitions (Key : in String; Defs : in Package_Definitions) is
      
         procedure Process_Import (C : in Name_Lists.Cursor) is
         begin
            Put_Line (File, "import " & Name_Lists.Element (C) & ';');
         end Process_Import;
               
         procedure Process_Type (C : in Name_Lists.Cursor) is
         
            procedure Process_Fields (Key : in String; Fields : in Type_Fields) is
            
               procedure Process_Field (C : in Name_Lists.Cursor) is
                  Field_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Field_Def (Key : in String; F : in Field_Definition) is
                  begin
                     Put (File, "      " & Field_Name & " : ");
                  
                     if F.Optional then
                        Put (File, "optional ");
                     end if;
                     
                     Put_Line (File, To_String (F.Type_Name) & ';');
                  end Process_Field_Def;
                  
                  CF : Field_Maps.Cursor;
               begin
                  CF := Fields.Definitions.Find (Field_Name);
                  Field_Maps.Query_Element (CF, Process_Field_Def'Access);
               end Process_Field;
            
            begin
               Fields.Ordered_Names.Iterate (Process_Field'Access);
            end Process_Fields;
         
            Type_Name : constant String := Name_Lists.Element (C);
            
            CT : Type_Maps.Cursor;
            
         begin
            Put_Line (File, "   type " & Type_Name & " is");
            
            CT := Defs.Type_Definitions.Find (Type_Name);
            Type_Maps.Query_Element (CT, Process_Fields'Access);
            
            Put_Line (File, "   end " & Type_Name & ';');
            New_Line (File);
         end Process_Type;
      
         procedure Process_Interface (C : in Name_Lists.Cursor) is
         
            procedure Process_Messages (Key : in String; Messages : in Interface_Messages) is
            
               procedure Process_Message (C : in Name_Lists.Cursor) is
                  Message_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Message_Def (Key : in String; M : in Message_Definition) is
                  begin
                     if M.Oneway then
                        Put (File, "      oneway message ");
                     else
                        Put (File, "      message ");
                     end if;
                     
                     Put (File, Message_Name);
                  
                     if M.In_Param_Name /= Null_Unbounded_String or
                        M.Out_Param_Name /= Null_Unbounded_String then
                        
                        Put (File, " (");
                        
                        if M.In_Param_Name /= Null_Unbounded_String then
                           Put (File, To_String (M.In_Param_Name) & " : in " &
                              To_String (M.In_Param_Type));
                           
                           if M.Out_Param_Name /= Null_Unbounded_String then
                              Put (File, "; ");
                           end if;
                        end if;
                        
                        if M.Out_Param_Name /= Null_Unbounded_String then
                           Put (File, To_String (M.Out_Param_Name) & " : out " &
                              To_String (M.Out_Param_Type));
                        end if;

                        Put (File, ")");
                     end if;
                     
                     Put_Line (File, ";");
                  end Process_Message_Def;
                  
                  CM : Message_Maps.Cursor;
               begin
                  CM := Messages.Definitions.Find (Message_Name);
                  Message_Maps.Query_Element (CM, Process_Message_Def'Access);
               end Process_Message;
            
            begin
               Messages.Ordered_Names.Iterate (Process_Message'Access);
            end Process_Messages;
         
            Interface_Name : constant String := Name_Lists.Element (C);
            
            CI : Interface_Maps.Cursor;
            
         begin
            Put_Line (File, "   interface " & Interface_Name & " is");
            
            CI := Defs.Interface_Definitions.Find (Interface_Name);
            Interface_Maps.Query_Element (CI, Process_Messages'Access);
            
            Put_Line (File, "   end " & Interface_Name & ';');
            New_Line (File);
         end Process_Interface;
      
      begin
         Defs.Explicit_Imports.Iterate (Process_Import'Access);
         if not Defs.Explicit_Imports.Is_Empty then
            New_Line (File);
         end if;
         
         Put_Line (File, "package " & Package_Name & " is");
         New_Line (File);
         
         Defs.Ordered_Type_Names.Iterate (Process_Type'Access);
         
         Defs.Ordered_Interface_Names.Iterate (Process_Interface'Access);
         
         Put_Line (File, "end " & Package_Name & ';');
      end Process_Package_Definitions;
      
      C : Package_Maps.Cursor;
      
   begin
      Put_Line (File, "--");
      Put_Line (File, "--  IDL definitions for package " & Package_Name & '.');
      Put_Line (File, "--  This file was generated automatically by yami4idl.");
      Put_Line (File, "--");
      New_Line (File);
      
      C := All_Packages.Find (Package_Name);
      Package_Maps.Query_Element (C, Process_Package_Definitions'Access);
   end Write;

   procedure Generate (Output_Dir : in String) is

      procedure Process_Fully_Defined_Package (C : in Name_Lists.Cursor) is
      
         Package_Name : constant String := Name_Lists.Element (C);
         F_Name : constant String := File_Name (Output_Dir, Package_Name);
      
         File : File_Type;
      begin
         Create (File, Out_File, F_Name);
         Write (File, Package_Name);
         Close (File);
      end Process_Fully_Defined_Package;

   begin
      Fully_Defined_Packages.Iterate (Process_Fully_Defined_Package'Access);
   end Generate;

end IDL.Structures.IDL_Generator;

