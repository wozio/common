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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Exceptions;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Text_IO;
use Ada.Text_IO;

package body IDL.Structures.Ada_Generator is

   function Base_File_Name (Package_Name : in String) return String is
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
      
      return Base_Name;
   end Base_File_Name;
      
   function Specs_File_Name (Output_Dir : in String; Package_Name : in String) return String is
   begin
      return Ada.Directories.Compose (Output_Dir, Base_File_Name (Package_Name), "ads");
   end Specs_File_Name;
   
   function Body_File_Name (Output_Dir : in String; Package_Name : in String) return String is
   begin
      return Ada.Directories.Compose (Output_Dir, Base_File_Name (Package_Name), "adb");
   end Body_File_Name;
   
   function Ada_Name (IDL_Name : in String) return String is
   begin
      return IDL_Name;
   end Ada_Name;
   
   function Ada_Package_Prefix (IDL_Name : in String) return String is
      Package_Name : constant String := Name_Utils.Trim_Last_Component (IDL_Name);
   begin
      if Package_Name /= "" then
         return Package_Name & ".";
      else
         return "";
      end if;
   end Ada_Package_Prefix;
   
   function Ada_Field_Valid_Name (IDL_Name : in String) return String is
   begin
      return Ada_Name (IDL_Name & "_Valid");
   end Ada_Field_Valid_Name;

   function YAMI4_Field_Name (IDL_Name : in String) return String is
   begin
      --  "communication" field names are always lower_case with underscore
      
      return Ada.Characters.Handling.To_Lower (IDL_Name);
   end YAMI4_Field_Name;

   function YAMI4_Operation_Name (IDL_Name : in String) return String is
   begin
      --  "communication" message names are always lower-case with underscore
      
      return Ada.Characters.Handling.To_Lower (IDL_Name);
   end YAMI4_Operation_Name;

   function Ada_Type (IDL_Type : in String) return String is
   begin
      if IDL_Type = K_Boolean then
         return "Boolean";
      elsif IDL_Type = K_Integer then
         return "YAMI.Parameters.YAMI_Integer";
      elsif IDL_Type = K_Long_Long then
         return "YAMI.Parameters.YAMI_Long_Long_Integer";
      elsif IDL_Type = K_Float then
         return "YAMI.Parameters.YAMI_Long_Float";
      elsif IDL_Type = K_String then
         return "Unbounded_String";
      elsif IDL_Type = K_Binary then
         return "YAMI.Parameters.Binary_Holder";
      elsif IDL_Type = K_Boolean_Array then
         return "YAMI.Parameters.Boolean_Array_Holder";
      elsif IDL_Type = K_Integer_Array then
         return "YAMI.Parameters.YAMI_Integer_Array_Holder";
      elsif IDL_Type = K_Long_Long_Array then
         return "YAMI.Parameters.YAMI_Long_Long_Integer_Array_Holder";
      elsif IDL_Type = K_Float_Array then
         return "YAMI.Parameters.YAMI_Long_Float_Array_Holder";
      elsif IDL_Type = K_String_Array then
         return "YAMI.Parameters.String_Array";
      elsif IDL_Type = K_Binary_Array then
         return "YAMI.Parameters.Binary_Array";
      else
         --  user-defined type
         
         return Ada_Name (IDL_Type);
      end if;
   end Ada_Type;

   procedure Finish_Operation_Signature (File : in File_Type; M : in Message_Definition) is
   begin
      if M.In_Param_Name /= Null_Unbounded_String then
         Put_Line (File, ";");
         
         Put (File, "      " & Ada_Name (To_String (M.In_Param_Name)) &
            " : in " & Ada_Name (To_String (M.In_Param_Type)));
                        
         if M.Out_Param_Name /= Null_Unbounded_String then
            Put (File, "; ");
         end if;
      end if;
                     
      if M.Out_Param_Name /= Null_Unbounded_String then
         if M.In_Param_Name = Null_Unbounded_String then
            Put_Line (File, ";");
            Put (File, "      ");
         end if;
         
         Put (File, Ada_Name (To_String (M.Out_Param_Name)) &
            " : out " & Ada_Name (To_String (M.Out_Param_Type)));
      end if;
      
      Put (File, ")");
   end Finish_Operation_Signature;

   type Mode_Type is (Client, Server);
   Mode : Mode_Type;
            
   procedure Write_Specs (File : in File_Type; Package_Name : in String) is
   
      procedure Process_Package_Definitions (Key : in String; Defs : in Package_Definitions) is
      
         procedure Process_Import (C : in Name_Lists.Cursor) is
         begin
            Put_Line (File, "with " & Name_Lists.Element (C) & ";");
         end Process_Import;
               
         procedure Process_Type (C : in Name_Lists.Cursor) is
         
            procedure Process_Fields (Key : in String; Fields : in Type_Fields) is
            
               procedure Process_Field (C : in Name_Lists.Cursor) is
                  Field_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Field_Def (Key : in String; F : in Field_Definition) is
                  begin
                     if F.Optional then
                        Put_Line (File, "      " &
                           Ada_Field_Valid_Name (Field_Name) & " : Boolean := False;");
                     end if;
                     
                     Put_Line (File, "      " & Ada_Name (Field_Name) &
                        " : " & Ada_Type (To_String (F.Type_Name)) & ";");
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
            Put_Line (File, "   type " & Ada_Name (Type_Name) & " is record");
            
            CT := Defs.Type_Definitions.Find (Type_Name);
            Type_Maps.Query_Element (CT, Process_Fields'Access);
            
            Put_Line (File, "   end record;");
            New_Line (File);
            Put_Line (File, "   procedure Write (V_Y4 : in " & Ada_Name (Type_Name) & ";");
            Put_Line (File, "      P_Y4 : in out YAMI.Parameters.Parameters_Collection);");
            Put_Line (File, "   procedure Read (V_Y4 : out " & Ada_Name (Type_Name) & ";");
            Put_Line (File, "      P_Y4 : in YAMI.Parameters.Parameters_Collection);");
            New_Line (File);
         end Process_Type;
      
         procedure Process_Interface (C : in Name_Lists.Cursor) is
         
            Interface_Name : constant String := Name_Lists.Element (C);
            
            procedure Process_Messages (Key : in String; Messages : in Interface_Messages) is
            
               procedure Process_Message (C : in Name_Lists.Cursor) is
                  Message_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Message_Def (Key : in String; M : in Message_Definition) is
                  begin
                     if Mode = Server then
                        Put (File, "   procedure " & Ada_Name (Message_Name) &
                           " (Server_Y4 : in out " & Ada_Name (Interface_Name & "_Server"));
                     else
                        Put (File, "   procedure " & Ada_Name (Message_Name) &
                           " (Client_Y4 : in out " & Ada_Name (Interface_Name));
                     end if;

                     Finish_Operation_Signature (File, M);

                     if Mode = Server then
                        Put_Line (File, " is abstract;");
                     else
                        Put_Line (File, ";");
                     end if;
                  end Process_Message_Def;
                  
                  CM : Message_Maps.Cursor;
               begin
                  CM := Messages.Definitions.Find (Message_Name);
                  Message_Maps.Query_Element (CM, Process_Message_Def'Access);
               end Process_Message;
            
            begin
               Messages.Ordered_Names.Iterate (Process_Message'Access);
            end Process_Messages;
         
            CI : Interface_Maps.Cursor;
            
         begin
            Put_Line (File, "   type " & Ada_Name (Interface_Name) & " is tagged record");
            Put_Line (File, "      Agent : YAMI.Agents.Agent_Access;");
            Put_Line (File, "      Server_Location : Unbounded_String;");
            Put_Line (File, "      Object_Name : Unbounded_String;");
            Put_Line (File, "      Timeout : Duration;");
            Put_Line (File, "   end record;");
            New_Line (File);
            Put_Line (File, "   procedure Initialize_" & Ada_Name (Interface_Name));
            Put_Line (File, "     (Client_Y4 : out " & Ada_Name (Interface_Name) & ";");
            Put_Line (File, "      Agent : in out YAMI.Agents.Agent;");
            Put_Line (File, "      Server_Location : in String;");
            Put_Line (File, "      Object_Name : in String;");
            Put_Line (File, "      Timeout : in Duration := 0.0);");
            New_Line (File);

            CI := Defs.Interface_Definitions.Find (Interface_Name);
            Mode := Client;
            Interface_Maps.Query_Element (CI, Process_Messages'Access);
            
            New_Line (File);
            Put_Line (File, "   type " & Ada_Name (Interface_Name & "_Server") & " is abstract");
            Put_Line (File, "      new YAMI.Incoming_Messages.Message_Handler with null record;");
            New_Line (File);
            
            CI := Defs.Interface_Definitions.Find (Interface_Name);
            Mode := Server;
            Interface_Maps.Query_Element (CI, Process_Messages'Access);
            
            New_Line (File);
            Put_Line (File, "   overriding procedure Call (Server_Y4 : in out " &
               Ada_Name (Interface_Name & "_Server") & ";");
            Put_Line (File, "      Message_Y4 : in out YAMI.Incoming_Messages.Incoming_Message'Class);");
            New_Line (File);
         end Process_Interface;
      
      begin
         if not Defs.Explicit_Imports.Is_Empty then
            Defs.Explicit_Imports.Iterate (Process_Import'Access);
            New_Line (File);
         end if;

         Put_Line (File, "package " & Package_Name & " is");

         New_Line (File);
         
         Defs.Ordered_Type_Names.Iterate (Process_Type'Access);
         
         Defs.Ordered_Interface_Names.Iterate (Process_Interface'Access);
         
         Put_Line (File, "end " & Package_Name & ";");

      end Process_Package_Definitions;

      C : Package_Maps.Cursor;
      
   begin
      Put_Line (File, "--");
      Put_Line (File, "--  Ada specifications for package " & Package_Name & '.');
      Put_Line (File, "--  This file was generated automatically by yami4idl.");
      Put_Line (File, "--");
      New_Line (File);
      Put_Line (File, "with YAMI.Agents;");
      Put_Line (File, "with YAMI.Incoming_Messages;");
      Put_Line (File, "with YAMI.Outgoing_Messages;");
      Put_Line (File, "with YAMI.Parameters;");
      New_Line (File);
      Put_Line (File, "with Ada.Strings.Unbounded;");
      Put_Line (File, "use Ada.Strings.Unbounded;");
      New_Line (File);

      C := All_Packages.Find (Package_Name);
      Package_Maps.Query_Element (C, Process_Package_Definitions'Access);

   end Write_Specs;

   procedure Write_Body (File : in File_Type; Package_Name : in String) is
   
      procedure Process_Package_Definitions (Key : in String; Defs : in Package_Definitions) is
      
         procedure Process_Type (C : in Name_Lists.Cursor) is
         
            procedure Process_Fields_Writer (Key : in String; Fields : in Type_Fields) is
            
               procedure Process_Field (C : in Name_Lists.Cursor) is
                  Field_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Field_Def (Key : in String; F : in Field_Definition) is
                     Type_Name : constant String := To_String (F.Type_Name);
                  begin
                     if F.Optional then
                        Put_Line (File, "      if V_Y4." & Ada_Field_Valid_Name (Field_Name) & " then");
                     end if;

                     if Type_Name = K_Boolean then
                        Put_Line (File, "      P_Y4.Set_Boolean (""" &
                           YAMI4_Field_Name (Field_Name) & """, V_Y4." & Ada_Name (Field_Name) & ");");
                     elsif Type_Name = K_Integer then
                        Put_Line (File, "      P_Y4.Set_Integer (""" &
                           YAMI4_Field_Name (Field_Name) & """, V_Y4." & Ada_Name (Field_Name) & ");");
                     elsif Type_Name = K_Long_Long then
                        Put_Line (File, "      P_Y4.Set_Long_Long (""" &
                           YAMI4_Field_Name (Field_Name) & """, V_Y4." & Ada_Name (Field_Name) & ");");
                     elsif Type_Name = K_Float then
                        Put_Line (File, "      P_Y4.Set_Double_Float (""" &
                           YAMI4_Field_Name (Field_Name) & """, V_Y4." & Ada_Name (Field_Name) & ");");
                     elsif Type_Name = K_String then
                        Put_Line (File, "      P_Y4.Set_String (""" &
                           YAMI4_Field_Name (Field_Name) & """,");
                        Put_Line (File, "         To_String (V_Y4." & Ada_Name (Field_Name) & "));");
                     elsif Type_Name = K_Binary then
                        Put_Line (File, "      declare");
                        Put_Line (File, "         procedure Process " &
                           "(A : in Ada.Streams.Stream_Element_Array) is");
                        Put_Line (File, "         begin");
                        Put_Line (File, "            P_Y4.Set_Binary (""" &
                           YAMI4_Field_Name (Field_Name) & """, A);");
                        Put_Line (File, "         end Process;");
                        Put_Line (File, "      begin");
                        Put_Line (File, "         V_Y4." & Ada_Name (Field_Name) &
                           ".Query_Element (Process'Access);");
                        Put_Line (File, "      end;");
                     elsif Type_Name = K_Boolean_Array then
                        Put_Line (File, "      declare");
                        Put_Line (File, "         procedure Process " &
                           "(A : in YAMI.Parameters.Boolean_Array) is");
                        Put_Line (File, "         begin");
                        Put_Line (File, "            P_Y4.Set_Booleans (""" &
                           YAMI4_Field_Name (Field_Name) & """, A);");
                        Put_Line (File, "         end Process;");
                        Put_Line (File, "      begin");
                        Put_Line (File, "         V_Y4." & Ada_Name (Field_Name) &
                           ".Query_Element (Process'Access);");
                        Put_Line (File, "      end;");
                     elsif Type_Name = K_Integer_Array then
                        Put_Line (File, "      declare");
                        Put_Line (File, "         procedure Process " &
                           "(A : in YAMI.Parameters.YAMI_Integer_Array) is");
                        Put_Line (File, "         begin");
                        Put_Line (File, "            P_Y4.Set_Integers (""" &
                           YAMI4_Field_Name (Field_Name) & """, A);");
                        Put_Line (File, "         end Process;");
                        Put_Line (File, "      begin");
                        Put_Line (File, "         V_Y4." & Ada_Name (Field_Name) &
                           ".Query_Element (Process'Access);");
                        Put_Line (File, "      end;");
                     elsif Type_Name = K_Long_Long_Array then
                        Put_Line (File, "      declare");
                        Put_Line (File, "         procedure Process " &
                           "(A : in YAMI.Parameters.YAMI_Long_Long_Integer_Array) is");
                        Put_Line (File, "         begin");
                        Put_Line (File, "            P_Y4.Set_Long_Long_Integers (""" &
                           YAMI4_Field_Name (Field_Name) & """, A);");
                        Put_Line (File, "         end Process;");
                        Put_Line (File, "      begin");
                        Put_Line (File, "         V_Y4." & Ada_Name (Field_Name) &
                           ".Query_Element (Process'Access);");
                        Put_Line (File, "      end;");
                     elsif Type_Name = K_Float_Array then
                        Put_Line (File, "      declare");
                        Put_Line (File, "         procedure Process " &
                           "(A : in YAMI.Parameters.YAMI_Long_Float_Array) is");
                        Put_Line (File, "         begin");
                        Put_Line (File, "            P_Y4.Set_Long_Floats (""" &
                           YAMI4_Field_Name (Field_Name) & """, A);");
                        Put_Line (File, "         end Process;");
                        Put_Line (File, "      begin");
                        Put_Line (File, "         V_Y4." & Ada_Name (Field_Name) &
                           ".Query_Element (Process'Access);");
                        Put_Line (File, "      end;");
                     elsif Type_Name = K_String_Array then
                        Put_Line (File, "      P_Y4.Set_Strings (""" &
                           YAMI4_Field_Name (Field_Name) & """, V_Y4." & Ada_Name (Field_Name) & "));");
                     elsif Type_Name = K_Binary_Array then
                        Put_Line (File, "      P_Y4.Set_Binaries (""" &
                           YAMI4_Field_Name (Field_Name) & """, V_Y4." & Ada_Name (Field_Name) & "));");
                     else
                        --  user-defined type
                        
                        declare
                           Nested : constant String := Ada_Name (Field_Name) & "_Nested_Y4";
                        begin
                           Put_Line (File, "      declare");
                           Put_Line (File, "         " & Nested & " : YAMI.Parameters.Parameters_Collection :=");
                           Put_Line (File, "            YAMI.Parameters.Parameters_Collection.Create_Nested_Parameters");
                           Put_Line (File, "              (""" & YAMI4_Field_Name (Field_Name) & """));");
                           Put_Line (File, "      begin");
                           Put_Line (File, "         " &
                              Ada_Package_Prefix (Type_Name) & "Write (V_Y4." &
                              Ada_Name (Field_Name) & ", " & Nested & ");");
                           Put_Line (File, "      end;");
                        end;
                     end if;

                     if F.Optional then
                        Put_Line (File, "      end if;");
                     end if;

                  end Process_Field_Def;
                  
                  CF : Field_Maps.Cursor;
               begin
                  CF := Fields.Definitions.Find (Field_Name);
                  Field_Maps.Query_Element (CF, Process_Field_Def'Access);
               end Process_Field;
            
            begin
               Fields.Ordered_Names.Iterate (Process_Field'Access);
            end Process_Fields_Writer;
         
            procedure Process_Fields_Reader (Key : in String; Fields : in Type_Fields) is
            
               procedure Process_Field (C : in Name_Lists.Cursor) is
                  Field_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Field_Def (Key : in String; F : in Field_Definition) is
                     Type_Name : constant String := To_String (F.Type_Name);
                  begin
                     if F.Optional then
                        Put_Line (File, "      declare");
                        Put_Line (File, "         E_Y4 : YAMI.Parameters.Parameter_Entry;");
                        Put_Line (File, "      begin");
                           
                        Put_Line (File, "      P_Y4.Find (""" &
                           YAMI4_Field_Name (Field_Name) & """, E_Y4, V_Y4." &
                           Ada_Field_Valid_Name (Field_Name) & ");");
                        Put_Line (File, "      if V_Y4." & Ada_Field_Valid_Name (Field_Name) & " then");
                     end if;

                     if Type_Name = K_Boolean then
                        Put_Line (File, "      V_Y4." & Ada_Name (Field_Name) &
                           " := P_Y4.Get_Boolean (""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Integer then
                        Put_Line (File, "      V_Y4." & Ada_Name (Field_Name) &
                           " := P_Y4.Get_Integer (""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Long_Long then
                        Put_Line (File, "      V_Y4." & Ada_Name (Field_Name) &
                           " := P_Y4.Get_Long_Long (""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Float then
                        Put_Line (File, "      V_Y4." & Ada_Name (Field_Name) &
                           " := P_Y4.Get_Double_Float (""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_String then
                        Put_Line (File, "      V_Y4." & Ada_Name (Field_Name) &
                           " := To_Unbounded_String (P_Y4.Get_String (""" &
                           YAMI4_Field_Name (Field_Name) & """));");
                     elsif Type_Name = K_Binary then
                        Put_Line (File, "      V_Y4." & Ada_Name (Field_Name) &
                           ".Replace_Element (P_Y4.Get_Binary (""" &
                           YAMI4_Field_Name (Field_Name) & """));");
                     elsif Type_Name = K_Boolean_Array then
                        Put_Line (File, "      P_Y4.Get_Booleans (""" &
                           YAMI4_Field_Name (Field_Name) & """, V_Y4." & Ada_Name (Field_Name) & ");");
                     elsif Type_Name = K_Integer_Array then
                        Put_Line (File, "      P_Y4.Get_Integers (""" &
                           YAMI4_Field_Name (Field_Name) & """, V_Y4." & Ada_Name (Field_Name) & ");");
                     elsif Type_Name = K_Long_Long_Array then
                        Put_Line (File, "      P_Y4.Get_Long_Long_Integers (""" &
                           YAMI4_Field_Name (Field_Name) & """, V_Y4." & Ada_Name (Field_Name) & ");");
                     elsif Type_Name = K_Float_Array then
                        Put_Line (File, "      P_Y4.Get_Long_Floats (""" &
                           YAMI4_Field_Name (Field_Name) & """, V_Y4." & Ada_Name (Field_Name) & ");");
                     elsif Type_Name = K_String_Array then
                        Put_Line (File, "      P_Y4.Get_Strings (""" &
                           YAMI4_Field_Name (Field_Name) & """, V_Y4." & Ada_Name (Field_Name) & ");");
                     elsif Type_Name = K_Binary_Array then
                        Put_Line (File, "      P_Y4.Get_Binaries (""" &
                           YAMI4_Field_Name (Field_Name) & """, V_Y4." & Ada_Name (Field_Name) & ");");
                     else
                        --  user-defined type
                        
                        declare
                           Nested : constant String := Ada_Name (Field_Name) & "_Nested_Y4";
                        begin
                           Put_Line (File, "      declare");
                           Put_Line (File, "         " & Nested & " : YAMI.Parameters.Parameters_Collection :=");
                           Put_Line (File, "            Params.Get_Nested_Parameters (""" &
                              YAMI4_Field_Name (Field_Name) & """);");
                           Put_Line (File, "      begin");
                           Put_Line (File, "         " &
                              Ada_Package_Prefix (Type_Name) & "Read (V_Y4." &
                              Ada_Name (Field_Name) & ", " & Nested & ");");
                           Put_Line (File, "      end;");
                        end;
                     end if;

                     if F.Optional then
                        Put_Line (File, "      end if;");
                        Put_Line (File, "      end;");
                     end if;

                  end Process_Field_Def;
                  
                  CF : Field_Maps.Cursor;
               begin
                  CF := Fields.Definitions.Find (Field_Name);
                  Field_Maps.Query_Element (CF, Process_Field_Def'Access);
               end Process_Field;
            
            begin
               Fields.Ordered_Names.Iterate (Process_Field'Access);
            end Process_Fields_Reader;
            
            Type_Name : constant String := Name_Lists.Element (C);
            
            CT : Type_Maps.Cursor;
            
         begin
            Put_Line (File, "   procedure Write (V_Y4 : in " & Ada_Name (Type_Name) & ";");
            Put_Line (File, "      P_Y4 : in out YAMI.Parameters.Parameters_Collection) is");
            Put_Line (File, "   begin");

            CT := Defs.Type_Definitions.Find (Type_Name);
            Type_Maps.Query_Element (CT, Process_Fields_Writer'Access);

            Put_Line (File, "   end Write;");
            New_Line (File);
            Put_Line (File, "   procedure Read (V_Y4 : out " & Ada_Name (Type_Name) & ";");
            Put_Line (File, "      P_Y4 : in YAMI.Parameters.Parameters_Collection) is");
            Put_Line (File, "   begin");

            Type_Maps.Query_Element (CT, Process_Fields_Reader'Access);

            Put_Line (File, "   end Read;");
            New_Line (File);
         end Process_Type;
      
         procedure Process_Interface (C : in Name_Lists.Cursor) is
         
            Interface_Name : constant String := Name_Lists.Element (C);
            
            procedure Process_Messages (Interface_Name : in String; Messages : in Interface_Messages) is
            
               procedure Process_Message (C : in Name_Lists.Cursor) is
                  Message_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Message_Def (Key : in String; M : in Message_Definition) is
                  begin
                     if Mode = Client then
                        Put (File, "   procedure " & Ada_Name (Message_Name) &
                           " (Client_Y4 : in out " & Ada_Name (Interface_Name));

                        Finish_Operation_Signature (File, M);
                        
                        Put_Line (File, " is");

                        if M.In_Param_Name /= Null_Unbounded_String then
                           Put_Line (File, "      " & Ada_Name (To_String (M.In_Param_Name)) &
                              "_Y4 : YAMI.Parameters.Parameters_Collection :=");
                           Put_Line (File, "         YAMI.Parameters.Make_Parameters;");
                        end if;
                        
                        if M.Oneway then
                           Put_Line (File, "   begin");

                           if M.In_Param_Name /= Null_Unbounded_String then
                              Put_Line (File, "      " &
                                 Ada_Package_Prefix (To_String (M.In_Param_Type)) & "Write (" &
                                 Ada_Name (To_String (M.In_Param_Name)) & ", " &
                                 Ada_Name (To_String (M.In_Param_Name)) & "_Y4);");
                              New_Line (File);
                              Put_Line (File, "      Client_Y4.Agent.All.Send_One_Way " &
                                 "(To_String (Client_Y4.Server_Location),");
                              Put_Line (File, "         To_String (Client_Y4.Object_Name), """ &
                                 YAMI4_Operation_Name (Message_Name) & """,");
                              Put_Line (File, "         " & To_String (M.In_Param_Name) & "_Y4);");
                           else
                              Put_Line (File, "      Client_Y4.Agent.All.Send_One_Way " &
                                 "(To_String (Client_Y4.Server_Location),");
                              Put_Line (File, "         To_String (Client_Y4.Object_Name), """ &
                                 YAMI4_Operation_Name (Message_Name) & """);");
                           end if;

                        else -- two-way message

                           Put_Line (File, "      Msg_Y4 : aliased YAMI.Outgoing_Messages.Outgoing_Message;");
                           Put_Line (File, "   begin");
                        
                           if M.In_Param_Name /= Null_Unbounded_String then
                              Put_Line (File, "      " &
                                 Ada_Package_Prefix (To_String (M.In_Param_Type)) & "Write (" &
                                 Ada_Name (To_String (M.In_Param_Name)) & ", " &
                                 Ada_Name (To_String (M.In_Param_Name)) & "_Y4);");
                              New_Line (File);
                              Put_Line (File, "      Client_Y4.Agent.All.Send (To_String " &
                                 "(Client_Y4.Server_Location),");
                              Put_Line (File, "         To_String (Client_Y4.Object_Name), """ &
                                 YAMI4_Operation_Name (Message_Name) & """,");
                              Put_Line (File, "         " &
                                 To_String (M.In_Param_Name) & "_Y4, Msg_Y4'Unchecked_Access);");
                           else
                              Put_Line (File, "      Client_Y4.Agent.All.Send (To_String " &
                                 "(Client_Y4.Server_Location),");
                              Put_Line (File, "         To_String (Client_Y4.Object_Name), """ &
                                 YAMI4_Operation_Name (Message_Name) & """, Msg_Y4'Unchecked_Access);");
                           end if;
                        
                           New_Line (File);
                           Put_Line (File, "      if Client_Y4.Timeout /= 0.0 then");
                           Put_Line (File, "         select");
                           Put_Line (File, "            Msg_Y4.Wait_For_Completion;");
                           Put_Line (File, "         or");
                           Put_Line (File, "            delay Client_Y4.Timeout;");
                           Put_Line (File, "            raise YAMI.Runtime_Error with ""Operation timed out."";");
                           Put_Line (File, "         end select;");
                           Put_Line (File, "      else");
                           Put_Line (File, "         Msg_Y4.Wait_For_Completion;");
                           Put_Line (File, "      end if;");
                           New_Line (File);
                           Put_Line (File, "      case Msg_Y4.State is");
                           Put_Line (File, "         when YAMI.Outgoing_Messages.Replied =>");
                           
                           if M.Out_Param_Name /= Null_Unbounded_String then
                              Put_Line (File, "            declare");
                              Put_Line (File, "               procedure Process_Y4");
                              Put_Line (File, "                 (Content_Y4 : " &
                                 "in out YAMI.Parameters.Parameters_Collection) is");
                              Put_Line (File, "               begin");
                              Put_Line (File, "                  " &
                                 Ada_Package_Prefix (To_String (M.Out_Param_Type)) & "Read (" &
                                 To_String (M.Out_Param_Name) & ", Content_Y4);");
                              Put_Line (File, "               end Process_Y4;");
                              Put_Line (File, "            begin");
                              Put_Line (File, "               Msg_Y4.Process_Reply_Content (Process_Y4'Access);");
                              Put_Line (File, "            end;");
                           else
                              Put_Line (File, "            null;");
                           end if;
                        
                           Put_Line (File, "         when YAMI.Outgoing_Messages.Abandoned =>");
                           Put_Line (File, "            raise YAMI.Runtime_Error with");
                           Put_Line (File, "               ""Operation was abandoned due to communication errors."";");
                           Put_Line (File, "         when YAMI.Outgoing_Messages.Rejected =>");
                           Put_Line (File, "            raise YAMI.Runtime_Error with");
                           Put_Line (File, "               ""Operation was rejected: "" & Msg_Y4.Exception_Message;");
                           Put_Line (File, "         when others =>");
                           Put_Line (File, "            null;");
                           Put_Line (File, "      end case;");
                        end if;

                        Put_Line (File, "   end " & Ada_Name (Message_Name) & ";");
                        New_Line (File);
                     else --  Mode = Server
                        Put_Line (File, "if Message_Name_Y4 = """ & YAMI4_Operation_Name (Message_Name) & """ then");
                        Put_Line (File, "         declare");
                        
                        if M.In_Param_Name /= Null_Unbounded_String then
                           Put_Line (File, "            " & Ada_Name (To_String (M.In_Param_Name)) & " : " &
                              Ada_Name (To_String (M.In_Param_Type)) & ";");
                           Put_Line (File, "            procedure Process_Y4");
                           Put_Line (File, "              (Content_Y4 : in out YAMI.Parameters.Parameters_Collection) is");
                           Put_Line (File, "            begin");
                           Put_Line (File, "               " &
                              Ada_Package_Prefix (To_String (M.In_Param_Type)) & "Read (" &
                              Ada_Name (To_String (M.In_Param_Name)) & ", Content_Y4);");
                           Put_Line (File, "            end Process_Y4;");
                        end if;

                        if M.Out_Param_Name /= Null_Unbounded_String then
                           Put_Line (File, "            " & Ada_Name (To_String (M.Out_Param_Name)) & " : " &
                              Ada_Name (To_String (M.Out_Param_Type)) & ";");
                           Put_Line (File, "            " & Ada_Name (To_String (M.Out_Param_Name)) & "_Y4 : " &
                              "YAMI.Parameters.Parameters_Collection :=");
                           Put_Line (File, "               YAMI.Parameters.Make_Parameters;");
                        end if;

                        Put_Line (File, "         begin");
                        
                        if M.In_Param_Name /= Null_Unbounded_String then
                           Put_Line (File, "            Message_Y4.Process_Content (Process_Y4'Access);");
                        end if;

                        Put (File, "            " &
                           Ada_Name (Interface_Name & "_Server") & "'Class (Server_Y4)." & Ada_Name (Message_Name));

                        if M.In_Param_Name /= Null_Unbounded_String or
                           M.Out_Param_Name /= Null_Unbounded_String then
                           Put (File, " (");
                        end if;
                        
                        if M.In_Param_Name /= Null_Unbounded_String then
                           Put (File, Ada_Name (To_String (M.In_Param_Name)));
                           if M.Out_Param_Name /= Null_Unbounded_String then
                              Put (File, ", ");
                           end if;
                        end if;
                        
                        if M.Out_Param_Name /= Null_Unbounded_String then
                           Put (File, Ada_Name (To_String (M.Out_Param_Name)));
                        end if;
                        
                        if M.In_Param_Name /= Null_Unbounded_String or
                           M.Out_Param_Name /= Null_Unbounded_String then
                           Put (File, ")");
                        end if;
                        
                        Put_Line (File, ";");
    
                        if not M.Oneway then
                           if M.Out_Param_Name /= Null_Unbounded_String then
                              Put_Line (File, "            " &
                                 Ada_Package_Prefix (To_String (M.Out_Param_Type)) & "Write (" &
                                 Ada_Name (To_String (M.Out_Param_Name)) & ", " &
                                 Ada_Name (To_String (M.Out_Param_Name)) & "_Y4);");
                              Put_Line (File, "            Message_Y4.Reply (" &
                                 Ada_Name (To_String (M.Out_Param_Name)) & "_Y4);");
                           else
                              Put_Line (File, "            Message_Y4.Reply;");
                           end if;
                        end if;
                        
                        Put_Line (File, "         end;");
                        Put (File, "      els");
                     end if;
                  end Process_Message_Def;
                  
                  CM : Message_Maps.Cursor;
               begin
                  CM := Messages.Definitions.Find (Message_Name);
                  Message_Maps.Query_Element (CM, Process_Message_Def'Access);
               end Process_Message;
            
            begin
               Messages.Ordered_Names.Iterate (Process_Message'Access);
            end Process_Messages;
         
            CI : Interface_Maps.Cursor;
            
         begin
            Put_Line (File, "   procedure Initialize_" & Ada_Name (Interface_Name));
            Put_Line (File, "     (Client_Y4 : out " & Ada_Name (Interface_Name) & ";");
            Put_Line (File, "      Agent : in out YAMI.Agents.Agent;");
            Put_Line (File, "      Server_Location : in String;");
            Put_Line (File, "      Object_Name : in String;");
            Put_Line (File, "      Timeout : in Duration := 0.0) is");
            Put_Line (File, "   begin");
            Put_Line (File, "      Client_Y4.Agent := Agent'Unchecked_Access;");
            Put_Line (File, "      Client_Y4.Server_Location := To_Unbounded_String (Server_Location);");
            Put_Line (File, "      Client_Y4.Object_Name := To_Unbounded_String (Object_Name);");
            Put_Line (File, "      Client_Y4.Timeout := Timeout;");
            Put_Line (File, "   end Initialize_" & Ada_Name (Interface_Name) & ";");
            New_Line (File);

            CI := Defs.Interface_Definitions.Find (Interface_Name);
            Mode := Client;
            Interface_Maps.Query_Element (CI, Process_Messages'Access);
            
            Put_Line (File, "   procedure Call (Server_Y4 : in out " &
               Ada_Name (Interface_Name & "_Server;"));
            Put_Line (File, "      Message_Y4 : in out YAMI.Incoming_Messages.Incoming_Message'Class) is");
            Put_Line (File, "      Message_Name_Y4 : constant String := Message_Y4.Message_Name;");
            Put_Line (File, "   begin");
            Put (File, "      ");
            
            CI := Defs.Interface_Definitions.Find (Interface_Name);
            Mode := Server;
            Interface_Maps.Query_Element (CI, Process_Messages'Access);
            
            Put_Line (File, "e");
            Put_Line (File, "         raise YAMI.Runtime_Error with ""Unknown operation name."";");
            Put_Line (File, "      end if;");
            Put_Line (File, "   end Call;");
            New_Line (File);
         end Process_Interface;

      begin
         --Defs.Explicit_Imports.Iterate (Process_Import'Access);
         --if not Defs.Explicit_Imports.Is_Empty then
         --   New_Line (File);
         --end if;

         Put_Line (File, "with Ada.Strings.Unbounded;");
         Put_Line (File, "use Ada.Strings.Unbounded;");
         New_Line (File);
         
         Put_Line (File, "package body " & Ada_Name (Package_Name) & " is");
         New_Line (File);
         
         Defs.Ordered_Type_Names.Iterate (Process_Type'Access);
         
         Defs.Ordered_Interface_Names.Iterate (Process_Interface'Access);

         Put_Line (File, "end " & Ada_Name (Package_Name) & ";");
         
      end Process_Package_Definitions;

      C : Package_Maps.Cursor;
      
   begin
      Put_Line (File, "--");
      Put_Line (File, "--  Ada implementations for package " & Package_Name & '.');
      Put_Line (File, "--  This file was generated automatically by yami4idl.");
      Put_Line (File, "--");
      New_Line (File);

      C := All_Packages.Find (Package_Name);
      Package_Maps.Query_Element (C, Process_Package_Definitions'Access);
   end Write_Body;

   procedure Generate (Output_Dir : in String) is

      procedure Process_Fully_Defined_Package (C : in Name_Lists.Cursor) is
         Package_Name : constant String := Name_Lists.Element (C);
      
         File : File_Type;
      begin
         Create (File, Out_File, Specs_File_Name (Output_Dir, Package_Name));
         Write_Specs (File, Package_Name);
         Close (File);

         Create (File, Out_File, Body_File_Name (Output_Dir, Package_Name));
         Write_Body (File, Package_Name);
         Close (File);
      exception
         when E : Unsupported =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E) &
               "Package name: " & Package_Name);
      end Process_Fully_Defined_Package;

   begin
      Fully_Defined_Packages.Iterate (Process_Fully_Defined_Package'Access);
   end Generate;

end IDL.Structures.Ada_Generator;

