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

package body IDL.Structures.CPP_Generator is

   Casing : Casing_Mode;
   
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
      
   function Header_File_Name (Output_Dir : in String; Package_Name : in String) return String is
   begin
      return Ada.Directories.Compose (Output_Dir, Base_File_Name (Package_Name), "h");
   end Header_File_Name;
   
   function Impl_File_Name (Output_Dir : in String; Package_Name : in String) return String is
   begin
      return Ada.Directories.Compose (Output_Dir, Base_File_Name (Package_Name), "cpp");
   end Impl_File_Name;
   
   function CPP_Name (IDL_Name : in String) return String is

      CPP : Unbounded_String;
      
      New_Word : Boolean := True;
      
      procedure Process_Next_Char (C : in Character) is
      begin
         case Casing is
            when Ident =>
               Append (CPP, C);
            when Lower_Case | Default =>
               Append (CPP, Ada.Characters.Handling.To_Lower (C));
            when Camel_Case =>
               if C = Ada.Characters.Latin_1.Low_Line then
                  New_Word := True;
               else
                  if New_Word then
                     Append (CPP, Ada.Characters.Handling.To_Upper (C));
                  else
                     Append (CPP, Ada.Characters.Handling.To_Lower (C));
                  end if;
                  
                  New_Word := False;
               end if;
         end case;
      end Process_Next_Char;
      
   begin
      for I in IDL_Name'Range loop
         if IDL_Name (I) /= Ada.Characters.Latin_1.Full_Stop then
            Process_Next_Char (IDL_Name (I));
         else
            Append (CPP, "::");
            New_Word := True;
         end if;
      end loop;
            
      return To_String (CPP);
   end CPP_Name;
   
   function CPP_Field_Valid_Name (IDL_Name : in String) return String is
   begin
      return CPP_Name (IDL_Name & "_Valid");
   end CPP_Field_Valid_Name;

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

   function CPP_Type (IDL_Type : in String) return String is
   begin
      if IDL_Type = K_Boolean then
         return "bool";
      elsif IDL_Type = K_Integer then
         return "int";
      elsif IDL_Type = K_Long_Long then
         return "long long";
      elsif IDL_Type = K_Float then
         return "double";
      elsif IDL_Type = K_String then
         return "std::string";
      elsif IDL_Type = K_Binary then
         return "std::vector<char>";
      elsif IDL_Type = K_Boolean_Array then
         return "std::vector<bool>";
      elsif IDL_Type = K_Integer_Array then
         return "std::vector<int>";
      elsif IDL_Type = K_Long_Long_Array then
         return "std::vector<long long>";
      elsif IDL_Type = K_Float_Array then
         return "std::vector<double>";
      elsif IDL_Type = K_String_Array then
         return "std::vector<std::string>";
      elsif IDL_Type = K_Binary_Array then
         return "std::vector<std::vector<char> >";
      else
         --  user-defined type
         
         return CPP_Name (IDL_Type);
      end if;
   end CPP_Type;

   procedure Put_Operation_Signature (File : in File_Type; M : in Message_Definition) is
   begin
      Put (File, "(");
      if M.In_Param_Name /= Null_Unbounded_String then
         Put (File, "const " & CPP_Name (To_String (M.In_Param_Type)) &
            " & " & CPP_Name (To_String (M.In_Param_Name)));
                        
         if M.Out_Param_Name /= Null_Unbounded_String then
            Put (File, ", ");
         end if;
      end if;
                     
      if M.Out_Param_Name /= Null_Unbounded_String then
         Put (File, CPP_Name (To_String (M.Out_Param_Type)) &
            " & " & CPP_Name (To_String (M.Out_Param_Name)));
      end if;
      
      Put (File, ")");
   end Put_Operation_Signature;

   type Mode_Type is (Client, Server);
   Mode : Mode_Type;
            
   procedure Write_Header (File : in File_Type; Package_Name : in String) is
   
      function Header_Guard return String is
         Guard : String := "YAMI4_IDL_" &
            Ada.Characters.Handling.To_Upper (Package_Name) & "_H_INCLUDED";
      begin
         for I in Guard'Range loop
            if Guard (I) = Ada.Characters.Latin_1.Full_Stop then
               Guard (I) := Ada.Characters.Latin_1.Low_Line;
            end if;
         end loop;
         
         return Guard;
      end Header_Guard;
   
      procedure Process_Package_Definitions (Key : in String; Defs : in Package_Definitions) is
      
         procedure Process_Import (C : in Name_Lists.Cursor) is
         begin
            Put_Line (File, "#include """ & Header_File_Name ("", Name_Lists.Element (C)) & """");
         end Process_Import;
               
         procedure Process_Type (C : in Name_Lists.Cursor) is
         
            procedure Process_Fields (Key : in String; Fields : in Type_Fields) is
            
               procedure Process_Field (C : in Name_Lists.Cursor) is
                  Field_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Field_Def (Key : in String; F : in Field_Definition) is
                  begin
                     if F.Optional then
                        Put_Line (File, "    bool " & CPP_Field_Valid_Name (Field_Name) & ";");
                     end if;
                     
                     Put_Line (File, "    " & CPP_Type (To_String (F.Type_Name)) &
                        " " & CPP_Name (Field_Name) & ";");
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
            Put_Line (File, "struct " & CPP_Name (Type_Name));
            Put_Line (File, "{");
            Put_Line (File, "    " & CPP_Name (Type_Name) & "();");
            New_Line (File);
            Put_Line (File, "    void write(yami::parameters & params) const;");
            Put_Line (File, "    void read(const yami::parameters & params);");
            New_Line (File);
            
            CT := Defs.Type_Definitions.Find (Type_Name);
            Type_Maps.Query_Element (CT, Process_Fields'Access);
            
            Put_Line (File, "};");
            New_Line (File);
         end Process_Type;
      
         procedure Process_Interface (C : in Name_Lists.Cursor) is
         
            procedure Process_Messages (Key : in String; Messages : in Interface_Messages) is
            
               procedure Process_Message (C : in Name_Lists.Cursor) is
                  Message_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Message_Def (Key : in String; M : in Message_Definition) is
                  begin
                     if Mode = Server then
                        Put (File, "    virtual ");
                     else
                        Put (File, "    ");
                     end if;
                     
                     Put (File, "void " & CPP_Name (Message_Name));

                     Put_Operation_Signature (File, M);

                     if Mode = Server then
                        Put_Line (File, " = 0;");
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
         
            Interface_Name : constant String := Name_Lists.Element (C);
            
            CI : Interface_Maps.Cursor;
            
         begin
            Put_Line (File, "class " & CPP_Name (Interface_Name));
            Put_Line (File, "{");
            Put_Line (File, "public:");
            New_Line (File);
            Put_Line (File, "    " & CPP_Name (Interface_Name) & "(yami::agent & client_agent,");
            Put_Line (File, "        const std::string & server_location, const std::string & object_name,");
            Put_Line (File, "        int timeout = 0);");
            New_Line (File);
            
            CI := Defs.Interface_Definitions.Find (Interface_Name);
            Mode := Client;
            Interface_Maps.Query_Element (CI, Process_Messages'Access);
            
            New_Line (File);
            Put_Line (File, "private:");
            New_Line (File);
            Put_Line (File, "    yami::agent & agent_;");
            Put_Line (File, "    const std::string server_location_;");
            Put_Line (File, "    const std::string object_name_;");
            Put_Line (File, "    const std::size_t timeout_;");
            Put_Line (File, "};");
            New_Line (File);
            Put_Line (File, "class " & CPP_Name (Interface_Name & "_Server"));
            Put_Line (File, "{");
            Put_Line (File, "public:");
            New_Line (File);
            Put_Line (File, "    virtual ~" & CPP_Name (Interface_Name & "_Server") & "() {}");
            New_Line (File);
            
            CI := Defs.Interface_Definitions.Find (Interface_Name);
            Mode := Server;
            Interface_Maps.Query_Element (CI, Process_Messages'Access);
            
            New_Line (File);
            Put_Line (File, "    void operator()(yami::incoming_message & im_);");
            Put_Line (File, "};");
            New_Line (File);
         end Process_Interface;
      
         procedure Wrap_In_Namespaces (Package_Name : in String) is
         begin
            if Package_Name = "" then
               --  all namespaces have been defined
            
               Defs.Ordered_Type_Names.Iterate (Process_Type'Access);
         
               Defs.Ordered_Interface_Names.Iterate (Process_Interface'Access);
            else
               --  strip external package name and turn it into namespace
            
               declare
                  Last_Of_Outer_Package : Positive := Package_Name'Last;
               begin
                  for I in Package_Name'Range loop
                     if Package_Name (I) = Ada.Characters.Latin_1.Full_Stop then
                        Last_Of_Outer_Package := I - 1;
                        exit;
                     end if;
                  end loop;
               
                  declare
                     Namespace : constant String :=
                        CPP_Name
                          (Package_Name (Package_Name'First .. Last_Of_Outer_Package));
                  begin
                     Put_Line (File, "namespace " & Namespace);
                     Put_Line (File, "{");
                     New_Line (File);
               
                     Wrap_In_Namespaces
                       (Package_Name (Last_Of_Outer_Package + 2 .. Package_Name'Last));
               
                     Put_Line (File, "} // namespace " & Namespace);
                     New_Line (File);
                  end;
               end;
            end if;
         end Wrap_In_Namespaces;
         
         Some_Imports_Added : Boolean := False;
      
      begin
         if not Defs.Explicit_Imports.Is_Empty then
            Defs.Explicit_Imports.Iterate (Process_Import'Access);
            Some_Imports_Added := True;
         end if;

         if not Defs.Implicit_Imports.Is_Empty then
            Defs.Implicit_Imports.Iterate (Process_Import'Access);
            Some_Imports_Added := True;
         end if;
         
         if Some_Imports_Added then
            New_Line (File);
         end if;

         Wrap_In_Namespaces (Package_Name);
         
      end Process_Package_Definitions;

      Guard : constant String := Header_Guard;

      C : Package_Maps.Cursor;
      
   begin
      Put_Line (File, "//");
      Put_Line (File, "// C++ type definitions for package " & Package_Name & '.');
      Put_Line (File, "// This file was generated automatically by yami4idl.");
      Put_Line (File, "//");
      New_Line (File);
      Put_Line (File, "#ifndef " & Guard);
      Put_Line (File, "#define " & Guard);
      New_Line (File);
      Put_Line (File, "#include <yami4-cpp/parameters.h>");
      Put_Line (File, "#include <string>");
      Put_Line (File, "#include <vector>");
      New_Line (File);
      Put_Line (File, "namespace yami");
      Put_Line (File, "{");
      Put_Line (File, "    class agent;");
      Put_Line (File, "    class incoming_message;");
      Put_Line (File, "}");
      New_Line (File);

      C := All_Packages.Find (Package_Name);
      Package_Maps.Query_Element (C, Process_Package_Definitions'Access);

      Put_Line (File, "#endif // " & Guard);
   end Write_Header;

   procedure Write_Implementation (File : in File_Type; Package_Name : in String) is
   
      procedure Process_Package_Definitions (Key : in String; Defs : in Package_Definitions) is
      
--         procedure Process_Import (C : in Name_Lists.Cursor) is
--         begin
--            Put_Line (File, "import " & Name_Lists.Element (C) & ';');
--         end Process_Import;
               
         procedure Process_Type (C : in Name_Lists.Cursor) is
         
            procedure Process_Fields_Cleaner (Key : in String; Fields : in Type_Fields) is
            
               procedure Process_Field (C : in Name_Lists.Cursor) is
                  Field_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Field_Def (Key : in String; F : in Field_Definition) is
                     Type_Name : constant String := To_String (F.Type_Name);
                  begin
                     if F.Optional then
                        Put_Line (File, "    " & CPP_Field_Valid_Name (Field_Name) & " = false;");
                     end if;
                  end Process_Field_Def;
                  
                  CF : Field_Maps.Cursor;
               begin
                  CF := Fields.Definitions.Find (Field_Name);
                  Field_Maps.Query_Element (CF, Process_Field_Def'Access);
               end Process_Field;
            
            begin
               Fields.Ordered_Names.Iterate (Process_Field'Access);
            end Process_Fields_Cleaner;

            procedure Process_Fields_Writer (Key : in String; Fields : in Type_Fields) is
            
               procedure Process_Field (C : in Name_Lists.Cursor) is
                  Field_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Field_Def (Key : in String; F : in Field_Definition) is
                     Type_Name : constant String := To_String (F.Type_Name);
                  begin
                     if F.Optional then
                        Put_Line (File, "    if (" & CPP_Field_Valid_Name (Field_Name) & ")");
                        Put_Line (File, "    {");
                        --Put (File, "    ");
                     end if;

                     if Type_Name = K_Boolean then
                        Put_Line (File, "    params.set_boolean(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           CPP_Name (Field_Name) & ");");
                     elsif Type_Name = K_Integer then
                        Put_Line (File, "    params.set_integer(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           CPP_Name (Field_Name) & ");");
                     elsif Type_Name = K_Long_Long then
                        Put_Line (File, "    params.set_long_long(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           CPP_Name (Field_Name) & ");");
                     elsif Type_Name = K_Float then
                        Put_Line (File, "    params.set_double_float(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           CPP_Name (Field_Name) & ");");
                     elsif Type_Name = K_String then
                        Put_Line (File, "    params.set_string_shallow(""" &
                           YAMI4_Field_Name (Field_Name) & """,");
                        Put_Line (File, "        " & CPP_Name (Field_Name) & ".c_str(), " &
                           CPP_Name (Field_Name) & ".size());");
                     elsif Type_Name = K_Binary then
                        Put_Line (File, "    params.set_binary_shallow(""" &
                           YAMI4_Field_Name (Field_Name) & """,");
                        Put_Line (File, "        &" & CPP_Name (Field_Name) & "[0], " &
                           CPP_Name (Field_Name) & ".size());");
                     elsif Type_Name = K_Boolean_Array then
                        Put_Line (File, "    {");
                        Put_Line (File, "        std::size_t size_ = " & CPP_Name (Field_Name) & ".size();");
                        Put_Line (File, "        bool * tmp_ = new bool[size_];");
                        Put_Line (File, "        for (std::size_t i_ = 0; i_ != size_; ++i_)");
                        Put_Line (File, "        {");
                        Put_Line (File, "            tmp_[i_] = " & CPP_Name (Field_Name) & "[i_];");
                        Put_Line (File, "        }");
                        Put_Line (File, "        params.set_boolean_array(""" &
                           YAMI4_Field_Name (Field_Name) & """, tmp_, size_);");
                        Put_Line (File, "        delete [] tmp_;");
                        Put_Line (File, "    }");
                     elsif Type_Name = K_Integer_Array then
                        Put_Line (File, "    params.set_integer_array_shallow(""" &
                           YAMI4_Field_Name (Field_Name) & """,");
                        Put_Line (File, "        &" & CPP_Name (Field_Name) & "[0], " &
                           CPP_Name (Field_Name) & ".size());");
                     elsif Type_Name = K_Long_Long_Array then
                        Put_Line (File, "    params.set_long_long_array_shallow(""" &
                           YAMI4_Field_Name (Field_Name) & """,");
                        Put_Line (File, "        &" & CPP_Name (Field_Name) & "[0], " &
                           CPP_Name (Field_Name) & ".size());");
                     elsif Type_Name = K_Float_Array then
                        Put_Line (File, "    params.set_double_float_array_shallow(""" &
                           YAMI4_Field_Name (Field_Name) & """,");
                        Put_Line (File, "        &" & CPP_Name (Field_Name) & "[0], " &
                           CPP_Name (Field_Name) & ".size());");
                     elsif Type_Name = K_String_Array then
                        Put_Line (File, "    {");
                        Put_Line (File, "        std::size_t size_ = " & CPP_Name (Field_Name) & ".size();");
                        Put_Line (File, "        params.create_string_array(""" &
                           YAMI4_Field_Name (Field_Name) & """, size_);");
                        Put_Line (File, "        for (std::size_t i_ = 0; i_ != size_; ++i_)");
                        Put_Line (File, "        {");
                        Put_Line (File, "            params.set_string_in_array(""" &
                           YAMI4_Field_Name (Field_Name) & """, i_, " &  CPP_Name (Field_Name) & "[i_]);");
                        Put_Line (File, "        }");
                        Put_Line (File, "    }");
                     elsif Type_Name = K_Binary_Array then
                        Put_Line (File, "    {");
                        Put_Line (File, "        std::size_t size_ = " & CPP_Name (Field_Name) & ".size();");
                        Put_Line (File, "        params.create_binary_array(""" &
                           YAMI4_Field_Name (Field_Name) & """, size_);");
                        Put_Line (File, "        for (std::size_t i_ = 0; i_ != size_; ++i_)");
                        Put_Line (File, "        {");
                        Put_Line (File, "            params.set_binary_in_array(");
                        Put_Line (File, "                """ &
                           YAMI4_Field_Name (Field_Name) & """, i_, &" &
                           CPP_Name (Field_Name) & "[i_][0], " & CPP_Name (Field_Name) & "[i_].size());");
                        Put_Line (File, "        }");
                        Put_Line (File, "    }");
                     else
                        --  user-defined type
                        
                        declare
                           Nested : constant String := CPP_Name (Field_Name) & "_nested";
                        begin
                           Put_Line (File, "    yami::parameters " & Nested &
                              "(params.create_nested_parameters(""" &
                              YAMI4_Field_Name (Field_Name) & """));");
                           
                           Put_Line (File, "    " &
                              CPP_Name (Field_Name) & ".write(" & Nested & ");");
                        end;
                     end if;

                     if F.Optional then
                        Put_Line (File, "    }");
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
            
               Param_Entry_Already_Defined : Boolean := False;
            
               procedure Process_Field (C : in Name_Lists.Cursor) is
                  Field_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Field_Def (Key : in String; F : in Field_Definition) is
                     Type_Name : constant String := To_String (F.Type_Name);
                  begin
                     if F.Optional then
                        if not Param_Entry_Already_Defined then
                           Put_Line (File, "    yami::parameter_entry e_;");
                           
                           Param_Entry_Already_Defined := True;
                        end if;
                        
                        Put_Line (File, "    " & CPP_Field_Valid_Name (Field_Name) & " = params.find(""" &
                           YAMI4_Field_Name (Field_Name) & """, e_);");
                        Put_Line (File, "    if (" & CPP_Field_Valid_Name (Field_Name) & ")");
                        Put_Line (File, "    {");
                     end if;

                     if Type_Name = K_Boolean then
                        Put_Line (File, "    " & CPP_Name (Field_Name) &
                           " = params.get_boolean(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Integer then
                        Put_Line (File, "    " & CPP_Name (Field_Name) &
                           " = params.get_integer(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Long_Long then
                        Put_Line (File, "    " & CPP_Name (Field_Name) &
                           " = params.get_long_long(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Float then
                        Put_Line (File, "    " & CPP_Name (Field_Name) &
                           " = params.get_double_float(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_String then
                        Put_Line (File, "    " & CPP_Name (Field_Name) &
                           " = params.get_string(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Binary then
                        Put_Line (File, "    {");
                        Put_Line (File, "        std::size_t size_;");
                        Put_Line (File, "        const char * buf_ = reinterpret_cast<const char *>(");
                        Put_Line (File, "            params.get_binary(""" &
                           YAMI4_Field_Name (Field_Name) & """, size_));");
                        Put_Line (File, "        " & CPP_Name (Field_Name) & ".assign(buf_, buf_ + size_);");
                        Put_Line (File, "    }");
                     elsif Type_Name = K_Boolean_Array then
                        Put_Line (File, "    {");
                        Put_Line (File, "        std::size_t size_;");
                        Put_Line (File, "        const bool * buf_ = params.get_boolean_array(");
                        Put_Line (File, "            """ & YAMI4_Field_Name (Field_Name) & """, size_);");
                        Put_Line (File, "        " & CPP_Name (Field_Name) & ".assign(buf_, buf_ + size_);");
                        Put_Line (File, "    }");
                     elsif Type_Name = K_Integer_Array then
                        Put_Line (File, "    {");
                        Put_Line (File, "        std::size_t size_;");
                        Put_Line (File, "        const int * buf_ = params.get_integer_array(");
                        Put_Line (File, "            """ & YAMI4_Field_Name (Field_Name) & """, size_);");
                        Put_Line (File, "        " & CPP_Name (Field_Name) & ".assign(buf_, buf_ + size_);");
                        Put_Line (File, "    }");
                     elsif Type_Name = K_Long_Long_Array then
                        Put_Line (File, "    {");
                        Put_Line (File, "        std::size_t size_;");
                        Put_Line (File, "        const long long * buf_ = params.get_long_long_array(");
                        Put_Line (File, "            """ & YAMI4_Field_Name (Field_Name) & """, size_);");
                        Put_Line (File, "        " & CPP_Name (Field_Name) & ".assign(buf_, buf_ + size_);");
                        Put_Line (File, "    }");
                     elsif Type_Name = K_Float_Array then
                        Put_Line (File, "    {");
                        Put_Line (File, "        std::size_t size_;");
                        Put_Line (File, "        const double * buf_ = params.get_double_float_array(");
                        Put_Line (File, "            """ & YAMI4_Field_Name (Field_Name) & """, size_);");
                        Put_Line (File, "        " & CPP_Name (Field_Name) & ".assign(buf_, buf_ + size_);");
                        Put_Line (File, "    }");
                     elsif Type_Name = K_String_Array then
                        Put_Line (File, "    {");
                        Put_Line (File, "        std::size_t size_ = params.get_string_array_length(""" &
                           YAMI4_Field_Name (Field_Name) & """);");
                        Put_Line (File, "        " & CPP_Name (Field_Name) & ".resize(size_);");
                        Put_Line (File, "        for (std::size_t i_ = 0; i_ != size_; ++i_)");
                        Put_Line (File, "        {");
                        Put_Line (File, "            " & CPP_Name (Field_Name) &
                           "[i_] = params.get_string_in_array(""" & YAMI4_Field_Name (Field_Name) & """, i_);");
                        Put_Line (File, "        }");
                        Put_Line (File, "    }");
                     elsif Type_Name = K_Binary_Array then
                        Put_Line (File, "    {");
                        Put_Line (File, "        std::size_t size_ = params.get_binary_array_length(""" &
                           YAMI4_Field_Name (Field_Name) & """);");
                        Put_Line (File, "        " & CPP_Name (Field_Name) & ".resize(size_);");
                        Put_Line (File, "        for (std::size_t i_ = 0; i_ != size_; ++i_)");
                        Put_Line (File, "        {");
                        Put_Line (File, "            std::size_t bufSize_;");
                        Put_Line (File, "            const char * buf_ = reinterpret_cast<const char *>(");
                        Put_Line (File, "                params.get_binary_in_array(""" &
                           YAMI4_Field_Name (Field_Name) & """, i_, bufSize_));");
                        Put_Line (File, "            " & CPP_Name (Field_Name) &
                           "[i_].assign(buf_, buf_ + bufSize_);");
                        Put_Line (File, "        }");
                        Put_Line (File, "    }");
                     else
                        --  user-defined type
                        
                        declare
                           Nested : constant String := CPP_Name (Field_Name) & "_nested";
                        begin
                           Put_Line (File, "    yami::parameters " & Nested &
                              "(params.get_nested_parameters(""" &
                              YAMI4_Field_Name (Field_Name) & """));");
                           
                           Put_Line (File, "    " &
                              CPP_Name (Field_Name) & ".read(" & Nested & ");");
                        end;
                     end if;

                     if F.Optional then
                        Put_Line (File, "    }");
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
            Put_Line (File, CPP_Name (Type_Name) & "::" & CPP_Name (Type_Name) & "()");
            Put_Line (File, "{");

            CT := Defs.Type_Definitions.Find (Type_Name);
            Type_Maps.Query_Element (CT, Process_Fields_Cleaner'Access);

            Put_Line (File, "}");
            New_Line (File);
            Put_Line (File, "void " & CPP_Name (Type_Name) & "::write(yami::parameters & params) const");
            Put_Line (File, "{");

            Type_Maps.Query_Element (CT, Process_Fields_Writer'Access);

            Put_Line (File, "}");
            New_Line (File);
            Put_Line (File, "void " & CPP_Name (Type_Name) & "::read(const yami::parameters & params)");
            Put_Line (File, "{");

            Type_Maps.Query_Element (CT, Process_Fields_Reader'Access);

            Put_Line (File, "}");
            New_Line (File);
         end Process_Type;
      
         procedure Process_Interface (C : in Name_Lists.Cursor) is
         
            procedure Process_Messages (Interface_Name : in String; Messages : in Interface_Messages) is
            
               procedure Process_Message (C : in Name_Lists.Cursor) is
                  Message_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Message_Def (Key : in String; M : in Message_Definition) is
                  begin
                     if Mode = Client then
                        Put (File, "void " & CPP_Name (Interface_Name) & "::" &
                           CPP_Name (Message_Name));
                        
                        Put_Operation_Signature (File, M);

                        New_Line (File);
                        Put_Line (File, "{");
                        
                        if M.In_Param_Name /= Null_Unbounded_String then
                           Put_Line (File, "    yami::parameters " &
                              CPP_Name (To_String (M.In_Param_Name)) & "_;");
                           Put_Line (File, "    " & CPP_Name (To_String (M.In_Param_Name)) &
                              ".write(" & CPP_Name (To_String (M.In_Param_Name)) & "_);");
                        end if;

                        if M.Oneway then
                           if M.In_Param_Name /= Null_Unbounded_String then
                              Put_Line (File, "    agent_.send_one_way(server_location_, object_name_, """ &
                                 YAMI4_Operation_Name (Message_Name) & """, " &
                                 CPP_Name (To_String (M.In_Param_Name)) & "_);");
                           else
                              Put_Line (File, "    agent_.send_one_way(server_location_, object_name_, """ &
                                 YAMI4_Operation_Name (Message_Name) & """);");
                           end if;
                        else
                           if M.In_Param_Name /= Null_Unbounded_String then
                              Put_Line (File, "    std::auto_ptr<yami::outgoing_message> om_(");
                              Put_Line (File, "        agent_.send(server_location_, object_name_, """ &
                                 YAMI4_Operation_Name (Message_Name) & """, " &
                                 CPP_Name (To_String (M.In_Param_Name)) & "_));");
                           else
                              Put_Line (File, "    std::auto_ptr<yami::outgoing_message> om_(");
                              Put_Line (File, "        agent_.send(server_location_, object_name_, """ &
                                 YAMI4_Operation_Name (Message_Name) & """));");
                           end if;
                           
                           New_Line (File);
                           Put_Line (File, "    if (timeout_ != 0)");
                           Put_Line (File, "    {");
                           Put_Line (File, "        bool on_time_ = om_->wait_for_completion(timeout_);");
                           Put_Line (File, "        if (on_time_ == false)");
                           Put_Line (File, "        {");
                           Put_Line (File, "            throw yami::yami_runtime_error(""Operation timed out."");");
                           Put_Line (File, "        }");
                           Put_Line (File, "    }");
                           Put_Line (File, "    else");
                           Put_Line (File, "    {");
                           Put_Line (File, "        om_->wait_for_completion();");
                           Put_Line (File, "    }");
                           New_Line (File);
                           Put_Line (File, "    const yami::message_state state_ = om_->get_state();");
                           Put_Line (File, "    switch (state_)");
                           Put_Line (File, "    {");
                           Put_Line (File, "    case yami::replied:");

                           if M.Out_Param_Name /= Null_Unbounded_String then
                              Put_Line (File, "        " & CPP_Name (To_String (M.Out_Param_Name)) & ".read(om_->get_reply());");
                           end if;

                           Put_Line (File, "        break;");
                           Put_Line (File, "    case yami::abandoned:");
                           Put_Line (File, "        throw yami::yami_runtime_error(");
                           Put_Line (File, "            ""Operation was abandoned due to communication errors."");");
                           Put_Line (File, "    case yami::rejected:");
                           Put_Line (File, "        throw yami::yami_runtime_error(");
                           Put_Line (File, "            ""Operation was rejected: "" + om_->get_exception_msg());");
                           New_Line (File);
                           Put_Line (File, "    // these are for completeness:");
                           Put_Line (File, "    case yami::posted:");
                           Put_Line (File, "    case yami::transmitted:");
                           Put_Line (File, "        break;");
                           Put_Line (File, "    }");
                        end if;

                        Put_Line (File, "}");
                        New_Line (File);

                     else --  Mode = Server
                        Put_Line (File, "    if (msg_name_ == """ & YAMI4_Operation_Name (Message_Name) & """)");
                        Put_Line (File, "    {");
                        
                        if M.In_Param_Name /= Null_Unbounded_String then
                           Put_Line (File, "        " & CPP_Name (To_String (M.In_Param_Type)) & " " &
                              CPP_Name (To_String (M.In_Param_Name)) & ";");
                           Put_Line (File, "        " &
                              CPP_Name (To_String (M.In_Param_Name)) & ".read(im_.get_parameters());");
                        end if;

                        if M.Out_Param_Name /= Null_Unbounded_String then
                           Put_Line (File, "        " & CPP_Name (To_String (M.Out_Param_Type)) & " " &
                              CPP_Name (To_String (M.Out_Param_Name)) & ";");
                        end if;

                        if M.In_Param_Name /= Null_Unbounded_String or
                           M.Out_Param_Name /= Null_Unbounded_String then
                           New_Line (File);
                        end if;
                        
                        Put (File, "        " & CPP_Name (Message_Name) & "(");

                        if M.In_Param_Name /= Null_Unbounded_String then
                           Put (File, CPP_Name (To_String (M.In_Param_Name)));
                           if M.Out_Param_Name /= Null_Unbounded_String then
                              Put (File, ", ");
                           end if;
                        end if;
                        
                        if M.Out_Param_Name /= Null_Unbounded_String then
                           Put (File, CPP_Name (To_String (M.Out_Param_Name)));
                        end if;
                        
                        Put_Line (File, ");");
    
                        if not M.Oneway then
                           New_Line (File);
                           
                           if M.Out_Param_Name /= Null_Unbounded_String then
                              Put_Line (File, "        yami::parameters " &
                                 CPP_Name (To_String (M.Out_Param_Name)) & "_;");
                              Put_Line (File, "        " &
                                 CPP_Name (To_String (M.Out_Param_Name)) & ".write(" &
                                 CPP_Name (To_String (M.Out_Param_Name)) & "_);");
                              Put_Line (File, "        im_.reply(" &
                                 CPP_Name (To_String (M.Out_Param_Name)) & "_);");
                           else
                              Put_Line (File, "        im_.reply();");
                           end if;
                        end if;
                        
                        Put_Line (File, "    }");
                        Put_Line (File, "    else");
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
         
            Interface_Name : constant String := Name_Lists.Element (C);
            
            CI : Interface_Maps.Cursor;
            
         begin
            Put_Line (File, CPP_Name (Interface_Name) & "::" &
               CPP_Name (Interface_Name) & "(yami::agent & client_agent,");
            Put_Line (File, "    const std::string & server_location, const std::string & object_name,");
            Put_Line (File, "    int timeout)");
            Put_Line (File, "    : agent_(client_agent),");
            Put_Line (File, "    server_location_(server_location),");
            Put_Line (File, "    object_name_(object_name),");
            Put_Line (File, "    timeout_(timeout)");
            Put_Line (File, "{");
            Put_Line (File, "}");
            New_Line (File);

            CI := Defs.Interface_Definitions.Find (Interface_Name);
            Mode := Client;
            Interface_Maps.Query_Element (CI, Process_Messages'Access);
            
            Put_Line (File, "void " & CPP_Name (Interface_Name & "_Server") &
               "::operator()(yami::incoming_message & im_)");
            Put_Line (File, "{");
            Put_Line (File, "    const std::string & msg_name_ = im_.get_message_name();");
            New_Line (File);
            
            CI := Defs.Interface_Definitions.Find (Interface_Name);
            Mode := Server;
            Interface_Maps.Query_Element (CI, Process_Messages'Access);
            
            Put_Line (File, "    {");
            Put_Line (File, "        throw yami::yami_runtime_error(""Unknown operation name."");");
            Put_Line (File, "    }");
            Put_Line (File, "}");
            New_Line (File);
         end Process_Interface;

      begin
--         Defs.Explicit_Imports.Iterate (Process_Import'Access);
--         if not Defs.Explicit_Imports.Is_Empty then
--            New_Line (File);
--         end if;

         Put_Line (File, "using namespace " & CPP_Name (Package_Name) & ";");
         New_Line (File);
         
         Defs.Ordered_Type_Names.Iterate (Process_Type'Access);
         
         Defs.Ordered_Interface_Names.Iterate (Process_Interface'Access);

      end Process_Package_Definitions;

      C : Package_Maps.Cursor;
      
   begin
      Put_Line (File, "//");
      Put_Line (File, "// C++ implementations for package " & Package_Name & '.');
      Put_Line (File, "// This file was generated automatically by yami4idl.");
      Put_Line (File, "//");
      New_Line (File);
      Put_Line (File, "#include """ & Header_File_Name ("", Package_Name) & """");
      New_Line (File);
      Put_Line (File, "#include <yami4-cpp/agent.h>");
      Put_Line (File, "#include <yami4-cpp/errors.h>");
      Put_Line (File, "#include <yami4-cpp/incoming_message.h>");
      Put_Line (File, "#include <yami4-cpp/outgoing_message.h>");
      New_Line (File);

      C := All_Packages.Find (Package_Name);
      Package_Maps.Query_Element (C, Process_Package_Definitions'Access);
   end Write_Implementation;

   procedure Generate (Output_Dir : in String; Casing_Style : in Casing_Mode) is

      procedure Process_Fully_Defined_Package (C : in Name_Lists.Cursor) is
         Package_Name : constant String := Name_Lists.Element (C);
      
         File : File_Type;
      begin
         Create (File, Out_File, Header_File_Name (Output_Dir, Package_Name));
         Write_Header (File, Package_Name);
         Close (File);

         Create (File, Out_File, Impl_File_Name (Output_Dir, Package_Name));
         Write_Implementation (File, Package_Name);
         Close (File);
      end Process_Fully_Defined_Package;

   begin
      Casing := Casing_Style;
      Fully_Defined_Packages.Iterate (Process_Fully_Defined_Package'Access);
   end Generate;

end IDL.Structures.CPP_Generator;

