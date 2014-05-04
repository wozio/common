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

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Text_IO;
use Ada.Text_IO;

package body IDL.Structures.Java_Generator is

   Casing : Casing_Mode;
   Out_Dir : Unbounded_String;
   
   function Java_Name (IDL_Name : in String; Class_Name : in Boolean) return String is

      Java : Unbounded_String;
      
      New_Word : Boolean := Class_Name;
      
      procedure Process_Next_Char (C : in Character) is
      begin
         case Casing is
            when Ident =>
               Append (Java, C);
            when Lower_Case =>
               Append (Java, Ada.Characters.Handling.To_Lower (C));
            when Camel_Case | Default =>
               if C = Ada.Characters.Latin_1.Low_Line then
                  New_Word := True;
               else
                  if New_Word then
                     Append (Java, Ada.Characters.Handling.To_Upper (C));
                  else
                     Append (Java, Ada.Characters.Handling.To_Lower (C));
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
            Append (Java, ".");
            New_Word := True;
         end if;
      end loop;
            
      return To_String (Java);
   end Java_Name;

   function Java_Package_Name (IDL_Name : in String) return String is
      Java : Unbounded_String;
      C : Character;
   begin
      for I in IDL_Name'Range loop
         C := IDL_Name (I);
         if C /= Ada.Characters.Latin_1.Full_Stop then
            --  skip underscores
            if C /= Ada.Characters.Latin_1.Low_Line then
               Append (Java, Ada.Characters.Handling.To_Lower (C));
            end if;
         else
            Append (Java, ".");
         end if;
      end loop;
            
      return To_String (Java);
   end Java_Package_Name;
   
   function Java_Class_Name (IDL_Name : in String) return String is
      Package_Name : constant String := Name_Utils.Trim_Last_Component (IDL_Name);
      Plain_Class_Name : constant String := Name_Utils.Last_Component (IDL_Name, True);
   begin
      if Package_Name /= "" then
         return Java_Package_Name (Package_Name) & "." &
            Java_Name (Plain_Class_Name, True);
      else
         return Java_Name (Plain_Class_Name, True);
      end if;
   end Java_Class_Name;
   
   -- recursive helper that transforms a.b.c.d names into a/b/c/d paths
   function Name_To_Path (From_Dir : in String; Name : in String) return String is
      First : constant String := Name_Utils.First_Component (Name, True);
      Tail : constant String := Name_Utils.Trim_First_Component (Name);
   begin
      if Tail = "" then
         return Ada.Directories.Compose (From_Dir, First);
      else
         return Name_To_Path (Ada.Directories.Compose (From_Dir, First), Tail);
      end if;
   end Name_To_Path;

   function File_Name (Package_Name : in String; Class_Name : in String) return String is
      J_Package_Name : constant String := Java_Package_Name (Package_Name);
      J_Class_Name : constant String := Java_Class_Name (Class_Name);
   begin
      return Ada.Directories.Compose
        (Name_To_Path (To_String (Out_Dir), J_Package_Name), J_Class_Name, "java");
   end File_Name;
   
   procedure Make_Directory_For_Package
     (Package_Name : in String; Directory_Created : out Boolean) is

      Dir_Name : constant String :=
         Name_To_Path (To_String (Out_Dir), Java_Package_Name (Package_Name));
   begin
      Directory_Created := False;
      begin
         Ada.Directories.Create_Path (Dir_Name);
         Directory_Created := True;
      exception
         when others =>
            Put_Line ("cannot create directory " & Dir_Name &
               " for package " & Package_Name & ", skipping these files");
      end;
   end Make_Directory_For_Package;
   
   function Java_Member_Name (IDL_Name : in String) return String is
   begin
      --  member names never have package prefixes
      return Java_Name (IDL_Name, False);
   end Java_Member_Name;
   
   function Java_Field_Valid_Name (IDL_Name : in String) return String is
   begin
      return Java_Member_Name (IDL_Name & "_Valid");
   end Java_Field_Valid_Name;

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

   function Java_Type (IDL_Type : in String) return String is
   begin
      if IDL_Type = K_Boolean then
         return "boolean";
      elsif IDL_Type = K_Integer then
         return "int";
      elsif IDL_Type = K_Long_Long then
         return "long";
      elsif IDL_Type = K_Float then
         return "double";
      elsif IDL_Type = K_String then
         return "String";
      elsif IDL_Type = K_Binary then
         return "byte[]";
      elsif IDL_Type = K_Boolean_Array then
         return "boolean[]";
      elsif IDL_Type = K_Integer_Array then
         return "int[]";
      elsif IDL_Type = K_Long_Long_Array then
         return "long[]";
      elsif IDL_Type = K_Float_Array then
         return "double[]";
      elsif IDL_Type = K_String_Array then
         return "String[]";
      elsif IDL_Type = K_Binary_Array then
         return "byte[][]";
      else
         --  user-defined type
         
         return Java_Class_Name (IDL_Type);
      end if;
   end Java_Type;

   procedure Put_Operation_Signature (File : in File_Type; M : in Message_Definition) is
   begin
      Put (File, "(");
      if M.In_Param_Name /= Null_Unbounded_String then
         Put (File, Java_Class_Name (To_String (M.In_Param_Type)) &
            " " & Java_Member_Name (To_String (M.In_Param_Name)));
                        
         if M.Out_Param_Name /= Null_Unbounded_String then
            Put (File, ", ");
         end if;
      end if;
                     
      if M.Out_Param_Name /= Null_Unbounded_String then
         Put (File, Java_Class_Name (To_String (M.Out_Param_Type)) &
            " " & Java_Member_Name (To_String (M.Out_Param_Name)));
      end if;
      
      Put (File, ")");
   end Put_Operation_Signature;

   procedure Write_Files (Package_Name : in String) is
   
      procedure Process_Package_Definitions (Key : in String; Defs : in Package_Definitions) is
      
         type Mode_Type is (Client, Server);
         Mode : Mode_Type;
         
         File : File_Type;

         procedure Write_File_Head is

            procedure Process_Import (C : in Name_Lists.Cursor) is
            begin
               Put_Line (File, "import " & Java_Package_Name (Name_Lists.Element (C)) & ".*;");
            end Process_Import;
               
         begin
            Put_Line (File, "//");
            Put_Line (File, "// Java package and class definitions for package " & Package_Name & '.');
            Put_Line (File, "// This file was generated automatically by yami4idl.");
            Put_Line (File, "//");
            New_Line (File);
            Put_Line (File, "package " & Java_Package_Name (Package_Name) & ";");
            New_Line (File);

            Defs.Implicit_Imports.Iterate (Process_Import'Access);

            New_Line (File);
            Put_Line (File, "import com.inspirel.yami.Agent;");
            Put_Line (File, "import com.inspirel.yami.IncomingMessage;");
            Put_Line (File, "import com.inspirel.yami.IncomingMessageCallback;");
            Put_Line (File, "import com.inspirel.yami.NoSuchNameException;");
            Put_Line (File, "import com.inspirel.yami.OutgoingMessage;");
            Put_Line (File, "import com.inspirel.yami.Parameters;");
            Put_Line (File, "import com.inspirel.yami.YAMIIOException;");
            New_Line (File);
            Put_Line (File, "import java.util.concurrent.TimeoutException;");
            New_Line (File);

         end Write_File_Head;
         
         procedure Process_Type (C : in Name_Lists.Cursor) is
         
            procedure Process_Fields (Key : in String; Fields : in Type_Fields) is
            
               procedure Process_Field (C : in Name_Lists.Cursor) is
                  Field_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Field_Def (Key : in String; F : in Field_Definition) is
                  begin
                     if F.Optional then
                        Put_Line (File, "    public boolean " & Java_Field_Valid_Name (Field_Name) & ";");
                     end if;
                     
                     Put_Line (File, "    public " & Java_Type (To_String (F.Type_Name)) &
                        " " & Java_Member_Name (Field_Name) & ";");
                  end Process_Field_Def;
                  
                  CF : Field_Maps.Cursor;
               begin
                  CF := Fields.Definitions.Find (Field_Name);
                  Field_Maps.Query_Element (CF, Process_Field_Def'Access);
               end Process_Field;
            
            begin
               Fields.Ordered_Names.Iterate (Process_Field'Access);
            end Process_Fields;
         
            procedure Process_Fields_Cleaner (Key : in String; Fields : in Type_Fields) is
            
               procedure Process_Field (C : in Name_Lists.Cursor) is
                  Field_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Field_Def (Key : in String; F : in Field_Definition) is
                     Type_Name : constant String := To_String (F.Type_Name);
                  begin
                     if F.Optional then
                        Put_Line (File, "        " &
                           Java_Field_Valid_Name (Field_Name) & " = false;");
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
                        Put_Line (File, "        if (" &
                           Java_Field_Valid_Name (Field_Name) & ") {");
                     end if;

                     if Type_Name = K_Boolean then
                        Put_Line (File, "        params.setBoolean(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           Java_Member_Name (Field_Name) & ");");
                     elsif Type_Name = K_Integer then
                        Put_Line (File, "        params.setInteger(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           Java_Member_Name (Field_Name) & ");");
                     elsif Type_Name = K_Long_Long then
                        Put_Line (File, "        params.setLong(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           Java_Member_Name (Field_Name) & ");");
                     elsif Type_Name = K_Float then
                        Put_Line (File, "        params.setDouble(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           Java_Member_Name (Field_Name) & ");");
                     elsif Type_Name = K_String then
                        Put_Line (File, "        params.setString(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           Java_Member_Name (Field_Name) & ");");
                     elsif Type_Name = K_Binary then
                        Put_Line (File, "        params.setBinary(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           Java_Member_Name (Field_Name) & ");");
                     elsif Type_Name = K_Boolean_Array then
                        Put_Line (File, "        params.setBooleanArray(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           Java_Member_Name (Field_Name) & ");");
                     elsif Type_Name = K_Integer_Array then
                        Put_Line (File, "        params.setIntegerArray(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           Java_Member_Name (Field_Name) & ");");
                     elsif Type_Name = K_Long_Long_Array then
                        Put_Line (File, "        params.setLongArray(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           Java_Member_Name (Field_Name) & ");");
                     elsif Type_Name = K_Float_Array then
                        Put_Line (File, "        params.setDoubleArray(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           Java_Member_Name (Field_Name) & ");");
                     elsif Type_Name = K_String_Array then
                        Put_Line (File, "        params.setStringArray(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           Java_Member_Name (Field_Name) & ");");
                     elsif Type_Name = K_Binary_Array then
                        Put_Line (File, "        params.setBinaryArray(""" &
                           YAMI4_Field_Name (Field_Name) & """, " &
                           Java_Member_Name (Field_Name) & ");");
                     else
                        --  user-defined type
                        
                        declare
                           Nested : constant String := Java_Member_Name (Field_Name) & "Nested";
                        begin
                           Put_Line (File, "        Parameters " & Nested & " = new Parameters();");
                           Put_Line (File, "        " &
                              Java_Member_Name (Field_Name) & ".write(" & Nested &");");
                              
                           Put_Line (File, "        params.setNestedParameters(""" &
                              YAMI4_Field_Name (Field_Name) & """, " & Nested & ");");
                        end;
                     end if;

                     if F.Optional then
                        Put_Line (File, "        }");
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
                           Put_Line (File, "        Parameters.Entry e;");
                           
                           Param_Entry_Already_Defined := True;
                        end if;
                        
                        Put_Line (File, "        e = params.find(""" &
                           YAMI4_Field_Name (Field_Name) & """);");
                        Put_Line (File, "        " &
                           Java_Field_Valid_Name (Field_Name) & " = e != null;");
                        Put_Line (File, "        if (" & Java_Field_Valid_Name (Field_Name) & ") {");
                     end if;

                     if Type_Name = K_Boolean then
                        Put_Line (File, "        " & Java_Member_Name (Field_Name) &
                           " = params.getBoolean(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Integer then
                        Put_Line (File, "        " & Java_Member_Name (Field_Name) &
                           " = params.getInteger(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Long_Long then
                        Put_Line (File, "        " & Java_Member_Name (Field_Name) &
                           " = params.getLong(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Float then
                        Put_Line (File, "        " & Java_Member_Name (Field_Name) &
                           " = params.getDouble(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_String then
                        Put_Line (File, "        " & Java_Member_Name (Field_Name) &
                           " = params.getString(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Binary then
                        Put_Line (File, "        " & Java_Member_Name (Field_Name) &
                           " = params.getBinary(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Boolean_Array then
                        Put_Line (File, "        " & Java_Member_Name (Field_Name) &
                           " = params.getBooleanArray(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Integer_Array then
                        Put_Line (File, "        " & Java_Member_Name (Field_Name) &
                           " = params.getIntegerArray(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Long_Long_Array then
                        Put_Line (File, "        " & Java_Member_Name (Field_Name) &
                           " = params.getLongArray(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Float_Array then
                        Put_Line (File, "        " & Java_Member_Name (Field_Name) &
                           " = params.getDoubleArray(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_String_Array then
                        Put_Line (File, "        " & Java_Member_Name (Field_Name) &
                           " = params.getStringArray(""" & YAMI4_Field_Name (Field_Name) & """);");
                     elsif Type_Name = K_Binary_Array then
                        Put_Line (File, "        " & Java_Member_Name (Field_Name) &
                           " = params.getBinaryArray(""" & YAMI4_Field_Name (Field_Name) & """);");
                     else
                        --  user-defined type
                        
                        declare
                           Nested : constant String := Java_Member_Name (Field_Name) & "Nested";
                        begin
                           Put_Line (File, "        Parameters " & Nested &
                              " = params.getNestedParameters(""" &
                              YAMI4_Field_Name (Field_Name) & """);");
                           
                           Put_Line (File, "        " &
                              Java_Member_Name (Field_Name) & ".read(" & Nested & ");");
                        end;
                     end if;

                     if F.Optional then
                        Put_Line (File, "        }");
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
            Create (File, Out_File, File_Name (Package_Name, Type_Name));

            Write_File_Head;
            
            Put_Line (File, "public class " & Java_Class_Name (Type_Name) & " {");
            New_Line (File);
            
            CT := Defs.Type_Definitions.Find (Type_Name);
            Type_Maps.Query_Element (CT, Process_Fields'Access);
            
            New_Line (File);
            Put_Line (File, "    public " & Java_Class_Name (Type_Name) & "() {");

            Type_Maps.Query_Element (CT, Process_Fields_Cleaner'Access);

            Put_Line (File, "    }");
            New_Line (File);
            Put_Line (File, "    public void write(Parameters params) {");
            
            Type_Maps.Query_Element (CT, Process_Fields_Writer'Access);
            
            Put_Line (File, "    }");
            New_Line (File);
            Put_Line (File, "    public void read(Parameters params) {");
            
            Type_Maps.Query_Element (CT, Process_Fields_Reader'Access);
            
            Put_Line (File, "    }");
            Put_Line (File, "}");

            Close (File);
         end Process_Type;
      
         procedure Process_Interface (C : in Name_Lists.Cursor) is
         
            procedure Process_Messages (Key : in String; Messages : in Interface_Messages) is
            
               procedure Process_Message (C : in Name_Lists.Cursor) is
                  Message_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Message_Def (Key : in String; M : in Message_Definition) is
                  begin
                     if Mode = Server then
                        Put (File, "    public abstract ");
                     else
                        Put (File, "    public ");
                     end if;
                     
                     Put (File, "void " & Java_Member_Name (Message_Name));
                     Put_Operation_Signature (File, M);
                     New_Line (File);
                     Put (File, "        throws Exception");

                     if Mode = Server then
                        Put_Line (File, ";");
                        New_Line (File);
                     else
                        Put_Line (File, " {");
                        New_Line (File);
                        
                        if M.In_Param_Name /= Null_Unbounded_String then
                           Put_Line (File, "        Parameters " &
                              Java_Member_Name (To_String (M.In_Param_Name)) & "Y4 = new Parameters();");
                           Put_Line (File, "        " &
                                 Java_Member_Name (To_String (M.In_Param_Name)) & ".write(" &
                           Java_Member_Name (To_String (M.In_Param_Name)) & "Y4);");
                           New_Line (File);
                        end if;
                        
                        if M.Oneway then
                           if M.In_Param_Name /= Null_Unbounded_String then
                              Put_Line (File, "        agent.sendOneWay(serverLocation, objectName,");
                              Put_Line (File, "            """ &
                                 YAMI4_Operation_Name (Message_Name) & """, " &
                                 Java_Member_Name (To_String (M.In_Param_Name)) & "Y4);");
                           else
                              Put_Line (File, "        agent.sendOneWay(serverLocation, objectName,");
                              Put_Line (File, "            """ &
                                 YAMI4_Operation_Name (Message_Name) & """, null);");
                           end if;
                        else
                           if M.In_Param_Name /= Null_Unbounded_String then
                              Put_Line (File, "        OutgoingMessage msg = " &
                                 "agent.send(serverLocation, objectName,");
                              Put_Line (File, "            """ &
                                 YAMI4_Operation_Name (Message_Name) & """, " &
                                 Java_Member_Name (To_String (M.In_Param_Name)) & "Y4);");
                           else
                              Put_Line (File, "        OutgoingMessage msg = " &
                                 "agent.send(serverLocation, objectName,");
                              Put_Line (File, "            """ &
                                 YAMI4_Operation_Name (Message_Name) & """, null);");
                           end if;

                           New_Line (File);
                           Put_Line (File, "        if (timeout != 0L) {");
                           Put_Line (File, "            boolean onTime = msg.waitForCompletion(timeout);");
                           Put_Line (File, "            if (onTime == false) {");
                           Put_Line (File, "                " &
                              "throw new TimeoutException(""Operation timed out."");");
                           Put_Line (File, "            }");
                           Put_Line (File, "        } else {");
                           Put_Line (File, "            msg.waitForCompletion();");
                           Put_Line (File, "        }");
                           New_Line (File);
                           Put_Line (File, "        OutgoingMessage.MessageState state = msg.getState();");
                           Put_Line (File, "        switch (state) {");
                           Put_Line (File, "        case REPLIED:");
                           
                           if M.Out_Param_Name /= Null_Unbounded_String then
                              Put_Line (File, "            " &
                                 Java_Member_Name (To_String (M.Out_Param_Name)) & ".read(msg.getReply());");
                           end if;
                           
                           Put_Line (File, "            msg.close();");
                           Put_Line (File, "            break;");
                           Put_Line (File, "        case ABANDONED:");
                           Put_Line (File, "            msg.close();");
                           Put_Line (File, "            throw new YAMIIOException(");
                           Put_Line (File, "                ""Operation was abandoned due to communication errors."");");
                           Put_Line (File, "        case REJECTED:");
                           Put_Line (File, "            msg.close();");
                           Put_Line (File, "            throw new YAMIIOException(");
                           Put_Line (File, "                ""Operation was rejected: "" + msg.getExceptionMsg());");
                           Put_Line (File, "        }");
                        end if;

                        Put_Line (File, "    }");
                        New_Line (File);
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
         
            procedure Process_Message_Dispatchers (Key : in String; Messages : in Interface_Messages) is
            
               procedure Process_Message (C : in Name_Lists.Cursor) is
                  Message_Name : constant String := Name_Lists.Element (C);
                  
                  procedure Process_Message_Def (Key : in String; M : in Message_Definition) is
                  begin
                     Put_Line (File, "if (msgName.equals(""" &
                        YAMI4_Operation_Name (Message_Name) & """)) {");

                     if M.In_Param_Name /= Null_Unbounded_String then
                        Put_Line (File, "            " &
                           Java_Class_Name (To_String (M.In_Param_Type)) & " " &
                           Java_Member_Name (To_String (M.In_Param_Name)) &
                              " = new " & Java_Class_Name (To_String (M.In_Param_Type)) & "();");
                        Put_Line (File, "            " &
                           Java_Member_Name (To_String (M.In_Param_Name)) &
                           ".read(msg.getParameters());");
                        New_Line (File);
                     end if;
                        
                     if M.Out_Param_Name /= Null_Unbounded_String then
                        Put_Line (File, "            " &
                           Java_Class_Name (To_String (M.Out_Param_Type)) & " " &
                           Java_Member_Name (To_String (M.Out_Param_Name)) &
                           " = new " & Java_Class_Name (To_String (M.Out_Param_Type)) & "();");
                     end if;

                     if M.In_Param_Name /= Null_Unbounded_String or
                        M.Out_Param_Name /= Null_Unbounded_String then
                        New_Line (File);
                     end if;
                        
                     Put (File, "            " & Java_Member_Name (Message_Name) & "(");

                     if M.In_Param_Name /= Null_Unbounded_String then
                        Put (File, Java_Member_Name (To_String (M.In_Param_Name)));
                        if M.Out_Param_Name /= Null_Unbounded_String then
                           Put (File, ", ");
                        end if;
                     end if;
                        
                     if M.Out_Param_Name /= Null_Unbounded_String then
                        Put (File, Java_Member_Name (To_String (M.Out_Param_Name)));
                     end if;
                        
                     Put_Line (File, ");");
    
                     if not M.Oneway then
                        New_Line (File);
                          
                        if M.Out_Param_Name /= Null_Unbounded_String then
                           Put_Line (File, "            Parameters " &
                              Java_Member_Name (To_String (M.Out_Param_Name)) & "Y4 = new Parameters();");
                           Put_Line (File, "            " &
                              Java_Member_Name (To_String (M.Out_Param_Name)) & ".write(" &
                              Java_Member_Name (To_String (M.Out_Param_Name)) & "Y4);");
                           Put_Line (File, "            msg.reply(" &
                              Java_Member_Name (To_String (M.Out_Param_Name)) & "Y4);");
                        else
                           Put_Line (File, "            msg.reply(null);");
                        end if;
                     end if;
                        
                     Put (File, "        } else ");
                  end Process_Message_Def;
                  
                  CM : Message_Maps.Cursor;
               begin
                  CM := Messages.Definitions.Find (Message_Name);
                  Message_Maps.Query_Element (CM, Process_Message_Def'Access);
               end Process_Message;
            
            begin
               Messages.Ordered_Names.Iterate (Process_Message'Access);
            end Process_Message_Dispatchers;

            Interface_Name : constant String := Name_Lists.Element (C);
            
            CI : Interface_Maps.Cursor;
            
         begin
            --  generate client code
            
            Create (File, Out_File, File_Name (Package_Name, Interface_Name));

            Write_File_Head;
            
            Put_Line (File, "public class " & Java_Class_Name (Interface_Name) & " {");
            New_Line (File);
            Put_Line (File, "    private Agent agent;");
            Put_Line (File, "    private String serverLocation;");
            Put_Line (File, "    private String objectName;");
            Put_Line (File, "    private long timeout;");
            New_Line (File);
            Put_Line (File, "    public " & Java_Class_Name (Interface_Name) & "(");
            Put_Line (File, "        Agent agent, String serverLocation, String objectName, long timeout) {");
            New_Line (File);
            Put_Line (File, "        this.agent = agent;");
            Put_Line (File, "        this.serverLocation = serverLocation;");
            Put_Line (File, "        this.objectName = objectName;");
            Put_Line (File, "        this.timeout = timeout;");
            Put_Line (File, "    }");
            New_Line (File);
            Put_Line (File, "    public " & Java_Class_Name (Interface_Name) & "(");
            Put_Line (File, "        Agent agent, String serverLocation, String objectName) {");
            New_Line (File);
            Put_Line (File, "        this(agent, serverLocation, objectName, 0L);");
            Put_Line (File, "    }");
            New_Line (File);
            
            CI := Defs.Interface_Definitions.Find (Interface_Name);
            Mode := Client;
            Interface_Maps.Query_Element (CI, Process_Messages'Access);
            
            New_Line (File);
            Put_Line (File, "}");
            
            Close (File);
            
            --  generate server code
            
            Create (File, Out_File, File_Name (Package_Name, Interface_Name & "_Server"));

            Write_File_Head;
            
            Put_Line (File, "public abstract class " & Java_Class_Name (Interface_Name & "_Server"));
            Put_Line (File, "    implements IncomingMessageCallback {");
            New_Line (File);
            Put_Line (File, "    public " & Java_Class_Name (Interface_Name & "_Server") & "() {");
            Put_Line (File, "    }");
            New_Line (File);
            
            Mode := Server;
            Interface_Maps.Query_Element (CI, Process_Messages'Access);
            
            Put_Line (File, "    public void call(IncomingMessage msg) throws Exception {");
            Put_Line (File, "        String msgName = msg.getMessageName();");
            New_Line (File);
            Put (File, "        ");

            Interface_Maps.Query_Element (CI, Process_Message_Dispatchers'Access);

            Put_Line (File, "{");
            Put_Line (File, "            throw new NoSuchNameException(""Unknown operation name."");");
            Put_Line (File, "        }");
            Put_Line (File, "    }");
            Put_Line (File, "}");
            
            Close (File);
         end Process_Interface;
  
      begin

         Defs.Ordered_Type_Names.Iterate (Process_Type'Access);

         Defs.Ordered_Interface_Names.Iterate (Process_Interface'Access);

      end Process_Package_Definitions;

      C : Package_Maps.Cursor;
      
   begin
      C := All_Packages.Find (Package_Name);
      Package_Maps.Query_Element (C, Process_Package_Definitions'Access);
   end Write_Files;

   procedure Generate (Output_Dir : in String; Casing_Style : in Casing_Mode) is

      procedure Process_Fully_Defined_Package (C : in Name_Lists.Cursor) is
         Package_Name : constant String := Name_Lists.Element (C);
         Directory_Created : Boolean;
      begin
         Make_Directory_For_Package (Package_Name, Directory_Created);

         if Directory_Created then
            Write_Files (Package_Name);
         end if;
      end Process_Fully_Defined_Package;

   begin
      Casing := Casing_Style;
      Out_Dir := To_Unbounded_String (Output_Dir);
      Fully_Defined_Packages.Iterate (Process_Fully_Defined_Package'Access);
   end Generate;

end IDL.Structures.Java_Generator;

