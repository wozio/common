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

with Ada.Characters.Latin_1;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package body IDL.Structures is

   Processing_Mode : Input_Mode := Full;
   
   function Type_Is_Primitive (Type_Name : in String) return Boolean is
   begin
      return Type_Name = K_Boolean or
         Type_Name = K_Integer or
         Type_Name = K_Long_Long or
         Type_Name = K_Float or
         Type_Name = K_String or
         Type_Name = K_Binary or
         Type_Name = K_Boolean_Array or
         Type_Name = K_Integer_Array or
         Type_Name = K_Long_Long_Array or
         Type_Name = K_Float_Array or
         Type_Name = K_String_Array or
         Type_Name = K_Binary_Array;
   end Type_Is_Primitive;
   
   procedure Verify_Type_Is_Not_Primitive (Type_Name : in String) is
   begin
      if Type_Is_Primitive (Type_Name) then
         
         raise Invalid_Input with
            "'" & Type_Name & "' - basic types cannot be redefined";
      end if;
   end Verify_Type_Is_Not_Primitive;


   --  these are names of imported packages that are collected
   --  before the actual package name is known
   Pending_Explicit_Imports : Name_Lists.List;
   Pending_Implicit_Imports : Name_Lists.List;
   

   procedure Collect_Import (Imported_Package_Name : in String; Explicit : in Boolean) is
   begin
      if Explicit then
         Pending_Explicit_Imports.Append (Imported_Package_Name);
      else -- implicit
         Pending_Implicit_Imports.Append (Imported_Package_Name);
      end if;
   end Collect_Import;
   
   procedure Create_Package (New_Package_Name : in String) is
      CP : Package_Maps.Cursor;
      Dummy_Inserted : Boolean;
      
      use type Package_Maps.Cursor;
   begin
      CP := All_Packages.Find (New_Package_Name);
      if CP = Package_Maps.No_Element then
      
         All_Packages.Insert
           (New_Package_Name,
            Package_Definitions'
              (Created_As => Processing_Mode,
               Explicit_Imports => Pending_Explicit_Imports,
               Implicit_Imports => Pending_Implicit_Imports,
               Type_Definitions => Type_Maps.Empty_Map,
               Ordered_Type_Names => Name_Lists.Empty_List,
               Interface_Definitions => Interface_Maps.Empty_Map,
               Ordered_Interface_Names => Name_Lists.Empty_List),
            CP, Dummy_Inserted);
            
      else
         --  such package was already created - check for mode conflicts:
         --  - it is OK to import the already defined or already imported package
         --  - it is OK to fully define the already imported package
         --  - it is an error to attempt to redefine the already defined package
         
         declare
            procedure Update_Existing_Package
              (Package_Name : in String;
               Pkg : in out Package_Definitions) is
            begin
               if Pkg.Created_As = Full then
                  if Processing_Mode = Full then
                     raise Invalid_Input with
                        "'" & New_Package_Name & "' - packages cannot be redefined";
                  else
                     --  do not touch the existing package if it was created as full
                     return;
                  end if;
               end if;
                  
               --  reconstruct the existing package from scratch
               Pkg.Created_As := Processing_Mode;
               Pkg.Explicit_Imports := Pending_Explicit_Imports;
               Pkg.Implicit_Imports := Pending_Implicit_Imports;
               Pkg.Type_Definitions := Type_Maps.Empty_Map;
               Pkg.Ordered_Type_Names := Name_Lists.Empty_List;
               Pkg.Interface_Definitions := Interface_Maps.Empty_Map;
               Pkg.Ordered_Interface_Names := Name_Lists.Empty_List;
               
            end Update_Existing_Package;
         begin
            All_Packages.Update_Element (CP, Update_Existing_Package'Access);
         end;
      end if;
      
      Pending_Explicit_Imports := Name_Lists.Empty_List;
      Pending_Implicit_Imports := Name_Lists.Empty_List;
      
   end Create_Package;
   
   function Type_Is_User_Defined
     (Package_Name : in String; Type_Name : in String) return Boolean is
     
      function Type_Is_Known_In_Package
        (Package_Name : in String; Check_Hierarchy : in Boolean; Type_Name : in String)
         return Boolean is

         Type_Found : Boolean;
      
         procedure Query_Package_Types
           (Package_Name : in String; Pkg : in Package_Definitions) is
        
            CT : Type_Maps.Cursor;
         
            use type Type_Maps.Cursor;
         begin
            CT := Pkg.Type_Definitions.Find (Type_Name);
            Type_Found := CT /= Type_Maps.No_Element;
         end Query_Package_Types;
     
         CP : Package_Maps.Cursor;
      
         use type Package_Maps.Cursor;
      begin
         if Package_Name = "" then
            return False;
         end if;
         
         CP := All_Packages.Find (Package_Name);
         if CP /= Package_Maps.No_Element then
            Package_Maps.Query_Element (CP, Query_Package_Types'Access);
         end if;
         
         if Type_Found then
            return True;
         else
            --  if the name is not known in the given package,
            --  then try in the parent package(s)
            
            if Check_Hierarchy then
               return Type_Is_Known_In_Package
                 (Name_Utils.Trim_Last_Component (Package_Name),
                  Check_Hierarchy, Type_Name);
            else
               return False;
            end if;
         end if;
      end Type_Is_Known_In_Package;
      
   begin
      if Name_Utils.Name_Is_Qualified (Type_Name) then
         --  qualified name lookup
         
         declare
            Explicit_Package_Name : constant String :=
               Name_Utils.Trim_Last_Component (Type_Name);
            Simple_Type_Name  : constant String :=
               Name_Utils.Last_Component (Type_Name);
         begin
            --  note: qualified name lookup does not traverse package hierarchy
            
            return Type_Is_Known_In_Package
               (Explicit_Package_Name, False, Simple_Type_Name);
         end;
      else
         --  this is a simple name - search for it in the same package
         --  or in parent packages
         
         return Type_Is_Known_In_Package (Package_Name, True, Type_Name);
      end if;
   end Type_Is_User_Defined;
   
   procedure Verify_Field_Type_Is_Known
     (Package_Name : in String; Field_Type_Name : in String) is
   begin
      if not Type_Is_Primitive (Field_Type_Name) and
         not Type_Is_User_Defined (Package_Name, Field_Type_Name) then

         raise Invalid_Input with
            "'" & Field_Type_Name & "' - unknown field type name";
      end if;
   end Verify_Field_Type_Is_Known;

   procedure Create_Type (Package_Name : in String; New_Type_Name : in String) is
      
      procedure Update_Package_Types
        (Package_Name : in String;
         Pkg : in out Package_Definitions) is
         
         CT : Type_Maps.Cursor;
         Inserted : Boolean;
      begin
         Pkg.Type_Definitions.Insert
           (New_Type_Name,
            Type_Fields'(Definitions => Field_Maps.Empty_Map,
                         Ordered_Names => Name_Lists.Empty_List),
            CT, Inserted);
           
         if Inserted then
            Pkg.Ordered_Type_Names.Append (New_Type_Name);
         else
            if Processing_Mode = Full then
               raise Invalid_Input with
                  "'" & New_Type_Name & "' - types cannot be redefined";
            end if;
         end if;
         
      end Update_Package_Types;

      CP : Package_Maps.Cursor;

      use type Package_Maps.Cursor;
   begin
      Verify_Type_Is_Not_Primitive (New_Type_Name);

      --  the package exists already
      
      CP := All_Packages.Find (Package_Name);

      All_Packages.Update_Element (CP, Update_Package_Types'Access);
   end Create_Type;
   
   procedure Create_Field
     (Package_Name : in String; Enclosing_Type_Name : in String;
      New_Field_Name : in String; Field_Type_Name : in String;
      Optional : in Boolean) is

      procedure Update_Package_Types
        (Package_Name : in String;
         Pkg : in out Package_Definitions) is
         
         procedure Update_User_Type
           (Enclosing_Type_Name : in String; Fields : in out Type_Fields) is
        
            CF : Field_Maps.Cursor;
            Inserted : Boolean;
         begin
            Fields.Definitions.Insert
              (New_Field_Name,
               Field_Definition'(To_Unbounded_String (Field_Type_Name), Optional),
               CF, Inserted);
         
            if Inserted then
               Fields.Ordered_Names.Append (New_Field_Name);
            else
               if Processing_Mode = Full then
                  raise Invalid_Input with
                     "'" & New_Field_Name &
                     "' - duplicate field name, fields cannot be redefined";
               end if;
            end if;
            
         end Update_User_Type;

         CT : Type_Maps.Cursor;
      
         use type Type_Maps.Cursor;
      begin
         CT := Pkg.Type_Definitions.Find (Enclosing_Type_Name);
         Pkg.Type_Definitions.Update_Element (CT, Update_User_Type'Access);
      end Update_Package_Types;

      CP : Package_Maps.Cursor;

      use type Package_Maps.Cursor;
   begin
      if Processing_Mode = Import then
         --  no need to populate fields in imported packages
         return;
      end if;
      
      Verify_Field_Type_Is_Known (Package_Name, Field_Type_Name);
      
      --  the package exists already
      
      CP := All_Packages.Find (Package_Name);

      All_Packages.Update_Element (CP, Update_Package_Types'Access);
   end Create_Field;

   procedure Create_Interface
     (Package_Name : in String; New_Interface_Name : in String) is

      procedure Update_Package_Interfaces
        (Package_Name : in String;
         Pkg : in out Package_Definitions) is
         
         CI : Interface_Maps.Cursor;
         Inserted : Boolean;
      begin
         Pkg.Interface_Definitions.Insert
           (New_Interface_Name,
            Interface_Messages'(Definitions => Message_Maps.Empty_Map,
                                Ordered_Names => Name_Lists.Empty_List),
            CI, Inserted);
           
         if Inserted then
            Pkg.Ordered_Interface_Names.Append (New_Interface_Name);
         else
            if Processing_Mode = Full then
               raise Invalid_Input with
                  "'" & New_Interface_Name & "' - interfaces cannot be redefined";
            end if;
         end if;
         
      end Update_Package_Interfaces;

      CP : Package_Maps.Cursor;

      use type Package_Maps.Cursor;
   begin
      if Processing_Mode = Import then
         --  no need to populate interfaces in imported packages
         return;
      end if;
      
      --  the package exists already (enforced by grammar)
      
      CP := All_Packages.Find (Package_Name);

      All_Packages.Update_Element (CP, Update_Package_Interfaces'Access);
   end Create_Interface;

   procedure Create_Message
     (Package_Name : in String; Enclosing_Interface_Name : in String;
      New_Message_Name : in String; Oneway : in Boolean;
      In_Param_Name : in String; In_Param_Type : in String;
      Out_Param_Name : in String; Out_Param_Type : in String) is

      procedure Update_Package_Interfaces
        (Package_Name : in String;
         Pkg : in out Package_Definitions) is
         
         procedure Update_Interface
           (Enclosing_Interface_Name : in String;
            Messages : in out Interface_Messages) is
        
            CM : Message_Maps.Cursor;
            Inserted : Boolean;
         begin
            Messages.Definitions.Insert
              (New_Message_Name,
               Message_Definition'
                 (Oneway => Oneway,
                  In_Param_Name => To_Unbounded_String (In_Param_Name),
                  In_Param_Type => To_Unbounded_String (In_Param_Type),
                  Out_Param_Name => To_Unbounded_String (Out_Param_Name),
                  Out_Param_Type => To_Unbounded_String (Out_Param_Type)),
               CM, Inserted);
         
            if Inserted then
               Messages.Ordered_Names.Append (New_Message_Name);
            else
               if Processing_Mode = Full then
                  raise Invalid_Input with
                     "'" & New_Message_Name &
                     "' - duplicate message name, messages cannot be redefined " &
                     "within the same interface";
               end if;
            end if;
            
         end Update_Interface;

         CI : Interface_Maps.Cursor;
      
         use type Interface_Maps.Cursor;
      begin
         CI := Pkg.Interface_Definitions.Find (Enclosing_Interface_Name);
         Pkg.Interface_Definitions.Update_Element (CI, Update_Interface'Access);
      end Update_Package_Interfaces;

      CP : Package_Maps.Cursor;

      use type Package_Maps.Cursor;
   begin
      if Processing_Mode = Import then
         --  no need to populate messages in imported packages
         return;
      end if;
      
      --  the package and interface exist already (enforce by grammar)
      --  and parameter types were already verified as well
      
      CP := All_Packages.Find (Package_Name);

      All_Packages.Update_Element (CP, Update_Package_Interfaces'Access);
   end Create_Message;

   procedure Verify_Param_Type_Is_User_Defined
     (Package_Name : in String; Type_Name : in String) is
   begin
      if not Type_Is_User_Defined (Package_Name, Type_Name) then

         raise Invalid_Input with
            "'" & Type_Name & "' - unknown type name";
      end if;
   end Verify_Param_Type_Is_User_Defined;

   procedure Finish_Package (Package_Name : in String) is
   begin
      if Processing_Mode = Full then
         Fully_Defined_Packages.Append (Package_Name);
      end if;
   end Finish_Package;
   
   procedure Set_Mode (Mode : in Input_Mode) is
   begin
      Processing_Mode := Mode;
   end Set_Mode;
   
end IDL.Structures;

