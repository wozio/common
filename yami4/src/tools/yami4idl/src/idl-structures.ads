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

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package IDL.Structures is

   procedure Collect_Import (Imported_Package_Name : in String; Explicit : in Boolean);
   
   procedure Create_Package (New_Package_Name : in String);

   procedure Create_Type (Package_Name : in String; New_Type_Name : in String);

   procedure Create_Field (Package_Name : in String; Enclosing_Type_Name : in String;
                           New_Field_Name : in String; Field_Type_Name : in String;
                           Optional : in Boolean);

   procedure Create_Interface (Package_Name : in String; New_Interface_Name : in String);

   procedure Create_Message
     (Package_Name : in String; Enclosing_Interface_Name : in String;
      New_Message_Name : in String; Oneway : in Boolean;
      In_Param_Name : in String; In_Param_Type : in String;
      Out_Param_Name : in String; Out_Param_Type : in String);
   
   procedure Verify_Param_Type_Is_User_Defined
      (Package_Name : in String; Type_Name : in String);

   procedure Finish_Package (Package_Name : in String);

   procedure Set_Mode (Mode : in Input_Mode);

private

   K_Boolean : constant String := "Boolean";
   K_Integer : constant String := "Integer";
   K_Long_Long : constant String := "Long_Long";
   K_Float : constant String := "Float";
   K_String : constant String := "String";
   K_Binary : constant String := "Binary";
   K_Boolean_Array : constant String := "Boolean_Array";
   K_Integer_Array : constant String := "Integer_Array";
   K_Long_Long_Array : constant String := "Long_Long_Array";
   K_Float_Array : constant String := "Float_Array";
   K_String_Array : constant String := "String_Array";
   K_Binary_Array : constant String := "Binary_Array";

   type Field_Definition is record
      Type_Name : Unbounded_String;
      Optional : Boolean;
   end record;

   type Message_Definition is record
      Oneway : Boolean;
      In_Param_Name : Unbounded_String; --  or "" if no such param
      In_Param_Type : Unbounded_String;
      Out_Param_Name : Unbounded_String;
      Out_Param_Type : Unbounded_String;
   end record;
   
   package Field_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type => String, --  field name
         Element_Type => Field_Definition,
         Hash => Ada.Strings.Hash,
         Equivalent_Keys => "=");

   type Type_Fields is record
      Definitions : Field_Maps.Map;
      Ordered_Names : Name_Lists.List;
   end record;

   package Message_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type => String, --  message name
         Element_Type => Message_Definition,
         Hash => Ada.Strings.Hash,
         Equivalent_Keys => "=");
   
   type Interface_Messages is record
      Definitions : Message_Maps.Map;
      Ordered_Names : Name_Lists.List;
   end record;

   --  this container keeps ordered field definitions for all types
   package Type_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type => String, --  type name
         Element_Type => Type_Fields,
         Hash => Ada.Strings.Hash,
         Equivalent_Keys => "=");

   --  this container keeps ordered message definitions for all interfaces
   package Interface_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type => String, --  interface name
         Element_Type => Interface_Messages,
         Hash => Ada.Strings.Hash,
         Equivalent_Keys => "=");
   
   type Package_Definitions is record
      Created_As : Input_Mode;
      
      Explicit_Imports : Name_Lists.List;
      Implicit_Imports : Name_Lists.List;
      
      Type_Definitions : Type_Maps.Map;
      Ordered_Type_Names : Name_Lists.List;
      
      Interface_Definitions : Interface_Maps.Map;
      Ordered_Interface_Names : Name_Lists.List;
   end record;
   
   --  this container keeps ordered type definitions for all packages
   package Package_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type => String, --  package name or "" for top-level definitions
         Element_Type => Package_Definitions,
         Hash => Ada.Strings.Hash,
         Equivalent_Keys => "=");
   
   --  this list includes only packages that were parsed and processed
   --  in the Full mode, as opposed to imported packages
   --  these packages are used as sources for code generation
   Fully_Defined_Packages : Name_Lists.List;

   --  main data structure with complete and incomplete definitions
   --  for all visited packages
   All_Packages : Package_Maps.Map;

end IDL.Structures;

