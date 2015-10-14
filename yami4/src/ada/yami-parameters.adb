--  Copyright Maciej Sobczak 2008-2015.
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

with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;

package body YAMI.Parameters is

   --  convenience renamings from the YAMI.Details package

   subtype Void_Ptr is Details.Void_Ptr;
   subtype Char_Ptr is Details.Char_Ptr;
   subtype Char_Array is Details.Char_Array;
   subtype Size_T is Details.Size_T;
   subtype Int is Details.Int;
   subtype Long_Long is Details.Long_Long;
   subtype Double is Details.Double;

   subtype Void_Ptr_Array is Details.Void_Ptr_Array;
   subtype Size_T_Array is Details.Size_T_Array;

   procedure Check_Result (Res : in Int) renames Details.Check_Result;

   --  helper function for decoding entry types
   function Decode_Entry_Type (T : in Int) return Parameter_Type is
      Entry_Type : Parameter_Type;
   begin
      case T is
         when 1 => Entry_Type := Boolean_Type;
         when 2 => Entry_Type := Integer_Type;
         when 3 => Entry_Type := Long_Long_Integer_Type;
         when 4 => Entry_Type := Long_Float_Type;
         when 5 => Entry_Type := String_Type;
         when 6 => Entry_Type := Binary_Type;
         when 7 => Entry_Type := Boolean_Array_Type;
         when 8 => Entry_Type := Integer_Array_Type;
         when 9 => Entry_Type := Long_Long_Integer_Array_Type;
         when 10 => Entry_Type := Long_Float_Array_Type;
         when 11 => Entry_Type := String_Array_Type;
         when 12 => Entry_Type := Binary_Array_Type;
         when 13 => Entry_Type := Nested_Parameters_Type;
         when 14 => Entry_Type := Nested_Parameters_Array_Type;
         when others =>
            pragma Assert (False);
            null;
      end case;
      return Entry_Type;
   end Decode_Entry_Type;

   --  helper function for array operations, used in more than one subprogram

   function Get_Bool (Array_Base : in System.Address;
                      Index : in Size_T) return Int;
   pragma Import (C, Get_Bool, "get_bool_from_array");

   function Get_Int (Array_Base : in System.Address;
                     Index : in Size_T) return Int;
   pragma Import (C, Get_Int, "get_int_from_array");

   function Get_Long_Long (Array_Base : in System.Address;
                           Index : in Size_T) return Long_Long;
   pragma Import (C, Get_Long_Long, "get_long_long_from_array");

   function Get_Double (Array_Base : in System.Address;
                        Index : in Size_T) return Double;
   pragma Import (C, Get_Double, "get_double_from_array");

   function Buffer_To_Stream_Element_Array
     (Buffer_Ptr : in Void_Ptr;
      Length : in Size_T)
     return Ada.Streams.Stream_Element_Array is

      subtype Return_Type is Ada.Streams.Stream_Element_Array
        (1 .. Ada.Streams.Stream_Element_Offset (Length));
      Ptr : Void_Ptr := Buffer_Ptr;

   begin
      return Ret : Return_Type do
         for I in Ret'Range loop
            declare
               Byte : Ada.Streams.Stream_Element;
               for Byte'Address use Ptr;
            begin
               Ret (I) := Byte;
            end;
            Ptr := System.Storage_Elements."+" (Ptr, 1);
         end loop;
      end return;
   end Buffer_To_Stream_Element_Array;

   overriding
   function Core_Object (Params : in Parameters_Collection)
                            return Void_Ptr is
   begin
      if Params.Nested then
         --  this object delegates to the nested object inside another one
         return Params.Delegated;
      else
         --  this object hosts the body for C's content
         return Params.Value (Params.Value'First)'Address;
      end if;
   end Core_Object;

   --  helpers for proper handling of empty strings
   --  this helper is needed, because Interfaces.C.To_C cannot handle
   --  empty strings - the helper always returns non-empty char_array
   --  (the 0 length value is properly interpreted at the C side)
   function Safe_To_C (S : in String) return Interfaces.C.char_array is
   begin
      if S'Length /= 0 then
         return Interfaces.C.To_C (Item => S, Append_Nul => False);
      else
         return Interfaces.C.To_C (Item => S, Append_Nul => True);
      end if;
   end Safe_To_C;

   function Safe_From_C (Str : in Char_Ptr; Length : in Size_T) return String is
      use type Char_Ptr;
      use type Size_T;
   begin
      if Str /= Interfaces.C.Strings.Null_Ptr and Length /= 0 then
         return Interfaces.C.Strings.Value (Str, Length);
      else
         return "";
      end if;
   end Safe_From_C;

   --
   --  Operations of the Parameters_Collection type.
   --

   procedure Common_Make_Parameters
     (Params : out Parameters_Collection;
      Working_Area : in System.Address := System.Null_Address;
      Area_Size : in System.Storage_Elements.Storage_Count) is

      procedure Parameters_Create (C_Params : out Parameters_Value;
                                   Working_Area : in Void_Ptr;
                                   Size : in Size_T);
      pragma Import (C, Parameters_Create, "parameters_create");

      use type System.Address;
      use type System.Storage_Elements.Storage_Count;

   begin
      --  Working_Area and Area_Size must be both zero or both non-zero
      if Working_Area /= System.Null_Address xor Area_Size /= 0 then
         raise Logic_Error with "Invalid working area parameters.";
      end if;

      Parameters_Create (Params.Value, Working_Area, Size_T (Area_Size));

      Params.Initialized := True;

   end Common_Make_Parameters;

   function Make_Parameters
     (Working_Area : in System.Address := System.Null_Address;
      Area_Size : in System.Storage_Elements.Storage_Count := 0)
     return Parameters_Collection is

   begin
      return Params : Parameters_Collection (Nested => False) do
         Common_Make_Parameters (Params, Working_Area, Area_Size);
      end return;
   end Make_Parameters;

   procedure Do_Free is new Ada.Unchecked_Deallocation
     (Object => Parameters_Collection,
      Name => Parameters_Collection_Access);

   function New_Parameters
     (Working_Area : in System.Address := System.Null_Address;
      Area_Size : in System.Storage_Elements.Storage_Count := 0)
     return Parameters_Collection_Access is

      Params : Parameters_Collection_Access :=
        new Parameters_Collection (Nested => False);
   begin
      Common_Make_Parameters (Params.all, Working_Area, Area_Size);
      return Params;
   exception
      when others =>
         Do_Free (Params);
         raise;
   end New_Parameters;

   procedure Finalize (Params : in out Parameters_Collection) is

      procedure Destroy_Parameters (C_Params : in Void_Ptr);
      pragma Import (C, Destroy_Parameters, "destroy_parameters");

   begin
      if Params.Initialized and then not Params.Nested then
         Destroy_Parameters (Core_Object (Params));
      end if;
   end Finalize;

   procedure Free (Params : in out Parameters_Collection_Access) is
   begin
      Do_Free (Params);
   end Free;

   procedure Set_Boolean (Params : in out Parameters_Collection;
                          Name : in String; Value : in Boolean) is

      function Parameters_Set_Boolean (C_Params : in Void_Ptr;
                                       C_Name : in Char_Array;
                                       Name_Length : in Size_T;
                                       Val : in Int) return Int;
      pragma Import (C, Parameters_Set_Boolean, "parameters_set_boolean");

      Val : Int;

   begin
      if Value then
         Val := 1;
      else
         Val := 0;
      end if;
      Check_Result
        (Parameters_Set_Boolean
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length), Val));
   end Set_Boolean;

   function Get_Boolean (Params : in Parameters_Collection;
                         Name : in String) return Boolean is

      function Parameters_Get_Boolean (C_Params : in Void_Ptr;
                                       C_Name : in Char_Array;
                                       Name_Length : in Size_T;
                                       Val : access Int) return Int;
      pragma Import (C, Parameters_Get_Boolean, "parameters_get_boolean");

      Val : aliased Int;

      use type Int;

   begin
      Check_Result
        (Parameters_Get_Boolean
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Val'Access));
      return Val /= 0;
   end Get_Boolean;

   procedure Set_Integer (Params : in out Parameters_Collection;
                          Name : in String; Value : in YAMI_Integer) is

      function Parameters_Set_Integer (C_Params : in Void_Ptr;
                                       C_Name : in Char_Array;
                                       Name_Length : in Size_T;
                                       Val : in Int) return Int;
      pragma Import (C, Parameters_Set_Integer, "parameters_set_integer");

   begin
      Check_Result
        (Parameters_Set_Integer
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Int (Value)));
   end Set_Integer;

   function Get_Integer (Params : in Parameters_Collection;
                         Name : in String) return YAMI_Integer is

      function Parameters_Get_Integer (C_Params : in Void_Ptr;
                                       C_Name : in Char_Array;
                                       Name_Length : in Size_T;
                                       Val : access Int) return Int;
      pragma Import (C, Parameters_Get_Integer, "parameters_get_integer");

      Val : aliased Int;

      use type Int;

   begin
      Check_Result
        (Parameters_Get_Integer
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Val'Access));
      return YAMI_Integer (Val);
   end Get_Integer;

   procedure Set_Long_Long
     (Params : in out Parameters_Collection;
      Name : in String; Value : in YAMI_Long_Long_Integer) is

      function Parameters_Set_Long_Long (C_Params : in Void_Ptr;
                                         C_Name : in Char_Array;
                                         Name_Length : in Size_T;
                                         Val : in Long_Long) return Int;
      pragma Import (C, Parameters_Set_Long_Long, "parameters_set_long_long");

   begin
      Check_Result
        (Parameters_Set_Long_Long
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Long_Long (Value)));
   end Set_Long_Long;

   function Get_Long_Long (Params : in Parameters_Collection;
                           Name : in String) return YAMI_Long_Long_Integer is

      function Parameters_Get_Long_Long (C_Params : in Void_Ptr;
                                         C_Name : in Char_Array;
                                         Name_Length : in Size_T;
                                         Val : access Long_Long) return Int;
      pragma Import (C, Parameters_Get_Long_Long, "parameters_get_long_long");

      Val : aliased Long_Long;

      use type Long_Long;

   begin
      Check_Result
        (Parameters_Get_Long_Long
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Val'Access));
      return YAMI_Long_Long_Integer (Val);
   end Get_Long_Long;

   procedure Set_Long_Float (Params : in out Parameters_Collection;
                             Name : in String; Value : in YAMI_Long_Float) is

      function Parameters_Set_Double_Float (C_Params : in Void_Ptr;
                                            C_Name : in Char_Array;
                                            Name_Length : in Size_T;
                                            Val : in Double) return Int;
      pragma Import (C, Parameters_Set_Double_Float,
                     "parameters_set_double_float");

   begin
      Check_Result
        (Parameters_Set_Double_Float
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Double (Value)));
   end Set_Long_Float;

   function Get_Long_Float (Params : in Parameters_Collection;
                            Name : in String) return YAMI_Long_Float is

      function Parameters_Get_Double_Float (C_Params : in Void_Ptr;
                                            C_Name : in Char_Array;
                                            Name_Length : in Size_T;
                                            Val : access Double) return Int;
      pragma Import (C, Parameters_Get_Double_Float,
                     "parameters_get_double_float");

      Val : aliased Double;

      use type Double;

   begin
      Check_Result
        (Parameters_Get_Double_Float
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Val'Access));
      return YAMI_Long_Float (Val);
   end Get_Long_Float;

   procedure Set_String (Params : in out Parameters_Collection;
                         Name : in String; Value : in String) is

      function Parameters_Set_String (C_Params : in Void_Ptr;
                                      C_Name : in Char_Array;
                                      Name_Length : in Size_T;
                                      Val : in Char_Array;
                                      Val_Length : in Size_T) return Int;
      pragma Import (C, Parameters_Set_String, "parameters_set_string");

   begin
      Check_Result
        (Parameters_Set_String
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Safe_To_C (Value), Size_T (Value'Length)));
   end Set_String;

   function Get_String (Params : in Parameters_Collection;
                        Name : in String) return String is

      function Parameters_Get_String (C_Params : in Void_Ptr;
                                      C_Name : in Char_Array;
                                      Name_Length : in Size_T;
                                      Val : access Char_Ptr;
                                      Val_Length : access Size_T) return Int;
      pragma Import (C, Parameters_Get_String, "parameters_get_string");

      Value : aliased Char_Ptr;
      Length : aliased Size_T;

   begin
      Check_Result
        (Parameters_Get_String
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Value'Access, Length'Access));
      return Safe_From_C (Value, Length);
   end Get_String;

   procedure Set_Binary (Params : in out Parameters_Collection;
                          Name : in String;
                          Value : in Ada.Streams.Stream_Element_Array) is

      function Parameters_Set_Binary (C_Params : in Void_Ptr;
                                      C_Name : in Char_Array;
                                      Name_Length : in Size_T;
                                      Val : in Void_Ptr;
                                      Val_Length : in Size_T) return Int;
      pragma Import (C, Parameters_Set_Binary, "parameters_set_binary");

   begin
      Check_Result
        (Parameters_Set_Binary
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Value (Value'First)'Address, Size_T (Value'Length)));
   end Set_Binary;

   function Get_Binary (Params : in Parameters_Collection;
                        Name : in String)
                       return Ada.Streams.Stream_Element_Array is

      function Parameters_Get_Binary (C_Params : in Void_Ptr;
                                      C_Name : in Char_Array;
                                      Name_Length : in Size_T;
                                      Val : access Void_Ptr;
                                      Val_Length : access Size_T) return Int;
      pragma Import (C, Parameters_Get_Binary, "parameters_get_binary");

      Buffer_Ptr : aliased Void_Ptr;
      Length : aliased Size_T;

   begin
      Check_Result
        (Parameters_Get_Binary
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Buffer_Ptr'Access, Length'Access));
      return Buffer_To_Stream_Element_Array (Buffer_Ptr, Length);
   end Get_Binary;

   procedure Set_Boolean_Array (Params : in out Parameters_Collection;
                                Name : in String;
                                Values : in Boolean_Array_Type) is

      type Int_Array is array (Index_Type range <>) of Int;

      function Parameters_Create_Boolean_Array
        (C_Params : in Void_Ptr;
         C_Name : in Char_Array;
         Name_Length : in Size_T;
         Array_Length : in Size_T;
         New_Array : access System.Address)
        return Int;
      pragma Import (C, Parameters_Create_Boolean_Array,
                     "parameters_create_boolean_array");

      procedure Set_Bool (Array_Base : in System.Address;
                          Index : in Size_T;
                          Val : in Int);
      pragma Import (C, Set_Bool, "set_bool_in_array");

      New_Array : aliased System.Address;
      J : Size_T := 0;

      use type Size_T;

   begin
      Check_Result
        (Parameters_Create_Boolean_Array
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Size_T (Values'Length), New_Array'Access));
      for I in Values'Range loop
         if Values (I) then
            Set_Bool (New_Array, J, 1);
         else
            Set_Bool (New_Array, J, 0);
         end if;
         J := J + 1;
      end loop;
   end Set_Boolean_Array;

   function Get_Boolean_Array_Length (Params : in Parameters_Collection;
                                      Name : in String) return Count_Type is

      function Parameters_Get_Boolean_Array_Length
        (C_Params : in Void_Ptr;
         C_Name : in Char_Array;
         Name_Length : in Size_T;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameters_Get_Boolean_Array_Length,
                     "parameters_get_boolean_array_length");

      Length : aliased Size_T;

   begin
      Check_Result
        (Parameters_Get_Boolean_Array_Length
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Length'Access));
      return Count_Type (Length);
   end Get_Boolean_Array_Length;

   procedure Get_Boolean_Array (Params : in Parameters_Collection;
                                Name : in String;
                                Values : out Boolean_Array_Type) is

      function Parameters_Get_Boolean_Array
        (C_Params : in Void_Ptr;
         C_Name : in Char_Array;
         Name_Length : in Size_T;
         Internal_Array : access System.Address;
         Array_Length : access Size_T) return Int;
      pragma Import (C, Parameters_Get_Boolean_Array,
                     "parameters_get_boolean_array");

      Internal_Array : aliased System.Address;
      Internal_Array_Length : aliased Size_T;
      J : Size_T := 0;

      use type Int;
      use type Size_T;

   begin
      Check_Result
        (Parameters_Get_Boolean_Array
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Internal_Array'Access, Internal_Array_Length'Access));
      if Internal_Array_Length /= Values'Length then
         --  array mismatch
         raise Constraint_Error;
      end if;
      for I in Values'Range loop
         Values (I) := Get_Bool (Internal_Array, J) /= 0;
         J := J + 1;
      end loop;
   end Get_Boolean_Array;

   procedure Set_Integer_Array (Params : in out Parameters_Collection;
                                Name : in String;
                                Values : in Integer_Array_Type) is

      type Int_Array is array (Index_Type range <>) of Int;

      function Parameters_Create_Integer_Array
        (C_Params : in Void_Ptr;
         C_Name : in Char_Array;
         Name_Length : in Size_T;
         Array_Length : in Size_T;
         New_Array : access System.Address)
        return Int;
      pragma Import (C, Parameters_Create_Integer_Array,
                     "parameters_create_integer_array");

      procedure Set_Int (Array_Base : in System.Address;
                         Index : in Size_T;
                         Val : in Int);
      pragma Import (C, Set_Int, "set_int_in_array");

      New_Array : aliased System.Address;
      J : Size_T := 0;

      use type Size_T;

   begin
      Check_Result
        (Parameters_Create_Integer_Array
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Size_T (Values'Length), New_Array'Access));
      for I in Values'Range loop
         Set_Int (New_Array, J, Int (Values (I)));
         J := J + 1;
      end loop;
   end Set_Integer_Array;

   function Get_Integer_Array_Length (Params : in Parameters_Collection;
                                      Name : in String) return Count_Type is

      function Parameters_Get_Integer_Array_Length
        (C_Params : in Void_Ptr;
         C_Name : in Char_Array;
         Name_Length : in Size_T;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameters_Get_Integer_Array_Length,
                     "parameters_get_integer_array_length");

      Length : aliased Size_T;

   begin
      Check_Result
        (Parameters_Get_Integer_Array_Length
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Length'Access));
      return Count_Type (Length);
   end Get_Integer_Array_Length;

   procedure Get_Integer_Array (Params : in Parameters_Collection;
                                Name : in String;
                                Values : out Integer_Array_Type) is

      function Parameters_Get_Integer_Array
        (C_Params : in Void_Ptr;
         C_Name : in Char_Array;
         Name_Length : in Size_T;
         Internal_Array : access System.Address;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameters_Get_Integer_Array,
                     "parameters_get_integer_array");

      Internal_Array : aliased System.Address;
      Internal_Array_Length : aliased Size_T;
      J : Size_T := 0;

      use type Int;
      use type Size_T;

   begin
      Check_Result
        (Parameters_Get_Integer_Array
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Internal_Array'Access, Internal_Array_Length'Access));
      if Internal_Array_Length /= Values'Length then
         --  array mismatch
         raise Constraint_Error;
      end if;
      for I in Values'Range loop
         Values (I) := Integer_Type (Get_Int (Internal_Array, J));
         J := J + 1;
      end loop;
   end Get_Integer_Array;

   procedure Set_Long_Long_Array (Params : in out Parameters_Collection;
                                  Name : in String;
                                  Values : in Long_Long_Integer_Array_Type) is

      type Long_Long_Array is array (Index_Type range <>) of Long_Long;

      function Parameters_Create_Long_Long_Array
        (C_Params : in Void_Ptr;
         C_Name : in Char_Array;
         Name_Length : in Size_T;
         Array_Length : in Size_T;
         New_Array : access System.Address)
        return Int;
      pragma Import (C, Parameters_Create_Long_Long_Array,
                     "parameters_create_long_long_array");

      procedure Set_Long_Long (Array_Base : in System.Address;
                               Index : in Size_T;
                               Val : in Long_Long);
      pragma Import (C, Set_Long_Long, "set_long_long_in_array");

      New_Array : aliased System.Address;
      J : Size_T := 0;

      use type Size_T;

   begin
      Check_Result
        (Parameters_Create_Long_Long_Array
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Size_T (Values'Length), New_Array'Access));
      for I in Values'Range loop
         Set_Long_Long (New_Array, J, Long_Long (Values (I)));
         J := J + 1;
      end loop;
   end Set_Long_Long_Array;

   function Get_Long_Long_Array_Length (Params : in Parameters_Collection;
                                        Name : in String) return Count_Type is

      function Parameters_Get_Long_Long_Array_Length
        (C_Params : in Void_Ptr;
         C_Name : in Char_Array;
         Name_Length : in Size_T;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameters_Get_Long_Long_Array_Length,
                     "parameters_get_long_long_array_length");

      Length : aliased Size_T;

   begin
      Check_Result
        (Parameters_Get_Long_Long_Array_Length
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Length'Access));
      return Count_Type (Length);
   end Get_Long_Long_Array_Length;

   procedure Get_Long_Long_Array
     (Params : in Parameters_Collection;
      Name : in String;
      Values : out Long_Long_Integer_Array_Type) is

      function Parameters_Get_Long_Long_Array
        (C_Params : in Void_Ptr;
         C_Name : in Char_Array;
         Name_Length : in Size_T;
         Internal_Array : access System.Address;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameters_Get_Long_Long_Array,
                     "parameters_get_long_long_array");

      Internal_Array : aliased System.Address;
      Internal_Array_Length : aliased Size_T;
      J : Size_T := 0;

      use type Long_Long;
      use type Size_T;

   begin
      Check_Result
        (Parameters_Get_Long_Long_Array
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Internal_Array'Access, Internal_Array_Length'Access));
      if Internal_Array_Length /= Values'Length then
         --  array mismatch
         raise Constraint_Error;
      end if;
      for I in Values'Range loop
         Values (I) :=
           Long_Long_Integer_Type (Get_Long_Long (Internal_Array, J));
         J := J + 1;
      end loop;
   end Get_Long_Long_Array;

   procedure Set_Long_Float_Array (Params : in out Parameters_Collection;
                                   Name : in String;
                                   Values : in Long_Float_Array_Type) is

      type Long_Float_Array is array (Index_Type range <>) of Double;

      function Parameters_Create_Double_Float_Array
        (C_Params : in Void_Ptr;
         C_Name : in Char_Array;
         Name_Length : in Size_T;
         Array_Length : in Size_T;
         New_Array : access System.Address)
        return Int;
      pragma Import (C, Parameters_Create_Double_Float_Array,
                     "parameters_create_double_float_array");

      procedure Set_Double (Array_Base : in System.Address;
                            Index : in Size_T;
                            Val : in Double);
      pragma Import (C, Set_Double, "set_double_in_array");

      New_Array : aliased System.Address;
      J : Size_T := 0;

      use type Size_T;

   begin
      Check_Result
        (Parameters_Create_Double_Float_Array
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Size_T (Values'Length), New_Array'Access));
      for I in Values'Range loop
         Set_Double (New_Array, J, Double (Values (I)));
         J := J + 1;
      end loop;
   end Set_Long_Float_Array;

   function Get_Long_Float_Array_Length (Params : in Parameters_Collection;
                                         Name : in String)
                                        return Count_Type is

      function Parameters_Get_Double_Float_Array_Length
        (C_Params : in Void_Ptr;
         C_Name : in Char_Array;
         Name_Length : in Size_T;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameters_Get_Double_Float_Array_Length,
                     "parameters_get_double_float_array_length");

      Length : aliased Size_T;

   begin
      Check_Result
        (Parameters_Get_Double_Float_Array_Length
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Length'Access));
      return Count_Type (Length);
   end Get_Long_Float_Array_Length;

   procedure Get_Long_Float_Array (Params : in Parameters_Collection;
                                   Name : in String;
                                   Values : out Long_Float_Array_Type) is

      function Parameters_Get_Double_Float_Array
        (C_Params : in Void_Ptr;
         C_Name : in Char_Array;
         Name_Length : in Size_T;
         Internal_Array : access System.Address;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameters_Get_Double_Float_Array,
                     "parameters_get_double_float_array");

      Internal_Array : aliased System.Address;
      Internal_Array_Length : aliased Size_T;
      J : Size_T := 0;

      use type Double;
      use type Size_T;

   begin
      Check_Result
        (Parameters_Get_Double_Float_Array
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Internal_Array'Access, Internal_Array_Length'Access));
      if Internal_Array_Length /= Values'Length then
         --  array mismatch
         raise Constraint_Error;
      end if;
      for I in Values'Range loop
         Values (I) := Long_Float_Type (Get_Double (Internal_Array, J));
         J := J + 1;
      end loop;
   end Get_Long_Float_Array;

   procedure Create_String_Array (Params : in Parameters_Collection;
                                  Name : in String;
                                  Length : in Count_Type) is

      function Parameters_Create_String_Array (C_Params : in Void_Ptr;
                                               C_Name : in Char_Array;
                                               Name_Length : in Size_T;
                                               Array_Length : in Size_T)
                                              return Int;
      pragma Import (C, Parameters_Create_String_Array,
                     "parameters_create_string_array");

   begin
      Check_Result
        (Parameters_Create_String_Array
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Size_T (Length)));
   end Create_String_Array;

   function Get_String_Array_Length (Params : in Parameters_Collection;
                                     Name : in String) return Count_Type is

      function Parameters_Get_String_Array_Length
        (C_Params : in Void_Ptr;
         C_Name : in Char_Array;
         Name_Length : in Size_T;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameters_Get_String_Array_Length,
                     "parameters_get_string_array_length");

      Length : aliased Size_T;

   begin
      Check_Result
        (Parameters_Get_String_Array_Length
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Length'Access));
      return Count_Type (Length);
   end Get_String_Array_Length;

   procedure Set_String_In_Array (Params : in Parameters_Collection;
                                  Name : in String;
                                  Index : in Index_Type;
                                  Value : in String) is

      function Parameters_Set_String_In_Array (C_Params : in Void_Ptr;
                                               C_Name : in Char_Array;
                                               Name_Length : in Size_T;
                                               Ind : in Size_T;
                                               C_Value : in Char_Array;
                                               Value_Length : in Size_T)
                                              return Int;
      pragma Import (C, Parameters_Set_String_In_Array,
                     "parameters_set_string_in_array");

   begin
      Check_Result
        (Parameters_Set_String_In_Array
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Size_T (Index - 1),
            Safe_To_C (Value), Size_T (Value'Length)));
   end Set_String_In_Array;

   function Get_String_In_Array (Params : in Parameters_Collection;
                                 Name : in String;
                                 Index : in Index_Type) return String is

      function Parameters_Get_String_In_Array (C_Params : in Void_Ptr;
                                               C_Name : in Char_Array;
                                               Name_Length : in Size_T;
                                               Ind : in Size_T;
                                               C_Value : access Char_Ptr;
                                               Value_Length : access Size_T)
                                              return Int;
      pragma Import (C, Parameters_Get_String_In_Array,
                     "parameters_get_string_in_array");

      Value : aliased Char_Ptr;
      Length : aliased Size_T;

      use type Char_Ptr;

   begin
      Check_Result
        (Parameters_Get_String_In_Array
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Size_T (Index - 1),
            Value'Access, Length'Access));
      return Safe_From_C (Value, Length);
   end Get_String_In_Array;

   procedure Create_Binary_Array (Params : in Parameters_Collection;
                                  Name : in String;
                                  Length : in Count_Type) is

      function Parameters_Create_Binary_Array (C_Params : in Void_Ptr;
                                               C_Name : in Char_Array;
                                               Name_Length : in Size_T;
                                               Array_Length : in Size_T)
                                              return Int;
      pragma Import (C, Parameters_Create_Binary_Array,
                     "parameters_create_binary_array");

   begin
      Check_Result
        (Parameters_Create_Binary_Array
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Size_T (Length)));
   end Create_Binary_Array;

   function Get_Binary_Array_Length (Params : in Parameters_Collection;
                                     Name : in String) return Count_Type is

      function Parameters_Get_Binary_Array_Length
        (C_Params : in Void_Ptr;
         C_Name : in Char_Array;
         Name_Length : in Size_T;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameters_Get_Binary_Array_Length,
                     "parameters_get_binary_array_length");

      Length : aliased Size_T;

   begin
      Check_Result
        (Parameters_Get_Binary_Array_Length
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Length'Access));
      return Count_Type (Length);
   end Get_Binary_Array_Length;

   procedure Set_Binary_In_Array
     (Params : in Parameters_Collection;
      Name : in String;
      Index : in Index_Type;
      Value : in Ada.Streams.Stream_Element_Array) is

      function Parameters_Set_Binary_In_Array (C_Params : in Void_Ptr;
                                               C_Name : in Char_Array;
                                               Name_Length : in Size_T;
                                               Ind : in Size_T;
                                               C_Value : in Void_Ptr;
                                               Value_Length : in Size_T)
                                              return Int;
      pragma Import (C, Parameters_Set_Binary_In_Array,
                     "parameters_set_binary_in_array");

   begin
      Check_Result
        (Parameters_Set_Binary_In_Array
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Size_T (Index - 1),
            Value (Value'First)'Address, Size_T (Value'Length)));
   end Set_Binary_In_Array;

   function Get_Binary_In_Array (Params : in Parameters_Collection;
                                 Name : in String;
                                 Index : in Index_Type)
                                return Ada.Streams.Stream_Element_Array is

      function Parameters_Get_Binary_In_Array (C_Params : in Void_Ptr;
                                               C_Name : in Char_Array;
                                               Name_Length : in Size_T;
                                               Ind : in Size_T;
                                               C_Value : access Void_Ptr;
                                               Value_Length : access Size_T)
                                              return Int;
      pragma Import (C, Parameters_Get_Binary_In_Array,
                     "parameters_get_binary_in_array");

      Buffer_Ptr : aliased Void_Ptr;
      Length : aliased Size_T;

   begin
      Check_Result
        (Parameters_Get_Binary_In_Array
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Size_T (Index - 1),
            Buffer_Ptr'Access, Length'Access));
      return Buffer_To_Stream_Element_Array (Buffer_Ptr, Length);
   end Get_Binary_In_Array;

   function Create_Nested_Parameters (Params : in Parameters_Collection;
                                      Name : in String)
                                     return Parameters_Collection is

      function Parameters_Create_Nested (C_Params : in Void_Ptr;
                                         C_Name : in Char_Array;
                                         Name_Length : in Size_T;
                                         Nested_Ptr : access Void_Ptr)
                                        return Int;
      pragma Import (C, Parameters_Create_Nested, "parameters_create_nested");

      Nested_Ptr : aliased Void_Ptr;

   begin
      Check_Result
        (Parameters_Create_Nested
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Nested_Ptr'Access));
      return Params : Parameters_Collection (Nested => True) do
         Params.Delegated := Nested_Ptr;
      end return;
   end Create_Nested_Parameters;

   function Get_Nested_Parameters (Params : in Parameters_Collection;
                                   Name : in String)
                                  return Parameters_Collection is

      function Parameters_Get_Nested (C_Params : in Void_Ptr;
                                      C_Name : in Char_Array;
                                      Name_Length : in Size_T;
                                      Nested_Ptr : access Void_Ptr)
                                     return Int;
      pragma Import (C, Parameters_Get_Nested, "parameters_get_nested");

      Nested_Ptr : aliased Void_Ptr;

   begin
      Check_Result
        (Parameters_Get_Nested
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Nested_Ptr'Access));
      return Params : Parameters_Collection (Nested => True) do
         Params.Delegated := Nested_Ptr;
      end return;
   end Get_Nested_Parameters;

   procedure Create_Nested_Array (Params : in Parameters_Collection;
                                  Name : in String;
                                  Length : in Count_Type) is

      function Parameters_Create_Nested_Array (C_Params : in Void_Ptr;
                                               C_Name : in Char_Array;
                                               Name_Length : in Size_T;
                                               Array_Length : in Size_T)
                                              return Int;
      pragma Import (C, Parameters_Create_Nested_Array,
                     "parameters_create_nested_array");

   begin
      Check_Result
        (Parameters_Create_Nested_Array
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Size_T (Length)));
   end Create_Nested_Array;

   function Get_Nested_Array_Length (Params : in Parameters_Collection;
                                     Name : in String)
                                    return Count_Type is

      function Parameters_Get_Nested_Array_Length
        (C_Params : in Void_Ptr;
         C_Name : in Char_Array;
         Name_Length : in Size_T;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameters_Get_Nested_Array_Length,
                     "parameters_get_nested_array_length");

      Length : aliased Size_T;

   begin
      Check_Result
        (Parameters_Get_Nested_Array_Length
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Length'Access));
      return Count_Type (Length);
   end Get_Nested_Array_Length;

   function Get_Nested_In_Array
     (Parent_Params : in Parameters_Collection;
      Name : in String;
      Index : in Index_Type) return Parameters_Collection is
   
      function Parameters_Get_Nested_In_Array (C_Params : in Void_Ptr;
                                               C_Name : in Char_Array;
                                               Name_Length : in Size_T;
                                               Ind : in Size_T;
                                               Nested_Ptr : access Void_Ptr)
                                              return Int;
      pragma Import (C, Parameters_Get_Nested_In_Array,
                     "parameters_get_nested_in_array");

      Nested_Ptr : aliased Void_Ptr;

   begin
      Check_Result
        (Parameters_Get_Nested_In_Array
           (Core_Object (Parent_Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Size_T (Index - 1),
            Nested_Ptr'Access));
      return Params : Parameters_Collection (Nested => True) do
         Params.Delegated := Nested_Ptr;
      end return;
   end Get_Nested_In_Array;

   procedure Remove (Params : in out Parameters_Collection;
                     Name : in String) is

      function Parameters_Remove (C_Params : in Void_Ptr;
                                  C_Name : in Char_Array;
                                  Name_Length : in Size_T)
                                 return Int;

      pragma Import (C, Parameters_Remove, "parameters_remove");

   begin
      Check_Result
        (Parameters_Remove
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length)));
   end Remove;

   procedure Clear (Params : in out Parameters_Collection) is

      procedure Parameters_Clear (C_Params : in Void_Ptr);
      pragma Import (C, Parameters_Clear, "parameters_clear");

   begin
      Parameters_Clear (Core_Object (Params));
   end Clear;

   function Length (Params : in Parameters_Collection) return Count_Type is

      function Parameters_Size (C_Params : in Void_Ptr) return Size_T;
      pragma Import (C, Parameters_Size, "parameters_size");

   begin
      return Count_Type (Parameters_Size (Core_Object (Params)));
   end Length;

   function Entry_Type (Params : in Parameters_Collection;
                        Name : in String) return Parameter_Type is

      function Parameters_Get_Type (C_Params : in Void_Ptr;
                                    C_Name : in Char_Array;
                                    Name_Length : in Size_T;
                                    Type_Code : access Int)
                                   return Int;
      pragma Import (C, Parameters_Get_Type, "parameters_get_type");

      Type_Code : aliased Int;

   begin
      Check_Result
        (Parameters_Get_Type
           (Core_Object (Params),
            Safe_To_C (Name), Size_T (Name'Length),
            Type_Code'Access));
      return Decode_Entry_Type (Type_Code);
   end Entry_Type;

   function First (Params : in Parameters_Collection)
                  return Parameter_Cursor is

      procedure Parameters_Get_Iterator
        (C_Params : in Void_Ptr;
         C_Iter : out Parameter_Iterator_Value;
         Result : out Int);
      pragma Import (C, Parameters_Get_Iterator, "parameters_get_iterator");

      Result : Int;
      use type Int;

   begin
      return Cursor : Parameter_Cursor do
         Parameters_Get_Iterator (Core_Object (Params), Cursor.V, Result);
         if Result = 7 then
            --  no such entries, translate it to empty cursor
            --  (in Ada this is not an error)
            Cursor := No_Parameter;
         else
            Check_Result (Result);
            Cursor.Empty := False;
         end if;
      end return;
   end First;

   procedure Find (Params : in Parameters_Collection;
                   Name : in String;
                   E : out Parameter_Entry;
                   Found : out Boolean) is

      procedure Parameters_Find (C_Params : in Void_Ptr;
                                 C_Name : in Char_Array;
                                 Name_Length : in Size_T;
                                 C_Entry : out Parameter_Entry_Value;
                                 Result : out Int);
      pragma Import (C, Parameters_Find, "parameters_find");

      Result : Int;
      use type Int;

   begin
      Parameters_Find
        (Core_Object (Params),
         Safe_To_C (Name), Size_T (Name'Length),
         E.V, Result);
      Found := Result /= 0;
   end Find;

   overriding
   function Serialize_Buffer_Size (Params : in Parameters_Collection)
                                  return Ada.Streams.Stream_Element_Count is

      function Parameters_Serialize_Buffer_Size (C_Params : in Void_Ptr;
                                                 Buffer_Size : access Size_T)
                                                return Int;
      pragma Import (C, Parameters_Serialize_Buffer_Size,
                     "parameters_serialize_buffer_size");

      Buffer_Size : aliased Size_T;

   begin
      Check_Result
        (Parameters_Serialize_Buffer_Size
           (Core_Object (Params), Buffer_Size'Access));
      return Ada.Streams.Stream_Element_Count (Buffer_Size);
   end Serialize_Buffer_Size;

   overriding
   procedure Serialize (Params : in Parameters_Collection;
                        Buffers : in Serializables.Serialization_Buffer_List)
   is

      function Parameters_Serialize (C_Params : in Void_Ptr;
                                     Bufs : in Void_Ptr_Array;
                                     Sizes : in Size_T_Array;
                                     Num_Of_Buffers : in Size_T)
                                    return Int;
      pragma Import (C, Parameters_Serialize, "parameters_serialize");

      Bufs : Void_Ptr_Array (Buffers'Range);
      Sizes : Size_T_Array (Buffers'Range);
      Num_Of_Buffers : constant Size_T := Buffers'Length;

   begin
      for I in Buffers'Range loop
         Bufs (I) := Buffers (I).Buffer (1)'Address;
         Sizes (I) := Buffers (I).Buffer'Length;
      end loop;

      Check_Result
        (Parameters_Serialize
           (Core_Object (Params), Bufs, Sizes, Num_Of_Buffers));
   end Serialize;

   procedure Deserialize
     (Params : in out Parameters_Collection;
      Buffers : in Serializables.Serialization_Buffer_List) is

      function Parameters_Deserialize (C_Params : in Void_Ptr;
                                       Bufs : in Void_Ptr_Array;
                                       Sizes : in Size_T_Array;
                                       Num_Of_Buffers : in Size_T)
                                      return Int;
      pragma Import (C, Parameters_Deserialize, "parameters_deserialize");

      Bufs : Void_Ptr_Array (Buffers'Range);
      Sizes : Size_T_Array (Buffers'Range);
      Num_Of_Buffers : constant Size_T := Buffers'Length;

   begin
      for I in Buffers'Range loop
         Bufs (I) := Buffers (I).Buffer (1)'Address;
         Sizes (I) := Buffers (I).Buffer'Length;
      end loop;

      Check_Result
        (Parameters_Deserialize
           (Core_Object (Params), Bufs, Sizes, Num_Of_Buffers));
   end Deserialize;

   procedure Deserialize
     (Params : in out Parameters_Collection;
      Buffers : in Core.Serialization_Buffers_Descriptor) is

      function Parameters_Deserialize
        (C_Params : in Void_Ptr;
         Bufs : access Void_Ptr; --  array of void*
         Sizes : access Size_T;  --  array of size_t
         Num_Of_Buffers : in Size_T)
        return Int;
      pragma Import (C, Parameters_Deserialize, "parameters_deserialize");

   begin
      Check_Result
        (Parameters_Deserialize
           (Core_Object (Params),
            Buffers.Buffers,
            Buffers.Buffer_Sizes,
            Buffers.Num_Of_Buffers));
   end Deserialize;

   --
   --  Operations of the Parameter_Cursor type.
   --

   function Has_Element (Position : in Parameter_Cursor) return Boolean is
   begin
      return not Position.Empty;
   end Has_Element;

   function Next (Position : in Parameter_Cursor) return Parameter_Cursor is

      function Parameter_Iterator_Has_Next
        (C_Iter : in Parameter_Iterator_Value) return Int;
      pragma Import (C, Parameter_Iterator_Has_Next,
                     "parameter_iterator_has_next");

      procedure Parameter_Iterator_Move_Next
        (C_Iter : in out Parameter_Iterator_Value);
      pragma Import (C, Parameter_Iterator_Move_Next,
                     "parameter_iterator_move_next");

      Result : Int;
      use type Int;

   begin
      if Position.Empty then
         raise Logic_Error with "No more elements.";
      end if;

      Result := Parameter_Iterator_Has_Next (Position.V);
      if Result /= 0 then
         return Cursor : Parameter_Cursor := Position do
            Parameter_Iterator_Move_Next (Cursor.V);
         end return;
      else
         return Cursor : Parameter_Cursor do
            Cursor.Empty := True;
         end return;
      end if;
   end Next;

   procedure Next (Position : in out Parameter_Cursor) is
   begin
      Position := Next (Position);
   end Next;

   function Get_Entry (Position : in Parameter_Cursor)
                      return Parameter_Entry is

      procedure Parameter_Iterator_Current
        (C_Iter : in Parameter_Iterator_Value;
         C_Entry : out Parameter_Entry_Value);
      pragma Import (C, Parameter_Iterator_Current,
                     "parameter_iterator_current");

   begin
      if Position.Empty then
         raise Logic_Error with "No element at this position.";
      end if;

      return E : Parameter_Entry do
         Parameter_Iterator_Current (Position.V, E.V);
      end return;
   end Get_Entry;

   --
   --  Operations of the Parameter_Entry type.
   --

   function Entry_Type (E : in Parameter_Entry) return Parameter_Type is

      function Parameter_Entry_Type (C_Entry : in Parameter_Entry_Value)
                                    return Int;
      pragma Import (C, Parameter_Entry_Type, "parameter_entry_type");

      Type_Code : constant Int := Parameter_Entry_Type (E.V);

   begin
      return Decode_Entry_Type (Type_Code);
   end Entry_Type;

   function Entry_Name (E : in Parameter_Entry) return String is

      procedure Parameter_Entry_Name (C_Entry : in Parameter_Entry_Value;
                                      C_Name : access Char_Ptr;
                                      Name_Length : access Size_T);
      pragma Import (C, Parameter_Entry_Name, "parameter_entry_name");

      Name : aliased Char_Ptr;
      Length : aliased Size_T;

   begin
      Parameter_Entry_Name (E.V, Name'Access, Length'Access);
      return Safe_From_C (Name, Length);
   end Entry_Name;

   function Get_Boolean (E : in Parameter_Entry) return Boolean is

      function Parameter_Entry_Get_Boolean
        (C_Entry : in Parameter_Entry_Value;
         Val : access Int) return Int;
      pragma Import (C, Parameter_Entry_Get_Boolean,
                     "parameter_entry_get_boolean");

      Val : aliased Int;
      use type Int;

   begin
      Check_Result (Parameter_Entry_Get_Boolean (E.V, Val'Access));
      return Val /= 0;
   end Get_Boolean;

   function Get_Integer (E : in Parameter_Entry) return YAMI_Integer is

      function Parameter_Entry_Get_Integer
        (C_Entry : in Parameter_Entry_Value;
         Val : access Int) return Int;
      pragma Import (C, Parameter_Entry_Get_Integer,
                     "parameter_entry_get_integer");

      Val : aliased Int;

   begin
      Check_Result (Parameter_Entry_Get_Integer (E.V, Val'Access));
      return YAMI_Integer (Val);
   end Get_Integer;

   function Get_Long_Long (E : in Parameter_Entry)
                          return YAMI_Long_Long_Integer is

      function Parameter_Entry_Get_Long_Long
        (C_Entry : in Parameter_Entry_Value;
         Val : access Long_Long) return Int;
      pragma Import (C, Parameter_Entry_Get_Long_Long,
                     "parameter_entry_get_long_long");

      Val : aliased Long_Long;

   begin
      Check_Result (Parameter_Entry_Get_Long_Long (E.V, Val'Access));
      return YAMI_Long_Long_Integer (Val);
   end Get_Long_Long;

   function Get_Long_Float (E : in Parameter_Entry) return YAMI_Long_Float is

      function Parameter_Entry_Get_Double_Float
        (C_Entry : in Parameter_Entry_Value;
         Val : access Double) return Int;
      pragma Import (C, Parameter_Entry_Get_Double_Float,
                     "parameter_entry_get_double_float");

      Val : aliased Double;

   begin
      Check_Result (Parameter_Entry_Get_Double_Float (E.V, Val'Access));
      return YAMI_Long_Float (Val);
   end Get_Long_Float;

   function Get_String (E : in Parameter_Entry) return String is

      procedure Parameter_Entry_Get_String
        (C_Entry : in Parameter_Entry_Value;
         C_Value : access Char_Ptr;
         Value_Length : access Size_T);
      pragma Import (C, Parameter_Entry_Get_String,
                     "parameter_entry_get_string");

      Value : aliased Char_Ptr;
      Length : aliased Size_T;

   begin
      Parameter_Entry_Get_String (E.V, Value'Access, Length'Access);
      return Safe_From_C (Value, Length);
   end Get_String;

   function Get_Binary (E : in Parameter_Entry)
                       return Ada.Streams.Stream_Element_Array is

      procedure Parameter_Entry_Get_Binary
        (C_Entry : in Parameter_Entry_Value;
         C_Value : access Void_Ptr;
         Value_Length : access Size_T);
      pragma Import (C, Parameter_Entry_Get_Binary,
                     "parameter_entry_get_binary");

      Buffer_Ptr : aliased Void_Ptr;
      Length : aliased Size_T;

   begin
      Parameter_Entry_Get_Binary (E.V, Buffer_Ptr'Access, Length'Access);
      return Buffer_To_Stream_Element_Array (Buffer_Ptr, Length);
   end Get_Binary;

   function Get_Boolean_Array_Length (E : in Parameter_Entry)
                                     return Count_Type is

      function Parameter_Entry_Get_Boolean_Array_Length
        (C_Entry : in Parameter_Entry_Value;
         Size : access Size_T) return Int;
      pragma Import (C, Parameter_Entry_Get_Boolean_Array_Length,
                     "parameter_entry_get_boolean_array_length");

      Length : aliased Size_T;

   begin
      Check_Result
        (Parameter_Entry_Get_Boolean_Array_Length (E.V, Length'Access));
      return Count_Type (Length);
   end Get_Boolean_Array_Length;

   procedure Entry_Get_Boolean_Array (E : in Parameter_Entry;
                                      Values : out Boolean_Array_Type) is

      function Parameter_Entry_Get_Boolean_Array
        (C_Entry : in Parameter_Entry_Value;
         Internal_Array : access System.Address;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameter_Entry_Get_Boolean_Array,
                     "parameter_entry_get_boolean_array");

      Internal_Array : aliased System.Address;
      Internal_Array_Length : aliased Size_T;
      J : Size_T := 0;

      use type Int;
      use type Size_T;

   begin
      Check_Result
        (Parameter_Entry_Get_Boolean_Array
           (E.V, Internal_Array'Access, Internal_Array_Length'Access));
      if Internal_Array_Length /= Values'Length then
         --  array mismatch
         raise Constraint_Error;
      end if;
      for I in Values'Range loop
         Values (I) := Get_Bool (Internal_Array, J) /= 0;
         J := J + 1;
      end loop;
   end Entry_Get_Boolean_Array;

   function Get_Integer_Array_Length (E : in Parameter_Entry)
                                     return Count_Type is

      function Parameter_Entry_Get_Integer_Array_Length
        (C_Entry : in Parameter_Entry_Value;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameter_Entry_Get_Integer_Array_Length,
                     "parameter_entry_get_integer_array_length");

      Length : aliased Size_T;

   begin
      Check_Result
        (Parameter_Entry_Get_Integer_Array_Length (E.V, Length'Access));
      return Count_Type (Length);
   end Get_Integer_Array_Length;

   procedure Entry_Get_Integer_Array (E : in Parameter_Entry;
                                      Values : out Integer_Array_Type) is

      function Parameter_Entry_Get_Integer_Array
        (C_Entry : in Parameter_Entry_Value;
         Internal_Array : access System.Address;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameter_Entry_Get_Integer_Array,
                     "parameter_entry_get_integer_array");

      Internal_Array : aliased System.Address;
      Internal_Array_Length : aliased Size_T;
      J : Size_T := 0;

      use type Int;
      use type Size_T;

   begin
      Check_Result
        (Parameter_Entry_Get_Integer_Array
           (E.V, Internal_Array'Access, Internal_Array_Length'Access));
      if Internal_Array_Length /= Values'Length then
         --  array mismatch
         raise Constraint_Error;
      end if;
      for I in Values'Range loop
         Values (I) := Integer_Type (Get_Int (Internal_Array, J));
         J := J + 1;
      end loop;
   end Entry_Get_Integer_Array;

   function Get_Long_Long_Array_Length (E : in Parameter_Entry)
                                       return Count_Type is

      function Parameter_Entry_Get_Long_Long_Array_Length
        (C_Entry : in Parameter_Entry_Value;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameter_Entry_Get_Long_Long_Array_Length,
                     "parameter_entry_get_long_long_array_length");

      Length : aliased Size_T;

   begin
      Check_Result
        (Parameter_Entry_Get_Long_Long_Array_Length (E.V, Length'Access));
      return Count_Type (Length);
   end Get_Long_Long_Array_Length;

   procedure Entry_Get_Long_Long_Array
     (E : in Parameter_Entry;
      Values : out Long_Long_Integer_Array_Type) is

      function Parameter_Entry_Get_Long_Long_Array
        (C_Entry : in Parameter_Entry_Value;
         Internal_Array : access System.Address;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameter_Entry_Get_Long_Long_Array,
                     "parameter_entry_get_long_long_array");

      Internal_Array : aliased System.Address;
      Internal_Array_Length : aliased Size_T;
      J : Size_T := 0;

      use type Long_Long;
      use type Size_T;

   begin
      Check_Result
        (Parameter_Entry_Get_Long_Long_Array
           (E.V, Internal_Array'Access, Internal_Array_Length'Access));
      if Internal_Array_Length /= Values'Length then
         --  array mismatch
         raise Constraint_Error;
      end if;
      for I in Values'Range loop
         Values (I) :=
           Long_Long_Integer_Type (Get_Long_Long (Internal_Array, J));
         J := J + 1;
      end loop;
   end Entry_Get_Long_Long_Array;

   function Get_Long_Float_Array_Length (E : in Parameter_Entry)
                                        return Count_Type is

      function Parameter_Entry_Get_Double_Float_Array_Length
        (C_Entry : in Parameter_Entry_Value;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameter_Entry_Get_Double_Float_Array_Length,
                     "parameter_entry_get_double_float_array_length");

      Length : aliased Size_T;

   begin
      Check_Result
        (Parameter_Entry_Get_Double_Float_Array_Length (E.V, Length'Access));
      return Count_Type (Length);
   end Get_Long_Float_Array_Length;

   procedure Entry_Get_Long_Float_Array
     (E : in Parameter_Entry;
      Values : out Long_Float_Array_Type) is

      function Parameter_Entry_Get_Double_Float_Array
        (C_Entry : in Parameter_Entry_Value;
         Internal_Array : access System.Address;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameter_Entry_Get_Double_Float_Array,
                     "parameter_entry_get_double_float_array");

      Internal_Array : aliased System.Address;
      Internal_Array_Length : aliased Size_T;
      J : Size_T := 0;

      use type Double;
      use type Size_T;

   begin
      Check_Result
        (Parameter_Entry_Get_Double_Float_Array
           (E.V, Internal_Array'Access, Internal_Array_Length'Access));
      if Internal_Array_Length /= Values'Length then
         --  array mismatch
         raise Constraint_Error;
      end if;
      for I in Values'Range loop
         Values (I) := Long_Float_Type (Get_Double (Internal_Array, J));
         J := J + 1;
      end loop;
   end Entry_Get_Long_Float_Array;

   function Get_String_Array_Length (E : in Parameter_Entry)
                                    return Count_Type is

      function Parameter_Entry_Get_String_Array_Length
        (C_Entry : in Parameter_Entry_Value;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameter_Entry_Get_String_Array_Length,
                     "parameter_entry_get_string_array_length");

      Length : aliased Size_T;

   begin
      Check_Result
        (Parameter_Entry_Get_String_Array_Length (E.V, Length'Access));
      return Count_Type (Length);
   end Get_String_Array_Length;

   function Get_String_In_Array (E : in Parameter_Entry;
                                Index : in Index_Type) return String is

      function Parameter_Entry_Get_String_In_Array
        (C_Entry : in Parameter_Entry_Value;
         Ind : in Size_T;
         C_Value : access Char_Ptr;
         Value_Length : access Size_T)
        return Int;
      pragma Import (C, Parameter_Entry_Get_String_In_Array,
                     "parameter_entry_get_string_in_array");

      Value : aliased Char_Ptr;
      Length : aliased Size_T;

      use type Char_Ptr;

   begin
      Check_Result
        (Parameter_Entry_Get_String_In_Array
           (E.V,
            Size_T (Index - 1),
            Value'Access, Length'Access));
      return Safe_From_C (Value, Length);
   end Get_String_In_Array;

   function Get_Binary_Array_Length (E : in Parameter_Entry)
                                    return Count_Type is

      function Parameter_Entry_Get_Binary_Array_Length
        (C_Entry : in Parameter_Entry_Value;
         Array_Length : access Size_T)
        return Int;
      pragma Import (C, Parameter_Entry_Get_Binary_Array_Length,
                     "parameter_entry_get_binary_array_length");

      Length : aliased Size_T;

   begin
      Check_Result
        (Parameter_Entry_Get_Binary_Array_Length (E.V, Length'Access));
      return Count_Type (Length);
   end Get_Binary_Array_Length;

   function Get_Binary_In_Array (E : in Parameter_Entry;
                                 Index : in Index_Type)
                                return Ada.Streams.Stream_Element_Array is

      function Parameter_Entry_Get_Binary_In_Array
        (C_Entry : in Parameter_Entry_Value;
         Ind : in Size_T;
         C_Value : access Void_Ptr;
         Value_Length : access Size_T)
        return Int;
      pragma Import (C, Parameter_Entry_Get_Binary_In_Array,
                     "parameter_entry_get_binary_in_array");

      Buffer_Ptr : aliased Void_Ptr;
      Length : aliased Size_T;

   begin
      Check_Result
        (Parameter_Entry_Get_Binary_In_Array
           (E.V,
            Size_T (Index - 1),
            Buffer_Ptr'Access, Length'Access));
      return Buffer_To_Stream_Element_Array (Buffer_Ptr, Length);
   end Get_Binary_In_Array;

   function Get_Nested_Parameters (E : in Parameter_Entry)
                                  return Parameters_Collection is

      function Parameter_Entry_Get_Nested (C_Entry : in Parameter_Entry_Value;
                                           Nested_Ptr : access Void_Ptr)
                                          return Int;
      pragma Import (C, Parameter_Entry_Get_Nested,
                     "parameter_entry_get_nested");

      Nested_Ptr : aliased Void_Ptr;

   begin
      Check_Result (Parameter_Entry_Get_Nested (E.V, Nested_Ptr'Access));
      return Params : Parameters_Collection (Nested => True) do
         Params.Delegated := Nested_Ptr;
      end return;
   end Get_Nested_Parameters;

end YAMI.Parameters;
