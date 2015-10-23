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

with YAMI.Parameters;
with YAMI.Raw_Buffer_Data_Sources;
with YAMI.Serializables;
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Streams;

with Ada.Text_IO;

procedure Parameters_Test is

   procedure Test_1 is
      Params : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;
      use type YAMI.Parameters.Count_Type;
   begin
      pragma Assert (Params.Length = 0);
      begin
         Params.Remove ("no such entry");
         pragma Assert (False);
      exception
         when E : YAMI.Logic_Error =>
            pragma Assert
              (Ada.Exceptions.Exception_Message (E) = "No such name.");
            null;
      end;
      declare
         It : YAMI.Parameters.Parameter_Cursor;
         E : YAMI.Parameters.Parameter_Entry;
         Found : Boolean;
         use type YAMI.Parameters.Parameter_Cursor;
      begin
         It := Params.First;
         pragma Assert (It = YAMI.Parameters.No_Parameter);

         Params.Find ("no such entry", E, Found);
         pragma Assert (not Found);
      end;
      declare
         Size : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         Buffer : aliased YAMI.Serializables.Serialization_Buffer (Size);
         Buffers : YAMI.Serializables.Serialization_Buffer_List (1 .. 1);
         Expected : Ada.Streams.Stream_Element_Array := (0, 0, 0, 0);

         Params_2 : YAMI.Parameters.Parameters_Collection :=
           YAMI.Parameters.Make_Parameters;

         use type Ada.Streams.Stream_Element_Count;
         use type Ada.Streams.Stream_Element_Array;
      begin
         pragma Assert (Size = 4); --  only number of entries
         Buffers (1) := Buffer'Unchecked_Access;
         Params.Serialize (Buffers);
         pragma Assert (Buffer.Buffer = Expected);
         Params_2.Deserialize (Buffers);
         pragma Assert (Params_2.Length = 0);

         Params_2.Clear;
         pragma Assert (Params_2.Length = 0);
      end;
   end Test_1;

   procedure Check_Serialization
     (Params : in YAMI.Parameters.Parameters_Collection;
      Expected : in Ada.Streams.Stream_Element_Array;
      Serialized_Size : in Ada.Streams.Stream_Element_Count) is

      Buffer :
        aliased YAMI.Serializables.Serialization_Buffer (Serialized_Size);
      Buffers : YAMI.Serializables.Serialization_Buffer_List (1 .. 1);
      use type Ada.Streams.Stream_Element_Count;
      use type Ada.Streams.Stream_Element_Array;

      Params2 : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;
   begin
      Buffers (1) := Buffer'Unchecked_Access;
      Params.Serialize (Buffers);
      pragma Assert (Buffer.Buffer = Expected);

      --  verify deserialization gives the same result
      Params2.Deserialize (Buffers);
      declare
         Serialized_Size2 : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         pragma Assert (Serialized_Size2 = Serialized_Size);

         Buffer2 : aliased
           YAMI.Serializables.Serialization_Buffer (Serialized_Size2);
         Buffers2 : YAMI.Serializables.Serialization_Buffer_List (1 .. 1);
      begin
         Buffers2 (1) := Buffer2'Unchecked_Access;
         Params2.Serialize (Buffers2);
         pragma Assert (Buffer2.Buffer = Expected);
      end;

      --  test for serialization into two buffers
      --  for different size relations
      declare
         First_Size : Ada.Streams.Stream_Element_Count := 4;
      begin
         while First_Size /= Serialized_Size loop
            declare
               Second_Size : Ada.Streams.Stream_Element_Count :=
                 Serialized_Size - First_Size;
               Buf1 : aliased
                 YAMI.Serializables.Serialization_Buffer (First_Size);
               Buf2 : aliased
                 YAMI.Serializables.Serialization_Buffer (Second_Size);
               Two_Buffers :
                 YAMI.Serializables.Serialization_Buffer_List (1 .. 2);
            begin
               Two_Buffers (1) := Buf1'Unchecked_Access;
               Two_Buffers (2) := Buf2'Unchecked_Access;
               Params.Serialize (Two_Buffers);
               pragma Assert
                 (Buf1.Buffer = Expected
                  (Expected'First .. Expected'First + First_Size - 1));
               pragma Assert (Buf2.Buffer = Expected
                              (Expected'First + First_Size .. Expected'Last));
            end;
            First_Size := First_Size + 4;
         end loop;
      end;

   end Check_Serialization;

   --  test for Boolean
   procedure Test_2 is
      Params : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;
      Param_Type : YAMI.Parameters.Parameter_Type;
      Value : Boolean;

      use type YAMI.Parameters.Parameter_Type;
      use type YAMI.Parameters.Count_Type;
   begin
      Params.Set_Boolean ("name", True);
      pragma Assert (Params.Length = 1);

      Param_Type := Params.Entry_Type ("name");
      pragma Assert (Param_Type = YAMI.Parameters.Boolean_Type);
      Value := Params.Get_Boolean ("name");
      pragma Assert (Value);

      Params.Set_Boolean ("name", False);
      pragma Assert (Params.Length = 1);
      Value := Params.Get_Boolean ("name");
      pragma Assert (not Value);

      declare
         I_Value : YAMI.Parameters.YAMI_Integer;
      begin
         I_Value := Params.Get_Integer ("name");
         pragma Assert (False);
      exception
         when E : YAMI.Logic_Error =>
            pragma Assert
              (Ada.Exceptions.Exception_Message (E) = "Bad type.");
            null;
      end;

      declare
         It : YAMI.Parameters.Parameter_Cursor;
         E : YAMI.Parameters.Parameter_Entry;
         use type YAMI.Parameters.Parameter_Cursor;
      begin
         It := Params.First;
         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.Boolean_Type);

         Value := YAMI.Parameters.Get_Boolean (E);
         pragma Assert (not Value);

         declare
            I_Value : YAMI.Parameters.YAMI_Integer;
         begin
            I_Value := YAMI.Parameters.Get_Integer (E);
            pragma Assert (False);
         exception
            when E : YAMI.Logic_Error =>
               pragma Assert
                 (Ada.Exceptions.Exception_Message (E) = "Bad type.");
               null;
         end;

         YAMI.Parameters.Next (It);
         pragma Assert (not YAMI.Parameters.Has_Element (It));
      end;

      declare
         Size : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         use type Ada.Streams.Stream_Element_Count;
      begin
         pragma Assert (Size =
                        4   --  number of entries
                        + 4 --  length of 1st entry name
                        + 4 --  "name"
                        + 4 --  type
                        + 4 --  value
                       );
         null;
      end;

      declare
         type My_Index_Type is new Positive;
         type My_Bool_Array is array (My_Index_Type range <>) of Boolean;
         My_Bools : My_Bool_Array := (True, False, True);
         procedure My_Set_Boolean_Array is
            new YAMI.Parameters.Set_Boolean_Array
           (Index_Type => My_Index_Type,
            Boolean_Array_Type => My_Bool_Array);
         Array_Length : YAMI.Parameters.Count_Type;
      begin
         My_Set_Boolean_Array (Params, "nameA", My_Bools);
         pragma Assert (Params.Length = 2);
         pragma Assert
           (Params.Entry_Type ("nameA") = YAMI.Parameters.Boolean_Array_Type);

         Array_Length := Params.Get_Boolean_Array_Length ("nameA");
         pragma Assert (Array_Length = 3);
         declare
            My_Bools_2 : My_Bool_Array (1 .. My_Index_Type (Array_Length));
            procedure My_Get_Boolean_Array is
               new YAMI.Parameters.Get_Boolean_Array
              (Index_Type => My_Index_Type,
               Boolean_Array_Type => My_Bool_Array);
         begin
            My_Get_Boolean_Array (Params, "nameA", My_Bools_2);
            pragma Assert (My_Bools_2 = (True, False, True));
            My_Bools_2 (3) := False;
            My_Set_Boolean_Array (Params, "nameA", My_Bools_2);
         end;
      end;

      declare
         It : YAMI.Parameters.Parameter_Cursor;
         E : YAMI.Parameters.Parameter_Entry;
         use type YAMI.Parameters.Parameter_Cursor;
         Array_Length : YAMI.Parameters.Count_Type;
      begin
         It := Params.First;
         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.Boolean_Type);
         pragma Assert (YAMI.Parameters.Entry_Name (E) = "name");

         YAMI.Parameters.Next (It);
         pragma Assert (YAMI.Parameters.Has_Element (It));

         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.Boolean_Array_Type);
         pragma Assert (YAMI.Parameters.Entry_Name (E) = "nameA");

         Array_Length := YAMI.Parameters.Get_Boolean_Array_Length (E);
         pragma Assert (Array_Length = 3);

         declare
            type My_Index_Type is new Positive;
            type My_Bool_Array is array (My_Index_Type range <>) of Boolean;
            My_Bools_3 : My_Bool_Array (1 .. My_Index_Type (Array_Length));
            procedure My_Get_Boolean_Array is
               new YAMI.Parameters.Entry_Get_Boolean_Array
              (Index_Type => My_Index_Type,
               Boolean_Array_Type => My_Bool_Array);
         begin
            My_Get_Boolean_Array (E, My_Bools_3);
            pragma Assert (My_Bools_3 = (True, False, False));
         end;

         YAMI.Parameters.Next (It);
         pragma Assert (not YAMI.Parameters.Has_Element (It));
      end;

      declare
         Size : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         use type Ada.Streams.Stream_Element_Count;

         Expected : Ada.Streams.Stream_Element_Array :=
           (
            2, 0, 0, 0,         --  num of entries
            4, 0, 0, 0,         --  length of "name"
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            1, 0, 0, 0,         --  type code for boolean
            0, 0, 0, 0,         --  false
            5, 0, 0, 0,         --  length of "nameA"
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('A')), 0, 0, 0,
            7, 0, 0, 0,         --  type code for boolean array
            3, 0, 0, 0,         --  length of array
            1, 0, 0, 0        --  packed array (100 -> 0x1)
           );
      begin
         pragma Assert (Size =
                        4   --  number of entries
                        + 4 --  length of 1st entry name
                        + 4 --  "name"
                        + 4 --  type
                        + 4 --  value
                        + 4 --  length of 2nd entry name
                        + 8 --  "nameA"
                        + 4 --  type
                        + 4 --  length of array
                        + 4 --  values for bool array
                       );

         Check_Serialization (Params, Expected, Size);
      end;

      declare
         E : YAMI.Parameters.Parameter_Entry;
         It : YAMI.Parameters.Parameter_Cursor;
         Found : Boolean;
      begin
         Params.Find ("name", E, Found);
         pragma Assert (Found);
         pragma Assert (YAMI.Parameters.Entry_Type (E) =
                          YAMI.Parameters.Boolean_Type);
         pragma Assert (YAMI.Parameters.Entry_Name (E) = "name");

         Params.Find ("nameA", E, Found);
         pragma Assert (Found);
         pragma Assert (YAMI.Parameters.Entry_Type (E) =
                          YAMI.Parameters.Boolean_Array_Type);
         pragma Assert (YAMI.Parameters.Entry_Name (E) = "nameA");

         Params.Find ("no such name", E, Found);
         pragma Assert (not Found);

         Params.Remove ("name");

         pragma Assert (Params.Length = 1);

         It := Params.First;
         E := YAMI.Parameters.Get_Entry (It);
         pragma Assert (YAMI.Parameters.Entry_Type (E) =
                          YAMI.Parameters.Boolean_Array_Type);
         pragma Assert (YAMI.Parameters.Entry_Name (E) = "nameA");

         YAMI.Parameters.Next (It);
         pragma Assert (not YAMI.Parameters.Has_Element (It));
      end;

      Params.Clear;
      pragma Assert (Params.Length = 0);
   end Test_2;

   --  test for Integer
   procedure Test_3 is
      Params : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;
      Param_Type : YAMI.Parameters.Parameter_Type;
      Value : YAMI.Parameters.YAMI_Integer;

      use type YAMI.Parameters.Parameter_Type;
      use type YAMI.Parameters.Count_Type;
      use type YAMI.Parameters.YAMI_Integer;
   begin
      Params.Set_Integer ("name", 123);
      pragma Assert (Params.Length = 1);

      Param_Type := Params.Entry_Type ("name");
      pragma Assert (Param_Type = YAMI.Parameters.Integer_Type);
      Value := Params.Get_Integer ("name");
      pragma Assert (Value = 123);

      declare
         Size : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         use type Ada.Streams.Stream_Element_Count;
      begin
         pragma Assert (Size =
                        4   --  number of entries
                        + 4 --  length of 1st entry name
                        + 4 --  "name"
                        + 4 --  type
                        + 4 --  value
                       );
         null;
      end;

      declare
         type My_Int_Type is new Integer;
         type My_Index_Type is new Positive;
         type My_Int_Array is array (My_Index_Type range <>) of My_Int_Type;
         My_Ints : My_Int_Array := (10, 20, 30);
         procedure My_Set_Integer_Array is
            new YAMI.Parameters.Set_Integer_Array
           (Integer_Type => My_Int_Type,
            Index_Type => My_Index_Type,
            Integer_Array_Type => My_Int_Array);
         Array_Length : YAMI.Parameters.Count_Type;
      begin
         My_Set_Integer_Array (Params, "nameA", My_Ints);
         pragma Assert (Params.Length = 2);
         pragma Assert
           (Params.Entry_Type ("nameA") = YAMI.Parameters.Integer_Array_Type);

         Array_Length := Params.Get_Integer_Array_Length ("nameA");
         pragma Assert (Array_Length = 3);
         declare
            My_Ints_2 : My_Int_Array (1 .. My_Index_Type (Array_Length));
            procedure My_Get_Integer_Array is
               new YAMI.Parameters.Get_Integer_Array
              (Integer_Type => My_Int_Type,
               Index_Type => My_Index_Type,
               Integer_Array_Type => My_Int_Array);
         begin
            My_Get_Integer_Array (Params, "nameA", My_Ints_2);
            pragma Assert (My_Ints_2 = (10, 20, 30));
            My_Ints_2 (3) := 31;
            My_Set_Integer_Array (Params, "nameA", My_Ints_2);
         end;
      end;

      declare
         It : YAMI.Parameters.Parameter_Cursor;
         E : YAMI.Parameters.Parameter_Entry;
         use type YAMI.Parameters.Parameter_Cursor;
         Array_Length : YAMI.Parameters.Count_Type;
      begin
         It := Params.First;
         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.Integer_Type);
         pragma Assert (YAMI.Parameters.Entry_Name (E) = "name");
         Value := YAMI.Parameters.Get_Integer (E);
         pragma Assert (Value = 123);

         YAMI.Parameters.Next (It);
         pragma Assert (YAMI.Parameters.Has_Element (It));

         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.Integer_Array_Type);
         pragma Assert (YAMI.Parameters.Entry_Name (E) = "nameA");

         Array_Length := YAMI.Parameters.Get_Integer_Array_Length (E);
         pragma Assert (Array_Length = 3);

         declare
            type My_Int_Type is new Integer;
            type My_Index_Type is new Positive;
            type My_Int_Array is array (My_Index_Type range <>) of
              My_Int_Type;
            My_Ints_3 : My_Int_Array (1 .. My_Index_Type (Array_Length));
            procedure My_Get_Integer_Array is
               new YAMI.Parameters.Entry_Get_Integer_Array
              (Integer_Type => My_Int_Type,
               Index_Type => My_Index_Type,
               Integer_Array_Type => My_Int_Array);
         begin
            My_Get_Integer_Array (E, My_Ints_3);
            pragma Assert (My_Ints_3 = (10, 20, 31));
         end;

         YAMI.Parameters.Next (It);
         pragma Assert (not YAMI.Parameters.Has_Element (It));
      end;

      declare
         Size : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         use type Ada.Streams.Stream_Element_Count;

         Expected : Ada.Streams.Stream_Element_Array :=
           (
            2, 0, 0, 0,         --  num of entries
            4, 0, 0, 0,         --  length of "name"
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            2, 0, 0, 0,         --  type code for integer
            123, 0, 0, 0,       --  123
            5, 0, 0, 0,         --  length of "nameA"
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('A')), 0, 0, 0,
            8, 0, 0, 0,         --  type code for integer array
            3, 0, 0, 0,         --  length of array
            10, 0, 0, 0,        --  array values
            20, 0, 0, 0,        --  array values
            31, 0, 0, 0         --  array values
           );
      begin
         pragma Assert (Size =
                        4   --  number of entries
                        + 4 --  length of 1st entry name
                        + 4 --  "name"
                        + 4 --  type
                        + 4 --  value
                        + 4 --  length of 2nd entry name
                        + 8 --  "nameA"
                        + 4 --  type
                        + 4 --  length of array
                        + 12 --  values for int array
                       );

         Check_Serialization (Params, Expected, Size);
      end;

      declare
         E : YAMI.Parameters.Parameter_Entry;
         It : YAMI.Parameters.Parameter_Cursor;
      begin
         It := Params.First;
         E := YAMI.Parameters.Get_Entry (It);
         pragma Assert (YAMI.Parameters.Entry_Name (E) = "name");

         Params.Remove ("name");
         pragma Assert (Params.Length = 1);
      end;
   end Test_3;

   --  test for Long_Long
   procedure Test_4 is
      Params : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;
      Param_Type : YAMI.Parameters.Parameter_Type;
      Value : YAMI.Parameters.YAMI_Long_Long_Integer;

      use type YAMI.Parameters.Parameter_Type;
      use type YAMI.Parameters.YAMI_Long_Long_Integer;
      use type YAMI.Parameters.Count_Type;
   begin
      Params.Set_Long_Long ("name", 1234567890);
      pragma Assert (Params.Length = 1);

      Param_Type := Params.Entry_Type ("name");
      pragma Assert (Param_Type = YAMI.Parameters.Long_Long_Integer_Type);
      Value := Params.Get_Long_Long ("name");
      pragma Assert (Value = 1234567890);

      declare
         Size : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         use type Ada.Streams.Stream_Element_Count;
      begin
         pragma Assert (Size =
                        4   --  number of entries
                        + 4 --  length of 1st entry name
                        + 4 --  "name"
                        + 4 --  type
                        + 8 --  value
                       );
         null;
      end;

      declare
         type My_Long_Type is new Long_Long_Integer;
         type My_Index_Type is new Positive;
         type My_Long_Array is array (My_Index_Type range <>) of My_Long_Type;
         My_Longs : My_Long_Array := (100, 200, 300);
         procedure My_Set_Long_Array is
            new YAMI.Parameters.Set_Long_Long_Array
           (Long_Long_Integer_Type => My_Long_Type,
            Index_Type => My_Index_Type,
            Long_Long_Integer_Array_Type => My_Long_Array);
         Array_Length : YAMI.Parameters.Count_Type;
      begin
         My_Set_Long_Array (Params, "nameA", My_Longs);
         pragma Assert (Params.Length = 2);
         pragma Assert (Params.Entry_Type ("nameA") =
                          YAMI.Parameters.Long_Long_Integer_Array_Type);

         Array_Length := Params.Get_Long_Long_Array_Length ("nameA");
         pragma Assert (Array_Length = 3);
         declare
            My_Longs_2 : My_Long_Array (1 .. My_Index_Type (Array_Length));
            procedure My_Get_Long_Array is
               new YAMI.Parameters.Get_Long_Long_Array
              (Long_Long_Integer_Type => My_Long_Type,
               Index_Type => My_Index_Type,
               Long_Long_Integer_Array_Type => My_Long_Array);
         begin
            My_Get_Long_Array (Params, "nameA", My_Longs_2);
            pragma Assert (My_Longs_2 = (100, 200, 300));
            My_Longs_2 (3) := 310;
            My_Set_Long_Array (Params, "nameA", My_Longs_2);
         end;
      end;

      declare
         It : YAMI.Parameters.Parameter_Cursor;
         E : YAMI.Parameters.Parameter_Entry;
         use type YAMI.Parameters.Parameter_Cursor;
         Array_Length : YAMI.Parameters.Count_Type;
      begin
         It := Params.First;
         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.Long_Long_Integer_Type);
         pragma Assert (YAMI.Parameters.Entry_Name (E) = "name");
         Value := YAMI.Parameters.Get_Long_Long (E);
         pragma Assert (Value = 1234567890);

         YAMI.Parameters.Next (It);
         pragma Assert (YAMI.Parameters.Has_Element (It));

         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert
           (Param_Type = YAMI.Parameters.Long_Long_Integer_Array_Type);
         pragma Assert (YAMI.Parameters.Entry_Name (E) = "nameA");

         Array_Length := YAMI.Parameters.Get_Long_Long_Array_Length (E);
         pragma Assert (Array_Length = 3);

         declare
            type My_Long_Type is new Long_Long_Integer;
            type My_Index_Type is new Positive;
            type My_Long_Array is array (My_Index_Type range <>) of
              My_Long_Type;
            My_Longs_3 : My_Long_Array (1 .. My_Index_Type (Array_Length));
            procedure My_Get_Long_Array is
               new YAMI.Parameters.Entry_Get_Long_Long_Array
              (Long_Long_Integer_Type => My_Long_Type,
               Index_Type => My_Index_Type,
               Long_Long_Integer_Array_Type => My_Long_Array);
         begin
            My_Get_Long_Array (E, My_Longs_3);
            pragma Assert (My_Longs_3 = (100, 200, 310));
         end;

         YAMI.Parameters.Next (It);
         pragma Assert (not YAMI.Parameters.Has_Element (It));
      end;

      declare
         Size : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         use type Ada.Streams.Stream_Element_Count;

         Expected : Ada.Streams.Stream_Element_Array :=
           (
            2, 0, 0, 0,         --  num of entries
            4, 0, 0, 0,         --  length of "name"
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            3, 0, 0, 0,         --  type code for long long
            210, 2, 150, 73,    --  1234567890
            0, 0, 0, 0,
            5, 0, 0, 0,         --  length of "nameA"
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('A')), 0, 0, 0,
            9, 0, 0, 0,         --  type code for long long array
            3, 0, 0, 0,         --  length of array
            100, 0, 0, 0,       --  array values
            0, 0, 0, 0,
            200, 0, 0, 0,       --  array values
            0, 0, 0, 0,
            54, 1, 0, 0,        --  array values
            0, 0, 0, 0
           );
      begin
         pragma Assert (Size =
                        4   --  number of entries
                        + 4 --  length of 1st entry name
                        + 4 --  "name"
                        + 4 --  type
                        + 8 --  value
                        + 4 --  length of 2nd entry name
                        + 8 --  "nameA"
                        + 4 --  type
                        + 4 --  length of array
                        + 24 --  values for long long array
                       );

         Check_Serialization (Params, Expected, Size);
      end;
   end Test_4;

   --  test for Long_Float
   procedure Test_5 is
      Params : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;
      Param_Type : YAMI.Parameters.Parameter_Type;
      Value : YAMI.Parameters.YAMI_Long_Float;

      use type YAMI.Parameters.Parameter_Type;
      use type YAMI.Parameters.YAMI_Long_Float;
      use type YAMI.Parameters.Count_Type;
   begin
      Params.Set_Long_Float ("name", 3.125);
      pragma Assert (Params.Length = 1);

      Param_Type := Params.Entry_Type ("name");
      pragma Assert (Param_Type = YAMI.Parameters.Long_Float_Type);
      Value := Params.Get_Long_Float ("name");
      pragma Assert (Value = 3.125);

      declare
         Size : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         use type Ada.Streams.Stream_Element_Count;
      begin
         pragma Assert (Size =
                        4   --  number of entries
                        + 4 --  length of 1st entry name
                        + 4 --  "name"
                        + 4 --  type
                        + 8 --  value
                       );
         null;
      end;

      declare
         type My_Float_Type is new Long_Float;
         type My_Index_Type is new Positive;
         type My_Float_Array is array (My_Index_Type range <>) of
           My_Float_Type;
         My_Floats : My_Float_Array := (1.875, 2.875, 3.875);
         procedure My_Set_Float_Array is
            new YAMI.Parameters.Set_Long_Float_Array
           (Long_Float_Type => My_Float_Type,
            Index_Type => My_Index_Type,
            Long_Float_Array_Type => My_Float_Array);
         Array_Length : YAMI.Parameters.Count_Type;
      begin
         My_Set_Float_Array (Params, "nameA", My_Floats);
         pragma Assert (Params.Length = 2);
         pragma Assert (Params.Entry_Type ("nameA") =
                          YAMI.Parameters.Long_Float_Array_Type);

         Array_Length := Params.Get_Long_Float_Array_Length ("nameA");
         pragma Assert (Array_Length = 3);
         declare
            My_Floats_2 : My_Float_Array (1 .. My_Index_Type (Array_Length));
            procedure My_Get_Float_Array is
               new YAMI.Parameters.Get_Long_Float_Array
              (Long_Float_Type => My_Float_Type,
               Index_Type => My_Index_Type,
               Long_Float_Array_Type => My_Float_Array);
         begin
            My_Get_Float_Array (Params, "nameA", My_Floats_2);
            pragma Assert (My_Floats_2 = (1.875, 2.875, 3.875));
            My_Floats_2 (3) := 3.625;
            My_Set_Float_Array (Params, "nameA", My_Floats_2);
         end;
      end;

      declare
         It : YAMI.Parameters.Parameter_Cursor;
         E : YAMI.Parameters.Parameter_Entry;
         use type YAMI.Parameters.Parameter_Cursor;
         Array_Length : YAMI.Parameters.Count_Type;
      begin
         It := Params.First;
         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.Long_Float_Type);
         pragma Assert (YAMI.Parameters.Entry_Name (E) = "name");
         Value := YAMI.Parameters.Get_Long_Float (E);
         pragma Assert (Value = 3.125);

         YAMI.Parameters.Next (It);
         pragma Assert (YAMI.Parameters.Has_Element (It));

         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.Long_Float_Array_Type);
         pragma Assert (YAMI.Parameters.Entry_Name (E) = "nameA");

         Array_Length := YAMI.Parameters.Get_Long_Float_Array_Length (E);
         pragma Assert (Array_Length = 3);

         declare
            type My_Float_Type is new Long_Float;
            type My_Index_Type is new Positive;
            type My_Float_Array is array (My_Index_Type range <>) of
              My_Float_Type;
            My_Floats_3 : My_Float_Array (1 .. My_Index_Type (Array_Length));
            procedure My_Get_Float_Array is
               new YAMI.Parameters.Entry_Get_Long_Float_Array
              (Long_Float_Type => My_Float_Type,
               Index_Type => My_Index_Type,
               Long_Float_Array_Type => My_Float_Array);
         begin
            My_Get_Float_Array (E, My_Floats_3);
            pragma Assert (My_Floats_3 = (1.875, 2.875, 3.625));
         end;

         YAMI.Parameters.Next (It);
         pragma Assert (not YAMI.Parameters.Has_Element (It));
      end;

      declare
         Size : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         use type Ada.Streams.Stream_Element_Count;

         Expected : Ada.Streams.Stream_Element_Array :=
           (
            2, 0, 0, 0,         --  num of entries
            4, 0, 0, 0,         --  length of "name"
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            4, 0, 0, 0,         --  type code for long float
            0, 0, 0, 0,
            0, 0, 9, 64,        --  3.125
            5, 0, 0, 0,         --  length of "nameA"
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('A')), 0, 0, 0,
            10, 0, 0, 0,        --  type code for long float array
            3, 0, 0, 0,         --  length of array
            0, 0, 0, 0,         --  array values
            0, 0, 254, 63,
            0, 0, 0, 0,         --  array values
            0, 0, 7, 64,
            0, 0, 0, 0,         --  array values
            0, 0, 13, 64
           );
      begin
         pragma Assert (Size =
                        4   --  number of entries
                        + 4 --  length of 1st entry name
                        + 4 --  "name"
                        + 4 --  type
                        + 8 --  value
                        + 4 --  length of 2nd entry name
                        + 8 --  "nameA"
                        + 4 --  type
                        + 4 --  length of array
                        + 24 --  values for long float array
                       );

         Check_Serialization (Params, Expected, Size);
      end;
   end Test_5;

   --  test for String
   procedure Test_6 is
      Params : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;
      Param_Type : YAMI.Parameters.Parameter_Type;

      Source : constant String :=
        "Kolorowe kredki w pudeleczku nosze," & Ada.Characters.Latin_1.LF &
        "kolorowe kredki, bardzo lubie je!";

      use type YAMI.Parameters.Parameter_Type;
      use type YAMI.Parameters.Count_Type;
   begin
      Params.Set_String ("some rather longer name", Source);
      pragma Assert (Params.Length = 1);

      Param_Type := Params.Entry_Type ("some rather longer name");
      pragma Assert (Param_Type = YAMI.Parameters.String_Type);

      declare
         Value : String := Params.Get_String ("some rather longer name");
      begin
         pragma Assert (Value = Source);
         null;
      end;

      declare
         Size : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         use type Ada.Streams.Stream_Element_Count;
      begin
         pragma Assert (Size =
                        4    --  number of entries
                        + 4  --  length of 1st entry name
                        + 24 --  "some rather longer name"
                        + 4  --  type
                        + 4  --  length
                        + 72 --  value "Kolorowe kredki..."
                       );
         null;
      end;

      Params.Create_String_Array ("nameA", 4);

      Param_Type := Params.Entry_Type ("nameA");
      pragma Assert (Param_Type = YAMI.Parameters.String_Array_Type);

      declare
         Array_Length : YAMI.Parameters.Count_Type :=
           Params.Get_String_Array_Length ("nameA");
         Value_1 : String := Params.Get_String_In_Array ("nameA", 1);
         Value_4 : String := Params.Get_String_In_Array ("nameA", 4);
      begin
         pragma Assert (Array_Length = 4);
         pragma Assert (Value_1'Length = 0);
         pragma Assert (Value_4'Length = 0);
         declare
            Value : String := Params.Get_String_In_Array ("nameA", 5);
         begin
            pragma Assert (False);
            null;
         end;
      exception
         when E : YAMI.Logic_Error =>
            pragma Assert
              (Ada.Exceptions.Exception_Message (E) = "No such index.");
            null;
      end;

      Params.Set_String_In_Array ("nameA", 1, "Kazio");
      Params.Set_String_In_Array ("nameA", 2, "Krzysio");
      Params.Set_String_In_Array ("nameA", 3, "Rysio");
      Params.Set_String_In_Array ("nameA", 4, "Zbysio");

      declare
         Value_1 : String := Params.Get_String_In_Array ("nameA", 1);
         Value_4 : String := Params.Get_String_In_Array ("nameA", 4);
      begin
         pragma Assert (Value_1 = "Kazio");
         pragma Assert (Value_4 = "Zbysio");
         null;
      end;

      declare
         It : YAMI.Parameters.Parameter_Cursor;
         E : YAMI.Parameters.Parameter_Entry;
         use type YAMI.Parameters.Parameter_Cursor;
         Array_Length : YAMI.Parameters.Count_Type;
      begin
         It := Params.First;
         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.String_Type);
         pragma Assert
           (YAMI.Parameters.Entry_Name (E) = "some rather longer name");

         declare
            Value : String := YAMI.Parameters.Get_String (E);
         begin
            pragma Assert (Value = Source);
            null;
         end;

         YAMI.Parameters.Next (It);
         pragma Assert (YAMI.Parameters.Has_Element (It));

         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.String_Array_Type);
         pragma Assert (YAMI.Parameters.Entry_Name (E) = "nameA");

         declare
            Value : String := YAMI.Parameters.Get_String_In_Array (E, 2);
         begin
            Array_Length := YAMI.Parameters.Get_String_Array_Length (E);
            pragma Assert (Array_Length = 4);
            pragma Assert (Value = "Krzysio");
            null;
         end;

         YAMI.Parameters.Next (It);
         pragma Assert (not YAMI.Parameters.Has_Element (It));
      end;

      declare
         Size : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         use type Ada.Streams.Stream_Element_Count;

         Expected : Ada.Streams.Stream_Element_Array :=
           (
            2, 0, 0, 0,         --  num of entries
            23, 0, 0, 0,         --  length of "some rather longer name"
            Ada.Streams.Stream_Element (Character'Pos ('s')),
            Ada.Streams.Stream_Element (Character'Pos ('o')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('t')),
            Ada.Streams.Stream_Element (Character'Pos ('h')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('l')),
            Ada.Streams.Stream_Element (Character'Pos ('o')),
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('g')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')), 0,

            5, 0, 0, 0,         --  type code for string
            69, 0, 0, 0,        --  length

            Ada.Streams.Stream_Element (Character'Pos ('K')),
            Ada.Streams.Stream_Element (Character'Pos ('o')),
            Ada.Streams.Stream_Element (Character'Pos ('l')),
            Ada.Streams.Stream_Element (Character'Pos ('o')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('o')),
            Ada.Streams.Stream_Element (Character'Pos ('w')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('k')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('d')),
            Ada.Streams.Stream_Element (Character'Pos ('k')),
            Ada.Streams.Stream_Element (Character'Pos ('i')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('w')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('p')),
            Ada.Streams.Stream_Element (Character'Pos ('u')),
            Ada.Streams.Stream_Element (Character'Pos ('d')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('l')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('c')),
            Ada.Streams.Stream_Element (Character'Pos ('z')),
            Ada.Streams.Stream_Element (Character'Pos ('k')),
            Ada.Streams.Stream_Element (Character'Pos ('u')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('o')),
            Ada.Streams.Stream_Element (Character'Pos ('s')),
            Ada.Streams.Stream_Element (Character'Pos ('z')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos (',')),
            Ada.Streams.Stream_Element
              (Character'Pos (Ada.Characters.Latin_1.LF)),
            Ada.Streams.Stream_Element (Character'Pos ('k')),
            Ada.Streams.Stream_Element (Character'Pos ('o')),
            Ada.Streams.Stream_Element (Character'Pos ('l')),
            Ada.Streams.Stream_Element (Character'Pos ('o')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('o')),
            Ada.Streams.Stream_Element (Character'Pos ('w')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('k')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('d')),
            Ada.Streams.Stream_Element (Character'Pos ('k')),
            Ada.Streams.Stream_Element (Character'Pos ('i')),
            Ada.Streams.Stream_Element (Character'Pos (',')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('b')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('d')),
            Ada.Streams.Stream_Element (Character'Pos ('z')),
            Ada.Streams.Stream_Element (Character'Pos ('o')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('l')),
            Ada.Streams.Stream_Element (Character'Pos ('u')),
            Ada.Streams.Stream_Element (Character'Pos ('b')),
            Ada.Streams.Stream_Element (Character'Pos ('i')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('j')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('!')), 0, 0, 0,

            5, 0, 0, 0,         --  length of "nameA"
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('A')), 0, 0, 0,
            11, 0, 0, 0,        --  type code for string array
            4, 0, 0, 0,         --  length of array

            5, 0, 0, 0,         --  length of first entry
            Ada.Streams.Stream_Element (Character'Pos ('K')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('z')),
            Ada.Streams.Stream_Element (Character'Pos ('i')),
            Ada.Streams.Stream_Element (Character'Pos ('o')), 0, 0, 0,

            7, 0, 0, 0,         --  length of second entry
            Ada.Streams.Stream_Element (Character'Pos ('K')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('z')),
            Ada.Streams.Stream_Element (Character'Pos ('y')),
            Ada.Streams.Stream_Element (Character'Pos ('s')),
            Ada.Streams.Stream_Element (Character'Pos ('i')),
            Ada.Streams.Stream_Element (Character'Pos ('o')), 0,

            5, 0, 0, 0,         --  length of third entry
            Ada.Streams.Stream_Element (Character'Pos ('R')),
            Ada.Streams.Stream_Element (Character'Pos ('y')),
            Ada.Streams.Stream_Element (Character'Pos ('s')),
            Ada.Streams.Stream_Element (Character'Pos ('i')),
            Ada.Streams.Stream_Element (Character'Pos ('o')), 0, 0, 0,

            6, 0, 0, 0,         --  length of fourth entry
            Ada.Streams.Stream_Element (Character'Pos ('Z')),
            Ada.Streams.Stream_Element (Character'Pos ('b')),
            Ada.Streams.Stream_Element (Character'Pos ('y')),
            Ada.Streams.Stream_Element (Character'Pos ('s')),
            Ada.Streams.Stream_Element (Character'Pos ('i')),
            Ada.Streams.Stream_Element (Character'Pos ('o')), 0, 0
           );
      begin
         pragma Assert (Size =
                        4    --  number of entries
                        + 4  --  length of 1st entry name
                        + 24 --  "some rather longer name"
                        + 4  --  type
                        + 4  --  length
                        + 72 --  "Kolorowe kredki..."
                        + 4  --  length of 2nd entry name
                        + 8  --  "nameA"
                        + 4  --  type
                        + 4  --  length of array
                        + 4  --  length of 1st array element
                        + 8  --  "Kazio"
                        + 4  --  length of 2nd array element
                        + 8  --  "Krzysio"
                        + 4  --  length of 3rd array element
                        + 8  --  "Rysio"
                        + 4  --  length of 4th array element
                        + 8  --  "Zbysio"
                       );

         Check_Serialization (Params, Expected, Size);
      end;
   end Test_6;

   --  test for binary
   procedure Test_7 is
      Params : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;
      Param_Type : YAMI.Parameters.Parameter_Type;

      Source : constant Ada.Streams.Stream_Element_Array :=
        (
         Ada.Streams.Stream_Element (Character'Pos ('a')),
         Ada.Streams.Stream_Element (Character'Pos ('b')),
         Ada.Streams.Stream_Element (Character'Pos ('c')),
         0,
         Ada.Streams.Stream_Element (Character'Pos ('d'))
        );

      Source_1 : constant Ada.Streams.Stream_Element_Array :=
        (
         Ada.Streams.Stream_Element (Character'Pos ('a')),
         Ada.Streams.Stream_Element (Character'Pos ('b')),
         Ada.Streams.Stream_Element (Character'Pos ('c'))
        );
      Source_2 : constant Ada.Streams.Stream_Element_Array :=
        (
         Ada.Streams.Stream_Element (Character'Pos ('k')),
         Ada.Streams.Stream_Element (Character'Pos ('l'))
        );
      Source_3 : constant Ada.Streams.Stream_Element_Array :=
        (
         Ada.Streams.Stream_Element (Character'Pos ('x')),
         Ada.Streams.Stream_Element (Character'Pos ('y')),
         Ada.Streams.Stream_Element (Character'Pos ('z'))
        );

      use type Ada.Streams.Stream_Element_Array;
      use type YAMI.Parameters.Parameter_Type;
      use type YAMI.Parameters.Count_Type;
   begin
      Params.Set_Binary ("some rather longer name", Source);
      pragma Assert (Params.Length = 1);

      Param_Type := Params.Entry_Type ("some rather longer name");
      pragma Assert (Param_Type = YAMI.Parameters.Binary_Type);

      declare
         Value : Ada.Streams.Stream_Element_Array :=
           Params.Get_Binary ("some rather longer name");
      begin
         pragma Assert (Value = Source);
         null;
      end;

      declare
         Size : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         use type Ada.Streams.Stream_Element_Count;
      begin
         pragma Assert (Size =
                        4    --  number of entries
                        + 4  --  length of 1st entry name
                        + 24 --  "some rather longer name"
                        + 4  --  type
                        + 4  --  length
                        + 8  --  source value
                       );
         null;
      end;

      Params.Create_Binary_Array ("nameA", 3);

      Param_Type := Params.Entry_Type ("nameA");
      pragma Assert (Param_Type = YAMI.Parameters.Binary_Array_Type);

      declare
         Array_Length : YAMI.Parameters.Count_Type :=
           Params.Get_Binary_Array_Length ("nameA");
         Value_1 : Ada.Streams.Stream_Element_Array :=
           Params.Get_Binary_In_Array ("nameA", 1);
         Value_3 : Ada.Streams.Stream_Element_Array :=
           Params.Get_Binary_In_Array ("nameA", 3);
      begin
         pragma Assert (Array_Length = 3);
         pragma Assert (Value_1'Length = 0);
         pragma Assert (Value_3'Length = 0);
         declare
            Value : Ada.Streams.Stream_Element_Array :=
              Params.Get_Binary_In_Array ("nameA", 4);
         begin
            pragma Assert (False);
            null;
         end;
      exception
         when E : YAMI.Logic_Error =>
            pragma Assert
              (Ada.Exceptions.Exception_Message (E) = "No such index.");
            null;
      end;

      Params.Set_Binary_In_Array ("nameA", 1, Source_1);
      Params.Set_Binary_In_Array ("nameA", 2, Source_2);
      Params.Set_Binary_In_Array ("nameA", 3, Source_3);

      declare
         Value_1 : Ada.Streams.Stream_Element_Array :=
           Params.Get_Binary_In_Array ("nameA", 1);
         Value_2 : Ada.Streams.Stream_Element_Array :=
           Params.Get_Binary_In_Array ("nameA", 2);
      begin
         pragma Assert (Value_1 = Source_1);
         pragma Assert (Value_2 = Source_2);
         null;
      end;

      declare
         It : YAMI.Parameters.Parameter_Cursor;
         E : YAMI.Parameters.Parameter_Entry;
         use type YAMI.Parameters.Parameter_Cursor;
         Array_Length : YAMI.Parameters.Count_Type;
      begin
         It := Params.First;
         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.Binary_Type);
         pragma Assert
           (YAMI.Parameters.Entry_Name (E) = "some rather longer name");

         declare
            Value : Ada.Streams.Stream_Element_Array :=
              YAMI.Parameters.Get_Binary (E);
         begin
            pragma Assert (Value = Source);
            null;
         end;

         YAMI.Parameters.Next (It);
         pragma Assert (YAMI.Parameters.Has_Element (It));

         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.Binary_Array_Type);
         pragma Assert (YAMI.Parameters.Entry_Name (E) = "nameA");

         declare
            Value_2 : Ada.Streams.Stream_Element_Array :=
              YAMI.Parameters.Get_Binary_In_Array (E, 2);
         begin
            Array_Length := YAMI.Parameters.Get_Binary_Array_Length (E);
            pragma Assert (Array_Length = 3);
            pragma Assert (Value_2 = Source_2);
            null;
         end;

         YAMI.Parameters.Next (It);
         pragma Assert (not YAMI.Parameters.Has_Element (It));
      end;

      declare
         Size : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         use type Ada.Streams.Stream_Element_Count;

         Expected : Ada.Streams.Stream_Element_Array :=
           (
            2, 0, 0, 0,         --  num of entries
            23, 0, 0, 0,        --  length of "some rather longer name"
            Ada.Streams.Stream_Element (Character'Pos ('s')),
            Ada.Streams.Stream_Element (Character'Pos ('o')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('t')),
            Ada.Streams.Stream_Element (Character'Pos ('h')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('l')),
            Ada.Streams.Stream_Element (Character'Pos ('o')),
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('g')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')), 0,

            6, 0, 0, 0,        --  type code for binary
            5, 0, 0, 0,        --  length

            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('b')),
            Ada.Streams.Stream_Element (Character'Pos ('c')),
            0,
            Ada.Streams.Stream_Element (Character'Pos ('d')),
            0, 0, 0,

            5, 0, 0, 0,         --  length of "nameA"
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('A')), 0, 0, 0,
            12, 0, 0, 0,        --  type code for binary array
            3, 0, 0, 0,         --  length of array

            3, 0, 0, 0,         --  length of first entry
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('b')),
            Ada.Streams.Stream_Element (Character'Pos ('c')),
            0,

            2, 0, 0, 0,         --  length of second entry
            Ada.Streams.Stream_Element (Character'Pos ('k')),
            Ada.Streams.Stream_Element (Character'Pos ('l')),
            0, 0,

            3, 0, 0, 0,         --  length of third entry
            Ada.Streams.Stream_Element (Character'Pos ('x')),
            Ada.Streams.Stream_Element (Character'Pos ('y')),
            Ada.Streams.Stream_Element (Character'Pos ('z')), 0
           );
      begin
         pragma Assert (Size =
                        4    --  number of entries
                        + 4  --  length of 1st entry name
                        + 24 --  "some rather longer name"
                        + 4  --  type
                        + 4  --  length
                        + 8  --  Source
                        + 4  --  length of 2nd entry name
                        + 8  --  "nameA"
                        + 4  --  type
                        + 4  --  length of array
                        + 4  --  length of 1st array element
                        + 4  --  "abc"
                        + 4  --  length of 2nd array element
                        + 4  --  "kl"
                        + 4  --  length of 3rd array element
                        + 4  --  "xyz"
                       );

         Check_Serialization (Params, Expected, Size);
      end;
   end Test_7;

   --  test for nesting
   procedure Test_8 is
      Params : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;
      Param_Type : YAMI.Parameters.Parameter_Type;

      use type YAMI.Parameters.Parameter_Type;
      use type YAMI.Parameters.Count_Type;
   begin
      Params.Set_Integer ("name", 123);
      pragma Assert (Params.Length = 1);

      declare
         Nested : YAMI.Parameters.Parameters_Collection :=
           Params.Create_Nested_Parameters ("nested");
      begin
         pragma Assert (Params.Length = 2);

         Nested.Set_Integer ("internal", 456);
         pragma Assert (Nested.Length = 1);

         Nested.Set_Long_Float ("internal2", 3.125);
         pragma Assert (Params.Length = 2);
         pragma Assert (Nested.Length = 2);

         declare
            Nested_2 : YAMI.Parameters.Parameters_Collection :=
              Nested.Create_Nested_Parameters ("more nested");
         begin
            pragma Assert (Params.Length = 2);
            pragma Assert (Nested.Length = 3);

            Nested_2.Set_Integer ("more internal", 789);
            pragma Assert (Params.Length = 2);
            pragma Assert (Nested.Length = 3);
            pragma Assert (Nested_2.Length = 1);

            Param_Type := Params.Entry_Type ("name");
            pragma Assert (Param_Type = YAMI.Parameters.Integer_Type);

            Param_Type := Params.Entry_Type ("nested");
            pragma Assert
              (Param_Type = YAMI.Parameters.Nested_Parameters_Type);

            Param_Type := Nested.Entry_Type ("internal");
            pragma Assert (Param_Type = YAMI.Parameters.Integer_Type);

            Param_Type := Nested.Entry_Type ("internal2");
            pragma Assert (Param_Type = YAMI.Parameters.Long_Float_Type);

            Param_Type := Nested.Entry_Type ("more nested");
            pragma Assert
              (Param_Type = YAMI.Parameters.Nested_Parameters_Type);

            Param_Type := Nested_2.Entry_Type ("more internal");
            pragma Assert (Param_Type = YAMI.Parameters.Integer_Type);

            begin
               Param_Type := Nested.Entry_Type ("blabla");
               pragma Assert (False);
            exception
               when E : YAMI.Logic_Error =>
                  pragma Assert
                    (Ada.Exceptions.Exception_Message (E) = "No such name.");
                  null;
            end;
         end;
      end;

      declare
         It : YAMI.Parameters.Parameter_Cursor;
         E : YAMI.Parameters.Parameter_Entry;
         use type YAMI.Parameters.Parameter_Cursor;
      begin
         It := Params.First;
         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.Integer_Type);

         YAMI.Parameters.Next (It);
         pragma Assert (YAMI.Parameters.Has_Element (It));

         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.Nested_Parameters_Type);

         declare
            Nested : YAMI.Parameters.Parameters_Collection :=
              YAMI.Parameters.Get_Nested_Parameters (E);
         begin
            pragma Assert (Nested.Length = 3);
            null;
         end;

         YAMI.Parameters.Next (It);
         pragma Assert (not YAMI.Parameters.Has_Element (It));
      end;

      declare
         Size : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         use type Ada.Streams.Stream_Element_Count;
         use type Ada.Streams.Stream_Element;

         Expected : Ada.Streams.Stream_Element_Array :=
           (
            2, 0, 0, 0,        --  num of entries
            4, 0, 0, 0,        --  length of "name"
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),

            2, 0, 0, 0,        --  type code for integer
            123, 0, 0, 0,      --  value

            6, 0, 0, 0,        --  length of "nested"
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('s')),
            Ada.Streams.Stream_Element (Character'Pos ('t')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('d')), 0, 0,

            13, 0, 0, 0,        --  type code for nested

            3, 0, 0, 0,         --  num of entries in nested

            8, 0, 0, 0,         --  length of "internal"
            Ada.Streams.Stream_Element (Character'Pos ('i')),
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('t')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('l')),

            2, 0, 0, 0,         --  type code for integer
            -56, 1, 0, 0,       --  value

            9, 0, 0, 0,         --  length of "internal2"
            Ada.Streams.Stream_Element (Character'Pos ('i')),
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('t')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('l')),
            Ada.Streams.Stream_Element (Character'Pos ('2')), 0, 0, 0,

            4, 0, 0, 0,         --  type code for double
            0, 0, 0, 0,
            0, 0, 9, 64,        --  3.125

            11, 0, 0, 0,        --  length of "more nested"
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('o')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('s')),
            Ada.Streams.Stream_Element (Character'Pos ('t')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('d')), 0,

            13, 0, 0, 0,        --  type code for nested

            1, 0, 0, 0,         --  num of entries in more nested

            13, 0, 0, 0,        --  length of "more internal"
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('o')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos (' ')),
            Ada.Streams.Stream_Element (Character'Pos ('i')),
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('t')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('r')),
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('l')), 0, 0, 0,

            2, 0, 0, 0,         --  type code for integer
            21, 3, 0, 0         --  value
           );
      begin
         pragma Assert (Size =
                        4    --  number of entries
                        + 4  --  length of 1st entry name
                        + 4  --  "name"
                        + 4  --  type
                        + 4  --  value
                        + 4  --  length of 2nd entry name
                        + 8  --  "nested"
                        + 4  --  type

                        + 4  --  number of params in nested
                        + 4  --  length of 1st entry name
                        + 8  --  "internal"
                        + 4  --  type
                        + 4  --  value
                        + 4  --  length of 2nd entry name
                        + 12 --  "internal2"
                        + 4  --  type
                        + 8  --  value
                        + 4  --  length of 3rd entry name
                        + 12 --  "more nested"
                        + 4  --  type

                        + 4  --  number of params in more nested
                        + 4  --  length of 1st entry name
                        + 16 --  "more internal"
                        + 4  --  type
                        + 4  --  value
                       );

         Check_Serialization (Params, Expected, Size);
      end;
   end Test_8;

   --  test for array of nested parameters
   procedure Test_8a is
      Params : YAMI.Parameters.Parameters_Collection :=
        YAMI.Parameters.Make_Parameters;
      Param_Type : YAMI.Parameters.Parameter_Type;

      use type YAMI.Parameters.Parameter_Type;
      use type YAMI.Parameters.Count_Type;
   begin
      Params.Create_Nested_Array ("name", 3);
      pragma Assert (Params.Length = 1);

      Params.Set_Integer ("i", 7);
      pragma Assert (Params.Length = 2);
      
      pragma Assert (Params.Get_Nested_Array_Length ("name") = 3);
      
      --  first nested
      declare
         Nested_1 : YAMI.Parameters.Parameters_Collection :=
            Params.Get_Nested_In_Array ("name", 1);
      begin
         Nested_1.Set_Integer ("x", 10);
         pragma Assert (Nested_1.Length = 1);
      end;

      --  second nested
      declare
         Nested_2 : YAMI.Parameters.Parameters_Collection :=
            Params.Get_Nested_In_Array ("name", 2);
      begin
         Nested_2.Set_Integer ("x", 20);
         Nested_2.Set_Integer ("y", 21);
         pragma Assert (Nested_2.Length = 2);
      end;

      --  third nested
      declare
         Nested_3 : YAMI.Parameters.Parameters_Collection :=
            Params.Get_Nested_In_Array ("name", 3);
      begin
         Nested_3.Set_Integer ("x", 30);
         Nested_3.Set_Integer ("y", 31);
         Nested_3.Set_Integer ("z", 32);
         pragma Assert (Nested_3.Length = 3);
      end;
      
      --  no more nested
      begin
         declare
            Nested_4 : YAMI.Parameters.Parameters_Collection :=
               Params.Get_Nested_In_Array ("name", 4);
         begin
            null;
         end;
      exception
         when E : YAMI.Logic_Error =>
            pragma Assert
              (Ada.Exceptions.Exception_Message (E) = "No such index.");
            null;
      end;

      Param_Type := Params.Entry_Type ("name");
      pragma Assert (Param_Type = YAMI.Parameters.Nested_Parameters_Array_Type);
            
      declare
         It : YAMI.Parameters.Parameter_Cursor;
         E : YAMI.Parameters.Parameter_Entry;
         use type YAMI.Parameters.Parameter_Cursor;
      begin
         It := Params.First;
         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.Nested_Parameters_Array_Type);

         YAMI.Parameters.Next (It);
         pragma Assert (YAMI.Parameters.Has_Element (It));

         E := YAMI.Parameters.Get_Entry (It);
         Param_Type := YAMI.Parameters.Entry_Type (E);
         pragma Assert (Param_Type = YAMI.Parameters.Integer_Type);

         YAMI.Parameters.Next (It);
         pragma Assert (not YAMI.Parameters.Has_Element (It));
      end;
      
      declare
         Size : Ada.Streams.Stream_Element_Count :=
           Params.Serialize_Buffer_Size;
         use type Ada.Streams.Stream_Element_Count;
         use type Ada.Streams.Stream_Element;

         Expected : Ada.Streams.Stream_Element_Array :=
           (
            2, 0, 0, 0,         --  num of entries
            4, 0, 0, 0,         --  length of "name"
            Ada.Streams.Stream_Element (Character'Pos ('n')),
            Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('m')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            
            14, 0, 0, 0,        --  type code for nested params array
            3, 0, 0, 0,         --  array length

            --  first nested

            1, 0, 0, 0,         --  num of entries
            1, 0, 0, 0,         --  length of "x"
            Ada.Streams.Stream_Element (Character'Pos ('x')), 0, 0, 0,
            2, 0, 0, 0,         --  type code for integer
            10, 0, 0, 0,        --  value

            --  second nested

            2, 0, 0, 0,         --  num of entries
            1, 0, 0, 0,         --  length of "x"
            Ada.Streams.Stream_Element (Character'Pos ('x')), 0, 0, 0,
            2, 0, 0, 0,         --  type code for integer
            20, 0, 0, 0,        --  value
            1, 0, 0, 0,         --  length of "y"
            Ada.Streams.Stream_Element (Character'Pos ('y')), 0, 0, 0,
            2, 0, 0, 0,         --  type code for integer
            21, 0, 0, 0,        --  value

            --  third nested

            3, 0, 0, 0,         --  num of entries
            1, 0, 0, 0,         --  length of "x"
            Ada.Streams.Stream_Element (Character'Pos ('x')), 0, 0, 0,
            2, 0, 0, 0,         --  type code for integer
            30, 0, 0, 0,        --  value
            1, 0, 0, 0,         --  length of "y"
            Ada.Streams.Stream_Element (Character'Pos ('y')), 0, 0, 0,
            2, 0, 0, 0,         --  type code for integer
            31, 0, 0, 0,        --  value
            1, 0, 0, 0,         --  length of "z"
            Ada.Streams.Stream_Element (Character'Pos ('z')), 0, 0, 0,
            2, 0, 0, 0,         --  type code for integer
            32, 0, 0, 0,        --  value

            1, 0, 0, 0,         --  length of "i"
            Ada.Streams.Stream_Element (Character'Pos ('i')), 0, 0, 0,
            2, 0, 0, 0,         --  type code for integer
            7, 0, 0, 0          --  value
           );
      begin
         pragma Assert (Size =
                        4    --  number of entries
                        + 4  --  length of 1st entry name
                        + 4  --  "name"
                        + 4  --  type
                        + 4  --  array length
        
                        --  first nested:

                        + 4  --  number of entries
                        + 4  --  length of 1st entry name
                        + 4  --  "x"
                        + 4  --  type
                        + 4  --  value

                        --  second nested:

                        + 4  --  number of entries
                        + 4  --  length of 1st entry name
                        + 4  --  "x"
                        + 4  --  type
                        + 4  --  value
                        + 4  --  length of 2nd entry name
                        + 4  --  "y"
                        + 4  --  type
                        + 4  --  value

                        --  third nested:
            
                        + 4  --  number of entries
                        + 4  --  length of 1st entry name
                        + 4  --  "x"
                        + 4  --  type
                        + 4  --  value
                        + 4  --  length of 2nd entry name
                        + 4  --  "y"
                        + 4  --  type
                        + 4  --  value
                        + 4  --  length of 3rd entry name
                        + 4  --  "z"
                        + 4  --  type
                        + 4  --  value

                        + 4  --  length of 2nd entry name
                        + 4  --  "i"
                        + 4  --  type
                        + 4  --  value
                       );

         Check_Serialization (Params, Expected, Size);
      end;
   end Test_8a;
      
   --  Test_9 for locking removed

   --  test for raw buffer serialization
   procedure Test_10 is
      Single_Src_Buf : aliased YAMI.Serializables.Serialization_Buffer :=
        (Size => 12, Buffer =>
           (Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('b')),
            Ada.Streams.Stream_Element (Character'Pos ('c')),
            Ada.Streams.Stream_Element (Character'Pos ('d')),
            Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('f')),
            Ada.Streams.Stream_Element (Character'Pos ('g')),
            Ada.Streams.Stream_Element (Character'Pos ('h')),
            Ada.Streams.Stream_Element (Character'Pos ('i')),
            Ada.Streams.Stream_Element (Character'Pos ('j')),
            Ada.Streams.Stream_Element (Character'Pos ('k')),
            Ada.Streams.Stream_Element (Character'Pos ('l'))));

      Single_Src_Buf_List :
        YAMI.Serializables.Serialization_Buffer_List (1 .. 1) :=
        (1 => Single_Src_Buf'Unchecked_Access);

      First_Src_Buf : aliased YAMI.Serializables.Serialization_Buffer :=
        (Size => 4, Buffer =>
           (Ada.Streams.Stream_Element (Character'Pos ('a')),
            Ada.Streams.Stream_Element (Character'Pos ('b')),
            Ada.Streams.Stream_Element (Character'Pos ('c')),
            Ada.Streams.Stream_Element (Character'Pos ('d'))));
      Second_Src_Buf : aliased YAMI.Serializables.Serialization_Buffer :=
        (Size => 8, Buffer =>
           (Ada.Streams.Stream_Element (Character'Pos ('e')),
            Ada.Streams.Stream_Element (Character'Pos ('f')),
            Ada.Streams.Stream_Element (Character'Pos ('g')),
            Ada.Streams.Stream_Element (Character'Pos ('h')),
            Ada.Streams.Stream_Element (Character'Pos ('i')),
            Ada.Streams.Stream_Element (Character'Pos ('j')),
            Ada.Streams.Stream_Element (Character'Pos ('k')),
            Ada.Streams.Stream_Element (Character'Pos ('l'))));
      Double_Src_Buf_List :
        YAMI.Serializables.Serialization_Buffer_List (1 .. 2) :=
        (1 => First_Src_Buf'Unchecked_Access,
         2 => Second_Src_Buf'Unchecked_Access);
   begin

      --  single buffer -> single buffer
      declare
         Raw : YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source :=
           YAMI.Raw_Buffer_Data_Sources.Make_Raw_Buffer_Data_Source
           (Single_Src_Buf_List);

         Target_Size : constant Ada.Streams.Stream_Element_Count :=
           Raw.Serialize_Buffer_Size;

         use type Ada.Streams.Stream_Element_Offset;

      begin
         pragma Assert (Target_Size = Single_Src_Buf.Buffer'Length);

         declare
            Target_Buf :
              aliased YAMI.Serializables.Serialization_Buffer (Target_Size);

            Target_Buffers :
              YAMI.Serializables.Serialization_Buffer_List (1 .. 1) :=
              (1 => Target_Buf'Unchecked_Access);

            use type YAMI.Serializables.Serialization_Buffer;

         begin
            Raw.Serialize (Target_Buffers);
            pragma Assert (Target_Buf = Single_Src_Buf);
         end;
      end;

      --  single buffer -> two buffers
      declare
         Raw : YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source :=
           YAMI.Raw_Buffer_Data_Sources.Make_Raw_Buffer_Data_Source
           (Single_Src_Buf_List);

         Target_Size : constant Ada.Streams.Stream_Element_Count :=
           Raw.Serialize_Buffer_Size;

         use type Ada.Streams.Stream_Element_Offset;

         Target_Size_1 : constant Ada.Streams.Stream_Element_Count := 8;
         Target_Size_2 : constant Ada.Streams.Stream_Element_Count :=
           Target_Size - Target_Size_1;

         Target_Buf_1 :
           aliased YAMI.Serializables.Serialization_Buffer (Target_Size_1);
         Target_Buf_2 :
           aliased YAMI.Serializables.Serialization_Buffer (Target_Size_2);

         Target_Buffers :
           YAMI.Serializables.Serialization_Buffer_List (1 .. 2) :=
           (1 => Target_Buf_1'Unchecked_Access,
            2 => Target_Buf_2'Unchecked_Access);

         use type Ada.Streams.Stream_Element_Array;

      begin
         Raw.Serialize (Target_Buffers);
         pragma Assert
           (Target_Buf_1.Buffer =
            Single_Src_Buf.Buffer (1 .. Target_Buf_1.Buffer'Length));
         pragma Assert
           (Target_Buf_2.Buffer =
            Single_Src_Buf.Buffer
            (Target_Buf_1.Buffer'Length + 1 .. Single_Src_Buf.Buffer'Last));
      end;

      --  two buffers -> single buffer
      declare
         Raw : YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source :=
           YAMI.Raw_Buffer_Data_Sources.Make_Raw_Buffer_Data_Source
           (Double_Src_Buf_List);

         Target_Size : constant Ada.Streams.Stream_Element_Count :=
           Raw.Serialize_Buffer_Size;

         use type Ada.Streams.Stream_Element_Offset;

      begin
         pragma Assert (Target_Size = First_Src_Buf.Buffer'Length +
                        Second_Src_Buf.Buffer'Length);

         declare
            Target_Buf :
              aliased YAMI.Serializables.Serialization_Buffer (Target_Size);

            Target_Buffers :
              YAMI.Serializables.Serialization_Buffer_List (1 .. 1) :=
              (1 => Target_Buf'Unchecked_Access);

            use type YAMI.Serializables.Serialization_Buffer;

         begin
            Raw.Serialize (Target_Buffers);
            pragma Assert (Target_Buf = Single_Src_Buf);
         end;
      end;

      --  two buffers -> two buffers (of different sizes)
      declare
         Raw : YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source :=
           YAMI.Raw_Buffer_Data_Sources.Make_Raw_Buffer_Data_Source
           (Double_Src_Buf_List);

         Target_Size : constant Ada.Streams.Stream_Element_Count :=
           Raw.Serialize_Buffer_Size;

         use type Ada.Streams.Stream_Element_Offset;

         Target_Size_1 : constant Ada.Streams.Stream_Element_Count := 8;
         Target_Size_2 : constant Ada.Streams.Stream_Element_Count :=
           Target_Size - Target_Size_1;

         Target_Buf_1 :
           aliased YAMI.Serializables.Serialization_Buffer (Target_Size_1);
         Target_Buf_2 :
           aliased YAMI.Serializables.Serialization_Buffer (Target_Size_2);

         Target_Buffers :
           YAMI.Serializables.Serialization_Buffer_List (1 .. 2) :=
           (1 => Target_Buf_1'Unchecked_Access,
            2 => Target_Buf_2'Unchecked_Access);

         use type Ada.Streams.Stream_Element_Array;

      begin
         Raw.Serialize (Target_Buffers);
         pragma Assert
           (Target_Buf_1.Buffer =
            Single_Src_Buf.Buffer (1 .. Target_Buf_1.Buffer'Length));
         pragma Assert
           (Target_Buf_2.Buffer =
            Single_Src_Buf.Buffer
            (Target_Buf_1.Buffer'Length + 1 .. Single_Src_Buf.Buffer'Last));
      end;

   end Test_10;

begin
   Test_1;
   Test_2;
   Test_3;
   Test_4;
   Test_5;
   Test_6;
   Test_7;
   Test_8;
   Test_8a;
-- Test_9;
   Test_10;
end Parameters_Test;
