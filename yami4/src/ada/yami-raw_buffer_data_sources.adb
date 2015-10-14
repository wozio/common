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

package body YAMI.Raw_Buffer_Data_Sources is

   --  convenience renamings from the YAMI.Details package

   subtype Void_Ptr is Details.Void_Ptr;
   subtype Size_T is Details.Size_T;
   subtype Int is Details.Int;

   subtype Void_Ptr_Array is Details.Void_Ptr_Array;
   subtype Size_T_Array is Details.Size_T_Array;

   procedure Check_Result (Res : in Int) renames Details.Check_Result;

   --
   --  Operations of Raw_Buffer_Data_Sources type.
   --

   function Make_Raw_Buffer_Data_Source
     (Buffers : in Serializables.Serialization_Buffer_List)
     return Raw_Buffer_Data_Source is

      procedure Raw_Buffer_Data_Source_Create
        (C_Raw : out Raw_Buffer_Data_Source_Value;
         Bufs : in Void_Ptr_Array;
         Sizes : in Size_T_Array;
         Num_Of_Buffers : in Size_T);
      pragma Import (C, Raw_Buffer_Data_Source_Create,
                     "raw_buffer_data_source_create");

      Num_Of_Buffers : constant Size_T := Buffers'Length;

   begin
      return Raw : Raw_Buffer_Data_Source (Buffers'Length) do
         for I in Buffers'Range loop
            Raw.Bufs (I) := Buffers (I).Buffer (1)'Address;
            Raw.Sizes (I) := Buffers (I).Buffer'Length;
         end loop;

         Raw_Buffer_Data_Source_Create
           (Raw.Value, Raw.Bufs, Raw.Sizes, Num_Of_Buffers);
         Raw.Initialized := True;
      end return;
   end Make_Raw_Buffer_Data_Source;

   function Make_Raw_Buffer_Data_Source
     (Buffers : in Core.Serialization_Buffers_Descriptor)
     return Raw_Buffer_Data_Source is

      procedure Raw_Buffer_Data_Source_Create
        (C_Raw : out Raw_Buffer_Data_Source_Value;
         Bufs : access Void_Ptr; --  array of void*
         Sizes : access Size_T;  --  array of size_t
         Num_Of_Buffers : in Size_T);
      pragma Import (C, Raw_Buffer_Data_Source_Create,
                     "raw_buffer_data_source_create");

   begin
      return Raw : Raw_Buffer_Data_Source (0) do
         --  in the case of buffer descriptor,
         --  the intermediate pointers are not used and
         --  descriptor values are propagated directly
         Raw_Buffer_Data_Source_Create
           (Raw.Value,
            Buffers.Buffers, Buffers.Buffer_Sizes, Buffers.Num_Of_Buffers);
         Raw.Initialized := True;
      end return;
   end Make_Raw_Buffer_Data_Source;

   overriding
   function Serialize_Buffer_Size (Raw : in Raw_Buffer_Data_Source)
                                  return Ada.Streams.Stream_Element_Count is

      function Raw_Buffer_Data_Source_Serialize_Buffer_Size
        (C_Params : in Void_Ptr;
         Buffer_Size : access Size_T)
        return Int;
      pragma Import (C, Raw_Buffer_Data_Source_Serialize_Buffer_Size,
                     "raw_buffer_data_source_serialize_buffer_size");

      Buffer_Size : aliased Size_T;

   begin
      Check_Result
        (Raw_Buffer_Data_Source_Serialize_Buffer_Size
           (Core_Object (Raw), Buffer_Size'Access));
      return Ada.Streams.Stream_Element_Count (Buffer_Size);
   end Serialize_Buffer_Size;

   overriding
   procedure Serialize (Raw : in Raw_Buffer_Data_Source;
                        Buffers : in Serializables.Serialization_Buffer_List)
   is

      function Raw_Buffer_Data_Source_Serialize
        (C_Params : in Void_Ptr;
         Bufs : in Void_Ptr_Array;
         Sizes : in Size_T_Array;
         Num_Of_Buffers : in Size_T)
        return Int;
      pragma Import (C, Raw_Buffer_Data_Source_Serialize,
                     "raw_buffer_data_source_serialize");

      Bufs : Void_Ptr_Array (Buffers'Range);
      Sizes : Size_T_Array (Buffers'Range);
      Num_Of_Buffers : constant Size_T := Buffers'Length;

   begin
      for I in Buffers'Range loop
         Bufs (I) := Buffers (I).Buffer (1)'Address;
         Sizes (I) := Buffers (I).Buffer'Length;
      end loop;

      Check_Result
        (Raw_Buffer_Data_Source_Serialize
           (Core_Object (Raw), Bufs, Sizes, Num_Of_Buffers));
   end Serialize;

   function Core_Object (Raw : in Raw_Buffer_Data_Source)
                        return Void_Ptr is
   begin
      return Raw.Value (Raw.Value'First)'Address;
   end Core_Object;

   procedure Finalize (Raw : in out Raw_Buffer_Data_Source) is

      procedure Destroy_Raw_Buffer_Data_Source (C_Raw : in Details.Void_Ptr);
      pragma Import (C, Destroy_Raw_Buffer_Data_Source,
                     "destroy_raw_buffer_data_source");

   begin
      if Raw.Initialized then
         Destroy_Raw_Buffer_Data_Source (Core_Object (Raw));
      end if;
   end Finalize;

end YAMI.Raw_Buffer_Data_Sources;
