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

with YAMI.Core;
with YAMI.Details;
with YAMI.Serializables;

with Ada.Finalization;
with Ada.Streams;
with Interfaces.C;

package YAMI.Raw_Buffer_Data_Sources is

   --
   --  Serializable wrapper for the raw binary data.
   --
   --  Serializable wrapper for the raw binary data allows to use
   --  already serialized content for message sending.
   --  The two major use cases for this type are efficient message forwarding
   --  (when the message is received and its content is used
   --  for another message) and support for custom data models
   --  and serializers.
   --
   type Raw_Buffer_Data_Source (<>) is limited
     new Serializables.Serializable with private;

   --
   --  Operations of the Raw_Buffer_Data_Source
   --

   --
   --  Constructs the buffer wrapper for the given set of buffers.
   --  The data buffer must have the same form as if given for
   --  the deserialization of Parameters_Collection.
   --
   function Make_Raw_Buffer_Data_Source
     (Buffers : in Serializables.Serialization_Buffer_List)
     return Raw_Buffer_Data_Source;

   --
   --  Constructs the buffer wrapper for the given set of buffers,
   --  as described by the low-level descriptor.
   --
   function Make_Raw_Buffer_Data_Source
     (Buffers : in Core.Serialization_Buffers_Descriptor)
     return Raw_Buffer_Data_Source;

   --
   --  Computes the total size of serialization buffer(s) for the current
   --  content of this object.
   --
   overriding
   function Serialize_Buffer_Size (Raw : in Raw_Buffer_Data_Source)
                                  return Ada.Streams.Stream_Element_Count;

   --
   --  Serializes the current content of this object into the given
   --  buffer(s).
   --
   --  The serialization buffer does not have to be contiguous and any number
   --  of buffer segments is allowed, provided that the size of each buffer
   --  segment is a multiple of 4 (32 bits).<br />
   --  The function scatters the serialized data into subsequent buffers
   --  as they become filled.<br />
   --
   overriding
   procedure Serialize (Raw : in Raw_Buffer_Data_Source;
                        Buffers : in Serializables.Serialization_Buffer_List);

   --
   --  Used only by the binding layer.
   --
   overriding
   function Core_Object (Raw : in Raw_Buffer_Data_Source)
                        return Details.Void_Ptr;

private

   Sizeof_Raw_Buffer_Data_Source : constant Interfaces.C.size_t;
   pragma Import (C, Sizeof_Raw_Buffer_Data_Source,
                  "sizeof_raw_buffer_data_source");

   type Raw_Buffer_Data_Source_Value is
     new Interfaces.C.char_array (1 .. Sizeof_Raw_Buffer_Data_Source);
   for Raw_Buffer_Data_Source_Value'Alignment use Details.Alignment;

   type Raw_Buffer_Data_Source (Buffers_Length : Natural) is
     new Ada.Finalization.Limited_Controlled
     and Serializables.Serializable with record
        Initialized : Boolean := False;
        Value : Raw_Buffer_Data_Source_Value;

        --  used to carry the intermediate pointers
        --  for the lifetime of this object, when initialized
        --  from Serialization_Buffer_List
        Bufs : Details.Void_Ptr_Array (1 .. Buffers_Length);
        Sizes : Details.Size_T_Array (1 .. Buffers_Length);
   end record;

   overriding
   procedure Finalize (Raw : in out Raw_Buffer_Data_Source);

end YAMI.Raw_Buffer_Data_Sources;
