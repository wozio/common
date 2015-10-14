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

with YAMI.Details;

with Ada.Streams;
with Ada.Unchecked_Deallocation;

package YAMI.Serializables is

   --
   --  Wrapper type for serialization and deserialization segments.
   --
   type Serialization_Buffer (Size : Ada.Streams.Stream_Element_Count) is
      record
         Buffer : Ada.Streams.Stream_Element_Array (1 .. Size);
      end record;
   type Serialization_Buffer_Access is access all Serialization_Buffer;

   --
   --  Deallocates the dynamically allocated buffer.
   --
   procedure Free (Buffer : in out Serialization_Buffer_Access);

   --
   --  List of serialization and deserialization segments.
   --
   type Serialization_Buffer_List is
     array (Positive range <>) of Serialization_Buffer_Access;

   --
   --  Common interface for serializable entities.
   --
   type Serializable is limited interface;

   --
   --  Computes the total size of serialization buffer(s) for the current
   --  content of this object.
   --
   function Serialize_Buffer_Size (S : in Serializable)
                                  return Ada.Streams.Stream_Element_Count
     is abstract;

   --
   --  Serializes the current content of this object into the given
   --  buffer(s).
   --
   --  The serialization buffer does not have to be contiguous and any number
   --  of buffer segments is allowed, provided that the size of each buffer
   --  segment is a multiple of 4 (32 bits).
   --  The function scatters the serialized data into subsequent buffers
   --  as they become filled.
   --
   procedure Serialize (S : in Serializable;
                        Buffers : in Serializables.Serialization_Buffer_List)
     is abstract;

   --
   --  Used only by the binding layer.
   --
   function Core_Object (S : in Serializable)
                        return Details.Void_Ptr is abstract;

end YAMI.Serializables;
