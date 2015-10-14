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
with System.Storage_Elements;

package YAMI.Parameters is

   --
   --  YAMI-specific types.
   --
   --  These types are most likely identical to standard Integer,
   --  Long_Long_Integer and Long_Float, but are defined distinctly
   --  to avoid interfacing problems with other compilers.
   --
   type YAMI_Integer is new Interfaces.C.int;
   type YAMI_Long_Long_Integer is new Interfaces.Integer_64;
   type YAMI_Long_Float is new Interfaces.C.double;

   --
   --  Type of parameter entry.
   --
   type Parameter_Type is (Boolean_Type,
                           Integer_Type,
                           Long_Long_Integer_Type,
                           Long_Float_Type,
                           String_Type,
                           Binary_Type,
                           Boolean_Array_Type,
                           Integer_Array_Type,
                           Long_Long_Integer_Array_Type,
                           Long_Float_Array_Type,
                           String_Array_Type,
                           Binary_Array_Type,
                           Nested_Parameters_Type,
                           Nested_Parameters_Array_Type);

   --
   --  The collection of message parameters,
   --  which are typed {name, value} pairs.
   --
   --  Each entry in this collection has a unique name and can have
   --  one of the following types:
   --  - Boolean or Boolean array
   --  - integer or integer array (for 32-bit signed values)
   --  - long integer or long integer array (for 64-bit signed values)
   --  - long float or long float array (for 64-bit floating point values)
   --  - String or String array
   --  - binary or binary array
   --  - nested parameters object, which provides its own scope for naming.
   --
   --  This type is not copyable and not task-safe, although distinct
   --  instances of this type can be used by different tasks without
   --  synchronization.
   --
   --  <b>Note:</b>
   --  The entries are <i>ordered</i> - the order in which they are created
   --  influences the final serialized form of the message payload.<br />
   --  Newly created entries are appended to the end of the collection unless
   --  there is an existing empty slot that can be reused - the appropriate
   --  slot is searched for from the beginning to the end of the collection
   --  and if no free slot is found the collection is extended at the end.
   --  <br />
   --  The above guarantee concerns the user code that relies on
   --  predictable serialization.
   --
   type Parameters_Collection (<>) is limited
     new Serializables.Serializable with private;
   type Parameters_Collection_Access is access all Parameters_Collection;

   --
   --  Type for representing length of internal arrays.
   --
   subtype Count_Type is YAMI_Integer range 0 .. YAMI_Integer'Last;

   --
   --  Index type for addressing individual components of internal arrays.
   --
   subtype Index_Type is YAMI_Integer range 1 .. YAMI_Integer'Last;

   --
   --  Iterator for inspecting entries in the collection.
   --
   type Parameter_Cursor is private;

   --
   --  Special iterator value representing end of iteration.
   --
   No_Parameter : constant Parameter_Cursor;

   --
   --  Read-only view on the parameters entry.
   --
   --  This view is a lightweight proxy that itself is copyable, but
   --  the copying of this type does not create new copies of
   --  the underlying entry.
   --
   type Parameter_Entry is private;

   --
   --  Operations of the Parameters_Collection type.
   --

   --
   --  Creates an empty parameters collection object.
   --
   --  The second version allocates the object dynamically,
   --  this object can still have its working area given explicitly.
   --
   --  <b>Notes:</b>
   --  - If <code>Working_Area = Null_Address and Area_Size = 0</code>
   --    (the default) then all dependent objects are separately allocated
   --    on the global store.
   --  - If <code>Working_Area /= Null_Address and Area_Size /= 0</code> then
   --    the given block is used as a private area.
   --
   --  <b>Note:</b>
   --  Do not attempt to create objects sharing the same working area.
   --
   function Make_Parameters
     (Working_Area : in System.Address := System.Null_Address;
      Area_Size : in System.Storage_Elements.Storage_Count := 0)
     return Parameters_Collection;
   function New_Parameters
     (Working_Area : in System.Address := System.Null_Address;
      Area_Size : in System.Storage_Elements.Storage_Count := 0)
     return Parameters_Collection_Access;

   --
   --  Deallocates the parameters collection object.
   --
   procedure Free (Params : in out Parameters_Collection_Access);

   --
   --  Inserts a new entry of type Boolean to the first available slot.
   --  If the entry with the given name already exists it is replaced
   --  without changing the order of entries.
   --
   procedure Set_Boolean (Params : in out Parameters_Collection;
                          Name : in String; Value : in Boolean);

   --
   --  Extracts the Boolean value from the entry given by its name.
   --
   function Get_Boolean (Params : in Parameters_Collection;
                         Name : in String)
                        return Boolean;

   --
   --  Inserts a new entry of type YAMI_Integer to the first available slot.
   --  If the entry with the given name already exists it is replaced
   --  without changing the order of entries.
   --
   procedure Set_Integer (Params : in out Parameters_Collection;
                          Name : in String; Value : in YAMI_Integer);

   --
   --  Extracts the YAMI_Integer value from the entry given by its name.
   --
   function Get_Integer (Params : in Parameters_Collection;
                         Name : in String)
                        return YAMI_Integer;

   --
   --  Inserts a new entry of type YAMI_Long_Long_Integer
   --  to the first available slot.
   --  If the entry with the given name already exists it is replaced
   --  without changing the order of entries.
   --
   procedure Set_Long_Long (Params : in out Parameters_Collection;
                            Name : in String;
                            Value : in YAMI_Long_Long_Integer);

   --
   --  Extracts the YAMI_Long_Long_Integer value
   --  from the entry given by its name.
   --
   function Get_Long_Long (Params : in Parameters_Collection;
                           Name : in String)
                          return YAMI_Long_Long_Integer;

   --
   --  Inserts a new entry of type YAMI_Long_Float
   --  to the first available slot.
   --  If the entry with the given name already exists it is replaced
   --  without changing the order of entries.
   --
   procedure Set_Long_Float (Params : in out Parameters_Collection;
                             Name : in String;
                             Value : in YAMI_Long_Float);

   --
   --  Extracts the YAMI_Long_Float value from the entry given by its name.
   --
   function Get_Long_Float (Params : in Parameters_Collection;
                            Name : in String)
                           return YAMI_Long_Float;

   --
   --  Inserts a new entry of type String to the first available slot.
   --  If the entry with the given name already exists it is replaced
   --  without changing the order of entries.
   --
   procedure Set_String (Params : in out Parameters_Collection;
                         Name : in String;
                         Value : in String);

   --
   --  Extracts the String value from the entry given by its name.
   --
   function Get_String (Params : in Parameters_Collection;
                        Name : in String)
                       return String;

   --
   --  Inserts a new entry of type binary to the first available slot.
   --  If the entry with the given name already exists it is replaced
   --  without changing the order of entries.
   --
   procedure Set_Binary (Params : in out Parameters_Collection;
                         Name : in String;
                         Value : in Ada.Streams.Stream_Element_Array);

   --
   --  Extracts the binary value from the entry given by its name.
   --
   function Get_Binary (Params : in Parameters_Collection;
                        Name : in String)
                       return Ada.Streams.Stream_Element_Array;

   --
   --  Inserts a new entry of type Boolean array to the first available slot.
   --  If the entry with the given name already exists it is replaced
   --  without changing the order of entries.
   --
   generic
      type Index_Type is (<>);
      type Boolean_Array_Type is array (Index_Type range <>) of Boolean;
   procedure Set_Boolean_Array (Params : in out Parameters_Collection;
                                Name : in String;
                                Values : in Boolean_Array_Type);

   --
   --  Extracts the length of Boolean array from the entry given by its name.
   --
   function Get_Boolean_Array_Length (Params : in Parameters_Collection;
                                      Name : in String)
                                     return Count_Type;

   --
   --  Extracts the array of Boolean values from the entry given by its name.
   --
   generic
      type Index_Type is (<>);
      type Boolean_Array_Type is array (Index_Type range <>) of Boolean;
   procedure Get_Boolean_Array (Params : in Parameters_Collection;
                                Name : in String;
                                Values : out Boolean_Array_Type);

   --
   --  Inserts a new entry of type Integer array to the first available slot.
   --  Values from the user-provided array are converted
   --  to the YAMI_Integer type.
   --  If the entry with the given name already exists it is replaced
   --  without changing the order of entries.
   --
   generic
      type Integer_Type is range <>;
      type Index_Type is (<>);
      type Integer_Array_Type is array (Index_Type range <>) of Integer_Type;
   procedure Set_Integer_Array (Params : in out Parameters_Collection;
                                Name : in String;
                                Values : in Integer_Array_Type);

   --
   --  Extracts the length of Integer array from the entry given by its name.
   --
   function Get_Integer_Array_Length (Params : in Parameters_Collection;
                                      Name : in String)
                                     return Count_Type;

   --
   --  Extracts the array of Integer values from the entry given by its name.
   --  Values from internally managed buffers are converted
   --  to the user-defined integer type.
   --
   generic
      type Integer_Type is range <>;
      type Index_Type is (<>);
      type Integer_Array_Type is array (Index_Type range <>) of Integer_Type;
   procedure Get_Integer_Array (Params : in Parameters_Collection;
                                Name : in String;
                                Values : out Integer_Array_Type);

   --
   --  Inserts a new entry of type Long_Long_Integer array
   --  to the first available slot.
   --  Values from the user-provided array are converted
   --  to the YAMI_Long_Long_Integer type.
   --  If the entry with the given name already exists it is replaced
   --  without changing the order of entries.
   --
   generic
      type Long_Long_Integer_Type is range <>;
      type Index_Type is (<>);
      type Long_Long_Integer_Array_Type is
        array (Index_Type range <>) of Long_Long_Integer_Type;
   procedure Set_Long_Long_Array (Params : in out Parameters_Collection;
                                  Name : in String;
                                  Values : in Long_Long_Integer_Array_Type);

   --
   --  Extracts the length of Long_Long_Integer array
   --  from the entry given by its name.
   --
   function Get_Long_Long_Array_Length (Params : in Parameters_Collection;
                                        Name : in String)
                                       return Count_Type;

   --
   --  Extracts the array of Long_Long_Integer values
   --  from the entry given by its name.
   --  Values from internally managed buffers are converted
   --  to the user-defined long long integer type.
   --
   generic
      type Long_Long_Integer_Type is range <>;
      type Index_Type is (<>);
      type Long_Long_Integer_Array_Type is
        array (Index_Type range <>) of Long_Long_Integer_Type;
   procedure Get_Long_Long_Array (Params : in Parameters_Collection;
                                  Name : in String;
                                  Values : out Long_Long_Integer_Array_Type);

   --
   --  Inserts a new entry of type Long_Float array
   --  to the first available slot.
   --  Values from the user-provided array are converted
   --  to the YAMI_Long_Float type.
   --  If the entry with the given name already exists it is replaced
   --  without changing the order of entries.
   --
   generic
      type Long_Float_Type is digits <>;
      type Index_Type is (<>);
      type Long_Float_Array_Type is
        array (Index_Type range <>) of Long_Float_Type;
   procedure Set_Long_Float_Array (Params : in out Parameters_Collection;
                                   Name : in String;
                                   Values : in Long_Float_Array_Type);

   --
   --  Extracts the length of Long_Float array
   --  from the entry given by its name.
   --
   function Get_Long_Float_Array_Length (Params : in Parameters_Collection;
                                         Name : in String)
                                        return Count_Type;

   --
   --  Extracts the array of Long_Float values
   --  from the entry given by its name.
   --  Values from internally managed buffers are converted
   --  to the user-defined long float type.
   --
   generic
      type Long_Float_Type is digits <>;
      type Index_Type is (<>);
      type Long_Float_Array_Type is
        array (Index_Type range <>) of Long_Float_Type;
   procedure Get_Long_Float_Array (Params : in Parameters_Collection;
                                   Name : in String;
                                   Values : out Long_Float_Array_Type);

   --
   --  Creates a new empty entry of type String array.
   --  If the entry with the given name already exists it is replaced
   --  without changing the order of entries.
   --
   procedure Create_String_Array (Params : in Parameters_Collection;
                                  Name : in String;
                                  Length : in Count_Type);

   --
   --  Extracts the length of String array from the entry given by its name.
   --
   function Get_String_Array_Length (Params : in Parameters_Collection;
                                     Name : in String)
                                    return Count_Type;

   --
   --  Inserts a new String value (possibly replacing the old one)
   --  to already existing String array at the given index.
   --
   procedure Set_String_In_Array (Params : in Parameters_Collection;
                                  Name : in String;
                                  Index : in Index_Type;
                                  Value : in String);

   --
   --  Extracts the String value from the given index of String array.
   --
   function Get_String_In_Array (Params : in Parameters_Collection;
                                 Name : in String;
                                 Index : in Index_Type)
                                return String;

   --
   --  Creates a new empty entry of type binary array.
   --  If the entry with the given name already exists it is replaced
   --  without changing the order of entries.
   --
   procedure Create_Binary_Array (Params : in Parameters_Collection;
                                  Name : in String;
                                  Length : in Count_Type);

   --
   --  Extracts the length of binary array from the entry given by its name.
   --
   function Get_Binary_Array_Length (Params : in Parameters_Collection;
                                     Name : in String)
                                    return Count_Type;

   --
   --  Inserts a new binary value (possibly replacing the old one)
   --  to already existing binary array at the given index.
   --
   procedure Set_Binary_In_Array
     (Params : in Parameters_Collection;
      Name : in String;
      Index : in Index_Type;
      Value : in Ada.Streams.Stream_Element_Array);

   --
   --  Extracts the binary value from the given index of binary array.
   --
   function Get_Binary_In_Array (Params : in Parameters_Collection;
                                 Name : in String;
                                 Index : in Index_Type)
                                return Ada.Streams.Stream_Element_Array;

   --
   --  Creates a new nested parameters collection entry
   --  in the first available slot.
   --  If the entry with the given name already exists it is replaced
   --  without changing the order of entries.
   --  The returned value is a proxy that refers
   --  to the internally managed nested object.
   --
   function Create_Nested_Parameters (Params : in Parameters_Collection;
                                      Name : in String)
                                     return Parameters_Collection;

   --
   --  Extracts nested parameters from the entry given by its name.
   --  The returned value is a proxy that refers
   --  to the internally managed nested object.
   --
   function Get_Nested_Parameters (Params : in Parameters_Collection;
                                   Name : in String)
                                  return Parameters_Collection;

   --
   --  Creates a new empty entry of type nested parameters array.
   --  If the entry with the given name already exists it is replaced
   --  without changing the order of entries.
   --  After the array is created, all its elements are
   --  empty parameters objects.
   --
   procedure Create_Nested_Array (Params : in Parameters_Collection;
                                  Name : in String;
                                  Length : in Count_Type);

   --
   --  Extracts the length of binary array from the entry given by its name.
   --
   function Get_Nested_Array_Length (Params : in Parameters_Collection;
                                     Name : in String)
                                    return Count_Type;

   --
   --  Extracts nested parameters from the given array.
   --  The returned value is a proxy that refers
   --  to the internally managed nested object.
   --
   function Get_Nested_In_Array
     (Parent_Params : in Parameters_Collection;
      Name : in String;
      Index : in Index_Type) return Parameters_Collection;

   --
   --  Removes the entry given by its name.
   --
   procedure Remove (Params : in out Parameters_Collection; Name : in String);

   --
   --  Clears the content of the parameters collection.
   --
   procedure Clear (Params : in out Parameters_Collection);

   --
   --  Returns the size of the collection - that is,
   --  the number of all non-empty slots.
   --
   function Length (Params : in Parameters_Collection) return Count_Type;

   --
   --  Extracts the type of the entry given by its name.
   --
   function Entry_Type (Params : in Parameters_Collection;
                        Name : in String)
                       return Parameter_Type;

   --
   --  Returns the iterator pointing to the beginning of the collection,
   --  which means the first used slot.
   --
   function First (Params : in Parameters_Collection) return Parameter_Cursor;

   --
   --  Returns the view on the entry specified by its name.
   --
   procedure Find (Params : in Parameters_Collection;
                   Name : in String;
                   E : out Parameter_Entry;
                   Found : out Boolean);

   --
   --  Computes the total size of serialization buffer(s) for the current
   --  content of this object.
   --
   overriding
   function Serialize_Buffer_Size (Params : in Parameters_Collection)
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
   procedure Serialize (Params : in Parameters_Collection;
                        Buffers : in Serializables.Serialization_Buffer_List);

   --
   --  Deserializes content from the given buffer(s).
   --
   --  The data buffer does not have to be contiguous and any number
   --  of buffer segments is allowed, provided that the size of each buffer
   --  segment is a multiple of 4 (32 bits).<br />
   --  The function gathers the serialized data from subsequent buffers
   --  as they are consumed.<br />
   --
   --  <b>Note:</b> The current content of this object is not cleared
   --  before attempting deserialization and each retrieved data element
   --  is <i>merged</i> into the current content as if done by individual
   --  calls to appropriate <code>Set_XYZ</code> procedures.<br />
   --  In most cases deserialization will be performed to the empty
   --  parameters object (to reconstruct it to the form that was used
   --  for serialization), but deserialization onto non-empty object
   --  might be occasionally useful as a way of merging two collections.
   --
   procedure Deserialize
     (Params : in out Parameters_Collection;
      Buffers : in Serializables.Serialization_Buffer_List);

   --
   --  Deserializes content from the given buffers
   --  (used for buffers that are already prepared by Core).
   --
   procedure Deserialize
     (Params : in out Parameters_Collection;
      Buffers : in Core.Serialization_Buffers_Descriptor);

   --
   --  Helper function for data transfer between Ada and Core.
   --
   overriding
   function Core_Object (Params : in Parameters_Collection)
                        return Details.Void_Ptr;

   --
   --  Operations of the Parameter_Cursor type.
   --

   --
   --  Returns true if the current position is associated with existing entry.
   --
   function Has_Element (Position : in Parameter_Cursor) return Boolean;

   --
   --  Advances to the next used entry (skips unused slots in the collection).
   --
   function Next (Position : in Parameter_Cursor) return Parameter_Cursor;
   procedure Next (Position : in out Parameter_Cursor);

   --
   --  Returns the read-only view on the parameters entry.
   --
   function Get_Entry (Position : in Parameter_Cursor) return Parameter_Entry;

   --
   --  Operations of the Parameter_Entry type.
   --

   --
   --  Returns the type of the underlying entry
   --  in the associated parameters object.
   --
   function Entry_Type (E : in Parameter_Entry) return Parameter_Type;

   --
   --  Returns the name of the underlying entry
   --  in the associated parameters object.
   --
   function Entry_Name (E : in Parameter_Entry) return String;

   --
   --  Extracts the Boolean value from the current entry.
   --
   function Get_Boolean (E : in Parameter_Entry) return Boolean;

   --
   --  Extracts the YAMI_Integer value from the current entry.
   --
   function Get_Integer (E : in Parameter_Entry) return YAMI_Integer;

   --
   --  Extracts the YAMI_Long_Long_Integer value from the current entry.
   --
   function Get_Long_Long (E : in Parameter_Entry)
                          return YAMI_Long_Long_Integer;

   --
   --  Extracts the YAMI_Long_Float value from the current entry.
   --
   function Get_Long_Float (E : in Parameter_Entry) return YAMI_Long_Float;

   --
   --  Extracts the String value from the current entry.
   --
   function Get_String (E : in Parameter_Entry) return String;

   --
   --  Extracts the binary value from the current entry.
   --
   function Get_Binary (E : in Parameter_Entry)
                       return Ada.Streams.Stream_Element_Array;

   --
   --  Extracts the length of Boolean array from the current entry.
   --
   function Get_Boolean_Array_Length (E : in Parameter_Entry)
                                     return Count_Type;

   --
   --  Extracts the Boolean array from the current entry.
   --
   generic
      type Index_Type is (<>);
      type Boolean_Array_Type is array (Index_Type range <>) of Boolean;
   procedure Entry_Get_Boolean_Array (E : in Parameter_Entry;
                                      Values : out Boolean_Array_Type);

   --
   --  Extracts the length of integer array from the current entry.
   --
   function Get_Integer_Array_Length (E : in Parameter_Entry)
                                     return Count_Type;

   --
   --  Extracts the integer array from the current entry.
   --  Values from internally managed array are converted
   --  to the user-defined Integer_Type.
   --
   generic
      type Integer_Type is range <>;
      type Index_Type is (<>);
      type Integer_Array_Type is array (Index_Type range <>) of Integer_Type;
   procedure Entry_Get_Integer_Array (E : in Parameter_Entry;
                                      Values : out Integer_Array_Type);

   --
   --  Extracts the length of long integer array from the current entry.
   --
   function Get_Long_Long_Array_Length (E : in Parameter_Entry)
                                       return Count_Type;

   --
   --  Extracts the long integer array from the current entry.
   --  Values from internally managed array are converted
   --  to the user-defined Long_Long_Integer_Type.
   --
   generic
      type Long_Long_Integer_Type is range <>;
      type Index_Type is (<>);
      type Long_Long_Integer_Array_Type is
        array (Index_Type range <>) of Long_Long_Integer_Type;
   procedure Entry_Get_Long_Long_Array
     (E : in Parameter_Entry;
      Values : out Long_Long_Integer_Array_Type);

   --
   --  Extracts the length of long float array from the current entry.
   --
   function Get_Long_Float_Array_Length (E : in Parameter_Entry)
                                        return Count_Type;

   --
   --  Extracts the long float array from the current entry.
   --  Values from internally managed array are converted
   --  to the user-defined Long_Float_Type.
   --
   generic
      type Long_Float_Type is digits <>;
      type Index_Type is (<>);
      type Long_Float_Array_Type is
        array (Index_Type range <>) of Long_Float_Type;
   procedure Entry_Get_Long_Float_Array (E : in Parameter_Entry;
                                         Values : out Long_Float_Array_Type);

   --
   --  Extracts the length of String array from the current entry.
   --
   function Get_String_Array_Length (E : in Parameter_Entry)
                                    return Count_Type;

   --
   --  Extracts the String value from the String array at the given index.
   --
   function Get_String_In_Array (E : in Parameter_Entry;
                                 Index : in Index_Type) return String;

   --
   --  Extracts the length of binary array from the current entry.
   --
   function Get_Binary_Array_Length (E : in Parameter_Entry)
                                    return Count_Type;

   --
   --  Extracts the binary value from the binary array at the given index.
   --
   function Get_Binary_In_Array (E : in Parameter_Entry;
                                 Index : in Index_Type)
                                return Ada.Streams.Stream_Element_Array;

   --
   --  Returns the view on the nested parameters object
   --  from the current entry.
   --
   function Get_Nested_Parameters (E : in Parameter_Entry)
                                  return Parameters_Collection;

private

   --
   --  Implementation of Parameters_Collection
   --

   Sizeof_Parameters : constant Interfaces.C.size_t;
   pragma Import (C, Sizeof_Parameters, "sizeof_parameters");

   type Parameters_Value is
     new Interfaces.C.char_array (1 .. Sizeof_Parameters);
   for Parameters_Value'Alignment use Details.Alignment;

   --  The Parameters_Collection can represent either a "free-standing"
   --  object (in which case the array body is kept for C's content)
   --  or can represent the nested object within another one
   --  (in which case the pointer to the delegated C's content is kept).
   --  In both cases only the pointer is passed to the C interface.
   type Parameters_Collection (Nested : Boolean) is
     new Ada.Finalization.Limited_Controlled
     and Serializables.Serializable with record
        Initialized : Boolean := False;
        case Nested is
           when True =>
              --  delegate to the nested object inside another one
              Delegated : System.Address;
           when False =>
              --  top-level object (not nested), host the body
              Value : Parameters_Value;
        end case;
   end record;

   overriding
   procedure Finalize (Params : in out Parameters_Collection);

   --
   --  Implementation of Parameter_Iterator
   --

   Sizeof_Parameter_Iterator : constant Interfaces.C.size_t;
   pragma Import (C, Sizeof_Parameter_Iterator, "sizeof_parameter_iterator");

   type Parameter_Iterator_Value is
     new Interfaces.C.char_array (1 .. Sizeof_Parameter_Iterator);
   for Parameter_Iterator_Value'Alignment use Details.Alignment;

   type Parameter_Cursor is record
      Empty : Boolean;
      V : Parameter_Iterator_Value;
   end record;

   --
   --  Implementation of Parameter_Entry
   --

   Sizeof_Parameter_Entry : constant Interfaces.C.size_t;
   pragma Import (C, Sizeof_Parameter_Entry, "sizeof_parameter_entry");

   type Parameter_Entry_Value is
     new Interfaces.C.char_array (1 .. Sizeof_Parameter_Entry);
   for Parameter_Entry_Value'Alignment use Details.Alignment;

   type Parameter_Entry is record
      V : Parameter_Entry_Value;
   end record;

   No_Parameter : constant Parameter_Cursor := (Empty => True, V => <>);

end YAMI.Parameters;
