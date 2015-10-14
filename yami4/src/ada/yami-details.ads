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

with Interfaces.C.Strings;
with System;

package YAMI.Details is

   --  helper type definitions for interfacing with C++
   subtype Void_Ptr is System.Address;
   subtype Char_Ptr is Interfaces.C.Strings.chars_ptr;
   subtype Char_Array is Interfaces.C.char_array;
   subtype Size_T is Interfaces.C.size_t;
   subtype Int is Interfaces.C.int;
   subtype Long_Long is Interfaces.Integer_64;
   subtype Double is Interfaces.C.double;

   type Void_Ptr_Array is array (Positive range <>) of Void_Ptr;
   type Size_T_Array is array (Positive range <>) of Size_T;

   --  helper function for decoding error codes and translating them
   --  into Ada exceptions
   procedure Check_Result (Res : in Int);

   --  error codes as used by core
   OK               : constant := 0;  --  Operation completed successfully.
   No_Such_Name     : constant := 1;  --  The given name was not found.
   Bad_Type         : constant := 2;  --  Unexpected type.
   No_Such_Index    : constant := 3;  --  Index out of range.
   No_Memory        : constant := 4;  --  Not enough memory.
   Nesting_Too_Deep : constant := 5;  --  Too deep nesting of parameters.
   Not_Enough_Space : constant := 6;  --  Not enough space in the buffer.
   No_Entries       : constant := 7;  --  There are no entries.
   Unexpected_Value : constant := 8;  --  The given value was not recognized.
   Bad_Protocol     : constant := 9;  --  Incorrect connection protocol.
   IO_Error         : constant := 10; --  Unable to perform the I/O operation.
   Timed_Out        : constant := 11; --  The requested operation timed out.
   Channel_Closed   : constant := 12; --  The operation failed due to EOF.
   Bad_State        : constant := 13; --  The object is in the wrong state.
   
   --  notification types as used by core
   Agent_Closed             : constant := 0; --  Agent was closed.
   Listener_Added           : constant := 1; --  Listener was added.
   Listener_Removed         : constant := 2; --  Listener was removed.
   Incoming_Connection_Open : constant := 3; --  New incoming connection.
   Outgoing_Connection_Open : constant := 4; --  New outgoing connection.
   Connection_Closed        : constant := 5; --  Connection was closed.
   Connection_Error         : constant := 6; --  Connection error condition.
   Message_Sent             : constant := 7; --  Message was sent.
   Message_Received         : constant := 8; --  Message was received.

   --  The alignment value cannot be reasonably extracted
   --  from the C interface, because it has to be a static expression.
   Alignment : constant := 8;

end YAMI.Details;
