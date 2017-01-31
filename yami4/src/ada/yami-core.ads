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

package YAMI.Core is

   --
   --  Descriptor of the communication channel.
   --
   type Channel_Descriptor is private;

   --
   --  Serialization buffer descriptor, used for passing raw
   --  buffer pointers from Core to Ada.
   --
   type Serialization_Buffers_Descriptor is record
      Buffers : access Details.Void_Ptr;
      Buffer_Sizes : access Details.Size_T;
      Num_Of_Buffers : Details.Size_T;
   end record;

private

   --
   --  Definition of Channel_Descriptor, reflecting the Core part.
   --
   type Channel_Descriptor is record
      Index : Details.Size_T;
      Sequence_Number : Details.Size_T;
   end record;

end YAMI.Core;
