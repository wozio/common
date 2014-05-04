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

with YAMI.Core;
with YAMI.Serializables;

--  TODO: workaround for GNAT GPL 2010 bug, to be removed
with YAMI.Raw_Buffer_Data_Sources;

with Ada.Streams;

package Cache.State is

   pragma Elaborate_Body;

   --
   --  Initializes the state module with configuration settings.
   --
   procedure Init (Max_Size : Ada.Streams.Stream_Element_Count;
                   Eviction_Time : in Duration);

   --
   --  Stores new value or replaces existing one.
   --
   procedure Set (Key : in String;
                  Value : in YAMI.Core.Serialization_Buffers_Descriptor;
                  Accepted : out Boolean);

   --
   --  Retrieves the stored value.
   --
   procedure Find_And_Process
     (Key : in String;
      Found : out Boolean;
      Process : not null access procedure
      (Value : in YAMI.Serializables.Serializable'Class));

   --  TODO: workaround for GNAT GPL 2010 bug, to be removed
   procedure Find_And_Process_Raw
     (Key : in String;
      Found : out Boolean;
      Process : not null access procedure
      (Value : in YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source));

   --
   --  Deletes the given value.
   --
   procedure Delete (Key : in String);

   --
   --  Removes values older than the configured expiration time.
   --
   procedure Evict_Old_Values;

end Cache.State;
