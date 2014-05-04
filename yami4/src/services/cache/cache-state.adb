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

with Log;
with YAMI.Raw_Buffer_Data_Sources;

with Ada.Calendar;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;

package body Cache.State is

   type Value_Type is record
      Buffer : YAMI.Serializables.Serialization_Buffer_Access;
      Time : Ada.Calendar.Time;
   end record;

   package Data_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => Value_Type,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => YAMI.Serializables.Serialization_Buffer,
      Name => YAMI.Serializables.Serialization_Buffer_Access);

   Data_Max_Size : Ada.Streams.Stream_Element_Count;
   Current_Size : Ada.Streams.Stream_Element_Count := 0;
   Data_Eviction_Time : Duration;

   --  helper procedure for ensuring that an element with the given key
   --  is not in the map
   procedure Exclude (M : in out Data_Maps.Map; Key : in String) is
      C : Data_Maps.Cursor;
      Buffer : YAMI.Serializables.Serialization_Buffer_Access;

      use type Data_Maps.Cursor;
      use type Ada.Streams.Stream_Element_Count;
   begin
      C := M.Find (Key);
      if C /= Data_Maps.No_Element then
         Buffer := Data_Maps.Element (C).Buffer;
         Current_Size := Current_Size - Buffer.all.Size;
         Free (Buffer);
         M.Delete (C);
      end if;
   end Exclude;

   protected type Values is

      procedure Set (Key : in String;
                     Value : in YAMI.Core.Serialization_Buffers_Descriptor;
                     Accepted : out Boolean);

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

      procedure Delete (Key : in String);

      procedure Evict_Old_Values;

   private
      All_Values : Data_Maps.Map;
   end Values;

   protected body Values is

      procedure Set (Key : in String;
                     Value : in YAMI.Core.Serialization_Buffers_Descriptor;
                     Accepted : out Boolean) is

         Raw_Source : YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source (0);

         Buffer : YAMI.Serializables.Serialization_Buffer_Access;
         Buffer_List : YAMI.Serializables.Serialization_Buffer_List (1 .. 1);

         use type Ada.Streams.Stream_Element_Count;

      begin
         YAMI.Raw_Buffer_Data_Sources.Init_Raw_Buffer_Data_Source
           (Raw_Source, Value);

         if Current_Size + Raw_Source.Serialize_Buffer_Size <=
           Data_Max_Size then
            begin
               Buffer := new YAMI.Serializables.Serialization_Buffer
                 (Raw_Source.Serialize_Buffer_Size);
               Buffer_List (1) := Buffer;

               Raw_Source.Serialize (Buffer_List);

               --  remove the previous value for this key (if it existed)
               --  and put the new one
               Exclude (All_Values, Key);
               All_Values.Insert (Key, Value_Type'(Buffer, Ada.Calendar.Clock));
               Current_Size := Current_Size + Buffer.all.Size;

               Accepted := True;

            exception
               when others =>
                  Free (Buffer);
                  raise;
            end;
         else
            Accepted := False;
         end if;
      end Set;

      procedure Find_And_Process
        (Key : in String;
         Found : out Boolean;
         Process : not null access procedure
         (Value : in YAMI.Serializables.Serializable'Class)) is

         C : Data_Maps.Cursor;
         Buffer : YAMI.Serializables.Serialization_Buffer_Access;

         use type Data_Maps.Cursor;
      begin
         C := All_Values.Find (Key);
         if C /= Data_Maps.No_Element then
            Buffer := Data_Maps.Element (C).Buffer;
            Found := True;

            declare
               Buffer_List :
                 YAMI.Serializables.Serialization_Buffer_List (1 .. 1);

               Raw_Buffer :
                 YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source (1);
            begin
               Buffer_List (1) := Buffer;
               YAMI.Raw_Buffer_Data_Sources.Init_Raw_Buffer_Data_Source
                 (Raw_Buffer, Buffer_List);

               Process.all (Raw_Buffer);
            end;
         else
            Found := False;
         end if;
      end Find_And_Process;

      --  TODO: workaround for GNAT GPL 2010 bug, to be removed
      procedure Find_And_Process_Raw
        (Key : in String;
         Found : out Boolean;
         Process : not null access procedure
         (Value : in YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source)) is

         C : Data_Maps.Cursor;
         Buffer : YAMI.Serializables.Serialization_Buffer_Access;

         use type Data_Maps.Cursor;
      begin
         C := All_Values.Find (Key);
         if C /= Data_Maps.No_Element then
            Buffer := Data_Maps.Element (C).Buffer;
            Found := True;

            declare
               Buffer_List :
                 YAMI.Serializables.Serialization_Buffer_List (1 .. 1);

               Raw_Buffer :
                 YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source (1);
            begin
               Buffer_List (1) := Buffer;
               YAMI.Raw_Buffer_Data_Sources.Init_Raw_Buffer_Data_Source
                 (Raw_Buffer, Buffer_List);

               Process.all (Raw_Buffer);
            end;
         else
            Found := False;
         end if;
      end Find_And_Process_Raw;

      procedure Delete (Key : in String) is
      begin
         Exclude (All_Values, Key);
      end Delete;

      procedure Evict_Old_Values is
         C : Data_Maps.Cursor;
         Tmp : Data_Maps.Cursor;

         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;

         use type Ada.Calendar.Time;

      begin
         C := All_Values.First;
         while Data_Maps.Has_Element (C) loop
            Tmp := Data_Maps.Next (C);
            if Now - Data_Maps.Element (C).Time > Data_Eviction_Time then
               declare
                  Key : constant String := Data_Maps.Key (C);
               begin
                  Exclude (All_Values, Key);
                  Log.Put (Cache.Evictor, "removed old value for " & Key);
               end;
            end if;
            C := Tmp;
         end loop;
      end Evict_Old_Values;

   end Values;

   V : Values;

   procedure Init (Max_Size : Ada.Streams.Stream_Element_Count;
                   Eviction_Time : in Duration) is
   begin
      Data_Max_Size := Max_Size;
      Data_Eviction_Time := Eviction_Time;
   end Init;

   procedure Set (Key : in String;
                  Value : in YAMI.Core.Serialization_Buffers_Descriptor;
                  Accepted : out Boolean) is
   begin
      V.Set (Key, Value, Accepted);
   end Set;

   procedure Find_And_Process
     (Key : in String;
      Found : out Boolean;
      Process : not null access procedure
      (Value : in YAMI.Serializables.Serializable'Class)) is
   begin
      V.Find_And_Process (Key, Found, Process);
   end Find_And_Process;

   --  TODO: workaround for GNAT GPL 2010 bug, to be removed
   procedure Find_And_Process_Raw
     (Key : in String;
      Found : out Boolean;
      Process : not null access procedure
      (Value : in YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source)) is
   begin
      V.Find_And_Process_Raw (Key, Found, Process);
   end Find_And_Process_Raw;

   procedure Delete (Key : in String) is
   begin
      V.Delete (Key);
   end Delete;

   procedure Evict_Old_Values is
   begin
      V.Evict_Old_Values;
   end Evict_Old_Values;

end Cache.State;
