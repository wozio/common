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

with IAL.Patterns;
with Log;
with YAMI.Core.Message_Progress_Handlers;

with Ada.Streams;

package body Broker.Routing is

   --  The Subscription type handles both regular subscriptions
   --  as well as forward channels.
   --  Notes about forward channels:
   --  1. forward channels are never removed
   --     (not even after failure or overflow)
   --  2. the original list of tags in the message
   --     is preserved as object name when the message is forwarded

   type Subscription is
     new YAMI.Core.Message_Progress_Handlers.Handler with record
        Active : Boolean := False;

        Pending_Messages : Natural := 0;
        Sent_Messages : YAMI.Parameters.YAMI_Long_Long_Integer := 0;
        Sent_Bytes : YAMI.Parameters.YAMI_Long_Long_Integer := 0;

        Tag_Patterns : String (1 .. Max_Pattern_Length);
        Tag_Patterns_Last : Natural := 0;
        Target_Object : String (1 .. Max_Name_Length);
        Target_Object_Last : Natural := 0;
        Target_Location : String (1 .. Max_Name_Length);
        Target_Location_Last : Natural := 0;
   end record;

   overriding
   procedure Progress
     (Sub : in out Subscription;
      Sent_Bytes : in Ada.Streams.Stream_Element_Count;
      Total_Byte_Count : in Ada.Streams.Stream_Element_Count) is

     use type YAMI.Parameters.YAMI_Long_Long_Integer;
     use type Ada.Streams.Stream_Element_Count;

   begin
      Sub.Sent_Bytes := Sub.Sent_Bytes +
        YAMI.Parameters.YAMI_Long_Long_Integer (Sent_Bytes);

      if Sent_Bytes = Total_Byte_Count then
         --  one of the pending messages was processed completely

         Sub.Sent_Messages := Sub.Sent_Messages + 1;
         Sub.Pending_Messages := Sub.Pending_Messages - 1;
      end if;
   end Progress;

   type Subscriptions is
     array (Positive range <>) of aliased Subscription;

   Subscription_Table : access Subscriptions;
   Client_Queue_Limit : Positive;
   Sub_Overflow_Policy : Overflow_Policy;

   procedure Init (Max_Subscriptions : in Positive;
                   Max_Client_Queue : in Positive;
                   Subscription_Overflow_Policy : in Overflow_Policy) is
   begin
      Subscription_Table := new Subscriptions (1 .. Max_Subscriptions);
      Client_Queue_Limit := Max_Client_Queue;
      Sub_Overflow_Policy := Subscription_Overflow_Policy;
   end Init;

   procedure Subscribe (Tags : in String;
                        Target_Object : in String;
                        Target_Location : in String) is

      Insert_Point : Natural := 0;
      Found_Duplicate : Boolean := False;

      function Is_Duplicate
        (Sub : in Subscription) return Boolean is
      begin
         return Sub.Tag_Patterns (1 .. Sub.Tag_Patterns_Last) = Tags and
           Sub.Target_Location (1 .. Sub.Target_Location_Last) =
           Target_Location and
           Sub.Target_Object (1 .. Sub.Target_Object_Last) = Target_Object;
      end Is_Duplicate;

   begin
      for I in Subscription_Table.all'Range loop
         declare
            Sub : Subscription renames Subscription_Table.all (I);
         begin
            if not Sub.Active then
               if Insert_Point = 0 then
                  Insert_Point := I;
               end if;
            else
               --  do not duplicate identical subscriptions

               if Is_Duplicate (Sub) then

                  Log.Put (Broker.Subscriptions,
                           Target_Location &
                             " refreshed subscription to " & Tags);

                  Found_Duplicate := True;
                  exit;
               end if;
            end if;
         end;
      end loop;

      if not Found_Duplicate then
         if Insert_Point /= 0 then

            if Target_Object = "*" then
               Log.Put (Broker.Subscriptions,
                        "forwarding " & Tags & " to " & Target_Location);
            else
               Log.Put (Broker.Subscriptions,
                        Target_Location & " subscribed to " & Tags);
            end if;

            declare
               Sub : Subscription renames
                 Subscription_Table.all (Insert_Point);
            begin
               Sub.Active := True;

               Sub.Tag_Patterns (1 .. Tags'Length) := Tags;
               Sub.Tag_Patterns_Last := Tags'Length;

               Sub.Target_Object (1 .. Target_Object'Length) := Target_Object;
               Sub.Target_Object_Last := Target_Object'Length;

               Sub.Target_Location (1 .. Target_Location'Length) :=
                 Target_Location;
               Sub.Target_Location_Last := Target_Location'Length;
            end;
         else
            Log.Put (Broker.Subscriptions,
                     Target_Location & " cannot subscribe due to overflow");
         end if;
      end if;
   end Subscribe;

   procedure Forward (Tags : in String;
                      Target_Location : in String) is
   begin
      --  forward channel is installed as a subcription with "magic"
      --  target object name

      Subscribe (Tags, "*", Target_Location);
   end Forward;

   procedure Iterate_Matching_Subscriptions
     (Tags : in String;
      Process : not null access procedure
      (Target_Object : in String;
       Target_Location : in String;
       Progress_Handler : in
       YAMI.Core.Message_Progress_Handlers.Handler_Access);
      Overflow : out Boolean) is

      function Is_Forward (Sub : in Subscription) return Boolean is
      begin
         return Sub.Target_Object (1 .. Sub.Target_Object_Last) = "*";
      end Is_Forward;

      use type YAMI.Parameters.Count_Type;

   begin
      Overflow := False;

      for I in Subscription_Table.all'Range loop
         if Subscription_Table.all (I).Active then
            declare
               Sub : Subscription renames Subscription_Table.all (I);
               Target_Location : String
                 renames Sub.Target_Location (1 .. Sub.Target_Location_Last);
            begin
               if IAL.Patterns.Multi_Hierarchic_Match
                 (Tags,
                  Sub.Tag_Patterns (1 .. Sub.Tag_Patterns_Last)) then

                  if Sub.Pending_Messages >= Client_Queue_Limit then

                     Overflow := True;

                     if Sub_Overflow_Policy = Unsubscribe then
                        Log.Put (Broker.Subscriptions,
                                 "queue full, abandoned subscription to " &
                                   Target_Location);

                        Sub.Active := False;
                     else
                        Log.Put (Broker.Subscriptions,
                                 "queue full, dropped update for " &
                                   Target_Location);
                     end if;
                  else
                     Process.all
                       (Sub.Target_Object (1 .. Sub.Target_Object_Last),
                        Target_Location, Sub'Access);

                     Sub.Pending_Messages := Sub.Pending_Messages + 1;
                  end if;
               end if;
            exception
               when others =>
                  --  in case of any error take down this subscription
                  --  (unless it is a forwarding channel)

                  if not Is_Forward (Sub) then
                     Log.Put (Broker.Subscriptions,
                              "abandoned subscription to " & Target_Location);

                     Sub.Active := False;
                  end if;
            end;
         end if;
      end loop;
   end Iterate_Matching_Subscriptions;

   procedure Fill_Detailed_Stats
     (Params : in out YAMI.Parameters.Parameters_Collection) is

      Active_Subscription_Count : YAMI.Parameters.Count_Type := 0;

      use type YAMI.Parameters.Index_Type;

   begin
      --  first count the active subscriptions
      for I in Subscription_Table.all'Range loop
         if Subscription_Table.all (I).Active then
            Active_Subscription_Count := Active_Subscription_Count + 1;
         end if;
      end loop;

      if Active_Subscription_Count /= 0 then
         --  create arrays and fill details
         declare
            type Overflow_Array_Type is
              array (YAMI.Parameters.Index_Type range <>) of Boolean;

            type Sent_Counters_Array_Type is
              array (YAMI.Parameters.Index_Type range <>) of
              YAMI.Parameters.YAMI_Long_Long_Integer;

            procedure Set_Boolean_Array is
               new YAMI.Parameters.Set_Boolean_Array
              (Index_Type => YAMI.Parameters.Index_Type,
               Boolean_Array_Type => Overflow_Array_Type);

            procedure Set_Long_Long_Array is
               new YAMI.Parameters.Set_Long_Long_Array
              (Index_Type => YAMI.Parameters.Index_Type,
               Long_Long_Integer_Type =>
                 YAMI.Parameters.YAMI_Long_Long_Integer,
               Long_Long_Integer_Array_Type => Sent_Counters_Array_Type);

            Overflow_Array : Overflow_Array_Type
              (1 .. Active_Subscription_Count);
            Sent_Messages_Array : Sent_Counters_Array_Type
              (1 .. Active_Subscription_Count);
            Sent_Bytes_Array : Sent_Counters_Array_Type
              (1 .. Active_Subscription_Count);

            Index : YAMI.Parameters.Index_Type := 1;

            Locations_Field_Name : constant String := "locations";

         begin
            Params.Create_String_Array
              (Locations_Field_Name, Active_Subscription_Count);

            for I in Subscription_Table.all'Range loop
               if Subscription_Table.all (I).Active then
                  declare
                     Sub : Subscription renames Subscription_Table.all (I);
                     Target_Location : String renames
                       Sub.Target_Location (1 .. Sub.Target_Location_Last);
                  begin
                     Overflow_Array (Index) :=
                       Sub.Pending_Messages = Client_Queue_Limit;

                     Sent_Messages_Array (Index) := Sub.Sent_Messages;

                     Sent_Bytes_Array (Index) := Sub.Sent_Bytes;

                     Params.Set_String_In_Array
                       (Locations_Field_Name, Index,
                        Sub.Target_Location (1 .. Sub.Target_Location_Last));
                  end;

                  Index := Index + 1;
               end if;
            end loop;

            --  string values are already in Params, scalar arrays
            --  have to be set separately:

            Set_Boolean_Array (Params, "overflows", Overflow_Array);
            Set_Long_Long_Array
              (Params, "sent_messages", Sent_Messages_Array);
            Set_Long_Long_Array (Params, "sent_bytes", Sent_Bytes_Array);
         end;
      end if;
   end Fill_Detailed_Stats;

end Broker.Routing;
