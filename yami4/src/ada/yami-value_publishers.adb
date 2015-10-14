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

with Ada.Streams;
with Ada.Unchecked_Deallocation;

package body YAMI.Value_Publishers is

   --  helper definitions

   --  Note:
   --  The "traditional" mutex is used instead of proper encapsulation
   --  within the protected object because some operations that need
   --  be executed in the mutually-exclusive context are potentially
   --  blocking and their use from protected operaions would be illegal.
   --  For example, Publish operation needs to have a stable view
   --  on the list of subscribers (mutual exclusion) and at the same
   --  time it posts messages for transmission (potential block).
   --  The Scoped_Lock type is a RAII wrapper for the mutex
   --  to ensure exception safety.

   protected body Mutex is
      entry Lock when not Locked is
      begin
         Locked := True;
      end Lock;
      procedure Unlock is
      begin
         Locked := False;
      end Unlock;
      function State return Boolean is
      begin
         return Locked;
      end State;
   end Mutex;

   type Scoped_Lock is limited
     new Ada.Finalization.Limited_Controlled with record
     Mtx : access Mutex;
   end record;

   overriding
   procedure Finalize (SL : in out Scoped_Lock) is
   begin
      SL.Mtx.all.Unlock;
   end Finalize;

   function Make_Scoped_Lock (Mtx : access Mutex) return Scoped_Lock is
   begin
      return SL : Scoped_Lock do
         Mtx.all.Lock;
         SL.Mtx := Mtx;
      end return;
   end Make_Scoped_Lock;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Outgoing_Messages.Outgoing_Message,
      Name => Outgoing_Messages.Outgoing_Message_Access);

   --  closes last messages for the given subscription
   procedure Update_For_Close
     (Key : in String;
      Element : in out Subscription_Info) is

      use type Outgoing_Messages.Outgoing_Message_Access;
   begin
      for I in 1 .. Element.Max_Queue_Length loop
         if Element.Last_Messages (I) /= null then
            Free (Element.Last_Messages (I));
         end if;
      end loop;
   end Update_For_Close;

   --  operations of Value_Publisher

   function Make_Value_Publisher
     (Command_Handler : Incoming_Messages.Message_Handler_Access := null;
      Max_Queue_Length : Positive := 1;
      Overflow_Handler : Overflow_Handler_Access := null)
     return Value_Publisher is
   begin
      return VP : Value_Publisher do
         VP.Command_Handler := Command_Handler;
         VP.Max_Queue_Length := Max_Queue_Length;
         VP.Overflow_Handler := Overflow_Handler;
      end return;
   end Make_Value_Publisher;

   procedure Subscribe (P : in out Value_Publisher;
                        Destination_Target : in String;
                        Destination_Object : in String) is

      C : Subscription_Maps.Cursor;
      Inserted : Boolean;

      procedure Init_Entry (Key : in String;
                            Element : in out Subscription_Info) is
      begin
         Element.Destination_Object :=
           Ada.Strings.Unbounded.To_Unbounded_String
           (Destination_Object);
      end Init_Entry;

      New_Subscription_Info : Subscription_Info (P.Max_Queue_Length);

      SL : Scoped_Lock := Make_Scoped_Lock (P.Mtx'Unchecked_Access);
   begin
      P.Subscriptions.Insert
        (Key => Destination_Target,
         New_Item => New_Subscription_Info,
         Position => C,
         Inserted => Inserted);
      if Inserted then
         --  this is a new subscription

         --  make sure the channel exists,
         --  so that further sends will not have to create it

         P.Controlling_Agent.Open_Connection (Destination_Target);

         --  initialize the new map entry
         P.Subscriptions.Update_Element (C, Init_Entry'Access);
      else
         --  there is already a subscription for this target, refresh it
         P.Subscriptions.Update_Element (C, Update_For_Close'Access);
         P.Subscriptions.Update_Element (C, Init_Entry'Access);
      end if;
   end Subscribe;

   procedure Unsubscribe (P : in out Value_Publisher;
                          Destination_Target : in String) is

      C : Subscription_Maps.Cursor;
      SL : Scoped_Lock := Make_Scoped_Lock (P.Mtx'Unchecked_Access);

      use type Subscription_Maps.Cursor;
   begin
      C := P.Subscriptions.Find (Destination_Target);
      if C /= Subscription_Maps.No_Element then
         P.Subscriptions.Update_Element (C, Update_For_Close'Access);
         P.Subscriptions.Delete (C);
      end if;
   end Unsubscribe;

   procedure Publish (P : in out Value_Publisher;
                      Value : in Serializables.Serializable'Class;
                      Priority : in Natural := 0) is

      SL : Scoped_Lock := Make_Scoped_Lock (P.Mtx'Unchecked_Access);

      C : Subscription_Maps.Cursor;
      Next_Position : Subscription_Maps.Cursor;
      Abandon_Current_Subscription : Boolean;
      Skip_Message : Boolean;

      procedure Process (Key : in String;
                         Element : in out Subscription_Info) is

         Auto_Connect : constant Boolean := False;

         Destination_Target : String renames Key;
         Destination_Object : constant String :=
           Ada.Strings.Unbounded.To_String (Element.Destination_Object);

         Used_Slots : Natural := 0;

         use type Outgoing_Messages.Outgoing_Message_Access;
         use type Outgoing_Messages.Message_State;
      begin
         --  check all previous messages that were still not processed
         --  by this subscriber

         Abandon_Current_Subscription := False;
         Skip_Message := False;

         for I in Element.Last_Messages'Range loop
            declare

               Msg : Outgoing_Messages.Outgoing_Message_Access :=
                 Element.Last_Messages (I);

               Msg_State : Outgoing_Messages.Message_State;
            begin
               if Msg /= null then
                  Used_Slots := Used_Slots + 1;

                  Msg_State := Msg.all.State;
                  case Msg_State is
                     when Outgoing_Messages.Transmitted |
                       Outgoing_Messages.Replied |
                       Outgoing_Messages.Rejected =>

                        --  this previous message has been successfully sent
                        --  -> remove it from the list

                        Free (Element.Last_Messages (I));

                     when Outgoing_Messages.Abandoned =>

                        --  the whole channel is broken
                        --  - abandon the subscription

                        Abandon_Current_Subscription := True;

                     when others =>
                        null;
                  end case;
               end if;
            end;
         end loop;

         if not Abandon_Current_Subscription then

            --  check if there is a place in the queue

            if Used_Slots = Element.Last_Messages'Length then

               --  the queue is full - as user for decision

               declare
                  Decision : Overflow_Action := Wait_For_Previous_Message;
               begin
                  if P.Overflow_Handler /= null then
                     begin
                        P.Overflow_Handler.all.Overflow
                          (Destination_Target,
                           Destination_Object,
                           Value,
                           Decision);
                     exception
                        when E : others =>
                           Decision := Abandon_Message;
                     end;
                  end if;

                  case Decision is
                     when Wait_For_Previous_Message =>

                        --  synchronize with the first message in the list
                        for I in Element.Last_Messages'Range loop
                           declare
                              Msg :
                                Outgoing_Messages.Outgoing_Message_Access :=
                                Element.Last_Messages (I);
                           begin
                              if Msg /= null then
                                 Msg.all.Wait_For_Transmission;
                                 Free (Element.Last_Messages (I));
                                 exit;
                              end if;
                           end;
                        end loop;

                     when Abandon_Message =>

                        --  do nothing with this message
                        Skip_Message := True;

                     when Abandon_Subscription =>

                        Abandon_Current_Subscription := True;

                  end case;
               end;
            end if;
         end if;

         if not Abandon_Current_Subscription and not Skip_Message then

            --  send the message

            declare
               Msg : Outgoing_Messages.Outgoing_Message_Access :=
                 new Outgoing_Messages.Outgoing_Message;

               Index : Positive;

            begin

               --  find the free slot in the list
               for I in Element.Last_Messages'Range loop
                  if Element.Last_Messages (I) = null then
                     Index := I;
                     exit;
                  end if;
               end loop;

               P.Controlling_Agent.all.Send
                 (Destination_Target,
                  Destination_Object,
                  "subscription_update",
                  Value,
                  Msg,
                  Priority, Auto_Connect);

               Element.Last_Messages (Index) := Msg;

            exception
               when others =>
                  --  in case of any error drop this subscription

                  Abandon_Current_Subscription := True;
                  Free (Msg);
            end;
         end if;
      end Process;

      use type Agents.Agent_Access;
      use type Subscription_Maps.Cursor;
   begin
      if P.Controlling_Agent = null then
         raise Logic_Error with "The object is in a wrong state.";
      end if;

      C := P.Subscriptions.First;
      while C /= Subscription_Maps.No_Element loop

         P.Subscriptions.Update_Element (C, Process'Access);

         Next_Position := Subscription_Maps.Next (C);
         if Abandon_Current_Subscription then
            --  the whole subscription should be deleted
            --  due to error while sending the update message

            P.Subscriptions.Update_Element (C, Update_For_Close'Access);
            P.Subscriptions.Delete (C);
         end if;
         C := Next_Position;
      end loop;
   end Publish;

   procedure Number_Of_Subscribers
     (P : in out Value_Publisher;
      Num : out Natural) is

      SL : Scoped_Lock := Make_Scoped_Lock (P.Mtx'Unchecked_Access);
   begin
      Num := Natural (P.Subscriptions.Length);
   end Number_Of_Subscribers;

   procedure Iterate_Subscribers_Info
     (P : in out Value_Publisher;
      Process : not null access procedure
      (Destination_Target : in String;
       Destination_Object : in String)) is

      procedure Iterator (C : in Subscription_Maps.Cursor) is
         procedure Translate (Key : in String;
                              Element : in Subscription_Info) is
         begin
            Process.all
              (Key,
               Ada.Strings.Unbounded.To_String (Element.Destination_Object));
         end Translate;
      begin
         Subscription_Maps.Query_Element (C, Translate'Access);
      end Iterator;

      SL : Scoped_Lock := Make_Scoped_Lock (P.Mtx'Unchecked_Access);
   begin
      P.Subscriptions.Iterate (Iterator'Access);
   end Iterate_Subscribers_Info;

   procedure Register_At
     (P : in out Value_Publisher;
      The_Agent : Agents.Agent_Access;
      Object_Name : in String) is

      use type Agents.Agent_Access;
   begin
      if P.Controlling_Agent /= null then
         raise Logic_Error with "The object is in a wrong state.";
      end if;

      The_Agent.all.Register_Object (Object_Name, P'Unchecked_Access);

      P.Controlling_Agent := The_Agent;
      P.Object_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Object_Name);
   end Register_At;

   procedure Unregister (P : in out Value_Publisher) is
      use type Agents.Agent_Access;
   begin
      if P.Controlling_Agent = null then
         raise Logic_Error with "The object is in a wrong state.";
      end if;

      P.Controlling_Agent.all.Unregister_Object
        (Ada.Strings.Unbounded.To_String (P.Object_Name));
      P.Controlling_Agent := null;
   end Unregister;

   procedure Call
     (P : in out Value_Publisher;
      Message : in out Incoming_Messages.Incoming_Message'Class) is

      Message_Name : constant String := Message.Message_Name;

      procedure Process (Content : in out Parameters.Parameters_Collection) is
         function Get_Destination_Target return String is
            E : Parameters.Parameter_Entry;
            Found : Boolean;

            use type Parameters.Parameter_Entry;
            use type Parameters.Parameter_Type;
         begin
            --  the destination target is either provided explicitly
            --  in the message payload or it is taken from the
            --  message source

            Content.Find ("destination_target", E, Found);
            if Found and then
              Parameters.Entry_Type (E) = Parameters.String_Type then
               return Parameters.Get_String (E);
            else
               return Message.Source;
            end if;
         end Get_Destination_Target;

         function Get_Destination_Object return String is
            E : Parameters.Parameter_Entry;
            Found : Boolean;

            use type Parameters.Parameter_Entry;
            use type Parameters.Parameter_Type;
         begin
            --  the destination object is either provided explicitly
            --  in the message payload or it is taken from the
            --  local object (the one receiving the "subscribe" message)

            Content.Find ("destination_object", E, Found);
            if Found and then
              Parameters.Entry_Type (E) = Parameters.String_Type then
               return Parameters.Get_String (E);
            else
               return Message.Object_Name;
            end if;
         end Get_Destination_Object;

         Destination_Target : constant String := Get_Destination_Target;

      begin
         if Message_Name = "subscribe" then
            declare
               Destination_Object : constant String := Get_Destination_Object;
            begin
               P.Subscribe (Destination_Target, Destination_Object);
            end;
         else --  Message_Name = "unsubscribe"
            P.Unsubscribe (Destination_Target);
         end if;
      end Process;

      use type Incoming_Messages.Message_Handler_Access;
   begin
      if Message_Name = "subscribe" or
        Message_Name = "unsubscribe" then

         Message.Process_Content (Process'Access);
      end if;

      --  any message - delegate to user
      if P.Command_Handler /= null then
         begin
            P.Command_Handler.all.Call (Message);
         exception
            when E : others =>
               null;
         end;
      else
         --  in the absence of user callback, just confirm this operation
         Message.Reply;
      end if;
   end Call;

   procedure Finalize (P : in out Value_Publisher) is
      SL : Scoped_Lock := Make_Scoped_Lock (P.Mtx'Unchecked_Access);

      C : Subscription_Maps.Cursor;

      use type Agents.Agent_Access;
   begin
      if P.Controlling_Agent /= null then
         P.Controlling_Agent.all.Unregister_Object
           (Ada.Strings.Unbounded.To_String (P.Object_Name));
      end if;

      C := P.Subscriptions.First;
      while Subscription_Maps.Has_Element (C) loop
         P.Subscriptions.Update_Element (C, Update_For_Close'Access);
         Subscription_Maps.Next (C);
      end loop;
   end Finalize;

end YAMI.Value_Publishers;
