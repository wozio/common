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

with YAMI.Agents;
with YAMI.Incoming_Messages;
with YAMI.Outgoing_Messages;
with YAMI.Serializables;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Finalization;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

package YAMI.Value_Publishers is

   --
   --  Callback interface type for reporting publication overflow conditions.
   --
   type Overflow_Handler is interface;
   type Overflow_Handler_Access is access all Overflow_Handler'Class;

   --
   --  User action for the overflow condition.
   --
   type Overflow_Action is (WAIT_FOR_PREVIOUS_MESSAGE,
                            ABANDON_MESSAGE,
                            ABANDON_SUBSCRIPTION);

   --
   -- User-defined reaction to the publication overflow condition.
   --
   procedure Overflow (H : in out Overflow_Handler;
                       Server_Name: in String;
                       Object_Name : in String;
                       Value : YAMI.Serializables.Serializable'Class;
                       Reaction : out Overflow_Action) is abstract;


   --
   --  The subscription publisher that notifies remote listeners
   --  with published value updates.
   --
   --  Remote listeners can subscribe and unsubscribe at any time.
   --
   --  The publisher is not registered at any agent after its creation.
   --
   --  Note:
   --  The "subscribe" and "unsubscribe" messages are also forwarded
   --  to the Command_Handler, but these two messages are already
   --  processed by the publisher's implementation.
   --
   type Value_Publisher (<>) is limited
     new Incoming_Messages.Message_Handler with private;

   --
   --  Constructor function for value publisher objects.
   --
   function Make_Value_Publisher
     (Command_Handler : Incoming_Messages.Message_Handler_Access := null;
      Max_Queue_Length : Positive := 1;
      Overflow_Handler : Overflow_Handler_Access := null)
     return Value_Publisher;

   --
   --  Subscribes the new listener.
   --
   --  This function is usually called internally as a result of
   --  processing the remote "subscribe" message, but can be also
   --  used locally if the listener's location is obtained via
   --  other means.
   --
   procedure Subscribe (P : in out Value_Publisher;
                        Destination_Target : in String;
                        Destination_Object : in String);

   --
   --  Unsubscribes the given listener.
   --
   procedure Unsubscribe (P : in out Value_Publisher;
                          Destination_Target : in String);

   --
   --  Publishes the new value.
   --
   --  Sends the update message to all active listeners with the given value.
   --  In case of any errors or communication problems, the problematic
   --  listener is automatically unsubscribed.
   --
   procedure Publish (P : in out Value_Publisher;
                      Value : in Serializables.Serializable'Class;
                      Priority : in Natural := 0);

   --
   --  Returns the number of active subscribers.
   --
   procedure Number_Of_Subscribers
     (P : in out Value_Publisher;
      Num : out Natural);

   --
   --  Inspects the information about all active subscribers.
   --
   --  Note:
   --  The iteration is performed in the mutually exclusive context
   --  so no operations affecting the same value publisher should be
   --  attempted from within the Process procedure.
   --
   procedure Iterate_Subscribers_Info
     (P : in out Value_Publisher;
      Process : not null access procedure
      (Destination_Target : in String;
       Destination_Object : in String));

   --  used internally

   procedure Register_At
     (P : in out Value_Publisher;
      The_Agent : Agents.Agent_Access;
      Object_Name : in String);

   procedure Unregister (P : in out Value_Publisher);

private

   type Messages is array (Positive range <>) of
     Outgoing_Messages.Outgoing_Message_Access;

   type Subscription_Info (Max_Queue_Length : Positive) is record
      Destination_Object : Ada.Strings.Unbounded.Unbounded_String;
      Last_Messages : Messages (1 .. Max_Queue_Length);
   end record;

   package Subscription_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String, --  destination target
      Element_Type => Subscription_Info,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   protected type Mutex is
      entry Lock;
      procedure Unlock;
      function State return Boolean;
   private
      Locked : Boolean := False;
   end Mutex;

   type Value_Publisher is
     new Ada.Finalization.Limited_Controlled
     and Incoming_Messages.Message_Handler with record
        Command_Handler : Incoming_Messages.Message_Handler_Access;
        Max_Queue_Length : Positive;
        Overflow_Handler : Overflow_Handler_Access;
        Controlling_Agent : Agents.Agent_Access;
        Object_Name : Ada.Strings.Unbounded.Unbounded_String;
        Subscriptions : Subscription_Maps.Map;
        Mtx : aliased Mutex;
   end record;

   overriding
   procedure Call
     (P : in out Value_Publisher;
      Message : in out Incoming_Messages.Incoming_Message'Class);

   overriding
   procedure Finalize (P : in out Value_Publisher);

end YAMI.Value_Publishers;
