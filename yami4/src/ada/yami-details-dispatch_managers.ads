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

with YAMI.Core.Agents;
with YAMI.Details.Name_Resolvers;
with YAMI.Details.Options;
with YAMI.Details.Water_Flow_Managers;
with YAMI.Incoming_Messages;

with Ada.Containers.Doubly_Linked_Lists;

package YAMI.Details.Dispatch_Managers is

   type Dispatch_Manager is tagged limited private;
   type Dispatch_Manager_Access is access all Dispatch_Manager;

   procedure Init
     (Manager : in out Dispatch_Manager;
      Object_Map : in Name_Resolvers.Name_Resolver_Access;
      Incoming_Flow : in Water_Flow_Managers.Manager_Access;
      Core_Agent : in Core.Agents.Agent_Access;
      Opts : in Options.Option_Values);

   procedure Enqueue (Manager : in out Dispatch_Manager;
                      Message : in Incoming_Messages.Incoming_Message_Access);

   procedure Stop_Tasks (Manager : in out Dispatch_Manager);

private

   task type Dispatcher_Task_Type (Manager : access Dispatch_Manager) is
      entry Start;
      entry Wait_Until_Finished;
   end Dispatcher_Task_Type;

   type Dispatcher_Task_Access is access all Dispatcher_Task_Type;

   package Dispatcher_Tasks_Lists is new Ada.Containers.Doubly_Linked_Lists
      (Element_Type => Dispatcher_Task_Access);

   package Incoming_Message_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Incoming_Messages.Incoming_Message_Access,
     "=" => Incoming_Messages."=");

   protected type Message_Queue is

      procedure Enqueue (Msg : in Incoming_Messages.Incoming_Message_Access);
      entry Take (Msg : out Incoming_Messages.Incoming_Message_Access);

   private

      Queue : Incoming_Message_Lists.List;
      Empty : Boolean := True;

   end Message_Queue;

   type Dispatch_Manager is tagged limited record
      Object_Map : Name_Resolvers.Name_Resolver_Access;
      Incoming_Flow : Water_Flow_Managers.Manager_Access;
      Core_Agent : Core.Agents.Agent_Access;
      Msg_Queue : Message_Queue;
      Dispatcher_Tasks : Dispatcher_Tasks_Lists.List;
   end record;

end YAMI.Details.Dispatch_Managers;
