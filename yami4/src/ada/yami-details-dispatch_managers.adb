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

with YAMI.Incoming_Messages;

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package body YAMI.Details.Dispatch_Managers is

   procedure Init
     (Manager : in out Dispatch_Manager;
      Object_Map : in Name_Resolvers.Name_Resolver_Access;
      Incoming_Flow : in Water_Flow_Managers.Manager_Access;
      Core_Agent : in Core.Agents.Agent_Access;
      Opts : in Options.Option_Values) is

      Dispatcher : Dispatcher_Task_Access;

   begin
      Manager.Object_Map := Object_Map;
      Manager.Incoming_Flow := Incoming_Flow;
      Manager.Core_Agent := Core_Agent;

      for I in 1 .. Opts.Dispatcher_Threads loop

         --  Unchecked_Access is justified here, because the tasks
         --  will not live longer than the manager
         Dispatcher := new Dispatcher_Task_Type (Manager'Unchecked_Access);

         Dispatcher.all.Start;

         Manager.Dispatcher_Tasks.Append (Dispatcher);

      end loop;

   end Init;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Incoming_Messages.Incoming_Message,
      Name => Incoming_Messages.Incoming_Message_Access);

   procedure Enqueue
     (Manager : in out Dispatch_Manager;
      Message : in Incoming_Messages.Incoming_Message_Access) is
   begin
      Manager.Incoming_Flow.all.Increase;
      Manager.Msg_Queue.Enqueue (Message);
   end Enqueue;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Dispatcher_Task_Type,
      Name => Dispatcher_Task_Access);

   procedure Stop_Tasks (Manager : in out Dispatch_Manager) is
      DT : Dispatcher_Task_Access;
   begin
      --  introduce poison pills, one for each dispatcher task
      for I in 1 .. Manager.Dispatcher_Tasks.Length loop
         Manager.Msg_Queue.Enqueue (null);
      end loop;

      --  join all tasks and deallocate them
      for I in 1 .. Manager.Dispatcher_Tasks.Length loop
         DT := Manager.Dispatcher_Tasks.First_Element;
         Manager.Dispatcher_Tasks.Delete_First;

         DT.Wait_Until_Finished;
         Free (DT);
      end loop;
   end Stop_Tasks;

   task body Dispatcher_Task_Type is
      Message : Incoming_Messages.Incoming_Message_Access;
      Handler : Incoming_Messages.Message_Handler_Access;

      Open_Incoming_Flow : Boolean;

      use type Incoming_Messages.Incoming_Message_Access;
      use type Incoming_Messages.Message_Handler_Access;
   begin

      --  synchronize with dispatch manager's Init
      --  and with agent's constructor
      accept Start do
         null;
      end Start;

      loop

         Manager.all.Msg_Queue.Take (Message);
         exit when Message = null;

         Handler := Manager.Object_Map.all.Resolve (Message.all.Object_Name);

         if Handler /= null then

            begin

               --  pass the incoming message to the user's handler
               Handler.all.Call (Message.all);

            exception
               --  all exceptions in the user code
               --  should be treated as rejections
               when E : others =>
                  begin
                     Message.all.Reject
                       (Ada.Exceptions.Exception_Message (E));
                  exception
                     --  ignore all error here
                     when others =>
                        null;
                  end;
            end;

            Free (Message);

         else
            --  the message was sent to the unknown object
            --  attempt to send back the rejection
            begin
               Message.all.Reject ("Unknown destination object.");
            exception
               --  ignore all error here
               when others =>
                  null;
            end;
         end if;

         Manager.Incoming_Flow.all.Decrease (Open_Incoming_Flow);
         if Open_Incoming_Flow then
            --  if the incoming flow allow flag changes from
            --  false to true it is necessary to wake up
            --  the worker task, which might be in the
            --  (otherwise) permanent sleep
            --  this can happen if the worker task was
            --  informed in its last cycle that input is not
            --  allowed - now it is allowed and the selector
            --  should be reconfigured to take this into account

            Manager.Core_Agent.all.Interrupt_Work_Waiter;

         end if;
      end loop;

      --  synchronize with agent's destructor
      accept Wait_Until_Finished do
         null;
      end Wait_Until_Finished;

   end Dispatcher_Task_Type;

   protected body Message_Queue is

      procedure Enqueue
        (Msg : in Incoming_Messages.Incoming_Message_Access) is
      begin
         Queue.Append (Msg);
         Empty := False;
      end Enqueue;

      entry Take
        (Msg : out Incoming_Messages.Incoming_Message_Access) when not Empty
         is
      begin
         Msg := Queue.First_Element;
         Queue.Delete_First;

         Empty := Queue.Is_Empty;
      end Take;

   end Message_Queue;

end YAMI.Details.Dispatch_Managers;
