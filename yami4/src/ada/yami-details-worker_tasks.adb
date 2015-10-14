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

package body YAMI.Details.Worker_Tasks is

   task body Worker_Task_Type is
      Allow_Outgoing_Traffic : constant Boolean := True;
      Allow_Incoming_Traffic : Boolean;

      Timeout : constant Duration := 10.0;  -- arbitrary, but within limits
      Dummy_Timed_Out : Boolean;
   begin

      --  synchronize with agent's constructor
      accept Start do
         null;
      end Start;

      while not Worker.all.Stop_Request.Should_Stop loop

         --  flow control:
         --  the outgoing traffic is controlled at the level of
         --  Send/Send_One_Way operations - that is, even before the message
         --  reaches the transport layer and so the output at the transport
         --  layer is always enabled
         --  the incoming traffic is controlled at the transport level,
         --  depending on the length of the incoming message queue

         Allow_Incoming_Traffic :=
           Worker.all.Incoming_Flow.all.Is_Allowed;

         begin
            Worker.all.Core_Agent.all.Do_Some_Work
              (Timeout, Dummy_Timed_Out,
               Allow_Outgoing_Traffic, Allow_Incoming_Traffic);
         exception
            when others =>
               --  ignore I/O errors here, they can be raised naturally
               --  when operating on failing channels

               null;
         end;
      end loop;

      --  synchronize with agent's destructor
      accept Wait_Until_Finished do
         null;
      end Wait_Until_Finished;

   end Worker_Task_Type;

   procedure Init
     (Worker : in out Worker_Type;
      Core_Agent : in Core.Agents.Agent_Access;
      Incoming_Flow : in Details.Water_Flow_Managers.Manager_Access) is
   begin
      Worker.Core_Agent := Core_Agent;
      Worker.Incoming_Flow := Incoming_Flow;

      Worker.Worker_Task.Start;
   end Init;

   procedure Stop (Worker : in out Worker_Type) is
   begin
      Worker.Stop_Request.Stop;
      Worker.Core_Agent.all.Interrupt_Work_Waiter;

      Worker.Worker_Task.Wait_Until_Finished;
   end Stop;

end YAMI.Details.Worker_Tasks;
