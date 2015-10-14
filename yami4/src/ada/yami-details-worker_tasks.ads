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
private with YAMI.Details.Stop_Flags;
with YAMI.Details.Water_Flow_Managers;

package YAMI.Details.Worker_Tasks is

   type Worker_Type is limited private;
   type Worker_Access is access all Worker_Type;

   procedure Init
     (Worker : in out Worker_Type;
      Core_Agent : in Core.Agents.Agent_Access;
      Incoming_Flow : in Details.Water_Flow_Managers.Manager_Access);

   procedure Stop (Worker : in out Worker_Type);

private

   task type Worker_Task_Type (Worker : access Worker_Type) is
      entry Start;
      entry Wait_Until_Finished;
   end Worker_Task_Type;

   type Worker_Type is limited record
      Core_Agent : Core.Agents.Agent_Access;
      Incoming_Flow : Details.Water_Flow_Managers.Manager_Access;
      Stop_Request : Stop_Flags.Stop_Flag;
      Worker_Task : Worker_Task_Type (Worker_Type'Access);
   end record;

end YAMI.Details.Worker_Tasks;
