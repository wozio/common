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

with YAMI.Connection_Event_Handlers;
with YAMI.Core.Closed_Connection_Handlers;
with YAMI.Core.New_Connection_Handlers;

package YAMI.Details.Core_Connection_Handlers is

   --  Core handler for closed and newly created connections,
   --  used to propagate notification to the user-provided
   --  connection event monitor

   type Closed_Or_New_Connection_Handler is
     new Core.Closed_Connection_Handlers.Handler and
         Core.New_Connection_Handlers.Handler with record
        User_Handler : Connection_Event_Handlers.Handler_Access;
   end record;

   type Handler_Access is access all
     Closed_Or_New_Connection_Handler'Class;

   overriding
   procedure Closed_Connection
     (H : in out Closed_Or_New_Connection_Handler;
      Source : in String);

   overriding
   procedure New_Connection
     (H : in out Closed_Or_New_Connection_Handler;
      Source : in String;
      Channel : in Core.Channel_Descriptor);

   procedure Report_New_Outgoing_Connection
     (H : in out Closed_Or_New_Connection_Handler;
      Target : in String);

   procedure Set_User_Event_Handler
     (H : in out Closed_Or_New_Connection_Handler;
      User_Handler : Connection_Event_Handlers.Handler_Access);

end YAMI.Details.Core_Connection_Handlers;
