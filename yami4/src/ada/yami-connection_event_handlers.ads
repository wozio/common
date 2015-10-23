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

package YAMI.Connection_Event_Handlers is

   --
   -- Type of connection event.
   --
   type Connection_Event is
     (New_Incoming_Connection,
      New_Outgoing_Connection,
      Connection_Closed);

   --
   --  Callback interface for reporting connection events.
   --
   type Handler is limited interface;
   type Handler_Access is access all Handler'Class;

   --
   --  Operations of the Handler interface.
   --

   --
   --  Report the connection event.
   --
   procedure Report (H : in out Handler;
                     Name : in String;
                     Event : in Connection_Event)
      is abstract;

end YAMI.Connection_Event_Handlers;
