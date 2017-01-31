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

with YAMI.Core.Event_Notification_Handlers;

package YAMI.Event_Notification_Handlers is

   --
   --  Callback interface for processing event notifications.
   --
   type Handler is limited interface and
     Core.Event_Notification_Handlers.Handler;
   type Handler_Access is access all Handler'Class;

   --
   --  Operations of the Handler interface.
   --

   procedure Agent_Created (H : in out Handler) is abstract;
   
   procedure Object_Registered (H : in out Handler; Name : in String)
      is abstract;
     
   procedure Object_Unregistered (H : in out Handler; Name : in String)
      is abstract;
     
end YAMI.Event_Notification_Handlers;
