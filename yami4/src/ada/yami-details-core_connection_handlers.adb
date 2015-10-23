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

package body YAMI.Details.Core_Connection_Handlers is

   --
   --  Operations of the Core_Closed_Or_New_Connection_Handler type.
   --

   procedure Closed_Connection
     (H : in out Closed_Or_New_Connection_Handler;
      Source : in String) is

      use type Connection_Event_Handlers.Handler_Access;

   begin
      if H.User_Handler /= null then
         begin
            H.User_Handler.Report
              (Source,
               Connection_Event_Handlers.Connection_Closed);
         exception
            when others =>
               null;
         end;
      end if;
   end Closed_Connection;

   procedure New_Connection
     (H : in out Closed_Or_New_Connection_Handler;
      Source : in String;
      Channel : in Core.Channel_Descriptor) is

      use type Connection_Event_Handlers.Handler_Access;

   begin
      if H.User_Handler /= null then
         begin
            H.User_Handler.Report
              (Source,
               Connection_Event_Handlers.New_Incoming_Connection);
         exception
            when others =>
               null;
         end;
      end if;
   end New_Connection;

   procedure Report_New_Outgoing_Connection
     (H : in out Closed_Or_New_Connection_Handler;
      Target : in String) is

      use type Connection_Event_Handlers.Handler_Access;

   begin
      if H.User_Handler /= null then
         begin
            H.User_Handler.Report
              (Target, Connection_Event_Handlers.New_Outgoing_Connection);
         exception
            when others =>
               null;
         end;
      end if;
   end Report_New_Outgoing_Connection;

   procedure Set_User_Event_Handler
     (H : in out Closed_Or_New_Connection_Handler;
      User_Handler : Connection_Event_Handlers.Handler_Access) is
   begin
      H.User_Handler := User_Handler;
   end Set_User_Event_Handler;

end YAMI.Details.Core_Connection_Handlers;
