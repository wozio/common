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

with YAMI.Core;
with YAMI.Details.Outgoing_Message_Notifications;
with YAMI.Parameters;

with Ada.Containers.Hashed_Maps;

package YAMI.Details.Outgoing_Message_Managers is

   function Long_Int_Hash (Key : in Parameters.YAMI_Long_Long_Integer)
                          return Ada.Containers.Hash_Type;

   package Handlers_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Parameters.YAMI_Long_Long_Integer,
      Element_Type =>
        Details.Outgoing_Message_Notifications.Message_Proxy_Access,
      Hash => Long_Int_Hash,
      Equivalent_Keys => Parameters."=",
     "=" => Details.Outgoing_Message_Notifications."=");

   No_Message_Id : constant := -1;

   protected type Manager is

      procedure Put
        (Id : in Parameters.YAMI_Long_Long_Integer;
         Msg_Proxy : in
         Details.Outgoing_Message_Notifications.Message_Proxy_Access);

      procedure Remove
        (Id : in Parameters.YAMI_Long_Long_Integer);

      procedure Report_Replied
        (Id : in Parameters.YAMI_Long_Long_Integer;
         Body_Buffers : in Core.Serialization_Buffers_Descriptor);

      procedure Report_Rejected
        (Id : in Parameters.YAMI_Long_Long_Integer;
         Reason : in String);

   private
      Handler_Map : Handlers_Maps.Map;

      --  cache for usual scenario
      Key_Cache : Parameters.YAMI_Long_Long_Integer := No_Message_Id;
      Handler_Cache :
        Details.Outgoing_Message_Notifications.Message_Proxy_Access;
   end Manager;

   type Manager_Access is access all Manager;

end YAMI.Details.Outgoing_Message_Managers;
