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

package body YAMI.Details.Outgoing_Message_Managers is

   function Long_Int_Hash (Key : in Parameters.YAMI_Long_Long_Integer)
                          return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key);
   end Long_Int_Hash;

   protected body Manager is

      procedure Put
        (Id : in Parameters.YAMI_Long_Long_Integer;
         Msg_Proxy : in
         Details.Outgoing_Message_Notifications.Message_Proxy_Access)
      is
         use type Parameters.YAMI_Long_Long_Integer;
      begin
         if Key_Cache = No_Message_Id then
            Key_Cache := Id;
            Handler_Cache := Msg_Proxy;
         else
            Handler_Map.Insert (Id, Msg_Proxy);
         end if;
      end Put;

      procedure Remove
        (Id : in Parameters.YAMI_Long_Long_Integer) is

         C : Handlers_Maps.Cursor;
         Proxy : Details.Outgoing_Message_Notifications.Message_Proxy_Access;

         use type Parameters.YAMI_Long_Long_Integer;
         use type Handlers_Maps.Cursor;
         use type Details.Outgoing_Message_Notifications.Message_Proxy_Access;
      begin
         if Key_Cache = Id then
            Proxy := Handler_Cache;
            Key_Cache := No_Message_Id;
         else
            C := Handler_Map.Find (Id);
            if C /= Handlers_Maps.No_Element then
               Proxy := Handlers_Maps.Element (C);
               Handler_Map.Delete (C);
            end if;
         end if;

         if Proxy /= null then
            Proxy.all.Detach;
         end if;
      end Remove;

      procedure Report_Replied
        (Id : in Parameters.YAMI_Long_Long_Integer;
         Body_Buffers : in Core.Serialization_Buffers_Descriptor) is

         C : Handlers_Maps.Cursor;
         Proxy : Details.Outgoing_Message_Notifications.Message_Proxy_Access;

         use type Parameters.YAMI_Long_Long_Integer;
         use type Handlers_Maps.Cursor;
         use type Details.Outgoing_Message_Notifications.Message_Proxy_Access;
      begin
         if Key_Cache = Id then
            Proxy := Handler_Cache;
         else
            C := Handler_Map.Find (Id);
            if C /= Handlers_Maps.No_Element then
               Proxy := Handlers_Maps.Element (C);
            end if;
         end if;

         if Proxy /= null then
            Proxy.all.Replied (Body_Buffers);

            --  there should be no more notifications for this handler

            Proxy.all.Detach;
            if Key_Cache = Id then
               Handler_Cache := null;
            else
               Handler_Map.Delete (C);
            end if;
         end if;
      end Report_Replied;

      procedure Report_Rejected
        (Id : in Parameters.YAMI_Long_Long_Integer;
         Reason : in String) is

         C : Handlers_Maps.Cursor;
         Proxy : Details.Outgoing_Message_Notifications.Message_Proxy_Access;

         use type Parameters.YAMI_Long_Long_Integer;
         use type Handlers_Maps.Cursor;
         use type Details.Outgoing_Message_Notifications.Message_Proxy_Access;
      begin
         if Key_Cache = Id then
            Proxy := Handler_Cache;
         else
            C := Handler_Map.Find (Id);
            if C /= Handlers_Maps.No_Element then
               Proxy := Handlers_Maps.Element (C);
            end if;
         end if;

         if Proxy /= null then
            Proxy.all.Rejected (Reason);

            --  there should be no more notifications for this handler

            Proxy.all.Detach;
            if Key_Cache = Id then
               Handler_Cache := null;
            else
               Handler_Map.Delete (C);
            end if;
         end if;
      end Report_Rejected;

   end Manager;

end YAMI.Details.Outgoing_Message_Managers;
