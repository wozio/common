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

with YAMI.Core.Message_Progress_Handlers;
with YAMI.Details.Water_Flow_Managers;
limited with YAMI.Outgoing_Messages;
with YAMI.Parameters;

with Ada.Streams;

package YAMI.Details.Outgoing_Message_Notifications is

   --  TODO: this is a workaround for the compiler bug,
   --  remove it when the Outgoing_Message type will be able to
   --  inherit directly from the notification handler
   --  (in that case, the operations of this type should be abstract
   --  and their bodies removed from .adb)
   type Notification_Handler is record
      OM : System.Address;
   end record;
   type Notification_Handler_Access is access all Notification_Handler;

   procedure Progress
     (H : in out Notification_Handler;
      Message_Id : in Parameters.YAMI_Long_Long_Integer;
      Sent_Bytes : in Ada.Streams.Stream_Element_Count;
      Total_Byte_Count : in Ada.Streams.Stream_Element_Count);

   procedure Replied
     (H : in out Notification_Handler;
      Message_Id : in Parameters.YAMI_Long_Long_Integer;
      Content_Buffers : in Core.Serialization_Buffers_Descriptor);

   procedure Rejected
     (H : in out Notification_Handler;
      Message_Id : in Parameters.YAMI_Long_Long_Integer;
      Reason : in String);

   --  Message_Proxy used to handle internal progress calls from core
   --  and from the outgoing message manager
   --  objects of this type need to exist as long as there is any
   --  possibility to get a progress or reply/reject notification,
   --  even if the Outgoing_Message object itself is already gone

   protected type Reference_Count (Initial : Natural) is
      procedure Decrement (Current : out Natural);
      procedure Deactivate;
      function Is_Active return Boolean;
   private
      Counter : Natural := Initial;
      Active : Boolean := True;
   end Reference_Count;

   --  TODO: this is a workaround, see above
   --  in the future Message should be access of class-wide type)
   type Message_Proxy (Message : access Notification_Handler) is
     limited new Core.Message_Progress_Handlers.Handler with record

      --  the initial refcount is 3, because of the following referees:
      --  - Outgoing_Message object at the user side
      --  - core part in terms of progress notification callback
      --  - outgoing message manager with its reply/reject notifications
      Ref_Count : Reference_Count (3);

      Message_Id : Parameters.YAMI_Long_Long_Integer;

      Outgoing_Flow : Details.Water_Flow_Managers.Manager_Access;

   end record;

   type Message_Proxy_Access is access all Message_Proxy;

   overriding
   procedure Progress
     (P : in out Message_Proxy;
      Sent_Bytes : in Ada.Streams.Stream_Element_Count;
      Total_Byte_Count : in Ada.Streams.Stream_Element_Count);

   procedure Replied
     (P : in out Message_Proxy;
      Content_Buffers : in Core.Serialization_Buffers_Descriptor);

   procedure Rejected (P : in out Message_Proxy; Reason : in String);

   procedure Deactivate (P : in out Message_Proxy);
   procedure Detach (P : in out Message_Proxy);

end YAMI.Details.Outgoing_Message_Notifications;
