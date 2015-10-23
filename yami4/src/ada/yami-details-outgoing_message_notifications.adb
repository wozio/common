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

with YAMI.Outgoing_Messages;

with Ada.Unchecked_Deallocation;

with Ada.Exceptions;
with System.Address_To_Access_Conversions;

package body YAMI.Details.Outgoing_Message_Notifications is

   --  TODO: this is a workaround for the compiler bug,
   --  remove it when the Outgoing_Message will be able to inherit
   --  from the notification interface
   package Outgoing_Message_Access_Conversions is
      new System.Address_To_Access_Conversions
     (Outgoing_Messages.Outgoing_Message);

   procedure Progress
     (H : in out Notification_Handler;
      Message_Id : in Parameters.YAMI_Long_Long_Integer;
      Sent_Bytes : in Ada.Streams.Stream_Element_Count;
      Total_Byte_Count : in Ada.Streams.Stream_Element_Count) is

      OM_A : Outgoing_Messages.Outgoing_Message_Access :=
        Outgoing_Messages.Outgoing_Message_Access
        (Outgoing_Message_Access_Conversions.To_Pointer(H.OM));

   begin
      OM_A.all.Progress (Message_Id, Sent_Bytes, Total_Byte_Count);
   end Progress;

   procedure Replied
     (H : in out Notification_Handler;
      Message_Id : in Parameters.YAMI_Long_Long_Integer;
      Content_Buffers : in Core.Serialization_Buffers_Descriptor) is

      OM_A : Outgoing_Messages.Outgoing_Message_Access :=
        Outgoing_Messages.Outgoing_Message_Access
        (Outgoing_Message_Access_Conversions.To_Pointer(H.OM));

   begin
      OM_A.all.Replied (Message_Id, Content_Buffers);
   end Replied;

   procedure Rejected
     (H : in out Notification_Handler;
      Message_Id : in Parameters.YAMI_Long_Long_Integer;
      Reason : in String) is

      OM_A : Outgoing_Messages.Outgoing_Message_Access :=
        Outgoing_Messages.Outgoing_Message_Access
        (Outgoing_Message_Access_Conversions.To_Pointer(H.OM));

   begin
      OM_A.all.Rejected (Message_Id, Reason);
   end Rejected;

   protected body Reference_Count is
      procedure Decrement (Current : out Natural) is
      begin
         Counter := Counter - 1;
         Current := Counter;
      end Decrement;

      procedure Deactivate is
      begin
         Active := False;
      end Deactivate;

      function Is_Active return Boolean is
      begin
         return Active;
      end Is_Active;
   end Reference_Count;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Message_Proxy,
      Name => Message_Proxy_Access);

   procedure Progress
     (P : in out Message_Proxy;
      Sent_Bytes : in Ada.Streams.Stream_Element_Count;
      Total_Byte_Count : in Ada.Streams.Stream_Element_Count) is

      use type Ada.Streams.Stream_Element_Count;
   begin
      --  "re-dispatch" to the main Outgoing_Message object

      if P.Ref_Count.Is_Active then
         Progress (P.Message.all, P.Message_Id, Sent_Bytes, Total_Byte_Count);
      end if;

      if Sent_Bytes = Total_Byte_Count then
         --  there will be no more progress notifications from core
         --  so one unit of refcount can be taken down
         Detach (P);

         --  the message is treated as leaving the output queue
         P.Outgoing_Flow.all.Decrease;
      end if;
   end Progress;

   procedure Replied
     (P : in out Message_Proxy;
      Content_Buffers : in Core.Serialization_Buffers_Descriptor) is
   begin
      if P.Ref_Count.Is_Active then
         Replied (P.Message.all, P.Message_Id, Content_Buffers);
      end if;
   end Replied;

   procedure Rejected (P : in out Message_Proxy; Reason : in String) is
   begin
      if P.Ref_Count.Is_Active then
         Rejected (P.Message.all, P.Message_Id, Reason);
      end if;
   end Rejected;

   procedure Deactivate (P : in out Message_Proxy) is
   begin
      P.Ref_Count.Deactivate;
   end Deactivate;

   procedure Detach (P : in out Message_Proxy) is
      Current : Natural;
      Ptr : Message_Proxy_Access;
   begin
      P.Ref_Count.Decrement (Current);
      if Current = 0 then
         Ptr := P'Unchecked_Access;
         Free (Ptr);
      end if;
   end Detach;

end YAMI.Details.Outgoing_Message_Notifications;
