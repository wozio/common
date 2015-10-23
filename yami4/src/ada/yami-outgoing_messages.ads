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
with YAMI.Details.Outgoing_Message_Managers;
with YAMI.Details.Outgoing_Message_Notifications;
with YAMI.Parameters;
with YAMI.Serializables;

with Ada.Finalization;
with Ada.Streams;
with Ada.Strings.Unbounded;
with System;

package YAMI.Outgoing_Messages is

   --
   --  Definition of all possible message states,
   --  as used by the Outgoing_Message protected type.
   --
   type Message_State is
     (
      --  Message was posted for transmission
      Posted,

      --  Message was fully transmitted
      Transmitted,

      --  Message was abandoned due to error or channel closing
      Abandoned,

      --  The reply was received
      Replied,

      --  Message was rejected
      Rejected
     );

   --  used to clean up internal resources
   type Finalization_Guard is limited private;

   --
   --  Type of the callback object used to deliver notifications
   --  related to outgoing messages.
   --  The notifications allow to pass the information about
   --  transmission progress, replies and rejections.
   --
   protected type Outgoing_Message is

      --
      --  Gets the current state of the message.
      --
      function State return Message_State;

      --
      --  Returns the current state of the message
      --  and its transmission progress metrics.
      --
      procedure Get_State
        (S : out Message_State;
         Sent_Bytes : out Ada.Streams.Stream_Element_Count;
         Total_Byte_Count : out Ada.Streams.Stream_Element_Count);

      --
      --  Waits for the transmission of this message.
      --  Note: The wait is terminated either when the transmission
      --  is successful or when the message is cancelled due to error
      --  or channel closing request.
      --
      entry Wait_For_Transmission;

      --
      --  Waits for the completion of this message.
      --  Note: The wait is terminated either when the message
      --  is completed - which means that there was a reply or rejection
      --  received for it - or when the message is cancelled due to error
      --  or channel closing request.
      --  When the message is completed,
      --  it will not receive any more notifications.
      --
      entry Wait_For_Completion;

      --
      --  Queries the reply body content.
      --
      procedure Process_Reply_Content
        (Process : not null access procedure
         (Content : in out Parameters.Parameters_Collection));

      --
      --  Queries the raw (binary) reply body content.
      --
      --  This operation can be executed only if the agent is configured
      --  for raw content delivery.
      --
      procedure Process_Raw_Reply_Content
        (Process : not null access procedure
         (Raw_Content : in Serializables.Serialization_Buffer_List));

      --
      --  Returns the rejection reason (or exception message).
      --
      function Exception_Message return String;

      --
      --  Detaches this handler from any future notifications
      --  and releases internal resources related to the given message.
      --  This procedure need not be called when the message is completed.
      --
      procedure Close;

      --  internally used interface

      --  TODO: remove OM_A when compiler bug is fixed
      procedure Init
        (OM_A : in System.Address;
         Message_Id : in Parameters.YAMI_Long_Long_Integer;
         Manager : in Details.Outgoing_Message_Managers.Manager_Access;
         Proxy :
         in Details.Outgoing_Message_Notifications.Message_Proxy_Access;
         Deliver_As_Raw : Boolean);

      --  TODO: workaround, remove when compiler bug is fixed
      procedure Get_Notification_Handler
        (NF : out
         Details.Outgoing_Message_Notifications.Notification_Handler_Access);

      procedure Progress
        (Message_Id : in Parameters.YAMI_Long_Long_Integer;
         Sent_Bytes : in Ada.Streams.Stream_Element_Count;
         Total_Byte_Count : in Ada.Streams.Stream_Element_Count);

      procedure Replied
        (Message_Id : in Parameters.YAMI_Long_Long_Integer;
         Content_Buffers : in Core.Serialization_Buffers_Descriptor);

      procedure Rejected
        (Message_Id : in Parameters.YAMI_Long_Long_Integer;
         Reason : in String);

   private
      Initialized : Boolean := False;
      Msg_Id : Parameters.YAMI_Long_Long_Integer := 0;
      Msg_Manager : Details.Outgoing_Message_Managers.Manager_Access;
      Deliver_As_Raw_Binary : Boolean;
      Msg_State : Message_State;
      Sent : Ada.Streams.Stream_Element_Count;
      Total : Ada.Streams.Stream_Element_Count;

      --  at most only one of these has valid (non-null) content
      Reply_Content : Parameters.Parameters_Collection_Access;
      Raw_Reply_Content : Serializables.Serialization_Buffer_List (1 .. 1);

      Rejection_Reason : Ada.Strings.Unbounded.Unbounded_String;

      --  TODO: this mixin is a workaround for the compiler bug
      --  Outgoing_Message should derive from the Notification_Handler
      --  interface directly
      Notif_Handler :
        aliased Details.Outgoing_Message_Notifications.Notification_Handler;

      Guard : Finalization_Guard;
   end Outgoing_Message;

   type Outgoing_Message_Access is access all Outgoing_Message;

private

   type Finalization_Guard is
     new Ada.Finalization.Limited_Controlled with record
        --  these are used to ultimately remove the message from
        --  the outgoing message manager
        Msg_Manager : Details.Outgoing_Message_Managers.Manager_Access;
        Msg_Id : Parameters.YAMI_Long_Long_Integer := 0;

        --  this is used to release the Reply_Content object
        --  when the whole Outgoing_Message is finalized
        Guarded_Params : Parameters.Parameters_Collection_Access;
        Guarded_Raw_Content : Serializables.Serialization_Buffer_Access;

        Proxy : Details.Outgoing_Message_Notifications.Message_Proxy_Access;
   end record;

   overriding
   procedure Finalize (G : in out Finalization_Guard);

end YAMI.Outgoing_Messages;
