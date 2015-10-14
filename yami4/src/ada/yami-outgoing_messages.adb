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

with YAMI.Raw_Buffer_Data_Sources;

package body YAMI.Outgoing_Messages is

   protected body Outgoing_Message is

      function State return Message_State is
      begin
         if not Initialized then
            raise Logic_Error with "Object has not been initialized.";
         end if;

         return Msg_State;
      end State;

      procedure Get_State
        (S : out Message_State;
         Sent_Bytes : out Ada.Streams.Stream_Element_Count;
         Total_Byte_Count : out Ada.Streams.Stream_Element_Count) is
      begin
         if not Initialized then
            raise Logic_Error with "Object has not been initialized.";
         end if;

         S := Msg_State;
         Sent_Bytes := Sent;
         Total_Byte_Count := Total;
      end Get_State;

      entry Wait_For_Transmission when
        not Initialized or Msg_State /= Posted is
      begin
         if not Initialized then
            raise Logic_Error with "Object has not been initialized.";
         end if;
      end Wait_For_Transmission;

      entry Wait_For_Completion when
        not Initialized or
        Msg_State = Abandoned or
        Msg_State = Replied or
        Msg_State = Rejected is
      begin
         if not Initialized then
            raise Logic_Error with "Object has not been initialized.";
         end if;
      end Wait_For_Completion;

      procedure Process_Reply_Content
        (Process : not null access procedure
         (Content : in out Parameters.Parameters_Collection)) is

         use type Parameters.Parameters_Collection_Access;
      begin
         if not Initialized or Msg_State /= Replied or
           Reply_Content = null then
            raise Logic_Error with "Object is in the wrong state.";
         end if;
         Process.all (Reply_Content.all);
      end Process_Reply_Content;

      procedure Process_Raw_Reply_Content
        (Process : not null access procedure
         (Raw_Content : in Serializables.Serialization_Buffer_List)) is

         use type Serializables.Serialization_Buffer_Access;
      begin
         if not Initialized or Msg_State /= Replied or
           Raw_Reply_Content (1) = null then
            raise Logic_Error with "Object is in the wrong state.";
         end if;
         Process.all (Raw_Reply_Content);
      end Process_Raw_Reply_Content;

      function Exception_Message return String is
      begin
         if not Initialized or Msg_State /= Rejected then
            raise Logic_Error with "Object is in the wrong state.";
         end if;
         return Ada.Strings.Unbounded.To_String (Rejection_Reason);
      end Exception_Message;

      procedure Close is
         use type Parameters.YAMI_Long_Long_Integer;
         use type
           Details.Outgoing_Message_Notifications.Message_Proxy_Access;
      begin
         if Msg_Id /= 0 then
            Msg_Manager.all.Remove (Msg_Id);
            Msg_Manager := null;
            Msg_Id := 0;
            if Guard.Proxy /= null then
               Guard.Proxy.all.Deactivate;
               Guard.Proxy.all.Detach;
               Guard.Proxy := null;
            end if;

            if Msg_State /= Replied and Msg_State /= Rejected then
               Msg_State := Abandoned;
            end if;
         end if;
         Initialized := False;
      end Close;

      procedure Init
        (OM_A : in System.Address;
         Message_Id : in Parameters.YAMI_Long_Long_Integer;
         Manager : in Details.Outgoing_Message_Managers.Manager_Access;
         Proxy :
         in Details.Outgoing_Message_Notifications.Message_Proxy_Access;
         Deliver_As_Raw : Boolean) is

         use type Parameters.Parameters_Collection_Access;
         use type Serializables.Serialization_Buffer_Access;
         use type
           Details.Outgoing_Message_Notifications.Message_Proxy_Access;
      begin
         Notif_Handler.OM := OM_A;

         Msg_Id := Message_Id;
         Msg_Manager := Manager;
         Msg_State := Posted;
         Sent := 0;
         Total := 0;

         Guard.Msg_Id := Message_Id;

         if Guard.Proxy /= null then
            Guard.Proxy.all.Deactivate;
            Guard.Proxy.all.Detach;
         end if;
         Guard.Proxy := Proxy;

         Deliver_As_Raw_Binary := Deliver_As_Raw;

         if Deliver_As_Raw_Binary then
            if Raw_Reply_Content (1) /= null then
               Serializables.Free (Guard.Guarded_Raw_Content);
               Raw_Reply_Content (1) := null;
            end if;
         else
            if Reply_Content = null then
               --  this is a first initialization of this object
               Reply_Content := Parameters.New_Parameters;
               Guard.Guarded_Params := Reply_Content;
            else
               Reply_Content.Clear;
            end if;
         end if;

         Guard.Msg_Manager := Manager;
         Initialized := True;
      end Init;

      --  TODO: this is a workaround for compiler bug
      --  when the Outgoing_Message type will be able to inherit from
      --  then notification handler, this operation should disappear
      procedure Get_Notification_Handler
        (NF : out
         Details.Outgoing_Message_Notifications.Notification_Handler_Access)
      is
      begin
         NF := Notif_Handler'Unchecked_Access;
      end Get_Notification_Handler;

      procedure Progress
        (Message_Id : in Parameters.YAMI_Long_Long_Integer;
         Sent_Bytes : in Ada.Streams.Stream_Element_Count;
         Total_Byte_Count : in Ada.Streams.Stream_Element_Count) is

         use type YAMI.Parameters.YAMI_Long_Long_Integer;
         use type Ada.Streams.Stream_Element_Count;
         use type
           Details.Outgoing_Message_Notifications.Message_Proxy_Access;

      begin
         if Message_Id = Msg_Id then
            Sent := Sent_Bytes;
            Total := Total_Byte_Count;

            if Sent_Bytes = Total_Byte_Count then
               --  there will be no more progress notifications
               --  for this message

               if Sent_Bytes /= 0 then
                  --  the transmission of the whole message
                  --  was successful

                  if Msg_State = Posted then
                     Msg_State := Transmitted;
                  end if;

               else
                  --  the message was abandoned
                  --  before it was fully transmitted

                  Msg_State := Abandoned;
                  Msg_Manager.all.Remove (Msg_Id);
                  Msg_Id := 0;

               end if;
            end if;
         end if;
      end Progress;

      procedure Replied
        (Message_Id : in Parameters.YAMI_Long_Long_Integer;
         Content_Buffers : in Core.Serialization_Buffers_Descriptor) is

         use type YAMI.Parameters.YAMI_Long_Long_Integer;
      begin
         if Message_Id = Msg_Id then
            if Msg_State = Posted or Msg_State = Transmitted then
               Msg_State := Replied;

               if Deliver_As_Raw_Binary then
                  --  repackage the raw binary content
                  --  so that this object can own it

                  declare
                     Buffer_Converter :
                       Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source :=
                       Raw_Buffer_Data_Sources.Make_Raw_Buffer_Data_Source
                       (Content_Buffers);

                     Target_Size :
                       constant Ada.Streams.Stream_Element_Count :=
                       Buffer_Converter.Serialize_Buffer_Size;

                     Target_Buffer :
                       Serializables.Serialization_Buffer_Access :=
                       new Serializables.Serialization_Buffer (Target_Size);
                  begin
                     Raw_Reply_Content (1) := Target_Buffer;
                     Buffer_Converter.Serialize (Raw_Reply_Content);

                     Guard.Guarded_Raw_Content := Target_Buffer;
                  exception
                     when others =>
                        Serializables.Free (Target_Buffer);
                        Raw_Reply_Content (1) := null;
                        raise;
                  end;
               else
                  --  deserialize the content so that it is
                  --  available as parameters collection

                  Reply_Content.Deserialize (Content_Buffers);
               end if;
            end if;
         end if;
      end Replied;

      procedure Rejected
        (Message_Id : in Parameters.YAMI_Long_Long_Integer;
         Reason : in String) is

         use type YAMI.Parameters.YAMI_Long_Long_Integer;
      begin
         if Message_Id = Msg_Id then
            if Msg_State = Posted or Msg_State = Transmitted then
               Msg_State := Rejected;
               Rejection_Reason :=
                 Ada.Strings.Unbounded.To_Unbounded_String (Reason);
            end if;
         end if;
      end Rejected;

   end Outgoing_Message;

   procedure Finalize (G : in out Finalization_Guard) is
      use type Parameters.YAMI_Long_Long_Integer;
      use type Details.Outgoing_Message_Notifications.Message_Proxy_Access;
      use type Details.Outgoing_Message_Managers.Manager_Access;
   begin
      if G.Msg_Id /= 0 and G.Msg_Manager /= null then
         G.Msg_Manager.all.Remove (G.Msg_Id);
      end if;
      Parameters.Free (G.Guarded_Params);
      Serializables.Free (G.Guarded_Raw_Content);
      if G.Proxy /= null then
         G.Proxy.all.Deactivate;
         G.Proxy.all.Detach;
      end if;
   end Finalize;

end YAMI.Outgoing_Messages;
