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

with YAMI.Details;

with Ada.Streams;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

package body YAMI.Core.Agents is

   --  convenience renamings from the YAMI.Details package

   subtype Void_Ptr is Details.Void_Ptr;
   subtype Char_Ptr is Details.Char_Ptr;
   subtype Char_Array is Details.Char_Array;
   subtype Size_T is Details.Size_T;
   subtype Int is Details.Int;
   subtype Long_Long is Details.Long_Long;
   subtype Double is Details.Double;

   subtype Void_Ptr_Array is Details.Void_Ptr_Array;
   subtype Size_T_Array is Details.Size_T_Array;

   procedure Check_Result (Res : in Int) renames Details.Check_Result;

   package Message_Handler_Address_To_Access_Conversions is
      new System.Address_To_Access_Conversions
     (Object => Incoming_Message_Handlers.Handler'Class);

   --  helper for translating C++ function callback into Ada interface call
   procedure Incoming_Message_Dispatch_Callback_Translator
     (C_Message_Handler_Address : in Void_Ptr;
      C_Source : in Char_Ptr;
      Header_Buffers : access Void_Ptr; --  array of void*
      Header_Buf_Sizes : access Size_T; --  array of size_t
      Num_Of_Header_Buffers : in Size_T;
      Body_Buffers : access Void_Ptr;   --  array of void*
      Body_Buf_Sizes : access Size_T;   --  array of size_t
      Num_Of_Body_Buffers : in Size_T);
   pragma Convention (C, Incoming_Message_Dispatch_Callback_Translator);

   procedure Incoming_Message_Dispatch_Callback_Translator
     (C_Message_Handler_Address : in Void_Ptr;
      C_Source : in Char_Ptr;
      Header_Buffers : access Void_Ptr; --  array of void*
      Header_Buf_Sizes : access Size_T; --  array of size_t
      Num_Of_Header_Buffers : in Size_T;
      Body_Buffers : access Void_Ptr;   --  array of void*
      Body_Buf_Sizes : access Size_T;   --  array of size_t
      Num_Of_Body_Buffers : in Size_T) is

      Message_Handler : Incoming_Message_Handlers.Handler_Access :=
        Incoming_Message_Handlers.Handler_Access
        (Message_Handler_Address_To_Access_Conversions.To_Pointer
           (C_Message_Handler_Address));

      Source : String := Interfaces.C.Strings.Value (C_Source);

      Header_Buffer_Descriptor : Serialization_Buffers_Descriptor :=
        (Buffers => Header_Buffers,
         Buffer_Sizes => Header_Buf_Sizes,
         Num_Of_Buffers => Num_Of_Header_Buffers);

      Body_Buffer_Descriptor : Serialization_Buffers_Descriptor :=
        (Buffers => Body_Buffers,
         Buffer_Sizes => Body_Buf_Sizes,
         Num_Of_Buffers => Num_Of_Body_Buffers);

   begin
      Message_Handler.all.Call (Source,
                                Header_Buffer_Descriptor,
                                Body_Buffer_Descriptor);
   exception
      when others =>
         --  ignore all exceptions, protect the C++ part
         null;
   end Incoming_Message_Dispatch_Callback_Translator;

   package Message_Progress_Address_To_Access_Conversions is
      new System.Address_To_Access_Conversions
     (Object => Message_Progress_Handlers.Handler'Class);

   --  helper for translating C++ function callback into Ada interface call
   procedure Message_Progress_Callback_Translator
     (C_Progress_Handler_Address : in Void_Ptr;
      Sent_Bytes : in Size_T;
      Total_Byte_Count : in Size_T);
   pragma Convention (C, Message_Progress_Callback_Translator);

   procedure Message_Progress_Callback_Translator
     (C_Progress_Handler_Address : in Void_Ptr;
      Sent_Bytes : in Size_T;
      Total_Byte_Count : in Size_T) is

      Progress_Handler : Message_Progress_Handlers.Handler_Access :=
        Message_Progress_Handlers.Handler_Access
        (Message_Progress_Address_To_Access_Conversions.To_Pointer
           (C_Progress_Handler_Address));

      use type Message_Progress_Handlers.Handler_Access;
   begin
      Progress_Handler.all.Progress
        (Ada.Streams.Stream_Element_Count (Sent_Bytes),
         Ada.Streams.Stream_Element_Count (Total_Byte_Count));
   exception
      when others =>
         --  ignore all exceptions, protect the C++ part
         null;
   end Message_Progress_Callback_Translator;

   package New_Connection_Address_To_Access_Conversions is
      new System.Address_To_Access_Conversions
     (Object => New_Connection_Handlers.Handler'Class);

   --  helper for translating C++ function callback into Ada interface call
   procedure New_Connection_Callback_Translator
     (C_Connection_Handler_Address : in Void_Ptr;
      C_Source : in Char_Ptr;
      C_Index : in Size_T;
      C_Seq_Num : in Size_T);
   pragma Convention (C, New_Connection_Callback_Translator);

   procedure New_Connection_Callback_Translator
     (C_Connection_Handler_Address : in Void_Ptr;
      C_Source : in Char_Ptr;
      C_Index : in Size_T;
      C_Seq_Num : in Size_T) is

      New_Connection_Handler : New_Connection_Handlers.Handler_Access :=
        New_Connection_Handlers.Handler_Access
        (New_Connection_Address_To_Access_Conversions.To_Pointer
           (C_Connection_Handler_Address));

      Source : String := Interfaces.C.Strings.Value (C_Source);

      Channel : Channel_Descriptor :=
        (Index => C_Index, Sequence_Number => C_Seq_Num);

   begin
      New_Connection_Handler.all.New_Connection (Source, Channel);
   exception
      when others =>
         --  ignore all exceptions, protect the C++ part
         null;
   end New_Connection_Callback_Translator;

   package Closed_Connection_Address_To_Access_Conversions is
      new System.Address_To_Access_Conversions
     (Object => Closed_Connection_Handlers.Handler'Class);

   --  helper for translating C++ function callback into Ada interface call
   procedure Closed_Connection_Callback_Translator
     (C_Closed_Connection_Handler_Address : in Void_Ptr;
      C_Source : in Char_Ptr;
      C_Dummy_Reason : in Int);
   pragma Convention (C, Closed_Connection_Callback_Translator);

   procedure Closed_Connection_Callback_Translator
     (C_Closed_Connection_Handler_Address : in Void_Ptr;
      C_Source : in Char_Ptr;
      C_Dummy_Reason : in Int) is

      Closed_Connection_Handler : Closed_Connection_Handlers.Handler_Access :=
        Closed_Connection_Handlers.Handler_Access
        (Closed_Connection_Address_To_Access_Conversions.To_Pointer
           (C_Closed_Connection_Handler_Address));

      Source : String := Interfaces.C.Strings.Value (C_Source);

   begin
      Closed_Connection_Handler.all.Closed_Connection (Source);
   exception
      when others =>
         --  ignore all exceptions, protect the C++ part
         null;
   end Closed_Connection_Callback_Translator;

   package Event_Notification_Address_To_Access_Conversions is
      new System.Address_To_Access_Conversions
     (Object => Event_Notification_Handlers.Handler'Class);

   --  helper for translating C++ function callback into Ada interface call
   procedure Event_Notification_Callback_Translator
     (C_Event_Notification_Handler_Address : in Void_Ptr;
      C_Event_Type : in Int;
      C_Target : in Char_Ptr;
      C_Size : in Size_T);
   pragma Convention (C, Event_Notification_Callback_Translator);

   procedure Event_Notification_Callback_Translator
     (C_Event_Notification_Handler_Address : in Void_Ptr;
      C_Event_Type : in Int;
      C_Target : in Char_Ptr;
      C_Size : in Size_T) is

      Event_Handler : Event_Notification_Handlers.Handler_Access :=
        Event_Notification_Handlers.Handler_Access
        (Event_Notification_Address_To_Access_Conversions.To_Pointer
           (C_Event_Notification_Handler_Address));
      
      function Safe_From_C (Str : in Char_Ptr) return String is
         use type Char_Ptr;
         use type Size_T;
      begin
         if Str /= Interfaces.C.Strings.Null_Ptr then
            return Interfaces.C.Strings.Value (Str);
         else
            return "";
         end if;
      end Safe_From_C;

      Target : String := Safe_From_C (C_Target);
      Size : Ada.Streams.Stream_Element_Count :=
        Ada.Streams.Stream_Element_Count (C_Size);

   begin
      case C_Event_Type is
         when Details.Agent_Closed =>
           Event_Handler.all.Agent_Closed;
         when Details.Listener_Added =>
            Event_Handler.all.Listener_Added (Target);
         when Details.Listener_Removed =>
            Event_Handler.all.Listener_Removed (Target);
         when Details.Incoming_Connection_Open =>
            Event_Handler.all.Incoming_Connection_Open (Target);
         when Details.Outgoing_Connection_Open =>
            Event_Handler.all.Outgoing_Connection_Open (Target);
         when Details.Connection_Closed =>
            Event_Handler.all.Connection_Closed (Target);
         when Details.Connection_Error =>
            Event_Handler.all.Connection_Error (Target);
         when Details.Message_Sent =>
            Event_Handler.all.Message_Sent (Target, Size);
         when Details.Message_Received =>
            Event_Handler.all.Message_Received (Target, Size);
         when others =>
            pragma Assert (False);
            null;
      end case;
   exception
      when others =>
         --  ignore all exceptions, protect the C++ part
         null;
   end Event_Notification_Callback_Translator;

   --
   --  Operations of the Agent type.
   --

   --  common helper for contructor function
   procedure Common_Agent_Create
     (The_Agent : out Agent;
      Options : in Void_Ptr;
      Message_Handler : in Incoming_Message_Handlers.Handler_Access;
      Closed_Connection_Handler :
      in Closed_Connection_Handlers.Handler_Access;
      Working_Area : in System.Address;
      Area_Size : in System.Storage_Elements.Storage_Count) is

      procedure Agent_Create (C_Agent : out Agent_Value;
                              Options : in Void_Ptr;
                              Incoming_Dispatcher : in Void_Ptr;
                              Incoming_Dispatcher_Hint : in Void_Ptr;
                              Closed_Handler : in Void_Ptr;
                              Closed_Handler_Hint : in Void_Ptr;
                              Working_Area : in Void_Ptr;
                              Size : in Size_T;
                              Result : out Int);
      pragma Import (C, Agent_Create, "agent_create");

      Result : Int;
      use type Int;

      use type System.Address;
      use type System.Storage_Elements.Storage_Count;

   begin
      --  Working_Area and Area_Size must be both zero or both non-zero
      if Working_Area /= System.Null_Address xor Area_Size /= 0 then
         raise Logic_Error with "Invalid working area parameters.";
      end if;

      Agent_Create
        (The_Agent.Value,
         Options,
         Incoming_Message_Dispatch_Callback_Translator'Address,
         Message_Handler_Address_To_Access_Conversions.To_Address
           (Message_Handler_Address_To_Access_Conversions.Object_Pointer
              (Message_Handler)),
         Closed_Connection_Callback_Translator'Address,
         Closed_Connection_Address_To_Access_Conversions.To_Address
           (Closed_Connection_Address_To_Access_Conversions.Object_Pointer
              (Closed_Connection_Handler)),
         Working_Area,
         Size_T (Area_Size),
         Result);
      Check_Result (Result);

      The_Agent.Initialized := True;
   end Common_Agent_Create;

   function Make_Agent
     (Message_Handler : in Incoming_Message_Handlers.Handler_Access;
      Closed_Connection_Handler :
      in Closed_Connection_Handlers.Handler_Access := null;
      Working_Area : in System.Address := System.Null_Address;
      Area_Size : in System.Storage_Elements.Storage_Count := 0)
     return Agent is

   begin
      return The_Agent : Agent do
         Common_Agent_Create
           (The_Agent,
            System.Null_Address, --  no options
            Message_Handler,
            Closed_Connection_Handler,
            Working_Area,
            Area_Size);
      end return;
   end Make_Agent;

   procedure Do_Free is new Ada.Unchecked_Deallocation
     (Object => Agent, Name => Agent_Access);

   function New_Agent
     (Message_Handler : in Incoming_Message_Handlers.Handler_Access;
      Closed_Connection_Handler :
      in Closed_Connection_Handlers.Handler_Access := null;
      Working_Area : in System.Address := System.Null_Address;
      Area_Size : in System.Storage_Elements.Storage_Count := 0)
     return Agent_Access is

      The_Agent : Agent_Access := new Agent;
   begin
      Common_Agent_Create
        (The_Agent.all,
         System.Null_Address, --  no options
         Message_Handler,
         Closed_Connection_Handler,
         Working_Area,
         Area_Size);
      return The_Agent;
   exception
      when others =>
         Do_Free (The_Agent);
         raise;
   end New_Agent;

   function Make_Agent
     (Options : in Parameters.Parameters_Collection;
      Message_Handler : in Incoming_Message_Handlers.Handler_Access;
      Closed_Connection_Handler :
      in Closed_Connection_Handlers.Handler_Access := null;
      Working_Area : in System.Address := System.Null_Address;
      Area_Size : in System.Storage_Elements.Storage_Count := 0)
     return Agent is
   begin
      return The_Agent : Agent do
         Common_Agent_Create
           (The_Agent,
            Options.Core_Object,
            Message_Handler,
            Closed_Connection_Handler,
            Working_Area,
            Area_Size);
      end return;
   end Make_Agent;

   function New_Agent
     (Options : in Parameters.Parameters_Collection;
      Message_Handler : in Incoming_Message_Handlers.Handler_Access;
      Closed_Connection_Handler :
      in Closed_Connection_Handlers.Handler_Access := null;
      Working_Area : in System.Address := System.Null_Address;
      Area_Size : in System.Storage_Elements.Storage_Count := 0)
     return Agent_Access is

      The_Agent : Agent_Access := new Agent;
   begin
      Common_Agent_Create
        (The_Agent.all,
         Options.Core_Object,
         Message_Handler,
         Closed_Connection_Handler,
         Working_Area,
         Area_Size);
      return The_Agent;
   exception
      when others =>
         Do_Free (The_Agent);
         raise;
   end New_Agent;

   procedure Free (The_Agent : in out Agent_Access) is
   begin
      Do_Free (The_Agent);
   end Free;

   procedure Finalize (The_Agent : in out Agent) is

      procedure Destroy_Agent (C_Agent : in out Agent_Value);
      pragma Import (C, Destroy_Agent, "destroy_agent");

   begin
      if The_Agent.Initialized then
         Destroy_Agent (The_Agent.Value);
      end if;
   end Finalize;

   procedure Install_Event_Notification_Handler
     (The_Agent : in out Agent;
      Handler : in Event_Notification_Handlers.Handler_Access) is
      
      procedure Agent_Install_Event_Notifications
        (C_Agent : in out Agent_Value;
         C_Event_Callback : in Void_Ptr;
         C_Event_Callback_Hint : in Void_Ptr);
      pragma Import (C, Agent_Install_Event_Notifications,
                     "agent_install_event_notifications");
      
   begin
      Agent_Install_Event_Notifications
        (The_Agent.Value,
         Event_Notification_Callback_Translator'Address,
         Event_Notification_Address_To_Access_Conversions.To_Address
           (Event_Notification_Address_To_Access_Conversions.Object_Pointer
              (Handler)));
   end Install_Event_Notification_Handler;
        
   procedure Open (The_Agent : in out Agent;
                   Target : in String) is

      procedure Agent_Open (C_Agent : in out Agent_Value;
                            C_Target : in Char_Array;
                            Result : out Int);
      pragma Import (C, Agent_Open, "agent_open");

      Result : Int;

   begin
      Agent_Open (The_Agent.Value, Interfaces.C.To_C (Target), Result);
      Check_Result (Result);
   end Open;

   procedure Open (The_Agent : in out Agent;
                   Target : in String;
                   Descriptor : out Channel_Descriptor;
                   Created_New : out Boolean) is

      procedure Agent_Open_Descr (C_Agent : in out Agent_Value;
                                  C_Target : in Char_Array;
                                  C_Index : out Size_T;
                                  C_Seq_Num : out Size_T;
                                  C_Created_New : out Int;
                                  Result : out Int);
      pragma Import (C, Agent_Open_Descr, "agent_open_descr");

      Index : Size_T;
      Seq_Num : Size_T;
      C_Created_New : Int;
      Result : Int;

      use type Int;

   begin
      Agent_Open_Descr (The_Agent.Value, Interfaces.C.To_C (Target),
                        Index, Seq_Num, C_Created_New, Result);
      Check_Result (Result);

      Descriptor := (Index => Index, Sequence_Number => Seq_Num);
      Created_New := C_Created_New /= 0;
   end Open;

   procedure Is_Open (The_Agent : in out Agent;
                      Target : in String;
                      Result : out Boolean;
                      Descriptor : out Channel_Descriptor) is

      procedure Agent_Is_Open (C_Agent : in out Agent_Value;
                               C_Target : in Char_Array;
                               C_Index : out Size_T;
                               C_Seq_Num : out Size_T;
                               C_Result : out Int);
      pragma Import (C, Agent_Is_Open, "agent_is_open");

      Index : Size_T;
      Seq_Num : Size_T;
      C_Result : Int;

      use type Int;

   begin
      Agent_Is_Open (The_Agent.Value, Interfaces.C.To_C (Target),
                     Index, Seq_Num, C_Result);

      if C_Result = Details.OK then
         --  channel was found
         Descriptor := (Index => Index, Sequence_Number => Seq_Num);
         Result := True;
      elsif C_Result = Details.No_Such_Name then
         --  channel was not found
         Result := False;
      else
         --  error
         Check_Result (C_Result);
      end if;
   end Is_Open;

   procedure Close (The_Agent : in out Agent;
                    Descriptor : in Channel_Descriptor;
                    Priority : in Natural := 0) is

      procedure Agent_Close_CD (C_Agent : in out Agent_Value;
                                C_Index : in Size_T;
                                C_Seq_Num : in Size_T;
                                C_Priority : in Size_T;
                                Result : out Int);
      pragma Import (C, Agent_Close_CD, "agent_close_cd");

      Result : Int;

   begin
      Agent_Close_CD (The_Agent.Value,
                      Descriptor.Index, Descriptor.Sequence_Number,
                      Size_T (Priority), Result);
      Check_Result (Result);
   end Close;

   procedure Close (The_Agent : in out Agent;
                    Target : in String;
                    Priority : in Natural := 0) is

      procedure Agent_Close_Str (C_Agent : in out Agent_Value;
                                 C_Target : in Char_Array;
                                 C_Priority : in Size_T;
                                 Result : out Int);
      pragma Import (C, Agent_Close_Str, "agent_close_str");

      Result : Int;

   begin
      Agent_Close_Str (The_Agent.Value,
                       Interfaces.C.To_C (Target),
                       Size_T (Priority), Result);
      Check_Result (Result);
   end Close;

   procedure Hard_Close (The_Agent : in out Agent;
                         Descriptor : in Channel_Descriptor) is

      procedure Agent_Hard_Close_CD (C_Agent : in out Agent_Value;
                                     C_Index : in Size_T;
                                     C_Seq_Num : in Size_T;
                                     Result : out Int);
      pragma Import (C, Agent_Hard_Close_CD, "agent_hard_close_cd");

      Result : Int;

   begin
      Agent_Hard_Close_CD (The_Agent.Value,
                           Descriptor.Index, Descriptor.Sequence_Number,
                           Result);
      Check_Result (Result);
   end Hard_Close;

   procedure Hard_Close (The_Agent : in out Agent;
                         Target : in String) is

      procedure Agent_Hard_Close_Str (C_Agent : in out Agent_Value;
                                      C_Target : in Char_Array;
                                      Result : out Int);
      pragma Import (C, Agent_Hard_Close_Str, "agent_hard_close_str");

      Result : Int;

   begin
      Agent_Hard_Close_Str (The_Agent.Value,
                            Interfaces.C.To_C (Target),
                            Result);
      Check_Result (Result);
   end Hard_Close;

   procedure Post
     (The_Agent : in out Agent;
      Channel : in Channel_Descriptor;
      Message_Header : in Serializables.Serializable'Class;
      Message_Body : in Serializables.Serializable'Class;
      Priority : in Natural := 0;
      Progress_Handler : in Message_Progress_Handlers.Handler_Access := null)
   is

      procedure Agent_Post_CD (C_Agent : in out Agent_Value;
                               C_Index : in Size_T;
                               C_Seq_Num : in Size_T;
                               C_Header : in Void_Ptr;
                               C_Body : in Void_Ptr;
                               C_Priority : in Size_T;
                               C_Progress_Handler : in Void_Ptr;
                               C_Progress_Handler_Hint : in Void_Ptr;
                               Result : out Int);
      pragma Import (C, Agent_Post_CD, "agent_post_cd");

      Result : Int;

   begin
      Agent_Post_CD
        (The_Agent.Value,
         Channel.Index, Channel.Sequence_Number,
         Message_Header.Core_Object,
         Message_Body.Core_Object,
         Size_T (Priority),
         Message_Progress_Callback_Translator'Address,
         Message_Progress_Address_To_Access_Conversions.To_Address
           (Message_Progress_Address_To_Access_Conversions.Object_Pointer
              (Progress_Handler)),
         Result);
      Check_Result (Result);
   end Post;

   procedure Post
     (The_Agent : in out Agent;
      Target : in String;
      Message_Header : in Serializables.Serializable'Class;
      Message_Body : in Serializables.Serializable'Class;
      Priority : in Natural := 0;
      Progress_Handler : in Message_Progress_Handlers.Handler_Access := null)
   is

      procedure Agent_Post_Str (C_Agent : in out Agent_Value;
                                C_Target : in Char_Array;
                                C_Header : in Void_Ptr;
                                C_Body : in Void_Ptr;
                                C_Priority : in Size_T;
                                C_Progress_Handler : in Void_Ptr;
                                C_Progress_Handler_Hint : in Void_Ptr;
                                Result : out Int);
      pragma Import (C, Agent_Post_Str, "agent_post_str");

      Result : Int;

   begin
      Agent_Post_Str
        (The_Agent.Value,
         Interfaces.C.To_C (Target),
         Message_Header.Core_Object,
         Message_Body.Core_Object,
         Size_T (Priority),
         Message_Progress_Callback_Translator'Address,
         Message_Progress_Address_To_Access_Conversions.To_Address
           (Message_Progress_Address_To_Access_Conversions.Object_Pointer
              (Progress_Handler)),
         Result);
      Check_Result (Result);
   end Post;

   procedure Add_Listener
     (The_Agent : in out Agent;
      Target : in String;
      Resolved_Target : out String;
      Resolved_Target_Last : out Natural;
      Connection_Handler : in New_Connection_Handlers.Handler_Access := null)
   is

      procedure Agent_Add_Listener (C_Agent : in out Agent_Value;
                                    C_Target : in Char_Array;
                                    C_Resolved : access Char_Ptr;
                                    C_Connection_Handler : in Void_Ptr;
                                    C_Connection_Handler_Hint : in Void_Ptr;
                                    Result : out Int);
      pragma Import (C, Agent_Add_Listener, "agent_add_listener");

      C_Resolved : aliased Char_Ptr;
      Result : Int;

   begin
      Agent_Add_Listener
        (The_Agent.Value,
         Interfaces.C.To_C (Target),
         C_Resolved'Access,
         New_Connection_Callback_Translator'Address,
         New_Connection_Address_To_Access_Conversions.To_Address
           (New_Connection_Address_To_Access_Conversions.Object_Pointer
              (Connection_Handler)),
         Result);
      Check_Result (Result);

      declare
         Resolved_Target_Tmp : constant String :=
           Interfaces.C.Strings.Value (C_Resolved);
         Length : Natural := Resolved_Target_Tmp'Length;
      begin
         Resolved_Target
           (Resolved_Target'First .. Resolved_Target'First + Length - 1) :=
           Resolved_Target_Tmp;
         Resolved_Target_Last := Resolved_Target'First + Length - 1;
      end;
   end Add_Listener;

   procedure Remove_Listener (The_Agent : in out Agent; Target : in String) is

      procedure Agent_Remove_Listener (C_Agent : in out Agent_Value;
                                       C_Target : in Char_Array;
                                       Result : out Int);
      pragma Import (C, Agent_Remove_Listener, "agent_remove_listener");

      Result : Int;

   begin
      Agent_Remove_Listener
        (The_Agent.Value, Interfaces.C.To_C (Target), Result);
      Check_Result (Result);
   end Remove_Listener;

   procedure Do_Some_Work
     (The_Agent : in out Agent;
      Timeout : in Duration;
      Timed_Out : out Boolean;
      Allow_Outgoing_Traffic : in Boolean := True;
      Allow_Incoming_Traffic : in Boolean := True) is

      procedure Agent_Do_Some_Work (C_Agent : in out Agent_Value;
                                    C_Timeout : in Size_T;
                                    C_Allow_Outgoing : in Int;
                                    C_Allow_Incoming : in Int;
                                    Result : out Int);
      pragma Import (C, Agent_Do_Some_Work, "agent_do_some_work");

      C_Timeout : Size_T := Size_T (Timeout * 1000);
      C_Allow_Outgoing : Int;
      C_Allow_Incoming : Int;

      Result : Int;
      use type Int;

   begin
      if Allow_Outgoing_Traffic then
         C_Allow_Outgoing := 1;
      else
         C_Allow_Outgoing := 0;
      end if;

      if Allow_Incoming_Traffic then
         C_Allow_Incoming := 1;
      else
         C_Allow_Incoming := 0;
      end if;

      Agent_Do_Some_Work (The_Agent.Value,
                          C_Timeout,
                          C_Allow_Outgoing, C_Allow_Incoming,
                          Result);

      if Result = 11 then
         --  the operation timed out
         --  (there was no work to do within the specified time)
         --  but this is not an error here
         Timed_Out := True;
         Result := 0;
      else
         Timed_Out := False;
      end if;

      Check_Result (Result);
   end Do_Some_Work;

   procedure Interrupt_Work_Waiter (The_Agent : in out Agent) is

      procedure Agent_Interrupt_Work_Waiter (C_Agent : in out Agent_Value;
                                             Result : out Int);
      pragma Import (C, Agent_Interrupt_Work_Waiter,
                     "agent_interrupt_work_waiter");

      Result : Int;

   begin
      Agent_Interrupt_Work_Waiter (The_Agent.Value, Result);
      Check_Result (Result);
   end Interrupt_Work_Waiter;

end YAMI.Core.Agents;
