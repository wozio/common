--  Copyright Maciej Sobczak 2008-2014.
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

with Log;
with YAMI.Raw_Buffer_Data_Sources;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Streams;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package body Queue.Routing is

   --
   --  Internal data structures involve:
   --  1. map of queues, keyed by queue name
   --  2. each queue in the map is composed of actual bounded message queue
   --     and a queue of clients waiting for messages (waiting client queue)
   --
   --  The message/request flow is defined as:
   --
   --  1. when a new message arrives for the given queue:
   --     1a. if the queue does not exist and the creation policy is "static"
   --         then the message is rejected,
   --         otherwise ("dynamic") the given queue is created, and:
   --     1b. if there are waiting clients,
   --         the message is routed to the first waiting client,
   --         which is then removed from the waiting client queue,
   --         otherwise (if there are no waiting clients) the message is
   --         added to the message queue, in which case:
   --         - if the queue is already full the message is either rejected
   --           or some message is dropped, according to the overflow policy
   --
   --     if the message was not rejected, it is confirmed with empty reply
   --
   --  2. when a new client requests his message:
   --     2a. if there is at least one message in the message queue,
   --         it is popped from the queue and sent to the requesting client,
   --         otherwise (if there are no messages) the client is added
   --         to the waiting client queue, in which case:
   --         - if the waiting client queue is already full
   --           the request is rejected
   --
   --  3. when a new client try-requests (try-get) his message:
   --     3a. if there is at least one message in the message queue,
   --         it is popped from the queue and sent to the requesting client,
   --         otherwise (if there are no messages) the request is
   --         immediately rejected
   --
   --  Messages are sent to clients as responses to their original requests.
   --

   --
   --  Type of a single message (content) queue.
   --
   package Message_Content_Queues is
      new Ada.Containers.Doubly_Linked_Lists
      (Element_Type => YAMI.Serializables.Serialization_Buffer_Access,
       "=" => YAMI.Serializables."=");
   
   --
   --  Information about a single client waiting on the given queue.
   --
   type Waiting_Client is record
      Target : Unbounded_String;
      Requesting_Message_Id : YAMI.Parameters.YAMI_Long_Long_Integer;
   end record;
   
   --
   --  Type of queue of waiting clients.
   --
   package Waiting_Clients_Queues is new Ada.Containers.Doubly_Linked_Lists
      (Element_Type => Waiting_Client);
   
   --
   --  Bundle of content queue and client queue.
   --
   type Routing_Queue is record
      Message_Content_Queue : Message_Content_Queues.List;
      Total_Content_Size : Ada.Streams.Stream_Element_Count := 0;
      Waiting_Clients_Queue : Waiting_Clients_Queues.List;
   end record;
   
   --  Dummy helper to satisfy Hashed_Maps dependency.
   function Dummy_Equal_Queues (A : in Routing_Queue; B : in Routing_Queue)
      return Boolean is
   begin
      raise Program_Error with "This function should never be called.";
      return False;
   end Dummy_Equal_Queues;
   
   --
   --  Type of map that manages the complete routing state.
   --
   package Routing_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type => String,
       Element_Type => Routing_Queue,
       Hash => Ada.Strings.Hash,
       Equivalent_Keys => "=",
       "=" => Dummy_Equal_Queues);


   Max_Queue_Num_Of_Messages : Ada.Containers.Count_Type;
   Max_Queue_Size : Ada.Streams.Stream_Element_Count;
   Queue_Creation_Policy : Creation_Policy;

   --  Note: there is no need to make it protected
   --  as there is only one task accessing this structure
   Routing_Map : Routing_Maps.Map;
   
   procedure Free is new Ada.Unchecked_Deallocation
     (Object => YAMI.Serializables.Serialization_Buffer,
      Name => YAMI.Serializables.Serialization_Buffer_Access);

   procedure Init (Max_Queue_Num_Of_Messages_Config : Positive;
                   Max_Queue_Size_Config : in Positive;
                   Queue_Creation_Policy_Config : in Creation_Policy) is
   begin
      Max_Queue_Num_Of_Messages :=
         Ada.Containers.Count_Type (Max_Queue_Num_Of_Messages_Config);
      Max_Queue_Size :=
         Ada.Streams.Stream_Element_Count (Max_Queue_Size_Config);
      Queue_Creation_Policy := Queue_Creation_Policy_Config;
   end Init;

   procedure Create (Queue_Name : in String) is
      Dummy_Position : Routing_Maps.Cursor;
      Inserted : Boolean;
   begin
      Routing_Map.Insert (Queue_Name, Routing_Queue'(others => <>),
         Dummy_Position, Inserted);
      
      if Inserted then
         Log.Put (Queue.Queues, "created queue " & Queue_Name);
      end if;
   end Create;

   --  helper for finding and dynamic creation (if requested) of queues
   procedure Find_Queue
     (Queue_Name : in String;
      Queue_Position : out Routing_Maps.Cursor;
      New_Queue_Inserted : out Boolean;
      Create_If_Not_Exists : in Boolean) is
      
      use type Routing_Maps.Cursor;
   begin
      Queue_Position := Routing_Map.Find (Queue_Name);
      if Queue_Position = Routing_Maps.No_Element then
         if Queue_Creation_Policy = Dynamic and
            Create_If_Not_Exists then
            
            Routing_Map.Insert (Queue_Name, Routing_Queue'(others => <>),
               Queue_Position, New_Queue_Inserted);

            Log.Put (Queue.Queues, "created queue " & Queue_Name);
         else
            raise No_Such_Queue_Error with "There is no such queue.";
         end if;
      else
         New_Queue_Inserted := False;
      end if;
   end Find_Queue;
   
   procedure Put (Queue_Name : in String;
      Body_Buffers : in YAMI.Serializables.Serializable'Class;
      Dispatch : Dispatch_Procedure) is
      
      Queue_Position : Routing_Maps.Cursor;
      New_Queue_Inserted : Boolean;
      Should_Delete : Boolean := False;
      
      procedure Process_Queue (Key : in String;
                               Queue : in out Routing_Queue) is
                               
         Current_Num_Of_Messages : Ada.Containers.Count_Type;
         use type Ada.Containers.Count_Type;
         use type Ada.Streams.Stream_Element_Count;
      begin
         if New_Queue_Inserted or else
            Queue.Waiting_Clients_Queue.Is_Empty then
         
            --  there are no clients waiting in this queue,
            --  just store the message

            Current_Num_Of_Messages := Queue.Message_Content_Queue.Length;
            if Current_Num_Of_Messages = Max_Queue_Num_Of_Messages then
               raise Queue_Overflow_Error
                  with "Maximum length of the queue has been reached.";
            end if;
            
            --  copy the incoming buffer
            --  using standard serialization features
            declare
               New_Message_Size : Ada.Streams.Stream_Element_Count;
            begin
               New_Message_Size := Body_Buffers.Serialize_Buffer_Size;
               if Queue.Total_Content_Size + New_Message_Size >
                  Max_Queue_Size then
                  
                  raise Queue_Overflow_Error
                     with "Maximum size of the queue has been reached.";
               end if;
            
               declare
                  Buffer : YAMI.Serializables.Serialization_Buffer_Access;
                  Buffer_List :
                     YAMI.Serializables.Serialization_Buffer_List (1 .. 1);
               begin
                  Buffer := new YAMI.Serializables.Serialization_Buffer
                    (New_Message_Size);
                  Buffer_List (1) := Buffer;

                  Body_Buffers.Serialize (Buffer_List);

                  Queue.Message_Content_Queue.Append (Buffer);
                  
                  Queue.Total_Content_Size :=
                     Queue.Total_Content_Size + New_Message_Size;
               exception
                  when others =>
                     Free (Buffer);
                     raise;
               end;
            end;
         else
            --  there is at least one client waiting for the message
            --  - send the incoming message to that client and
            --    remove him from the queue of waiting clients
            
            declare
               procedure Process_First_Waiting_Client
                 (Client : in Waiting_Client) is
               begin
                  Dispatch.all (To_String (Client.Target),
                                Client.Requesting_Message_Id,
                                Body_Buffers);
               end Process_First_Waiting_Client;
            begin
               Waiting_Clients_Queues.Query_Element
                 (Queue.Waiting_Clients_Queue.First,
                  Process_First_Waiting_Client'Access);

               Queue.Waiting_Clients_Queue.Delete_First;
               
               --  if it was the only waiting client, then
               --  now both content queue and waiting client queue are empty
               --  - in this case tear down the whole structure
               
               Should_Delete := Queue.Waiting_Clients_Queue.Is_Empty;
            end;
         end if;
      end Process_Queue;

   begin
      Find_Queue (Queue_Name, Queue_Position, New_Queue_Inserted,
         Queue_Creation_Policy = Dynamic);
      
      Routing_Map.Update_Element (Queue_Position, Process_Queue'Access);
      
      if Should_Delete then
         Routing_Map.Delete (Queue_Position);
         
         Log.Put (Queues, "deleted queue " & Queue_Name);
      end if;
   end Put;
   
   --  helper, common part for both Get and Try_Get
   procedure Common_Get (Queue_Name : in String;
      Target : in String;
      Requesting_Message_Id : YAMI.Parameters.YAMI_Long_Long_Integer;
      Dispatch : Dispatch_Procedure;
      Queue_Client_If_No_Message_Ready : in Boolean;
      Create_Queue_If_Not_Exists : in Boolean) is

      Queue_Position : Routing_Maps.Cursor;
      New_Queue_Inserted : Boolean;
      Should_Delete : Boolean := False;

      procedure Process_Queue (Key : in String;
                               Queue : in out Routing_Queue) is
      begin
         if New_Queue_Inserted or else
            Queue.Message_Content_Queue.Is_Empty then
         
            --  there are no messages in this queue
            
            if Queue_Client_If_No_Message_Ready then
               --  add the new client to the waiting clients queue

               Queue.Waiting_Clients_Queue.Append
                 (Waiting_Client'(To_Unbounded_String (Target),
                                  Requesting_Message_Id));
            else
               raise No_Message_Ready_Error with "This queue is empty.";
            end if;
         else
            --  there is at least one message in the queue
            --  - send this message to the requesting client and
            --    remove it from the message queue
            
            declare
               Size_Of_First_Message : Ada.Streams.Stream_Element_Count;
               
               procedure Process_First_Message
                 (Body_Buffer :
                     in YAMI.Serializables.Serialization_Buffer_Access) is

                  Buffer_List :
                     YAMI.Serializables.Serialization_Buffer_List (1 .. 1);
                  Raw_Buffer :
                     YAMI.Raw_Buffer_Data_Sources.Raw_Buffer_Data_Source (1);
               begin
                  Size_Of_First_Message := Body_Buffer.Size;
                  
                  Buffer_List (1) := Body_Buffer;
                  YAMI.Raw_Buffer_Data_Sources.Init_Raw_Buffer_Data_Source
                    (Raw_Buffer, Buffer_List);
                  
                  Dispatch.all (Target,
                                Requesting_Message_Id,
                                Raw_Buffer);
               end Process_First_Message;
               
               use type Ada.Streams.Stream_Element_Offset;
            begin
               Message_Content_Queues.Query_Element
                 (Queue.Message_Content_Queue.First,
                  Process_First_Message'Access);

               Queue.Message_Content_Queue.Delete_First;
               Queue.Total_Content_Size :=
                  Queue.Total_Content_Size - Size_Of_First_Message;
               
               --  if it was the only message in the queue, then
               --  now both content queue and waiting client queue are empty
               --  - in this case tear down the whole structure
               
               Should_Delete := Queue.Message_Content_Queue.Is_Empty;
            end;
         end if;
      end Process_Queue;

   begin
      Find_Queue (Queue_Name, Queue_Position, New_Queue_Inserted,
         Create_Queue_If_Not_Exists);
      
      Routing_Map.Update_Element (Queue_Position, Process_Queue'Access);
      
      if Queue_Creation_Policy = Dynamic and Should_Delete then
         Routing_Map.Delete (Queue_Position);

         Log.Put (Queue.Queues, "deleted queue " & Queue_Name);
      end if;
   end Common_Get;
   
   procedure Get (Queue_Name : in String;
      Target : in String;
      Requesting_Message_Id : YAMI.Parameters.YAMI_Long_Long_Integer;
      Dispatch : Dispatch_Procedure) is
   begin
      Common_Get (Queue_Name, Target, Requesting_Message_Id, Dispatch,
         True, Queue_Creation_Policy = Dynamic);
   end Get;
   
   procedure Try_Get (Queue_Name : in String;
      Target : in String;
      Requesting_Message_Id : YAMI.Parameters.YAMI_Long_Long_Integer;
      Dispatch : Dispatch_Procedure) is
   begin
      Common_Get (Queue_Name, Target, Requesting_Message_Id, Dispatch,
         False, False);
   end Try_Get;
   
   procedure Fill_Simple_Stats
     (Params : in out YAMI.Parameters.Parameters_Collection) is
     
      Num_Of_Waiting_Messages : Ada.Containers.Count_Type := 0;
      Total_Of_Content_Sizes : Ada.Streams.Stream_Element_Count := 0;
      Num_Of_Waiting_Clients : Ada.Containers.Count_Type := 0;
      
      use type Ada.Containers.Count_Type;
      use type Ada.Streams.Stream_Element_Count;
      
      procedure Count_Messages_And_Clients
        (C : Routing_Maps.Cursor) is
        
        Queue_Name : String renames Routing_Maps.Key (C);
        Queue : Routing_Queue renames Routing_Maps.Element (C);
      begin
         Num_Of_Waiting_Messages := Num_Of_Waiting_Messages +
            Queue.Message_Content_Queue.Length;
         
         Total_Of_Content_Sizes := Total_Of_Content_Sizes +
            Queue.Total_Content_Size;
         
         Num_Of_Waiting_Clients := Num_Of_Waiting_Clients +
            Queue.Waiting_Clients_Queue.Length;
      end Count_Messages_And_Clients;
     
   begin
      Params.Set_Integer
        ("num_of_queues", YAMI.Parameters.YAMI_Integer (Routing_Map.Length));

      Routing_Map.Iterate (Count_Messages_And_Clients'Access);
      
      Params.Set_Integer
        ("num_of_all_waiting_messages",
         YAMI.Parameters.YAMI_Integer (Num_Of_Waiting_Messages));
      Params.Set_Long_Long
        ("total_of_all_content_sizes",
         YAMI.Parameters.YAMI_Long_Long_Integer (Total_Of_Content_Sizes));
      Params.Set_Integer
        ("num_of_all_waiting_clients",
         YAMI.Parameters.YAMI_Integer (Num_Of_Waiting_Clients));
   end Fill_Simple_Stats;
   
   procedure Fill_Detailed_Stats
     (Params : in out YAMI.Parameters.Parameters_Collection) is
     
      Num_Of_Queues : Ada.Containers.Count_Type := Routing_Map.Length;
      Queue_Names_Field_Name : constant String := "queue_names";
      
      use type Ada.Containers.Count_Type;

   begin
      if Num_Of_Queues = 0 then
         return;
      end if;

      declare      
         type Counter_Array_Type is
            array (YAMI.Parameters.Index_Type range <>)
            of Ada.Containers.Count_Type;
      
         type Size_Array_Type is
            array (YAMI.Parameters.Index_Type range <>)
            of Ada.Streams.Stream_Element_Count;
      
         Num_Of_Waiting_Messages :
            Counter_Array_Type (1 .. YAMI.Parameters.Index_Type (Num_Of_Queues));
         Total_Content_Sizes :
            Size_Array_Type (1 .. YAMI.Parameters.Index_Type (Num_Of_Queues));
         Num_Of_Waiting_Clients :
            Counter_Array_Type (1 .. YAMI.Parameters.Index_Type (Num_Of_Queues));
      
         procedure Set_Counters_Array is
            new YAMI.Parameters.Set_Integer_Array
              (Index_Type => YAMI.Parameters.Index_Type,
               Integer_Type => Ada.Containers.Count_Type,
               Integer_Array_Type => Counter_Array_Type);
      
         procedure Set_Sizes_Array is
            new YAMI.Parameters.Set_Long_Long_Array
              (Index_Type => YAMI.Parameters.Index_Type,
               Long_Long_Integer_Type => Ada.Streams.Stream_Element_Count,
               Long_Long_Integer_Array_Type => Size_Array_Type);
      
         C : Routing_Maps.Cursor;
         Index : YAMI.Parameters.Index_Type := 1;
      
         use type Routing_Maps.Cursor;
         use type YAMI.Parameters.Index_Type;
      
      begin
         Params.Create_String_Array (Queue_Names_Field_Name,
            YAMI.Parameters.YAMI_Integer (Num_Of_Queues));
      
         C := Routing_Map.First;
         while C /= Routing_Maps.No_Element loop
            declare
               Queue_Name : String := Routing_Maps.Key (C);
               Queue : Routing_Queue renames Routing_Maps.Element (C);
            begin
               Params.Set_String_In_Array
                 (Queue_Names_Field_Name, Index, Queue_Name);
            
               Num_Of_Waiting_Messages (Index) :=
                  Queue.Message_Content_Queue.Length;
               Total_Content_Sizes (Index) :=
                  Queue.Total_Content_Size;
               Num_Of_Waiting_Clients (Index) :=
                  Queue.Waiting_Clients_Queue.Length;
            end;
         
            Index := Index + 1;
            Routing_Maps.Next (C);
         end loop;
      
         Set_Counters_Array
            (Params, "num_of_waiting_messages", Num_Of_Waiting_Messages);
         Set_Sizes_Array
            (Params, "total_content_sizes", Total_Content_Sizes);
         Set_Counters_Array
            (Params, "num_of_waiting_clients", Num_Of_Waiting_Clients);
      end;   
   end Fill_Detailed_Stats;
   
end Queue.Routing;
