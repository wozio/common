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

with YAMI.Parameters;
with YAMI.Serializables;

package Queue.Routing is

   pragma Elaborate_Body;
   
   No_Such_Queue_Error : exception;
   Queue_Overflow_Error : exception;
   No_Message_Ready_Error : exception;

   procedure Init (Max_Queue_Num_Of_Messages_Config : Positive;
                   Max_Queue_Size_Config : in Positive;
                   Queue_Creation_Policy_Config : in Creation_Policy);

   --
   --  Creates new queue with the given name.
   --
   procedure Create (Queue_Name : in String);

   --
   --  Type of the procedure that is called to transmit the message
   --  to one of the waiting clients.
   --
   type Dispatch_Procedure is not null access procedure
        (Target : in String;
         Message_Id : in YAMI.Parameters.YAMI_Long_Long_Integer;
         Body_Buffers : in YAMI.Serializables.Serializable'Class);

   --
   --  Adds a new message to the queue.
   --  The Dispatch procedure is called if the new message
   --  matches some existing waiting client.
   --
   procedure Put (Queue_Name : in String;
      Body_Buffers : in YAMI.Serializables.Serializable'Class;
      Dispatch : Dispatch_Procedure);
   
   --
   --  Adds new waiting client to the queue.
   --  The Dispatch procedure is called if the new client
   --  comes when there is at least one message in the queue,
   --  otherwise the client is added to the queue of waiting clients.
   --
   procedure Get (Queue_Name : in String;
      Target : in String;
      Requesting_Message_Id : YAMI.Parameters.YAMI_Long_Long_Integer;
      Dispatch : Dispatch_Procedure);
      
   --
   --  Checks if there is a message ready in the queue
   --  and returns it to the requesting client.
   --  The Dispatch procedure is called if the new client
   --  comes when there is at least one message in the queue,
   --  otherwise the request is rejected.
   --
   procedure Try_Get (Queue_Name : in String;
      Target : in String;
      Requesting_Message_Id : YAMI.Parameters.YAMI_Long_Long_Integer;
      Dispatch : Dispatch_Procedure);

   --
   --  Fills in the given parameters object with statistics information
   --  of two different levels of details.
   --  Simple statistics just gives the number of existing queues and
   --  the number of waiting messages and waiting clients.
   --  Detailed stats lists number of messages and clients for all queues.
   --
   procedure Fill_Simple_Stats
     (Params : in out YAMI.Parameters.Parameters_Collection);
   procedure Fill_Detailed_Stats
     (Params : in out YAMI.Parameters.Parameters_Collection);

end Queue.Routing;
