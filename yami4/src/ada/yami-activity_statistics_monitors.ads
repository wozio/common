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

with YAMI.Event_Notification_Handlers;
with YAMI.Incoming_Messages;
with YAMI.Parameters;

with Ada.Calendar;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Streams;
with Ada.Strings.Hash;

package YAMI.Activity_Statistics_Monitors is
   
   type Statistics is private;
   
   --
   --  Simple activity statistics monitor.
   --
   --  This class defines the basic stats monitor that is based on
   --  the event notification mechanism. The monitor can also be used for
   --  remote inspection of the collected data, as it can directly
   --  play the role of regular object that implements the "get" command.
   --
   protected type Activity_Statistics_Monitor is
      new Event_Notification_Handlers.Handler
     and Incoming_Messages.Message_Handler with
     
     --
     --  Operations of the event notifications handler interface.
     --
     
     overriding procedure Agent_Created;
   
     overriding procedure Agent_Closed;
     
     overriding procedure Listener_Added (Target : in String);
     
     overriding procedure Listener_Removed (Target : in String);
     
     overriding procedure Incoming_Connection_Open (Target : in String);
     
     overriding procedure Outgoing_Connection_Open (Target : in String);
     
     overriding procedure Connection_Closed (Target : in String);
     
     overriding procedure Connection_Error (Target : in String);
     
     overriding procedure Object_Registered (Name : in String);
     
     overriding procedure Object_Unregistered (Name : in String);
     
     overriding procedure Message_Sent
       (Target : in String;
        Size : in Ada.Streams.Stream_Element_Count);
     
     overriding procedure Message_Received
       (Target : in String;
        Size : in Ada.Streams.Stream_Element_Count);
     
     --
     --  Operations of the incoming message handler.
     --
     
     overriding procedure Call
       (Message : in out Incoming_Messages.Incoming_Message'Class);
     
     --
     --  Retrieves all collected statistics.
     --
     procedure Get (Params : in out Parameters.Parameters_Collection;
                    Reset_Counters : in Boolean);
     
   private
      
      Stats : Statistics;
      
   end Activity_Statistics_Monitor;
   
private
   
   type Connection_Type is (Incoming, Outgoing);
   
   type Connection_Stats is record
      Conn_Type : Connection_Type;
      Messages_Sent : Natural := 0;
      Messages_Received : Natural := 0;
      Bytes_Sent : Ada.Streams.Stream_Element_Count := 0;
      Bytes_Received : Ada.Streams.Stream_Element_Count := 0;
   end record;
   
   procedure Reset (S : out Connection_Stats);
   
   package String_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type => String,
      Hash => Ada.Strings.Hash,
      Equivalent_Elements => "=");
   
   package Connection_Stats_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => Connection_Stats,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   package Connection_Errors_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => Natural,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Statistics is record
      Started : Ada.Calendar.Time;
      Listeners : String_Sets.Set;
      Objects : String_Sets.Set;
      Conn_Stats : Connection_Stats_Maps.Map;
      Conn_Errors : Connection_Errors_Maps.Map;
   end record;

end YAMI.Activity_Statistics_Monitors;
