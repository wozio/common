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

with YAMI.Core.Agents;
with YAMI.Connection_Event_Handlers;
with YAMI.Details.Core_Connection_Handlers;
private with YAMI.Details.Dispatch_Managers;
private with YAMI.Details.Handlers;
private with YAMI.Details.Id_Generators;
private with YAMI.Details.Name_Resolvers;
private with YAMI.Details.Options;
private with YAMI.Details.Outgoing_Message_Managers;
private with YAMI.Details.Water_Flow_Managers;
private with YAMI.Details.Worker_Tasks;
with YAMI.Event_Notification_Handlers;
with YAMI.Incoming_Messages;
with YAMI.Outgoing_Messages;
with YAMI.Parameters;
with YAMI.Serializables;

with Ada.Finalization;

package YAMI.Agents is

   --
   --  Maximum length of the target name.
   --
   Max_Target_Length : constant := 300;

   --
   --  Message broker.
   --
   --  The message broker that encapsulates physical channel management,
   --  incoming and outgoing message queues, listeners, resource
   --  management and working tasks.
   --
   --  A single agent object can manage many listeners, which are responsible
   --  for accepting remote connections, and many incoming and outgoing
   --  connections.
   --
   --  The agent objects can be created and destroyed without constraints
   --  on the stack, dynamically or as library-level objects.
   --
   type Agent (<>) is tagged limited private;

   --
   --  Access type designating the agent.
   --
   type Agent_Access is access all Agent;

   --
   --  Operations of the Agent type.
   --

   --
   --  Creates new agent object with default option values
   --  and starts its internal tasks.
   --  Alternative New_Agent function allocates the agent object dynamically.
   --
   function Make_Agent return Agent;
   function New_Agent return Agent_Access;

   --
   --  Creates new agent object with the given option values.
   --  Alternative New_Agent function allocates the agent object dynamically.
   --
   function Make_Agent (Options : in Parameters.Parameters_Collection)
                       return Agent;
   function New_Agent (Options : in Parameters.Parameters_Collection)
                      return Agent_Access;

   --
   --  Creates new agent object with event notification handler and
   --  the given option values.
   --  Alternative New_Agent function allocates the agent object dynamically.
   --
   function Make_Agent
     (Event_Handler : in Event_Notification_Handlers.Handler_Access;
      Options : in Parameters.Parameters_Collection)
     return Agent;
   function New_Agent
     (Event_Handler : in Event_Notification_Handlers.Handler_Access;
      Options : in Parameters.Parameters_Collection)
     return Agent_Access;

   --
   --  Frees the dynamically allocated agent object.
   --
   procedure Free (The_Agent : in out Agent_Access);

   --
   --  Adds new listener.
   --
   --  Adds a new listener for the given target address.
   --
   --  The supported target formats are:
   --  - "tcp://host:port" for TCP/IP connections, where <code>host</code>
   --    can be provided in the symbolic or numeric form
   --  - "tcp://*:port" for TCP/IP connections, for "any" local address
   --  - "tcp://port" for TCP/IP connections, for "any" local address
   --  - "udp://host:port" for UDP communication, with rules as for TCP/IP
   --  - "unix://path" for Unix connections
   --
   --  The port for TCP/IP and UDP protocols can be
   --  <code>0</code> or <code>*</code>,
   --  in which case the actual port number is assigned by the system.
   --
   procedure Add_Listener
     (The_Agent : in out Agent;
      Target : in String;
      Resolved_Target : out String;
      Resolved_Target_Last : out Natural);

   --
   --  Adds new listener.
   --
   procedure Add_Listener (The_Agent : in out Agent; Target : in String);

   --
   --  Removes existing listener.
   --
   --  Removes the listener denoted by its actual target name.
   --  Note that the actual target name might be different from the name
   --  provided when the listener was created, due to target resolution.
   --
   procedure Remove_Listener (The_Agent : in out Agent; Target : in String);

   --
   --  Registers the new logical destination object.
   --
   procedure Register_Object
     (The_Agent : in out Agent;
      Object_Name : in String;
      Handler : in Incoming_Messages.Message_Handler_Access);

   --
   --  Unregisters the logical destination object.
   --
   procedure Unregister_Object (The_Agent : in out Agent;
                                Object_Name : in String);

   --
   --  Opens the new channel or does nothing if the channel already exists.
   --
   --  This function is not necessary with automatic connection
   --  recovery option in Send and Send_One_Way.
   --
   procedure Open_Connection (The_Agent : in out Agent;
                              Target : in String);

   --
   --  Sends the new outgoing message to the given destination.
   --
   --  Note:
   --  This function implicitly opens a new communication channel
   --  if it is not already open. This channel is kept open until
   --  it is explicitly closed
   --  (see the <code>close_connection</code> function)
   --  or until the agent is destroyed or the communication error
   --  is detected.
   --
   procedure Send
     (The_Agent : in out Agent;
      Target : in String;
      Object_Name : in String;
      Message_Name : in String;
      Content : in Serializables.Serializable'Class;
      Message_Handler : in Outgoing_Messages.Outgoing_Message_Access;
      Priority : in Natural := 0;
      Auto_Connect : in Boolean := True);
   procedure Send
     (The_Agent : in out Agent;
      Target : in String;
      Object_Name : in String;
      Message_Name : in String;
      Message_Handler : in Outgoing_Messages.Outgoing_Message_Access;
      Priority : in Natural := 0;
      Auto_Connect : in Boolean := True);

   --
   --  Sends the new outgoing message.
   --
   --  Sends the new outgoing message to the given destination, without
   --  the possibility to track its progress.
   --
   --  See the description and notes for the <code>Send</code> procedure.
   --
   procedure Send_One_Way (The_Agent : in out Agent;
                           Target : in String;
                           Object_Name : in String;
                           Message_Name : in String;
                           Content : in Serializables.Serializable'Class;
                           Priority : in Natural := 0;
                           Auto_Connect : in Boolean := True);
   procedure Send_One_Way (The_Agent : in out Agent;
                           Target : in String;
                           Object_Name : in String;
                           Message_Name : in String;
                           Priority : in Natural := 0;
                           Auto_Connect : in Boolean := True);

   --
   --  Closes the given communication channel.
   --
   --  The priority allows to properly handle the existing outgoing
   --  messages that are waiting in the outgoing queue for transmission.
   --  The existing messages with lower priority are
   --  abandoned, whereas the existing messages with priority equal
   --  or higher to the one provided as parameter are retained in the
   --  outgoing queue and are properly pushed for transmission
   --  before the channel is physically closed.
   --  The channel is closed immediately only if there are no
   --  messages waiting in its outgoing queue.
   --
   procedure Close_Connection (The_Agent : in out Agent;
                               Target : in String;
                               Priority : in Natural := 0);

   --
   --  Immediately closes the given channel.
   --
   --  Closes the channel identified by the given descriptor.
   --
   --  The channel is closed immediately and those messages that are
   --  waiting in its outgoing queue are abandoned. Integrity of the
   --  message that was already partly transmitted is not guaranteed.
   --
   procedure Hard_Close_Connection (The_Agent : in out Agent;
                                    Target : in String);

   --
   --  Registers the monitor for connection-related events.
   --
   --  Note:
   --  The monitor callback is intentionally not synchronized.
   --  Use this procedure after constructing the agent, but before
   --  opening any connections.
   --
   procedure Register_Connection_Event_Monitor
     (The_Agent : in out Agent;
      User_Event_Handler : Connection_Event_Handlers.Handler_Access);

   --
   --  Obtains the state of overall outgoing flow.
   --
   --  Note:
   --  The outgoing flow is a combination of all outgoing traffic,
   --  and is not tied to any particular communication channel.
   --
   procedure Get_Outgoing_Flow_State
     (The_Agent : in out Agent;
      Current_Level : out Parameters.Count_Type;
      High_Water_Mark : out Parameters.Count_Type;
      Low_Water_Mark : out Parameters.Count_Type);

private

   type Agent is new Ada.Finalization.Limited_Controlled with record

      --  decoded option values
      Options : Details.Options.Option_Values;

      --  the core part, allocated dynamically (unknown discriminants)
      Core_Agent : Core.Agents.Agent_Access;

      --  low-level message handler
      Core_Message_Handler : aliased
        Details.Handlers.Core_Incoming_Message_Handler;

      --  low-level progress handler for one-way messages
      Core_One_Way_Progress_Handler : aliased
        Details.Handlers.One_Way_Message_Progress_Handler;

      --  low-level handler for connection events (closing and new incoming)
      Core_Connection_Handler : aliased
        Details.Core_Connection_Handlers.Closed_Or_New_Connection_Handler;

      --  name resolver for message routing
      Object_Map : aliased Details.Name_Resolvers.Name_Resolver;

      --  incoming message dispatch manager
      Dispatcher : aliased Details.Dispatch_Managers.Dispatch_Manager;

      --  message id generator
      Id_Generator : Details.Id_Generators.Id_Generator;

      --  outgoing message manager
      Outgoing_Msg_Manager : aliased
        Details.Outgoing_Message_Managers.Manager;

      --  flow control
      Outgoing_Flow : aliased Details.Water_Flow_Managers.Water_Flow_Manager;
      Incoming_Flow : aliased Details.Water_Flow_Managers.Water_Flow_Manager;

      --  background worker task
      Worker : Details.Worker_Tasks.Worker_Access;
      
      --  event notification callback
      Event_Handler : Event_Notification_Handlers.Handler_Access := null;

   end record;

   overriding
   procedure Finalize (The_Agent : in out Agent);

end YAMI.Agents;
