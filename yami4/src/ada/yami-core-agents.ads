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

with YAMI.Core.Closed_Connection_Handlers;
with YAMI.Core.Event_Notification_Handlers;
with YAMI.Core.Incoming_Message_Handlers;
with YAMI.Core.Message_Progress_Handlers;
with YAMI.Core.New_Connection_Handlers;
private with YAMI.Details;
with YAMI.Parameters;
with YAMI.Serializables;

with Ada.Finalization;
with Interfaces.C;
with System.Storage_Elements;

package YAMI.Core.Agents is

   --
   --  Maximum length of the target name.
   --
   Max_Target_Length : constant := 300;

   --
   --  Message broker.
   --
   --  The message broker that encapsulates physical channel management,
   --  incoming and outgoing message queues, listeners and resource
   --  management.
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
   --  Creates new agent object with default option values.
   --  Alternative New_Agent function allocates the agent object dynamically.
   --
   function Make_Agent
     (Message_Handler : in Incoming_Message_Handlers.Handler_Access;
      Closed_Connection_Handler :
      in Closed_Connection_Handlers.Handler_Access := null;
      Working_Area : in System.Address := System.Null_Address;
      Area_Size : in System.Storage_Elements.Storage_Count := 0)
     return Agent;
   function New_Agent
     (Message_Handler : in Incoming_Message_Handlers.Handler_Access;
      Closed_Connection_Handler :
      in Closed_Connection_Handlers.Handler_Access := null;
      Working_Area : in System.Address := System.Null_Address;
      Area_Size : in System.Storage_Elements.Storage_Count := 0)
     return Agent_Access;

   --
   --  Creates new agent object with the given option values.
   --  Alternative New_Agent function allocates the agent object dynamically.
   --
   function Make_Agent
     (Options : in Parameters.Parameters_Collection;
      Message_Handler : in Incoming_Message_Handlers.Handler_Access;
      Closed_Connection_Handler :
      in Closed_Connection_Handlers.Handler_Access := null;
      Working_Area : in System.Address := System.Null_Address;
      Area_Size : in System.Storage_Elements.Storage_Count := 0)
     return Agent;
   function New_Agent
     (Options : in Parameters.Parameters_Collection;
      Message_Handler : in Incoming_Message_Handlers.Handler_Access;
      Closed_Connection_Handler :
      in Closed_Connection_Handlers.Handler_Access := null;
      Working_Area : in System.Address := System.Null_Address;
      Area_Size : in System.Storage_Elements.Storage_Count := 0)
     return Agent_Access;

   --
   --  Frees the dynamically allocated agent object.
   --
   procedure Free (The_Agent : in out Agent_Access);
   
   --
   --  Installs the event notification handler.
   -- 
   --  Installs the logging monitor.
   --  The previously installed callback (if any) is overriden.
   -- 
   --  This function is not synchronized.
   --
   procedure Install_Event_Notification_Handler
     (The_Agent : in out Agent;
      Handler : in Event_Notification_Handlers.Handler_Access);
        
   --
   --  Creates new channel for the given target.
   --
   --  Create a new channel for the given target. If the channel
   --  already exists for the given target, this procedure does nothing.
   --
   --  The supported target formats are:
   --  - "tcp://host:port" for TCP/IP connections, where <code>host</code>
   --    can be provided in the symbolic or numeric form
   --  - "udp://host:port" for UDP communication, where <code>host</code>
   --    can be provided in the symbolic or numeric form
   --  - "unix://path" for Unix connections, where <code>path</code>
   --    can be relative or absolute
   --  - "file://filename" or "file://filename?write" for writing to
   --    regular files
   --  - "file://filename?read" for reading from regular files
   --  - "file://filename?append" for appending to regular files
   --
   procedure Open (The_Agent : in out Agent;
                   Target : in String);

   --
   --  Creates new channel for the given target.
   --
   --  Create a new channel for the given target and return its descriptor.
   --  If the channel already exists for the given target,
   --  this procedure does nothing.
   --
   procedure Open (The_Agent : in out Agent;
                   Target : in String;
                   Descriptor : out Channel_Descriptor;
                   Created_New : out Boolean);

   --
   --  Checks if the given channel is already open.
   --
   --  If the given channel already exists, Result is set to True and
   --  the channel's descriptor is returned.
   --  Otherwise Result is set to False.
   --
   procedure Is_Open (The_Agent : in out Agent;
                      Target : in String;
                      Result : out Boolean;
                      Descriptor : out Channel_Descriptor);

   --
   --  Closes the given channel.
   --
   --  Closes the channel identified by the given descriptor.
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
   procedure Close (The_Agent : in out Agent;
                    Descriptor : in Channel_Descriptor;
                    Priority : in Natural := 0);

   --
   --  Closes the given channel.
   --
   --  Closes the channel identified by the target name.
   --
   procedure Close (The_Agent : in out Agent;
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
   procedure Hard_Close (The_Agent : in out Agent;
                         Descriptor : in Channel_Descriptor);

   --
   --  Immediately closes the given channel.
   --
   --  Immediately closes the channel identified by the target name.
   --
   procedure Hard_Close (The_Agent : in out Agent;
                         Target : in String);

   --
   --  Posts new message for sending.
   --
   --  Posts a new message to the outgoing queue of the given channel.
   --
   --  The message is composed of two sets of parameters, one for the
   --  header information and one for the body. This distinction is
   --  supposed to support arbitrary routing conventions defined by
   --  user code. Any of these parts can be empty.
   --
   --  The priority of the message is taken into account for proper
   --  ordering of the frames in the outgoing queue - frames created
   --  for messages with higher priority will be transmitted before
   --  frames having lower priority. Messages with equal priority are
   --  ordered according to the FIFO regime.
   --
   --  The callback handler can be provided to allow the user code trace
   --  the progress of the message. For each frame that was successfully
   --  pushed for physical transmission the callback is performed with
   --  the number of bytes that were transmitted from the beginning of the
   --  message and the total number of bytes for the whole message.
   --  When these two arguments are equal then it indicates that the whole
   --  message has been transmitted. If both are zero it means that there
   --  was an error and the message was abandoned.
   --
   procedure Post
     (The_Agent : in out Agent;
      Channel : in Channel_Descriptor;
      Message_Header : in Serializables.Serializable'Class;
      Message_Body : in Serializables.Serializable'Class;
      Priority : in Natural := 0;
      Progress_Handler : in Message_Progress_Handlers.Handler_Access := null);

   --
   --  Posts new message for sending.
   --
   --  Posts a new message to the outgoing queue of the given channel
   --  where the channel is identified by its target.
   --
   procedure Post
     (The_Agent : in out Agent;
      Target : in String;
      Message_Header : in Serializables.Serializable'Class;
      Message_Body : in Serializables.Serializable'Class;
      Priority : in Natural := 0;
      Progress_Handler : in Message_Progress_Handlers.Handler_Access := null);

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
      Resolved_Target_Last : out Natural;
      Connection_Handler : in New_Connection_Handlers.Handler_Access := null);

   --
   --  Removes existing listener.
   --
   --  Removes the listener denoted by its actual target name.
   --  Note that the actual target name might be different from the name
   --  provided when the listener was created, due to target resolution.
   --
   procedure Remove_Listener (The_Agent : in out Agent; Target : in String);

   --
   --  Performs a portion of I/O or internal management work.
   --
   --  Performs a portion of work with the given timeout.
   --  If there is some pending work at the call time it is performed
   --  immediately and procedure returns without waiting for further work;
   --  otherwise the call blocks waiting for the work with the given timeout.
   --
   --  The pending work can include any of:
   --  - any of the listeners is ready to accept new connection
   --  - any of the channels is ready for reading data
   --  - any of the channels is ready for output operation and there are
   --    pending frames in its outgoing queue
   --  - there was some change in the internal data structures that needs
   --    to be acted upon
   --
   --  <b>Note</b>: All callbacks initiated by the agent are executed in
   --  the context of the task that calls this procedure.
   --  The task calling this procedure is also the only one that performs
   --  actual data transfer.
   --
   --  <b>Note</b>: The timeout value is subject to system limits
   --  as defined for the select function.
   --
   --  <b>Note</b>: In the typical usage scenario this procedure should be
   --  called in a tight loop.
   --
   procedure Do_Some_Work
     (The_Agent : in out Agent;
      Timeout : in Duration;
      Timed_Out : out Boolean;
      Allow_Outgoing_Traffic : in Boolean := True;
      Allow_Incoming_Traffic : in Boolean := True);

   --
   --  Artificially interrupts the wait state of Do_Some_Work.
   --
   procedure Interrupt_Work_Waiter (The_Agent : in out Agent);

private

   --
   --  Implementation of Agent.
   --

   Sizeof_Agent : constant Interfaces.C.size_t;
   pragma Import (C, Sizeof_Agent, "sizeof_agent");

   type Agent_Value is new Interfaces.C.char_array (1 .. Sizeof_Agent);
   for Agent_Value'Alignment use Details.Alignment;

   type Agent is new Ada.Finalization.Limited_Controlled with record
      Initialized : Boolean := False;
      Value : Agent_Value;
   end record;

   overriding
   procedure Finalize (The_Agent : in out Agent);

end YAMI.Core.Agents;
