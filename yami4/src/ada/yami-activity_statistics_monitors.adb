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

package body YAMI.Activity_Statistics_Monitors is

   procedure Reset (S : out Connection_Stats) is
   begin
      S.Messages_Sent := 0;
      S.Messages_Received := 0;
      S.Bytes_Sent := 0;
      S.Bytes_Received := 0;
   end Reset;
   
   protected body Activity_Statistics_Monitor is
      
      overriding procedure Agent_Created is
      begin
         Stats.Started := Ada.Calendar.Clock;
      end Agent_Created;
      
      overriding procedure Agent_Closed is
      begin
         null;
      end Agent_Closed;
      
      overriding procedure Listener_Added (Target : in String) is
      begin
         Stats.Listeners.Include (Target);
      end Listener_Added;
      
      overriding procedure Listener_Removed (Target : in String) is
      begin
         Stats.Listeners.Exclude (Target);
      end Listener_Removed;
      
      overriding procedure Incoming_Connection_Open (Target : in String) is
      begin
         Stats.Conn_Stats.Include
           (Target, (Conn_Type => Incoming, others => <>));
      end Incoming_Connection_Open;
      
      overriding procedure Outgoing_Connection_Open (Target : in String) is
      begin
         Stats.Conn_Stats.Include
           (Target, (Conn_Type => Outgoing, others => <>));
      end Outgoing_Connection_Open;
      
      overriding procedure Connection_Closed (Target : in String) is
      begin
         Stats.Conn_Stats.Exclude (Target);
      end Connection_Closed;
      
      overriding procedure Connection_Error (Target : in String) is
         C : constant Connection_Errors_Maps.Cursor :=
           Stats.Conn_Errors.Find (Target);
         Old_Count : Natural;
         
         use type Connection_Errors_Maps.Cursor;
         
      begin
         if C /= Connection_Errors_Maps.No_Element then
            Old_Count := Connection_Errors_Maps.Element (C);
            Stats.Conn_Errors.Replace_Element (C, Old_Count + 1);
         else
            Stats.Conn_Errors.Insert (Target, 0);
         end if;
      end Connection_Error;
      
      overriding procedure Object_Registered (Name : in String) is
      begin
         Stats.Objects.Include (Name);
      end Object_Registered;
      
      overriding procedure Object_Unregistered (Name : in String) is
      begin
         Stats.Objects.Exclude (Name);
      end Object_Unregistered;
      
      overriding procedure Message_Sent
        (Target : in String;
         Size : in Ada.Streams.Stream_Element_Count) is
         
         C : constant Connection_Stats_Maps.Cursor :=
           Stats.Conn_Stats.Find (Target);
         
         procedure Update (Target : in String;
                           S : in out Connection_Stats) is
            
            use type Ada.Streams.Stream_Element_Count;
            
         begin
            S.Messages_Sent := S.Messages_Sent + 1;
            S.Bytes_Sent := S.Bytes_Sent + Size;
         end Update;
         
      begin
         Stats.Conn_Stats.Update_Element (C, Update'Access);
      end Message_Sent;
      
      overriding procedure Message_Received
        (Target : in String;
         Size : in Ada.Streams.Stream_Element_Count) is
         
         C : constant Connection_Stats_Maps.Cursor :=
           Stats.Conn_Stats.Find (Target);
         
         procedure Update (Target : in String;
                           S : in out Connection_Stats) is
            
            use type Ada.Streams.Stream_Element_Count;
            
         begin
            S.Messages_Received := S.Messages_Received + 1;
            S.Bytes_Received := S.Bytes_Received + Size;
         end Update;
         
      begin
         Stats.Conn_Stats.Update_Element (C, Update'Access);
      end Message_Received;
      
      overriding procedure Call
        (Message : in out Incoming_Messages.Incoming_Message'Class) is
         
         Reply_Params : Parameters.Parameters_Collection :=
           Parameters.Make_Parameters;
            
         procedure Process
           (Content : in out Parameters.Parameters_Collection) is
            
            Found_Reset : Boolean;
            Reset_Entry : Parameters.Parameter_Entry;
            
            Reset_Counters : Boolean := False;
            
            use type Parameters.Parameter_Type;
         begin
            Content.Find ("reset", Reset_Entry, Found_Reset);
            if Found_Reset and then
              Parameters.Entry_Type (Reset_Entry) = Parameters.Boolean_Type
            then
               Reset_Counters := Parameters.Get_Boolean (Reset_Entry);
            end if;
            
            Get (Reply_Params, Reset_Counters);
         end Process;
         
      begin
         if Message.Message_Name /= "get" then
            Message.Reject ("Unknown message name.");
            return;
         end if;
         
         Message.Process_Content (Process'Access);
         Message.Reply (Reply_Params);
      end Call;
      
      procedure Get (Params : in out Parameters.Parameters_Collection;
                     Reset_Counters : in Boolean) is
         
         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         
         Index : Parameters.Index_Type;
         
         Listeners_Field : constant String := "listeners";
         Listeners_Cursor : String_Sets.Cursor;
         
         Objects_Field : constant String := "objects";
         Objects_Cursor : String_Sets.Cursor;
         
         Conn_Names_Field : constant String := "connection_names";
         Conn_Cursor : Connection_Stats_Maps.Cursor;
         
         type Counters_Array is
           array (Parameters.Count_Type range <>) of
           Parameters.YAMI_Long_Long_Integer;
         
         procedure Set_Counters_Array is new
           Parameters.Set_Long_Long_Array
           (Long_Long_Integer_Type => Parameters.YAMI_Long_Long_Integer,
            Index_Type => Parameters.Count_Type,
            Long_Long_Integer_Array_Type => Counters_Array);
         
         Conn_Errors_Names_Field : constant String := "error_names";
         Errors_Cursor : Connection_Errors_Maps.Cursor;
         
         use type Ada.Calendar.Time;
         use type Parameters.Index_Type;
         
      begin
         
         --  uptime information
         
         Params.Set_Integer
           ("uptime",
            Parameters.YAMI_Integer (Now - Stats.Started));
         
         --  list of active listeners
         
         Params.Create_String_Array
           (Listeners_Field,
            Parameters.Count_Type (Stats.Listeners.Length));
         
         Listeners_Cursor := Stats.Listeners.First;
         Index := 1;
         while String_Sets.Has_Element (Listeners_Cursor) loop
            Params.Set_String_In_Array
              (Listeners_Field,
               Index,
               String_Sets.Element (Listeners_Cursor));
            
            String_Sets.Next (Listeners_Cursor);
            Index := Index + 1;
         end loop;
         
         --  list of registered objects
         
         Params.Create_String_Array
           (Objects_Field,
            Parameters.Count_Type (Stats.Objects.Length));
         
         Objects_Cursor := Stats.Objects.First;
         Index := 1;
         while String_Sets.Has_Element (Objects_Cursor) loop
            Params.Set_String_In_Array
              (Objects_Field,
               Index,
               String_Sets.Element (Objects_Cursor));
            
            String_Sets.Next (Objects_Cursor);
            Index := Index + 1;
         end loop;
         
         --  connection statistics
         
         declare
            Connections_Length : constant Parameters.Index_Type :=
              Parameters.Count_Type (Stats.Conn_Stats.Length);
            
            Conn_Messages_Sent : Counters_Array (1 .. Connections_Length);
            Conn_Messages_Received : Counters_Array (1 .. Connections_Length);
            Conn_Bytes_Sent : Counters_Array (1 .. Connections_Length);
            Conn_Bytes_Received : Counters_Array (1 .. Connections_Length);
         begin
            Params.Create_String_Array
              (Conn_Names_Field,
               Parameters.Count_Type (Stats.Conn_Stats.Length));
            
            Conn_Cursor := Stats.Conn_Stats.First;
            Index := 1;
            while Connection_Stats_Maps.Has_Element (Conn_Cursor) loop
               Params.Set_String_In_Array
                 (Conn_Names_Field,
                  Index,
                  Connection_Stats_Maps.Key (Conn_Cursor));
               
               declare
                  S : Connection_Stats renames
                    Connection_Stats_Maps.Element (Conn_Cursor);
               begin
                  Conn_Messages_Sent (Index) :=
                    Parameters.YAMI_Long_Long_Integer
                    (S.Messages_Sent);
                  Conn_Messages_Received (Index) :=
                    Parameters.YAMI_Long_Long_Integer
                    (S.Messages_Received);
                  Conn_Bytes_Sent (Index) :=
                    Parameters.YAMI_Long_Long_Integer
                    (S.Bytes_Sent);
                  Conn_Bytes_Received (Index) :=
                    Parameters.YAMI_Long_Long_Integer
                    (S.Bytes_Received);
               end;
            
               Connection_Stats_Maps.Next (Conn_Cursor);
               Index := Index + 1;
            end loop;
            
            Set_Counters_Array
              (Params, "messages_sent", Conn_Messages_Sent);
            Set_Counters_Array
              (Params, "messages_received", Conn_Messages_Received);
            Set_Counters_Array
              (Params, "bytes_sent", Conn_Bytes_Sent);
            Set_Counters_Array
              (Params, "bytes_received", Conn_Bytes_Received);
         end;
         
         --  connection errors
         
         declare
            Errors_Length : constant Parameters.Count_Type :=
              Parameters.Count_Type (Stats.Conn_Errors.Length);
            
            Conn_Errors : Counters_Array (1 .. Errors_Length);
         begin
            Params.Create_String_Array
              (Conn_Errors_Names_Field,
               Parameters.Count_Type (Stats.Conn_Errors.Length));
            
            Errors_Cursor := Stats.Conn_Errors.First;
            Index := 1;
            while Connection_Errors_Maps.Has_Element (Errors_Cursor) loop
               Params.Set_String_In_Array
                 (Conn_Errors_Names_Field,
                  Index,
                  Connection_Errors_Maps.Key (Errors_Cursor));
               
               Conn_Errors (Index) :=
                 Parameters.YAMI_Long_Long_Integer
                 (Connection_Errors_Maps.Element (Errors_Cursor));
            
               Connection_Errors_Maps.Next (Errors_Cursor);
               Index := Index + 1;
            end loop;
            
            Set_Counters_Array (Params, "errors", Conn_Errors);
         end;
                  
         --  atomic reset, if requested
         
         if Reset_Counters then
            Stats.Conn_Errors.Clear;
            
            declare
               procedure Reset_Counters (Key : in String;
                                         Element : in out Connection_Stats) is
               begin
                  Reset (Element);
               end Reset_Counters;
            begin
               Conn_Cursor := Stats.Conn_Stats.First;
               while Connection_Stats_Maps.Has_Element (Conn_Cursor) loop
                  Stats.Conn_Stats.Update_Element
                    (Conn_Cursor, Reset_Counters'Access);
                  Connection_Stats_Maps.Next (Conn_Cursor);
               end loop;
            end;
         end if;
      end Get;
     
   end Activity_Statistics_Monitor;

end YAMI.Activity_Statistics_Monitors;
