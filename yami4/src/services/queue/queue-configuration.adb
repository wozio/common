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

with IAL.Utils;
with IAL.Properties;
with Log;

with Ada.Command_Line;
with Ada.IO_Exceptions;

package body Queue.Configuration is

   Default_Config_File_Name : constant String := "yami4queue.cfg";

   procedure Init (Success : out Boolean) is

      procedure Init_From_File (Config_File_Name : in String) is
      begin
         IAL.Properties.Load_Properties (Config_File_Name);
         Success := True;
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Put
              (Queue.Main, "Cannot read the configuration file: " &
                 Config_File_Name);
      end Init_From_File;

   begin
      Success := False;
      if Ada.Command_Line.Argument_Count /= 0 then
         Init_From_File (Ada.Command_Line.Argument (1));
      else
         Init_From_File (Default_Config_File_Name);
      end if;
   end Init;

   function Listener return String is
   begin
      return IAL.Properties.Get ("listener", "tcp://*:*");
   end Listener;

   function Max_Waiting_Clients return Positive is
   begin
      return Positive'Value
        (IAL.Properties.Get ("max_waiting_clients", "10"));
   end Max_Waiting_Clients;

   function Max_Queue_Num_Of_Messages return Positive is
   begin
      return Positive'Value
        (IAL.Properties.Get ("max_queue_num_of_messages", "10"));
   end Max_Queue_Num_Of_Messages;

   function Max_Queue_Size return Positive is
   begin
      return Positive'Value
        (IAL.Properties.Get ("max_queue_size", "1000000"));
   end Max_Queue_Size;

   function Queue_Creation_Policy return Creation_Policy is
   begin
      return Creation_Policy'Value
        (IAL.Properties.Get ("queue_creation_policy", "dynamic"));
   end Queue_Creation_Policy;

   function Log_Enabled (Module : in Queue_Module) return Boolean is
   begin
      return Boolean'value
        (IAL.Properties.Get ("log." & Queue_Module'Image (Module), "false"));
   end Log_Enabled;

   function Initial_Queue_Name (Index : in Positive) return String is
      Field_Name : constant String :=
        "queue." & IAL.Utils.Natural_Image (Index);
   begin
      return IAL.Properties.Get (Field_Name);
   end Initial_Queue_Name;

end Queue.Configuration;
