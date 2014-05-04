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

with IAL.Properties;
with Log;

with Ada.Command_Line;
with Ada.IO_Exceptions;

package body Broker.Configuration is

   Default_Config_File_Name : constant String := "yami4broker.cfg";

   procedure Init (Success : out Boolean) is

      procedure Init_From_File (Config_File_Name : in String) is
      begin
         IAL.Properties.Load_Properties (Config_File_Name);
         Success := True;
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Put
              (Broker.Main, "Cannot read the configuration file: " &
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

   function Warmup_Time return Duration is
   begin
      return Duration
        (Float (Natural'Value
                  (IAL.Properties.Get ("warmup", "0"))) / 1000.0);
   end Warmup_Time;

   function Max_Subscriptions return Positive is
   begin
      return Positive'Value
        (IAL.Properties.Get ("max_subscriptions", "100"));
   end Max_Subscriptions;

   function Max_Client_Queue return Positive is
   begin
      return Positive'Value
        (IAL.Properties.Get ("max_client_queue", "10"));
   end Max_Client_Queue;

   function Subscription_Overflow_Policy return Overflow_Policy is
   begin
      return Overflow_Policy'Value
        (IAL.Properties.Get ("overflow_policy", "reject_message"));
   end Subscription_Overflow_Policy;

   function Log_Enabled (Module : in Broker_Module) return Boolean is
   begin
      return Boolean'value
        (IAL.Properties.Get ("log." & Broker_Module'Image (Module), "false"));
   end Log_Enabled;

   function Positive_Image (Index : in Positive) return String is
      Index_Image : constant String := Positive'Image (Index);
   begin
      return Index_Image (2 .. Index_Image'Last);
   end Positive_Image;

   function Forward_Target (Index : in Positive) return String is
      Field_Name : constant String :=
        "forward." & Positive_Image (Index) & ".target";
   begin
      return IAL.Properties.Get (Field_Name);
   end Forward_Target;

   function Forward_Filter (Index : in Positive) return String is
      Field_Name : constant String :=
        "forward." & Positive_Image (Index) & ".filter";
   begin
      return IAL.Properties.Get (Field_Name);
   end Forward_Filter;

end Broker.Configuration;
