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

with YAMI.Option_Names;

package body YAMI.Details.Options is

   function Parse_Parameters (Opts : in Parameters.Parameters_Collection)
                             return Option_Values is

      procedure Read_Integer (Name : in String;
                              Value : out Parameters.YAMI_Integer;
                              Valid : out Boolean) is

         E : Parameters.Parameter_Entry;
         Found : Boolean;

         use type Parameters.Parameter_Type;
         use type Parameters.YAMI_Integer;

      begin
         Valid := False;
         Opts.Find (Name, E, Found);
         if Found and then
           Parameters.Entry_Type (E) = Parameters.Integer_Type then

            Value := Parameters.Get_Integer (E);
            Valid := Value >= 0;

         end if;
      end Read_Integer;

      procedure Read_Boolean (Name : in String;
                              Value : in out Boolean) is

         E : Parameters.Parameter_Entry;
         Found : Boolean;

         use type Parameters.Parameter_Type;

      begin
         Opts.Find (Name, E, Found);
         if Found and then
           Parameters.Entry_Type (E) = Parameters.Boolean_Type then

            Value := Parameters.Get_Boolean (E);

         end if;
      end Read_Boolean;

      Value : Parameters.YAMI_Integer;
      Valid : Boolean;

      Values : Option_Values;

   begin

      Read_Integer (Option_Names.Dispatcher_Threads, Value, Valid);
      if Valid then
         Values.Dispatcher_Threads := Value;
      end if;

      Read_Integer (Option_Names.Connection_Retries, Value, Valid);
      if Valid then
         Values.Connection_Retries := Value;
      end if;

      Read_Integer
        (Option_Names.Connection_Retry_Delay_Spread, Value, Valid);
      if Valid then
         Values.Connection_Retry_Delay_Spread := Duration (Value) * 0.001;
      end if;

      Read_Integer (Option_Names.Outgoing_High_Water_Mark, Value, Valid);
      if Valid then
         Values.Outgoing_High_Water_Mark := Value;
      end if;

      Read_Integer (Option_Names.Outgoing_Low_Water_Mark, Value, Valid);
      if Valid then
         Values.Outgoing_Low_Water_Mark := Value;
      end if;

      Read_Integer (Option_Names.Incoming_High_Water_Mark, Value, Valid);
      if Valid then
         Values.Incoming_High_Water_Mark := Value;
      end if;

      Read_Integer (Option_Names.Incoming_Low_Water_Mark, Value, Valid);
      if Valid then
         Values.Incoming_Low_Water_Mark := Value;
      end if;

      Read_Boolean (Option_Names.Deliver_As_Raw_Binary,
                    Values.Deliver_As_Raw_Binary);

      Read_Integer
        (Option_Names.Default_Failover_Timeout, Value, Valid);
      if Valid then
         Values.Default_Failover_Timeout := Duration (Value) * 0.001;
      end if;
      
      return Values;

   end Parse_Parameters;

end YAMI.Details.Options;
