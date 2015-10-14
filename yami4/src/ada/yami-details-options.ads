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

with YAMI.Parameters;

package YAMI.Details.Options is

   --  Options values relevant for the general-purpose part.
   type Option_Values is
      record
         Dispatcher_Threads : Parameters.YAMI_Integer := 1;
         Connection_Retries : Parameters.YAMI_Integer := 5;
         Connection_Retry_Delay_Spread : Duration := 0.1;
         Outgoing_High_Water_Mark : Parameters.YAMI_Integer := 100;
         Outgoing_Low_Water_Mark : Parameters.YAMI_Integer := 20;
         Incoming_High_Water_Mark : Parameters.YAMI_Integer := 100;
         Incoming_Low_Water_Mark : Parameters.YAMI_Integer := 20;
         Deliver_As_Raw_Binary : Boolean := False;
         Default_Failover_Timeout : Duration := 0.0;
      end record;

   function Parse_Parameters (Opts : in Parameters.Parameters_Collection)
                             return Option_Values;

end YAMI.Details.Options;
