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

package body YAMI.Details is

   procedure Check_Result (Res : in Int) is
   begin
      case Res is
         when OK => null;
         when No_Such_Name => raise Logic_Error
            with "No such name.";
         when Bad_Type => raise Logic_Error
            with "Bad type.";
         when No_Such_Index => raise Logic_Error
            with "No such index.";
         when No_Memory => raise Runtime_Error
            with "Not enough memory.";
         when Nesting_Too_Deep => raise Logic_Error
            with "Nesting of parameters is too deep.";
         when Not_Enough_Space => raise Runtime_Error with
            "Not enough space or not enough data in the buffer.";
         when No_Entries => raise Logic_Error
            with "No entries found.";
         when Unexpected_Value => raise Runtime_Error
            with "The value that was given or received is incorrect.";
         when Bad_Protocol => raise Logic_Error
            with "The given protocol is not supported.";
         when IO_Error => raise Runtime_Error
            with "I/O error.";
         when Timed_Out => raise Runtime_Error
            with "Operation timed out.";
         when Channel_Closed => raise Runtime_Error
            with "The channel was closed.";
         when Bad_State => raise Logic_Error
            with "The given object is in the wrong state.";
         when others =>
            pragma Assert (False);
            null;
      end case;
   end Check_Result;

end YAMI.Details;
