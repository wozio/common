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

with Ada.Calendar.Formatting;

package body IAL.Utils is
   
   function Natural_Image (X : in Natural) return String is
      Image : constant String := Natural'Image (X);
   begin
      return Image (2 .. Image'Last);
   end Natural_Image;
   
   function Long_Duration_Image (D : in Duration;
                                 Include_Time_Fraction : Boolean := False)
                                return String is
            
      Hours_In_Day : constant := 86_400;
            
      Num_Of_Days : Integer;
      Remaining_Duration : Duration;
            
      function Days_As_String return String is
      begin
         if Num_Of_Days = 0 then
            return "";
         elsif Num_Of_Days = 1 then
            return "1 day ";
         else
            return Integer'Image (Num_Of_Days) & " days ";
         end if;
      end Days_As_String;
            
   begin
      Num_Of_Days := Integer (D) / Hours_In_Day;
      Remaining_Duration := D - Duration (Num_Of_Days * Hours_In_Day);
            
      return Days_As_String &
        Ada.Calendar.Formatting.Image (Remaining_Duration);
   end Long_Duration_Image;

end IAL.Utils;
