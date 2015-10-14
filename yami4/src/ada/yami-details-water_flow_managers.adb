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

package body YAMI.Details.Water_Flow_Managers is

   use type Parameters.YAMI_Integer;

   protected body Water_Flow_Manager is
      procedure Set_Limits (H : Parameters.YAMI_Integer;
                            L : Parameters.YAMI_Integer) is
      begin
         High := H;
         Low := L;
      end Set_Limits;

      function Is_Allowed return Boolean is
      begin
         return Allow_Flow;
      end Is_Allowed;

      entry Wait_For_Permission when Allow_Flow is
      begin
         null;
      end Wait_For_Permission;

      procedure Increase is
      begin
         Current := Current + 1;
         if Current >= High then
            Allow_Flow := False;
         end if;
      end Increase;

      procedure Decrease is
         Dummy_Open_Flag : Boolean;
      begin
         Decrease (Dummy_Open_Flag);
      end Decrease;

      procedure Decrease (Open : out Boolean) is
      begin
         Current := Current - 1;
         if Current < Low then
            Allow_Flow := True;
            Open := True;
         else
            Open := False;
         end if;
      end Decrease;

      procedure Get_State (Current_Level : out Parameters.Count_Type;
                           High_Water_Mark : out Parameters.Count_Type;
                           Low_Water_Mark : out Parameters.Count_Type) is
      begin
         Current_Level := Parameters.Count_Type (Current);
         High_Water_Mark := Parameters.Count_Type (High);
         Low_Water_Mark := Parameters.Count_Type (Low);
      end Get_State;
   end Water_Flow_Manager;

end YAMI.Details.Water_Flow_Managers;
