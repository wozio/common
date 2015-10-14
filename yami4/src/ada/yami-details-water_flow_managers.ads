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

package YAMI.Details.Water_Flow_Managers is

   protected type Water_Flow_Manager is

      procedure Set_Limits (H : Parameters.YAMI_Integer;
                            L : Parameters.YAMI_Integer);

      function Is_Allowed return Boolean;

      entry Wait_For_Permission;

      procedure Increase;

      procedure Decrease;

      --  sets Open to True if as a result of this call the Allow_Flow flag
      --  is *changed* from False to True
      procedure Decrease (Open : out Boolean);

      procedure Get_State (Current_Level : out Parameters.Count_Type;
                           High_Water_Mark : out Parameters.Count_Type;
                           Low_Water_Mark : out Parameters.Count_Type);

   private
      High : Parameters.YAMI_Integer;
      Low : Parameters.YAMI_Integer;

      Current : Parameters.YAMI_Integer := 0;
      Allow_Flow : Boolean := True;
   end Water_Flow_Manager;

   type Manager_Access is access all Water_Flow_Manager;

end YAMI.Details.Water_Flow_Managers;
