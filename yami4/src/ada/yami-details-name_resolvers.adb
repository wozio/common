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

package body YAMI.Details.Name_Resolvers is

   protected body Name_Resolver is

      procedure Register_Object
        (Name : in String;
         Object : Incoming_Messages.Message_Handler_Access) is
      begin
         if Name = "*" then
            Any_Callback := Object;
         else
            Map.Insert (Name, Object);
         end if;
      end Register_Object;

      procedure Unregister_Object (Name : in String) is
      begin
         if Name = "*" then
            Any_Callback := null;
         else
            Map.Exclude (Name);
         end if;
      end Unregister_Object;

      function Resolve (Name : in String)
                       return Incoming_Messages.Message_Handler_Access is
         C : Object_Maps.Cursor := Map.Find (Name);
         use type Object_Maps.Cursor;
      begin
         if C /= Object_Maps.No_Element then
            return Object_Maps.Element (C);
         else
            return Any_Callback;
         end if;
      end Resolve;

   end Name_Resolver;

end YAMI.Details.Name_Resolvers;
