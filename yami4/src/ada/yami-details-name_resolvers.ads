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

with YAMI.Incoming_Messages;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package YAMI.Details.Name_Resolvers is

   use type Incoming_Messages.Message_Handler_Access;

   package Object_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => Incoming_Messages.Message_Handler_Access,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   protected type Name_Resolver is

      procedure Register_Object
        (Name : in String;
         Object : in Incoming_Messages.Message_Handler_Access);

      procedure Unregister_Object (Name : in String);

      function Resolve (Name : in String)
                       return Incoming_Messages.Message_Handler_Access;

   private
      Map : Object_Maps.Map;
      Any_Callback : Incoming_Messages.Message_Handler_Access := null;
   end Name_Resolver;

   type Name_Resolver_Access is access all Name_Resolver;

end YAMI.Details.Name_Resolvers;
