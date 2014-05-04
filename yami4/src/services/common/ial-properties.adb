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

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Text_IO;

package body IAL.Properties is

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => String,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   File_Property_Map : String_Maps.Map;

   Command_Line_Property_Map : String_Maps.Map;
   Cmd_Line_Parsed : Boolean := False;

   procedure Load_Properties (File_Name : in String) is

      F : Ada.Text_IO.File_Type;

      procedure Process_Entry (E : in String; M : in out String_Maps.Map) is
         Equals_Index : constant Natural := Ada.Strings.Fixed.Index (E, "=");
      begin
         if Equals_Index /= 0 and
           Equals_Index /= E'First and
           Equals_Index /= E'Last then
            declare
               Name : constant String :=
                 Ada.Strings.Fixed.Trim
                 (E (E'First .. Equals_Index - 1),
                  Ada.Strings.Both);

               Lowcase_Name : constant String :=
                 Ada.Characters.Handling.To_Lower (Name);

               Value : constant String :=
                 Ada.Strings.Fixed.Trim
                 (E (Equals_Index + 1 .. E'Last),
                  Ada.Strings.Both);
            begin
               M.Insert (Lowcase_Name, Value);
            end;
         end if;
      end Process_Entry;

   begin
      --  command line options need to be parsed only once,
      --  because they are immutable
      if not Cmd_Line_Parsed then
         for I in 1 .. Ada.Command_Line.Argument_Count loop
            declare
               Option : constant String := Ada.Command_Line.Argument (I);
            begin
               if Option'Last > 2 then
                  if Option (1 .. 2) = "-D" then
                     Process_Entry (Option (3 .. Option'Last),
                                    Command_Line_Property_Map);
                  end if;
               end if;
            end;
         end loop;
         Cmd_Line_Parsed := True;
      end if;

      Ada.Text_IO.Open (F, Ada.Text_IO.In_File, File_Name);
      while not Ada.Text_IO.End_Of_File (F) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (F);
            Line_Trimmed : constant String :=
              Ada.Strings.Fixed.Trim (Line, Ada.Strings.Both);
         begin
            if Line_Trimmed'Length /= 0 and then
              Line_Trimmed (Line_Trimmed'First) /= '#' then

               Process_Entry (Line_Trimmed, File_Property_Map);

            end if;
         end;
      end loop;
      Ada.Text_IO.Close (F);
   end Load_Properties;

   function Is_Defined (Name : in String) return Boolean is
      Lowcase_Name : constant String :=
        Ada.Characters.Handling.To_Lower (Name);
   begin
      return Command_Line_Property_Map.Contains (Lowcase_Name) or else
        File_Property_Map.Contains (Lowcase_Name);
   end Is_Defined;

   function Get (Name : in String; Default : in String := "") return String is
      Lowcase_Name : constant String :=
        Ada.Characters.Handling.To_Lower (Name);
      C : String_Maps.Cursor;
   begin
      --  first try the command line options, they have priority
      C := Command_Line_Property_Map.Find (Lowcase_Name);
      if String_Maps.Has_Element (C) then
         return String_Maps.Element (C);
      else
         --  then search in "regular" properties
         C := File_Property_Map.Find (Lowcase_Name);
         if String_Maps.Has_Element (C) then
            return String_Maps.Element (C);
         else
            return Default;
         end if;
      end if;
   end Get;

   procedure Iterate (Process : not null access
                      procedure (Name : in String;
                                 Value : in String)
                     ) is

      procedure Extract_From_Cursor (Position : in String_Maps.Cursor) is
      begin
         Process.all (String_Maps.Key (Position),
                      String_Maps.Element (Position));
      end Extract_From_Cursor;

   begin
      Command_Line_Property_Map.Iterate (Extract_From_Cursor'Access);
      File_Property_Map.Iterate (Extract_From_Cursor'Access);
   end Iterate;

   procedure Clear is
   begin
      File_Property_Map.Clear;
   end Clear;

end IAL.Properties;
