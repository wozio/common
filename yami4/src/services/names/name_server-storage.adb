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

with Log;

with Ada.Directories;
with Ada.Text_IO;

package body Name_Server.Storage is

   Max_Path_Lenth : constant := 300;

   Data_Directory_Name : String (1 .. Max_Path_Lenth);
   Data_Directory_Name_Last : Natural := 0;

   procedure Init (Data_Directory : in String) is
   begin
      Data_Directory_Name (1 .. Data_Directory'Length) := Data_Directory;
      Data_Directory_Name_Last := Data_Directory'Length;

      Ada.Directories.Create_Path
        (Data_Directory_Name (1 .. Data_Directory_Name_Last));

      Log.Put (Store, "initialized persistent store at " &
                 Data_Directory_Name (1 .. Data_Directory_Name_Last));
   end Init;

   procedure Store (Object_Name : in String;
                    Location : in String) is

      Path : constant String :=
        Ada.Directories.Compose
        (Data_Directory_Name (1 .. Data_Directory_Name_Last),
         Object_Name);

      F : Ada.Text_IO.File_Type;

   begin
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Path);
      Ada.Text_IO.Put_Line (F, Location);
      Ada.Text_IO.Close (F);

      Log.Put (Store, "    stored binding for " & Object_Name);
   end Store;

   procedure Iterate (Process : not null access
                      procedure (Object_Name : in String;
                                 Location : in String)) is

      Filter : Ada.Directories.Filter_Type :=
        (Ada.Directories.Ordinary_File => True, others => False);

      Count : Natural := 0;

      procedure Translate_Iteration
        (Directory_Entry : in Ada.Directories.Directory_Entry_Type) is

         Object_Name : constant String :=
           Ada.Directories.Simple_Name (Directory_Entry);

         Path : constant String :=
           Ada.Directories.Full_Name (Directory_Entry);

         F : Ada.Text_IO.File_Type;

      begin
         Ada.Text_IO.Open (F, Ada.Text_IO.In_File, Path);
         declare
            Location : constant String := Ada.Text_IO.Get_Line (F);
         begin
            Process (Object_Name, Location);
         end;
         Ada.Text_IO.Close (F);

         Count := Count + 1;
      end Translate_Iteration;

   begin
      Ada.Directories.Search
        (Data_Directory_Name (1 .. Data_Directory_Name_Last),
         "*",
         Filter,
         Translate_Iteration'Access);

      Log.Put (Store, "recovered" & Natural'Image (Count) &
                 " bindings from store");
   end Iterate;

end Name_Server.Storage;
