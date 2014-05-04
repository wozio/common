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
with Ada.Text_IO;

package body IAL.Log is

   type Message is record
      S : String (1 .. Max_Message_Size);
      S_Last : Natural;
   end record;

   --  rolling buffer with overflow detection
   type Messages is array (1 .. Max_Pending_Messages) of Message;

   protected Pending_Messages is
      procedure Put (Msg : in String);
      entry Consume (Msg : out Message);
   private
      Buffer : Messages;
      First : Natural := 0;
      Last : Natural := 0;
      Overflow : Boolean := False;

      Has_Message : Boolean := False;
   end Pending_Messages;

   protected body Pending_Messages is
      procedure Put (Msg : in String) is
      begin
         if not Overflow then
            if First = 0 then
               --  buffer empty, fill from the beginning
               First := 1;
               Last := 1;
            else
               Last := Last + 1;
               if Last > Max_Pending_Messages then
                  Last := 1;
               end if;
               if Last = First then
                  Overflow := True;
                  Last := Last - 1;
                  if Last = 0 then
                     Last := Max_Pending_Messages;
                  end if;
               end if;
            end if;

            if not Overflow then
               Buffer (Last).S (1 .. Msg'Length) := Msg;
               Buffer (Last).S_Last := Msg'Length;
            end if;
         end if;

         Has_Message := True;
      end Put;

      entry Consume (Msg : out Message) when Has_Message is
         Overflow_Msg : constant String :=
           "Log overflow, some log entries might have been lost";
      begin
         if First /= 0 then
            Msg := Buffer (First);

            if First = Last then
               First := 0;
               Last := 0;
            else
               First := First + 1;
               if First > Max_Pending_Messages then
                  First := 1;
               end if;
            end if;
         else
            Msg.S (1 .. Overflow_Msg'Length) := Overflow_Msg;
            Msg.S_Last := Overflow_Msg'Length;
            Overflow := False;
         end if;

         Has_Message := First /= 0 or Overflow;
      end Consume;
   end Pending_Messages;

   task Writer;
   task body Writer is
      Msg : Message;
   begin
      loop
         Pending_Messages.Consume (Msg);

         Ada.Text_IO.Put_Line (Msg.S (1 .. Msg.S_Last));
      end loop;
   end Writer;

   type Module_Flags is array (Module) of Boolean;

   protected Activity_Flags is
      procedure Enable (M : in Module; Active : in Boolean);
      function Is_Enabled (M : in Module) return Boolean;
   private
      Flags : Module_Flags := (others => True);
   end Activity_Flags;

   protected body Activity_Flags is
      procedure Enable (M : in Module; Active : in Boolean) is
      begin
         Flags (M) := Active;
      end Enable;

      function Is_Enabled (M : in Module) return Boolean is
      begin
         return Flags (M);
      end Is_Enabled;
   end Activity_Flags;

   procedure Put (M : in Module; Msg : in String) is
   begin
      if Activity_Flags.Is_Enabled (M) then
         declare
            Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            Time_Formatted : constant String :=
              Ada.Calendar.Formatting.Image (Date => Now,
                                             Include_Time_Fraction => True);
         begin
            Pending_Messages.Put
              (Time_Formatted & " [" & Module'Image (M) & "] " & Msg);
         end;
      end if;
   end Put;

   procedure Enable (M : in Module; Active : in Boolean) is
   begin
      Activity_Flags.Enable (M, Active);
   end Enable;

end IAL.Log;
