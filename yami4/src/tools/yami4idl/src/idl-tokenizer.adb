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

with IDL.Parser;

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;

use Ada.Characters.Handling;
use Ada.Characters.Latin_1;
use Ada.Strings.Unbounded;

package body IDL.Tokenizer is

   type Tokenizer_State_Type is
     (Whitespace,
      Comment,
      Text);
   
   Tokenizer_State : Tokenizer_State_Type := Whitespace;
   
   Acc : Unbounded_String;
   
   function Last_Accumulated_Length return Natural is
   begin
      return Length (Acc);
   end Last_Accumulated_Length;
   
   procedure Reset is
   begin
      Tokenizer_State := Whitespace;
      Acc := Null_Unbounded_String;
   end Reset;
      
   procedure Tokenize (C : in Character) is
   
      function Is_Special (C : in Character) return Boolean is
      begin
         return C = Left_Parenthesis or
            C = Right_Parenthesis or
            C = Colon or C = Semicolon;
      end Is_Special;
      
      function Is_White (C : in Character) return Boolean is
      begin
         return C = LF or C = Space or C = HT;
      end Is_White;
      
   begin
      case Tokenizer_State is
         when Whitespace =>
            if C = Minus_Sign then
               --  in this simple grammar
               --  one minus is enough to indicate comment
               Tokenizer_State := Comment;
               
            elsif Is_Letter (C) then
               Append (Acc, C);
               Tokenizer_State := Text;
               
            elsif Is_Special (C) then
               Parser.Process_Token (C & "");

            elsif Is_White (C) then
               null;
               
            else
               raise Invalid_Input;
            end if;
            
         when Comment =>
            if C = LF then
               Tokenizer_State := Whitespace;
            end if;
            
         when Text =>
            if Is_Alphanumeric (C) or C = Low_Line or C = Full_Stop then
               Append (Acc, C);
               
            elsif Is_Special (C) or Is_White (C) then
               Tokenizer_State := Whitespace;

               Parser.Process_Token (To_String (Acc));
               Acc := Null_Unbounded_String;

               if Is_Special (C) then
                  Parser.Process_Token (C & "");
               end if;
            
            elsif  C = Minus_Sign then
               Parser.Process_Token (To_String (Acc));
               Acc := Null_Unbounded_String;

               Tokenizer_State := Comment;
               
            else
               raise Invalid_Input;
            end if;
      end case;
   end Tokenize;

end IDL.Tokenizer;
