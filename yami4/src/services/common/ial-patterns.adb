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

package body IAL.Patterns is

   Comma  : constant Character := ',';
   Dot    : constant Character := '.';
   Star   : constant Character := '*';

   procedure Tokenize
     (S : in String;
      Separator : in Character;
      Process : not null access procedure (Token : in String;
                                           Stop : out Boolean)) is

      Left : Positive;
      Right : Positive := S'First;

      Stop : Boolean;
   begin
      while Right <= S'Last loop
         Left := Right;

         --  skip leading commas
         while Left <= S'Last and then S (Left) = Comma loop
            Left := Left + 1;
         end loop;

         Right := Left;

         --  find first comma on the right
         while Right <= S'Last and then S (Right) /= Comma loop
            Right := Right + 1;
         end loop;

         if Left <= S'Last then
            Process (S (Left .. Right - 1), Stop);
            exit when Stop;
         end if;
      end loop;
   end Tokenize;

   function Hierarchic_Match (S : in String;
                              Pattern : in String) return Boolean is

      Si : Positive := S'First;
      Pi : Positive := Pattern'First;

   begin
      loop
         if Si > S'Last or Pi > Pattern'Last then
            exit;
         end if;

         if Pattern (Pi) = Star then
            --  skip S until dot or end
            while Si <= S'Last and then S (Si) /= Dot loop
               Si := Si + 1;
            end loop;

            --  if star is the last character in the pattern,
            --  then we consider the rest of S to match
            --  (this is the "hierarchic" part of matching)

            if Pi = Pattern'Last then
               return True;
            end if;
            
            --  otherwise move on with the pattern,
            --  there are more components to check
            Pi := Pi + 1;

         elsif S (Si) /= Pattern (Pi) then
            exit;
         else
            Si := Si + 1;
            Pi := Pi + 1;
         end if;
      end loop;

      return Si = S'Last + 1 and Pi = Pattern'Last + 1;
   end Hierarchic_Match;

   function Multi_Hierarchic_Match (Tokens : in String;
                                    Patterns : in String) return Boolean is

      Result : Boolean := False;

      procedure Match_Tokens_Agsinst_Single_Pattern
        (Pattern : in String;
         Stop_Pattern : out Boolean) is

         procedure Match_Single_Token_Against_Single_Pattern
           (Token : in String; Stop_Token : out Boolean) is
         begin
            Result := Hierarchic_Match (Token, Pattern);
            Stop_Token := Result;
            Stop_Pattern := Result;
         end Match_Single_Token_Against_Single_Pattern;

      begin
         Tokenize (Tokens, Comma,
                   Match_Single_Token_Against_Single_Pattern'Access);
      end Match_Tokens_Agsinst_Single_Pattern;

   begin
      Tokenize (Patterns, Comma,
                Match_Tokens_Agsinst_Single_Pattern'Access);

      return Result;
   end Multi_Hierarchic_Match;

end IAL.Patterns;
