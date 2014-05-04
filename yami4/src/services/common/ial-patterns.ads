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

package IAL.Patterns is

   --
   --  Tokenizes the given string based on the separator.
   --  Note: empty tokens are not reported for adjacent separators.
   --
   procedure Tokenize
     (S : in String;
      Separator : in Character;
      Process : not null access procedure (Token : in String;
                                           Stop : out Boolean));

   --
   --  Checks the matching of S against the Pattern that can
   --  contain hierarchy wildcards, for example "abc.xyz.*"
   --
   function Hierarchic_Match (S : in String;
                              Pattern : in String) return Boolean;

   --
   --  Checks the matching of Tokens against Patterns.
   --  Tokens and patterns are comma-separated lists.
   --
   function Multi_Hierarchic_Match (Tokens : in String;
                                    Patterns : in String) return Boolean;

end IAL.Patterns;
