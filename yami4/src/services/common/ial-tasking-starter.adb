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

package body IAL.Tasking.Starter is

   protected Starter_Object is
      procedure Start;
      entry Wait_Until_Started;
   private
      Started : Boolean := False;
   end Starter_Object;

   protected body Starter_Object is
      procedure Start is
      begin
         Started := True;
      end Start;

      entry Wait_Until_Started when Started is
      begin
         null;
      end Wait_Until_Started;
   end Starter_Object;

   procedure Start is
   begin
      Starter_Object.Start;
   end Start;

   procedure Wait_Until_Started is
   begin
      Starter_Object.Wait_Until_Started;
   end Wait_Until_Started;

end IAL.Tasking.Starter;
