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

with Ada.Streams;

package YAMI.Core.Message_Progress_Handlers is

   --
   --  Callback interface for processing messages progress notifications.
   --
   type Handler is limited interface;
   type Handler_Access is access all Handler'Class;

   --
   --  Operations of the Handler interface.
   --

   --
   --  Updates the progress information.
   --
   procedure Progress
     (H : in out Handler;
      Sent_Bytes : in Ada.Streams.Stream_Element_Count;
      Total_Byte_Count : in Ada.Streams.Stream_Element_Count)
      is abstract;

end YAMI.Core.Message_Progress_Handlers;
