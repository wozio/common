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

package YAMI.Core.Incoming_Message_Handlers is

   --
   --  Callback interface for processing incoming messages.
   --
   type Handler is limited interface;
   type Handler_Access is access all Handler'Class;

   --
   --  Operations of the Handler interface.
   --

   --
   --  Processes the new incoming message.
   --
   procedure Call
     (H : in out Handler;
      Source : in String;
      Header_Buffers : in Core.Serialization_Buffers_Descriptor;
      Body_Buffers : in Core.Serialization_Buffers_Descriptor)
      is abstract;

end YAMI.Core.Incoming_Message_Handlers;
