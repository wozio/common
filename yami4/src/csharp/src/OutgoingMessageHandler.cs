// Copyright Paweł Kierski 2010, 2015.
// This file is part of YAMI4.
//
// YAMI4 is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// YAMI4 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

namespace Inspirel.YAMI
{
    /// <summary>
    /// Representes the method that will be called when
    /// state of the <see cref="OutgoingMessage"/> has 
    /// been changed.
    /// </summary>
    /// <param name="sender">The source of the event (always
    /// <see cref="OutgoingMessage"/> object) - object
    /// which state has been changed.
    /// </param>
    public delegate void OutgoingMessageHandler(OutgoingMessage sender);
}
