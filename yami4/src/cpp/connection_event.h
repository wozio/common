// Copyright Maciej Sobczak 2008-2015.
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

#ifndef YAMICPP_CONNECTION_EVENT_H_INCLUDED
#define YAMICPP_CONNECTION_EVENT_H_INCLUDED

namespace yami
{

/// Kind of connection event.
enum connection_event
{
    new_incoming_connection, ///< New incoming connection was created.
    new_outgoing_connection, ///< New outgoing connection was created.
    connection_closed        ///< Connection was closed.
};

} // namespace yami

#endif // YAMICPP_CONNECTION_EVENT_H_INCLUDED
