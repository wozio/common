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

package com.inspirel.yami;

/**
 * The callback interface that is used to notify the user code
 * about the change in the set of connections.
 */
public interface ConnectionEventCallback {
    
    /**
     * Type of connection event.
     */
    enum ConnectionEvent {
        NEW_INCOMING_CONNECTION,
        NEW_OUTGOING_CONNECTION,
        CONNECTION_CLOSED
    }
    
    /**
     * Callback function that is automatically called by the agent
     * whenever there is a change in the set of connections.
     * 
     * @param name name of the reported connection
     * @param event kind of connection event
     */
    void report(String name, ConnectionEvent event) throws Exception;
}
