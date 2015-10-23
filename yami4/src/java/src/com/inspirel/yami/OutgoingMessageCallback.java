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
 * that the state of the outgoing message has changed.
 * In particular, this callback can be used to receive message replies
 * without blocking the client thread.
 */
public interface OutgoingMessageCallback {
    
    /**
     * Callback function that is automatically called by the agent
     * whenever the state of outgoing message changes.
     * 
     * @param message the message descriptor
     * @throws Exception any exception, which will be ignored
     */
    void update(OutgoingMessage message) throws Exception;
}
