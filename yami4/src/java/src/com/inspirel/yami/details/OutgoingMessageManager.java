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

package com.inspirel.yami.details;

import com.inspirel.yami.Parameters;

import java.util.HashMap;
import java.util.Map;

public class OutgoingMessageManager {

    private final Map<Long, MessageProgressCallback> messages;

    public OutgoingMessageManager() {
        messages = new HashMap<Long, MessageProgressCallback>();
    }
    
    public void registerNewMessage(long messageId,
            MessageProgressCallback message) {
        synchronized (messages) {
            messages.put(Long.valueOf(messageId), message);
        }
    }
    
    public void reportReplied(long messageId,
            Parameters body, byte[] rawBody) {
        
        synchronized (messages) {
            MessageProgressCallback message =
                messages.get(Long.valueOf(messageId));
            if (message != null) {
                message.replied(body, rawBody);
            }
        }
    }
    
    public void reportRejected(long messageId, String reason) {
        synchronized (messages) {
            MessageProgressCallback message =
                messages.get(Long.valueOf(messageId));
            if (message != null) {
                message.rejected(reason);
            }
        }
    }
    
    public void unregisterMessage(long messageId) {
        synchronized (messages) {
            messages.remove(Long.valueOf(messageId));
        }
    }
}
