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

using System;
using System.Collections.Generic;

namespace Inspirel.YAMI.details
{
    internal class OutgoingMessageManager
    {
        private readonly IDictionary<long, MessageProgressCallback> 
            messages;

        public OutgoingMessageManager()
        {
            messages = new Dictionary<long, MessageProgressCallback>();
        }

        public virtual void registerNewMessage(
            long messageId, MessageProgressCallback message)
        {
            lock (messages)
            {
                messages.Add(messageId, message);
            }
        }

        public virtual void reportReplied(
            long messageId, Parameters body, byte[] rawBody)
        {
            lock (messages)
            {
                if(messages.ContainsKey(messageId))
                {
                    messages[messageId].replied(body, rawBody);
                }
            }
        }

        public virtual void reportRejected(long messageId, string reason)
        {
            lock (messages)
            {
                if(messages.ContainsKey(messageId))
                {
                    messages[messageId].rejected(reason);
                }
            }
        }

        public virtual void unregisterMessage(long messageId)
        {
            lock (messages)
            {
                messages.Remove(Convert.ToInt64(messageId));
            }
        }
    }

}