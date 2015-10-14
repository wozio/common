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

using System.Threading;
using Inspirel.YAMI.details;

namespace Inspirel.YAMI
{
    /// 
    /// <summary> Outgoing message.</summary>
    /// <remarks>
    /// <para>
    /// The handler (proxy) allowing to track the progress of outgoing 
    /// message, inspect its state and to obtain the reply content.
    /// </para>
    /// <para>
    /// <b>Note:</b> The objects of this class can be safely used
    /// from multiple threads.
    /// </para> 
    /// </remarks>
    public sealed class OutgoingMessage : MessageProgressCallback
    {
        /// <summary> Enumeration defining all possible message states. 
        /// </summary>
        public enum MessageState
        {
            /// <summary> Message was posted for transmission. </summary>
            POSTED,

            /// <summary> Message was fully transmitted. </summary>
            TRANSMITTED,

            /// <summary> Message was abandoned due to error or channel 
            /// closing. </summary>
            ABANDONED,

            /// <summary> The reply was received for the given message. 
            /// </summary>
            REPLIED,

            /// <summary> Message was rejected. </summary>
            REJECTED
        }


        /// <summary> Helper class for reporting full message state 
        /// </summary>
        public sealed class MessageStateInfo
        {
            /// <summary> Current state of the message. </summary>
            public readonly MessageState State;

            /// <summary> Number of bytes that were transmitted. </summary>
            public readonly int SentBytes;

            /// <summary> Total length of the message. </summary>
            public readonly int TotalByteCount;

            /// <summary> Reply body (valid only when state is REPLIED) 
            /// </summary>
            public readonly Parameters ReplyBody;

            /// <summary> Reply raw binary body (valid only when state is 
            /// REPLIED and the agent was configured for raw delivery). 
            /// </summary>
            public readonly byte[] RawReplyBody;

            /// <summary> 
            /// Rejection reson (valid only when state is REJECTED) 
            /// </summary>
            public readonly string RejectionReason;

            internal MessageStateInfo(MessageState state, int sentBytes, 
                int totalByteCount, Parameters replyBody, 
                byte[] rawReplyBody, string rejectionReason)
            {
                State = state;
                SentBytes = sentBytes;
                TotalByteCount = totalByteCount;
                ReplyBody = replyBody;
                RawReplyBody = rawReplyBody;
                RejectionReason = rejectionReason;
            }
        }

        #region MessageProgressCallback Members

        void MessageProgressCallback.progress(
            int progrSentBytes, int progrTotalByteCount)
        {
            OutgoingMessage message = (OutgoingMessage)this;

            bool lastNotification = false;
            lock(message)
            {
                message.sentBytes = progrSentBytes;
                message.totalByteCount = progrTotalByteCount;

                if(progrSentBytes == progrTotalByteCount)
                {
                    // there will be no more progress notifications
                    // for this message

                    lastNotification = true;

                    if(progrSentBytes != 0)
                    {
                        // the transmission of the whole message
                        // was successful

                        if(message.state == MessageState.POSTED)
                        {
                            message.state = MessageState.TRANSMITTED;

                            message.transmitted = true;
                            Monitor.PulseAll(message);
                        }

                    }
                    else
                    {
                        // the message was abandoned
                        // before it was fully transmitted

                        message.state = MessageState.ABANDONED;

                        message.transmitted = true;
                        message.completed = true;
                        Monitor.PulseAll(message);
                    }
                }
            }

            if(lastNotification)
            {
                // the message is treated as leaving the output queue

                message.outgoingFlowManager.decrease();

                message.updateUserCallback();
            }
        }

        void MessageProgressCallback.replied(Parameters body, byte[] rawBody)
        {
            OutgoingMessage message = (OutgoingMessage)this;

            lock(message)
            {
                if(state == MessageState.POSTED 
                    || state == MessageState.TRANSMITTED)
                {

                    MessageState previousState = state;

                    state = MessageState.REPLIED;
                    reply = body;
                    rawReply = rawBody;

                    if(previousState == MessageState.POSTED)
                    {
                        transmitted = true;
                    }

                    completed = true;
                    Monitor.PulseAll(message);
                }
            }

            message.updateUserCallback();
        }


        void MessageProgressCallback.rejected(string reason)
        {
            OutgoingMessage message = (OutgoingMessage)this;

            lock(message)
            {
                if(state == MessageState.POSTED 
                    || state == MessageState.TRANSMITTED)
                {

                    MessageState previousState = state;

                    state = MessageState.REJECTED;
                    rejectionReason = reason;

                    if(previousState == MessageState.POSTED)
                    {
                        transmitted = true;
                    }

                    completed = true;
                    Monitor.PulseAll(message);
                }
            }

            message.updateUserCallback();
        }

        #endregion

        private readonly long messageId;
        private readonly OutgoingMessageHandler updateCallback;
        private readonly OutgoingMessageManager messageManager;
        private readonly WaterFlowManager outgoingFlowManager;
        private MessageState state = MessageState.POSTED;
        private int sentBytes = 0;
        private int totalByteCount = 0;
        private Parameters reply = null;
        private byte[] rawReply = null;
        private string rejectionReason = null;

        private bool transmitted = false;
        private bool completed = false;

        internal OutgoingMessage(long messageId, 
            OutgoingMessageHandler updateCallback,
            OutgoingMessageManager messageManager, 
            WaterFlowManager outgoingFlowManager)
        {
            this.messageId = messageId;
            this.updateCallback = updateCallback;
            this.messageManager = messageManager;
            this.outgoingFlowManager = outgoingFlowManager;
        }

        /// <summary> Gets the current state of the message. </summary>
        public MessageState State
        {
            get
            {
                lock (this)
                {
                    return state;
                }
            }
        }

        /// <summary> 
        /// Gets the current state and information about the message 
        /// progress. 
        /// </summary>
        public MessageStateInfo StateInfo
        {
            get
            {
                lock (this)
                {
                    return new MessageStateInfo(
                        state, sentBytes, totalByteCount, 
                        reply, rawReply, rejectionReason);
                }
            }
        }

        /// 
        /// <summary> Gets the reply body.
        /// </summary>
        /// <value>Reply body if the current state is REPLIED</value> 
        /// <exception cref="BadStateException"> if the current state is 
        /// not REPLIED </exception>
        /// 
        public Parameters Reply
        {
            get
            {
                lock (this)
                {
                    if (state == MessageState.REPLIED && reply != null)
                    {
                        return reply;
                    }
    
                    throw new BadStateException(
                        "Cannot return the reply body for this message" 
                        + " (no reply arrived yet).");
                }
            }
        }

        /// <summary> Gets the raw reply body.
        /// </summary>
        /// <exception cref="BadStateException"> if the current state is not
        /// REPLIED or if the agent was not configured for raw delivery 
        /// </exception>
        public byte[] RawReply
        {
            get
            {
                lock (this)
                {
                    if (state == MessageState.REPLIED && rawReply != null)
                    {
                        return rawReply;
                    }
    
                    throw new BadStateException(
                        "Cannot return the reply body for this message" 
                        + " (no reply arrived yet).");
                }
            }
        }

        /// <summary> Gets the rejection reason for this message
        /// </summary>
        /// <value>Rejection reason (or exception message)
        /// if the current state is REJECTED </value>
        /// <exception cref="BadStateException"> if the current state is not 
        /// REJECTED </exception>
        public string ExceptionMsg
        {
            get
            {
                lock (this)
                {
                    if (state == MessageState.REJECTED)
                    {
                        return rejectionReason;
                    }
    
                    throw new BadStateException(
                        "Cannot return the rejection reason" 
                        + " (this message has not been rejected).");
                }
            }
        }


        /// <summary>
        /// ticks per millisecond
        /// </summary>
        private const int TICKS_PER_MS = 10000;

        /// <summary> Waits for the transmission of this message.</summary>
        /// <remarks>
        /// The wait is terminated either when the transmission
        /// is successful or when the message is cancelled due to error
        /// or channel closing request.
        /// </remarks>
        public void WaitForTransmission()
        {
            lock (this)
            {
                while (transmitted == false)
                {
                    try
                    {
                        Monitor.Wait(this);
                    }
                    catch(ThreadInterruptedException)
                    {
                        // ignore, will never happen
                    }
                }
            }
        }

        /// <summary> Waits for the transmission of this message.</summary>
        /// <remarks>
        /// The wait is terminated either when the transmission
        /// is successful or when the message is cancelled due to error
        /// or channel closing request or when the timeout expires.
        /// </remarks>
        /// <param name="relativeTimeout"> relative timeout in milliseconds 
        /// </param>
        public void WaitForTransmission(long relativeTimeout)
        {
            long now = System.DateTime.Now.Ticks;
            long deadline = now + relativeTimeout * TICKS_PER_MS;
            WaitForTransmissionAbsolute(deadline);
        }

        /// <summary> Waits for the transmission of this message.</summary>
        /// <remarks>
        /// The wait is terminated either when the transmission
        /// is successful or when the message is cancelled due to error
        /// or channel closing request or when the timeout expires.
        /// </remarks>
        /// <param name="absoluteTimeout"> absolute timeout in milliseconds,
        /// counted from the beginning of the epoch (UTC) </param>
        public void WaitForTransmissionAbsolute(long absoluteTimeout)
        {
            lock (this)
            {
                while (transmitted == false)
                {
                    try
                    {
                        long now = System.DateTime.Now.Ticks;
                        long delay = absoluteTimeout - now;
                        if (delay <= 0L)
                        {
                            break;
                        }
                        Monitor.Wait(this, (int)(delay / TICKS_PER_MS));
                    }
                    catch(ThreadInterruptedException)
                    {
                       // ignore, will never happen
                    }
                }
            }
        }

        /// <summary> Waits for the completion of this message.</summary>
        /// <remarks>
        /// The wait is terminated either when the message
        /// is completed - which means that there was a reply or rejection
        /// received for it - or when the message is cancelled due to error
        /// or channel closing request.
        /// </remarks>
        public void WaitForCompletion()
        {
            lock (this)
            {
                while (completed == false)
                {
                    try
                    {
                        Monitor.Wait(this);
                    }
                    catch(ThreadInterruptedException)
                    {
                        // ignore, will never happen
                    }
                }
            }
        }

        /// <summary> Waits for the completion of this message.</summary>
        /// <remarks>
        /// The wait is terminated either when the message
        /// is completed - which means that there was a reply or rejection
        /// received for it - or when the message is cancelled due to error
        /// or channel closing request or when the timeout expires.
        /// </remarks>
        /// <param name="relativeTimeout"> relative timeout in milliseconds 
        /// </param>
        public void WaitForCompletion(long relativeTimeout)
        {
            long now = System.DateTime.Now.Ticks;
            long deadline = now + relativeTimeout * TICKS_PER_MS;
            WaitForCompletionAbsolute(deadline);
        }

        /// <summary> Waits for the completion of this message.</summary>
        /// <remarks>
        /// The wait is terminated either when the message
        /// is completed - which means that there was a reply or rejection
        /// received for it - or when the message is cancelled due to error
        /// or channel closing request or when the timeout expires.
        /// </remarks>
        /// <param name="absoluteTimeout"> absolute timeout in milliseconds,
        /// counted from the beginning of the epoch (UTC) </param>
        public void WaitForCompletionAbsolute(long absoluteTimeout)
        {
            lock (this)
            {
                while (completed == false)
                {
                    try
                    {
                        long now = System.DateTime.Now.Ticks;
                        long delay = absoluteTimeout - now;
                        if(delay <= 0L)
                        {
                            break;
                        }
                        Monitor.Wait(this, (int)(delay / TICKS_PER_MS));
                    }
                    catch(ThreadInterruptedException)
                    {
                        // ignore, will never happen
                    }
                }
            }
        }

        /// <summary> Cleans up internal resources related to this message. 
        /// </summary>
        public void Close()
        {
            messageManager.unregisterMessage(messageId);
        }

        internal MessageProgressCallback ProgressCallback
        {
            get
            {
                return this;
            }
        }

        private void updateUserCallback()
        {
            if(updateCallback != null)
            {
                try
                {
                    updateCallback(this);
                }
                catch
                {
                }
            }
        }
    }

}
