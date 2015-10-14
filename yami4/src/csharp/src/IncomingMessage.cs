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

    /// 
    /// <summary> Incoming message handler.</summary>
    /// <remarks>
    /// <para>
    /// The handler allowing to inspect the details of the incoming message
    /// and sent back replies or rejection notifications.
    /// </para>
    /// <para>
    /// The user code interacts with objects of this type mainly in
    /// callbacks that are provided during object registration and that are 
    /// later called back when the incoming message arrives. The handler 
    /// objects can be stored aside for further processing even after the 
    /// callback returns, but should no be used after the agent itself is 
    /// closed.
    /// </para>
    /// <para>
    /// <b>Note:</b>
    /// The objects of this class are not supposed to be used
    /// from multiple threads.
    /// </para> 
    /// </remarks>
    /// <seealso cref="Agent.RegisterObject"/>
    /// <seealso cref="IncomingMessageHandler"/>
    /// <seealso cref="IncomingMessageArgs"/>
    public class IncomingMessage
    {

        private readonly Agent agent;
        private readonly long messageId;
        private readonly string sourceName;
        private readonly string objectName;
        private readonly string messageName;
        private readonly Parameters body;
        private readonly byte[] rawBody;
        private bool alreadyUsed;

        internal IncomingMessage(Agent agent, long messageId, 
            string sourceName, string objectName, string messageName, 
            Parameters body, byte[] rawBody)
        {
            this.agent = agent;
            this.messageId = messageId;
            this.sourceName = sourceName;
            this.objectName = objectName;
            this.messageName = messageName;
            this.body = body;
            this.rawBody = rawBody;
            alreadyUsed = false;
        }

        /// <summary> Gets the target name of the message sender 
        /// (the message source).
        /// </summary>
        /// <value>Source name</value>
        public virtual string Source
        {
            get
            {
                return sourceName;
            }
        }

        /// <summary> Gets the destination object name.</summary>
        /// <value>Destination object name</value>
        public virtual string ObjectName
        {
            get
            {
                return objectName;
            }
        }

        /// <summary> Gets the message name.</summary>
        /// <value>Message name</value>
        public virtual string MessageName
        {
            get
            {
                return messageName;
            }
        }

        /// <summary> Gets the message parameters (message body).</summary>
        /// <value>Message parameters</value>
        /// <exception cref="BadStateException">Thrown if incoming
        /// message don't have body</exception>
        public virtual Parameters Parameters
        {
            get
            {
                if (body == null)
                {
                    throw new BadStateException("incoming message");
                }
    
                return body;
            }
        }

        /// <summary> Get the message raw (binary) content. </summary>
        /// <value>Message raw content</value>
        /// <exception cref="BadStateException"> 
        /// If the agent was not configured
        /// for raw content delivery or message don't have body</exception>
        public virtual byte[] RawContent
        {
            get
            {
                if (rawBody == null)
                {
                    throw new BadStateException("incoming message");
                }
    
                return rawBody;
            }
        }

        /// <summary> Send back the reply with default (lowest) priority.
        /// </summary>
        /// <param name="replyBody"> the content of the reply
        /// (it can be null, in which case the empty object is sent) </param>
        /// <exception cref="YAMIIOException"> 
        /// if the reply channel cannot be established </exception>
        /// <exception cref="BadStateException">If the reply was
        /// already sent.</exception>
        public virtual void Reply(YAMISerializable replyBody)
        {
            Reply(replyBody, 0);
        }

        /// <summary> Send back the reply with given priority.
        /// </summary>
        /// <param name="replyBody"> the content of the reply
        /// (it can be null, in which case the empty object is sent) </param>
        /// <param name="priority"> the priority of reply </param>
        /// <exception cref="YAMIIOException">If the reply channel cannot be 
        /// established </exception>
        /// <exception cref="BadStateException">If the reply was
        /// already sent.</exception>
        public virtual void Reply(
            YAMISerializable replyBody, int priority)
        {
            if (alreadyUsed)
            {
                throw new BadStateException("incoming message");
            }
            agent.doSendReply(sourceName, messageId, replyBody, priority);
            alreadyUsed = true;
        }

        /// <summary> Send back the rejection (exception) notification
        /// with default (lowest) priority.
        /// </summary>
        /// <param name="reason"> arbitrary text that will be visible by the
        /// original sender as a reason for rejection </param>
        /// <exception cref="YAMIIOException">If the reply channel cannot be 
        /// established </exception>
        /// <exception cref="BadStateException">If the rejection
        /// already sent</exception>
        public virtual void Reject(string reason)
        {
            Reject(reason, 0);
        }

        /// <summary> Send back the rejection (exception) notification
        /// with given priority.
        /// </summary>
        /// <param name="reason"> arbitrary text that will be visible by the
        /// original sender as a reason for rejection </param>
        /// <param name="priority"> the priority of rejection </param>
        /// <exception cref="YAMIIOException">If the reply channel cannot be 
        /// established </exception>
        /// <exception cref="BadStateException">If the rejection
        /// already sent</exception>
        public virtual void Reject(string reason, int priority)
        {
            if (alreadyUsed)
            {
                throw new BadStateException("incoming message");
            }
            agent.doSendReject(sourceName, messageId, reason, priority);
            alreadyUsed = true;
        }
    }

}