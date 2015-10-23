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

using NUnit.Framework;
using System.Threading;

namespace Inspirel.YAMI
{
    [TestFixture]
    public class SubscriptionTest
    {
        private readonly string localAddress = "tcp://*:*";

        private static void pause()
        {
            Thread.Sleep(1000);
        }

        /// <summary> simple subscribe and unsubscribe </summary>
        [Test]
        public void testSubscribeUnsubscribe()
        {
            Agent publisherAgent = new Agent();

            string publisherAddress = 
                publisherAgent.AddListener(localAddress);

            ValuePublisher value = new ValuePublisher();
            publisherAgent.RegisterValuePublisher("my_value", value);

            // no subscribers yet
            Assert.AreEqual(0, value.NumberOfSubscribers);
            Assert.AreEqual(0, value.Subscribers.Count);

            // set up the subscriber side
            bool gotUpdate = false;

            Agent subscriberAgent = new Agent();
            subscriberAgent.RegisterObject("my_update_handler",
                delegate(object sender, IncomingMessageArgs args)
                {
                    Assert.AreEqual("subscription_update", 
                        args.Message.MessageName);
                    gotUpdate = true;
                });
 
            // subscribe
            Parameters parameters = new Parameters();
            parameters.SetString("destination_object", "my_update_handler");
            OutgoingMessage subscribeMsg = subscriberAgent.Send(
                publisherAddress, "my_value", "subscribe", parameters);

            subscribeMsg.WaitForCompletion();

        // there should be one subscriber, as seen at the publisher side

            Assert.AreEqual(1, value.NumberOfSubscribers);
            Assert.AreEqual(1, value.Subscribers.Count);
            Assert.AreEqual("my_update_handler", 
                value.Subscribers[0].DestinationObject);

            // publish some value
            Parameters dummy = new Parameters();
            value.Publish(dummy);

            // check if the listener got it
            pause();
            Assert.IsTrue(gotUpdate);

            // unsubscribe
            OutgoingMessage unsubscribeMsg = subscriberAgent.Send(
                publisherAddress, "my_value", "unsubscribe", null);

            unsubscribeMsg.WaitForCompletion();

            // there should be no subscribers
            Assert.AreEqual(0, value.NumberOfSubscribers);
            Assert.AreEqual(0, value.Subscribers.Count);

            // check that the updates do not arrive any longer
            gotUpdate = false;
            value.Publish(dummy);

            pause();

            Assert.IsFalse(gotUpdate);

            value.Close();
            subscriberAgent.Close();
            publisherAgent.Close();
        }

        /// <summary> 
        /// dispatch of unknown commands 
        /// </summary>
        [Test]
        public void testUnknownCommands()
        {
            // set up the publisher side
            Agent publisherAgent = new Agent();

            string publisherAddress = 
                publisherAgent.AddListener(localAddress);

            bool gotUnknown = false;

            ValuePublisher value = new ValuePublisher(
                delegate(object sender, IncomingMessageArgs args)
                {
                    Assert.AreEqual("unknown", args.Message.MessageName); 
                    gotUnknown = true; 
                    args.Message.Reply(null);
                });
            publisherAgent.RegisterValuePublisher("my_value", value);

            // set up the subscriber side
            Agent subscriberAgent = new Agent();

            // send unknown command
            OutgoingMessage unknownMsg = subscriberAgent.Send(
                publisherAddress, "my_value", "unknown", null);

            unknownMsg.WaitForCompletion();

            Assert.IsTrue(gotUnknown);

            subscriberAgent.Close();
            publisherAgent.Close();
        }
    }

}