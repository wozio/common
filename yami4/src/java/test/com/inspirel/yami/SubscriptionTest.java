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

import org.junit.Test;
import static org.junit.Assert.*;

public class SubscriptionTest {

    private final String localAddress = "tcp://*:*";

    // helper for passing ackowledgements
    private static class BooleanHolder {
        boolean value;

        BooleanHolder(boolean value) {
            this.value = value;
        }
    }

    private static void pause() {
        try {
            Thread.sleep(1000);
        } catch (InterruptedException ex) {
            // ignore
        }
    }

    /**
     * simple subscribe and unsubscribe
     */
    @Test
    public void testSubscribeUnsubscribe() throws YAMIIOException {

        // set up the publisher side

        Agent publisherAgent = new Agent();

        final String publisherAddress =
                publisherAgent.addListener(localAddress);

        ValuePublisher value = new ValuePublisher();
        publisherAgent.registerValuePublisher("my_value", value);

        // no subscribers yet

        assertTrue(value.getNumberOfSubscribers() == 0);
        assertTrue(value.getSubscribers().size() == 0);

        // set up the subscriber side

        final BooleanHolder gotUpdate = new BooleanHolder(false);

        Agent subscriberAgent = new Agent();
        subscriberAgent.registerObject("my_update_handler",
                new IncomingMessageCallback() {

            @Override
            public void call(IncomingMessage message) throws Exception {
                assertTrue(message.getMessageName().equals(
                        "subscription_update"));
                gotUpdate.value = true;
            }
        });

        // subscribe

        Parameters params = new Parameters();
        params.setString("destination_object", "my_update_handler");
        OutgoingMessage subscribeMsg = subscriberAgent.send(publisherAddress,
                "my_value", "subscribe", params);

        subscribeMsg.waitForCompletion();

        // there should be one subscriber, as seen at the publisher side

        assertTrue(value.getNumberOfSubscribers() == 1);
        assertTrue(value.getSubscribers().size() == 1);
        assertTrue(value.getSubscribers().get(0).destinationObject.equals(
                "my_update_handler"));

        // publish some value

        Parameters dummy = new Parameters();
        value.publish(dummy);

        // check if the listener got it

        pause();

        assertTrue(gotUpdate.value);

        // unsubscribe

        OutgoingMessage unsubscribeMsg =
                subscriberAgent.send(publisherAddress,
                "my_value", "unsubscribe", null);

        unsubscribeMsg.waitForCompletion();

        // there should be no subscribers

        assertTrue(value.getNumberOfSubscribers() == 0);
        assertTrue(value.getSubscribers().size() == 0);

        // check that the updates do not arrive any longer

        gotUpdate.value = false;
        value.publish(dummy);

        pause();

        assertFalse(gotUpdate.value);

        value.close();
        subscriberAgent.close();
        publisherAgent.close();
    }

    /**
     * dispatch of unknown commands
     */
    @Test
    public void testUnknownCommands() throws YAMIIOException {

        // set up the publisher side

        Agent publisherAgent = new Agent();

        final String publisherAddress =
                publisherAgent.addListener(localAddress);

        final BooleanHolder gotUnknown = new BooleanHolder(false);

        ValuePublisher value = new ValuePublisher(
                new IncomingMessageCallback() {

            @Override
            public void call(IncomingMessage message) throws Exception {
                assertTrue(message.getMessageName().equals("unknown"));
                gotUnknown.value = true;

                message.reply(null);
            }
        });
        publisherAgent.registerValuePublisher("my_value", value);

        // set up the subscriber side

        Agent subscriberAgent = new Agent();

        // send unknown command

        OutgoingMessage unknownMsg = subscriberAgent.send(publisherAddress,
                "my_value", "unknown", null);

        unknownMsg.waitForCompletion();

        assertTrue(gotUnknown.value);
        
        subscriberAgent.close();
        publisherAgent.close();
    }
}
