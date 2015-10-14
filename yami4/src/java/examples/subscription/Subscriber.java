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

import com.inspirel.yami.Agent;
import com.inspirel.yami.IncomingMessage;
import com.inspirel.yami.IncomingMessageCallback;
import com.inspirel.yami.Parameters;

public class Subscriber {
    private static class UpdateHandler
        implements IncomingMessageCallback {

        @Override
        public void call(IncomingMessage im)
            throws Exception {

            Parameters content = im.getParameters();

            int value = content.getInteger("value");

            System.out.println("received update " + value);
        }
    }

    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println(
                "expecting one parameter: " +
                "publisher destination");
            return;
        }

        String publisherAddress = args[0];

        try {
            Agent subscriberAgent = new Agent();

            // prepare subscription update callback

            final String updateObjectName =
                "update_handler";

            subscriberAgent.registerObject(
                updateObjectName, new UpdateHandler());

            // subscribe to the producer

            Parameters params = new Parameters();
            params.setString(
                "destination_object", updateObjectName);

            subscriberAgent.sendOneWay(publisherAddress,
                "random_number", "subscribe", params);

            System.out.println(
                "subscribed, waiting for updates");

            // block forever
            // and receive updates in background
            while (true) {
                Thread.sleep(10000);
            }
        } catch (Exception ex) {
            System.out.println(
                "error: " + ex.getMessage());
        }
    }
}
