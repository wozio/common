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
using System.Threading;
using Inspirel.YAMI;

namespace Subscription
{
    class Subscriber
    {
        private static void updateHandler(
            object sender, IncomingMessageArgs args)
        {
            Parameters content = args.Message.Parameters;

            int value = content.GetInteger("value");

            Console.WriteLine("received update {0}", value);
        }

        static void Main(string[] args)
        {
            if(args.Length != 1)
            {
                Console.WriteLine(
                    "expecting one parameter: " +
                    "publisher destination");
                return;
            }

            String publisherAddress = args[0];

            try
            {
                Agent subscriberAgent = new Agent();

                // prepare subscription update callback

                string updateObjectName =
                    "update_handler";

                subscriberAgent.RegisterObject(
                    updateObjectName, updateHandler);

                // subscribe to the producer

                Parameters param = new Parameters();
                param.SetString(
                    "destination_object", updateObjectName);

                subscriberAgent.SendOneWay(publisherAddress,
                    "random_number", "subscribe", param);

                Console.WriteLine(
                    "subscribed, waiting for updates");

                // block forever
                // and receive updates in background
                while(true)
                {
                    Thread.Sleep(10000);
                }
            }
            catch(Exception ex)
            {
                Console.WriteLine(
                    "error: {0}", ex.Message);
            }
        }
    }
}
