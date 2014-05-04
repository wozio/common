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
