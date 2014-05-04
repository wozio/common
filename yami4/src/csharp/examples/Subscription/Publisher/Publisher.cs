using System;
using System.Threading;
using Inspirel.YAMI;

namespace Subscription
{
    class Publisher
    {
        static void Main(string[] args)
        {
            if(args.Length != 1)
            {
                Console.WriteLine(
                    "expecting one parameter: " +
                    "publisher destination");
                return;
            }

            string publisherAddress = args[0];

            try
            {
                ValuePublisher randomValue =
                    new ValuePublisher();

                Agent publisherAgent = new Agent();

                string resolvedAddress =
                    publisherAgent.AddListener(publisherAddress);

                Console.WriteLine(
                    "The publisher is listening on {0}",
                    resolvedAddress);

                publisherAgent.RegisterValuePublisher(
                    "random_number", randomValue);

                // publish random values forever
                Parameters content = new Parameters();
                Random generator = new Random();
                while(true)
                {
                    int random = generator.Next(0, 100);
                    content.SetInteger("value", random);

                    Console.WriteLine("publishing value {0}", 
                        random);

                    randomValue.Publish(content);

                    Thread.Sleep(1000);
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
