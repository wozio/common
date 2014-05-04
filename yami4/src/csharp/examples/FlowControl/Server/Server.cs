using System;
using System.Threading;
using Inspirel.YAMI;

namespace FlowControl
{
    public class Server
    {
        private static void Consumer(
            object sender, IncomingMessageArgs args)
        {
            int index = 
                args.Message.Parameters.GetInteger("index");

            Console.WriteLine("processing message {0} from {1}", 
                index, args.Message.Source);
        }

        static void Main(string[] args)
        {
            if(args.Length != 1 && args.Length != 3)
            {
                Console.WriteLine(
                    "need 1 or 3 parameters:\n" +
                    "   - server address\n" +
                    "   - incoming high water mark\n" +
                    "   - incoming low water mark\n" +
                    "If only server address is given," +
                    " the limits will have default values");
                return;
            }

            string serverAddress = args[0];

            Parameters options = new Parameters();
            if(args.Length == 3)
            {
                int incomingHighWaterMark;
                int incomingLowWaterMark;

                try
                {
                    incomingHighWaterMark = int.Parse(args[1]);
                    incomingLowWaterMark = int.Parse(args[2]);
                }
                catch(FormatException)
                {
                    Console.WriteLine("invalid arguments");
                    return;
                }

                options.SetInteger("incoming_high_water_mark",
                    incomingHighWaterMark);
                options.SetInteger("incoming_low_water_mark",
                    incomingLowWaterMark);
            }

            try
            {
                Agent serverAgent = new Agent(options);

                string resolvedAddress =
                    serverAgent.AddListener(serverAddress);

                Console.WriteLine(
                    "The server is listening on " +
                    resolvedAddress);

                serverAgent.RegisterObject(
                    "object", Consumer);

                // block
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
