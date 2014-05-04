using System;
using System.Threading;
using Inspirel.YAMI;

namespace Printer
{
    class Server
    {
        private static void print(
            object sender, IncomingMessageArgs args)
        {
            Parameters param = args.Message.Parameters;

            // extract the content field
            // and print it on standard output

            Console.WriteLine(param.GetString("content"));
        }

        static void Main(string[] args)
        {
            if(args.Length != 1)
            {
                Console.WriteLine(
                    "expecting one parameter: server destination");
                return;
            }

            String serverAddress = args[0];

            try
            {
                Agent serverAgent = new Agent();

                String resolvedAddress =
                    serverAgent.AddListener(serverAddress);

                Console.WriteLine(
                    "The server is listening on " +
                    resolvedAddress);

                serverAgent.RegisterObject(
                    "printer", print);

                // block
                while(true)
                {
                    Thread.Sleep(10000);
                }
            }
            catch(Exception ex)
            {
                Console.WriteLine("error: {0}", ex.Message);
            }
        }
    }
}
