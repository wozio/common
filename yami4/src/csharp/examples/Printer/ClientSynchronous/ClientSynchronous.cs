using System;
using Inspirel.YAMI;

namespace Printer
{
    class ClientSynchronous
    {
        static void Main(string[] args)
        {
            if(args.Length != 1)
            {
                Console.WriteLine(
                    "expecting one parameter: server destination");
                return;
            }

            string serverAddress = args[0];

            try
            {
                Agent clientAgent = new Agent();

                // read lines of text from standard input
                // and post each one for transmission

                string inputLine = null;
                while((inputLine = Console.ReadLine()) != null)
                {
                    Parameters param = new Parameters();

                    // the "content" field name is arbitrary,
                    // but needs to be recognized at the server side

                    param.SetString("content", inputLine);

                    OutgoingMessage message = 
                        clientAgent.Send(serverAddress,
                            "printer", "print", param);

                    message.WaitForTransmission();
                    message.Close();
                }

                clientAgent.Close();
            }
            catch(Exception ex)
            {
                Console.WriteLine(
                   "error: {0}", ex.Message);
            }

        }
    }
}
