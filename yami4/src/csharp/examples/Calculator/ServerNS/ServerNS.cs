﻿using System;
using System.Threading;
using Inspirel.YAMI;

namespace Calculator
{
    class Server
    {
        private static void calculator(
            object sender, IncomingMessageArgs args)
        {
            // extract the parameters for calculations
            Parameters param = args.Message.Parameters;

            int a = param.GetInteger("a");
            int b = param.GetInteger("b");

            // prepare the answer
            // with results of four calculations
            Parameters replyParams = new Parameters();

            replyParams.SetInteger("sum", a + b);
            replyParams.SetInteger("difference", a - b);
            replyParams.SetInteger("product", a * b);

            // if the ratio cannot be computed,
            // it is not included in the response
            // the client will interpret that fact properly
            if(b != 0)
            {
                replyParams.SetInteger("ratio", a / b);
            }

            args.Message.Reply(replyParams);

            Console.WriteLine(
                "got message with parameters {0} and {1}" +
                ", response has been sent back", a, b);
        }

        static void Main(string[] args)
        {
            if(args.Length != 1)
            {
                System.Console.WriteLine(
                    "expecting one parameter: name server address");
                return;
            }

            string nameServerAddress = args[0];

            try
            {
                Agent serverAgent = new Agent();

                // prepare the server and bind its address
                // to the name server

                String resolvedAddress =
                    serverAgent.AddListener("tcp://*:*");

                System.Console.WriteLine(
                    "The server is listening on {0}",
                    resolvedAddress);

                Parameters bindParams = new Parameters();
                bindParams.SetString("object", "calculator");
                bindParams.SetString("location", resolvedAddress);
                
                OutgoingMessage nsBind = 
                    serverAgent.Send(nameServerAddress,
                            "names", "bind", bindParams);
                
                nsBind.WaitForCompletion();
                if (nsBind.State != 
                    OutgoingMessage.MessageState.REPLIED) {
                    Console.WriteLine("error: {0}" +
                            nsBind.ExceptionMsg);
                    return;
                }
                
                nsBind.Close();

                Console.WriteLine("Address bound by name server.");
                
                serverAgent.RegisterObject("calculator", calculator);

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
