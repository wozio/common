using System;
using System.Threading;
using Inspirel.YAMI;

namespace Calculator
{
    class Client
    {
        static void Main(string[] args)
        {
            if(args.Length != 3)
            {
                Console.WriteLine(
                    "expecting three parameters: " +
                    "server destination and two integers");
                return;
            }

            String serverAddress = args[0];

            int a;
            int b;
            try
            {
                a = int.Parse(args[1]);
                b = int.Parse(args[2]);
            }
            catch(Exception)
            {
                Console.WriteLine("cannot parse the parameters");
                return;
            }

            try
            {
                Agent clientAgent = new Agent();

                Parameters param = new Parameters();
                param.SetInteger("a", a);
                param.SetInteger("b", b);

                OutgoingMessage message =
                    clientAgent.Send(serverAddress,
                        "calculator", "calculate", param);

                message.WaitForCompletion();
                OutgoingMessage.MessageState state =
                    message.State;
                if(state == OutgoingMessage.MessageState.REPLIED)
                {
                    Parameters reply = message.Reply;

                    int sum = reply.GetInteger("sum");
                    int difference = reply.GetInteger("difference");
                    int product = reply.GetInteger("product");
                    int ratio = 0;

                    Parameters.Entry ratioEntry =
                        reply.Find("ratio");
                    bool ratioDefined = ratioEntry != null;
                    if(ratioDefined)
                    {
                        ratio = ratioEntry.GetInteger();
                    }

                    Console.WriteLine("sum        = {0}", sum);
                    Console.WriteLine("difference = {0}", 
                        difference);
                    Console.WriteLine("product    = {0}", product);

                    Console.Write("ratio      = ");
                    if(ratioDefined)
                    {
                        Console.WriteLine(ratio);
                    }
                    else
                    {
                        Console.WriteLine("<undefined>");
                    }
                }
                else if(state ==
                    OutgoingMessage.MessageState.REJECTED)
                {

                    Console.WriteLine(
                        "The message has been rejected: {0}",
                        message.ExceptionMsg);
                }
                else
                {
                    Console.WriteLine(
                        "The message has been abandoned.");
                }

                message.Close();
                clientAgent.Close();
            }
            catch(Exception ex)
            {
                Console.WriteLine("error: {0}" + ex.Message);
            }
        }
    }
}
