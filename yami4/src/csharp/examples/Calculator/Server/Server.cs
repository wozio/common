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
                    "expecting one parameter: server destination");
                return;
            }

            string serverAddress = args[0];

            try
            {
                Agent serverAgent = new Agent();

                String resolvedAddress =
                    serverAgent.AddListener(serverAddress);

                System.Console.WriteLine(
                    "The server is listening on {0}",
                    resolvedAddress);

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
