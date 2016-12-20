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

﻿using System;
using System.Threading;
using Inspirel.YAMI;

namespace Calculator
{
    class ClientNS
    {
        static void Main(string[] args)
        {
            if(args.Length != 3)
            {
                Console.WriteLine(
                    "expecting three parameters: " +
                    "name server address and two integers");
                return;
            }
            
            String nameServerAddress = args[0];

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

                // obtain the address of calculator server
                
                Parameters resolveParams = new Parameters();
                resolveParams.SetString("object", "calculator");
                
                OutgoingMessage nsQuery =
                    clientAgent.Send(nameServerAddress,
                            "names", "resolve", resolveParams);
                
                nsQuery.WaitForCompletion();
                if (nsQuery.State != 
                    OutgoingMessage.MessageState.REPLIED) {
                        Console.WriteLine("error: {0}",
                            nsQuery.ExceptionMsg);
                    return;
                }
                
                Parameters resolveReply = nsQuery.Reply;
                String calculatorAddress =
                    resolveReply.GetString("location");

                nsQuery.Close();
                
                // send message to the calculator object
                Parameters param = new Parameters();
                param.SetInteger("a", a);
                param.SetInteger("b", b);

                OutgoingMessage message =
                    clientAgent.Send(calculatorAddress,
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