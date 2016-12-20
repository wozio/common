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
