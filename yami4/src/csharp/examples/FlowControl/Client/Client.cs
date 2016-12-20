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
using Inspirel.YAMI;

namespace FlowControl
{
    public class Client
    {
        static void Main(string[] args)
        {
            if(args.Length != 1 && args.Length != 4)
            {
                Console.WriteLine(
                    "need 1 or 4 parameters:\n" +
                    "   - server address\n" +
                    "   - outgoing high water mark\n" +
                    "   - outgoing low water mark\n" +
                    "   - number of iterations\n" +
                    "If only server address is given," +
                    " the limits will have default values" +
                    " and the loop will be infinite");
                return;
            }

            string serverAddress = args[0];
            int numOfIterations = -1;

            Parameters options = new Parameters();
            if(args.Length == 4)
            {
                int outgoingHighWaterMark;
                int outgoingLowWaterMark;

                try
                {
                    outgoingHighWaterMark = int.Parse(args[1]);
                    outgoingLowWaterMark = int.Parse(args[2]);
                    numOfIterations = int.Parse(args[3]);
                }
                catch(FormatException)
                {
                    Console.WriteLine("invalid arguments");
                    return;
                }

                options.SetInteger("outgoing_high_water_mark",
                    outgoingHighWaterMark);
                options.SetInteger("outgoing_low_water_mark",
                    outgoingLowWaterMark);
            }

            try
            {
                Agent clientAgent = new Agent(options);

                Parameters param = new Parameters();

                int index = 1;
                while(true)
                {
                    param.SetInteger("index", index);

                    clientAgent.SendOneWay(serverAddress,
                        "object", "message", param);

                    Console.WriteLine(
                        "posted message " + index);

                    if(numOfIterations > 0)
                    {
                        if(index == numOfIterations)
                        {
                            break;
                        }
                    }

                    ++index;
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
