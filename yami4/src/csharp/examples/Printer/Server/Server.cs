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
