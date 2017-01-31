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
using Inspirel.YAMI;

namespace Printer
{
    class Client
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

                    clientAgent.SendOneWay(serverAddress,
                        "printer", "print", param);
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
