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

namespace Subscription
{
    class Publisher
    {
        static void Main(string[] args)
        {
            if(args.Length != 1)
            {
                Console.WriteLine(
                    "expecting one parameter: " +
                    "publisher destination");
                return;
            }

            string publisherAddress = args[0];

            try
            {
                ValuePublisher randomValue =
                    new ValuePublisher();

                Agent publisherAgent = new Agent();

                string resolvedAddress =
                    publisherAgent.AddListener(publisherAddress);

                Console.WriteLine(
                    "The publisher is listening on {0}",
                    resolvedAddress);

                publisherAgent.RegisterValuePublisher(
                    "random_number", randomValue);

                // publish random values forever
                Parameters content = new Parameters();
                Random generator = new Random();
                while(true)
                {
                    int random = generator.Next(0, 100);
                    content.SetInteger("value", random);

                    Console.WriteLine("publishing value {0}", 
                        random);

                    randomValue.Publish(content);

                    Thread.Sleep(1000);
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
