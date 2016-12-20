// Copyright Maciej Sobczak 2008-2015.
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

import com.inspirel.yami.Agent;
import com.inspirel.yami.Parameters;
import com.inspirel.yami.ValuePublisher;

import static java.lang.Math.abs;
import java.util.Random;

public class Publisher {

    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println(
                "expecting one parameter: " +
                "publisher destination");
            return;
        }

        String publisherAddress = args[0];

        try {
            ValuePublisher randomValue =
                new ValuePublisher();

            Agent publisherAgent = new Agent();

            String resolvedAddress =
                publisherAgent.addListener(publisherAddress);

            System.out.println(
                "The publisher is listening on " +
                resolvedAddress);

            publisherAgent.registerValuePublisher(
                "random_number", randomValue);

            // publish random values forever
            Parameters content = new Parameters();
            Random generator = new Random();
            while (true) {
                subscription.Payload p =
                    new subscription.Payload();
                    
                int random =
                    abs(generator.nextInt()) % 100;
                p.value = random;

                System.out.println(
                    "publishing value " + random);

                Parameters params = new Parameters();
                p.write(params);
                
                randomValue.publish(params);

                Thread.sleep(1000);
            }
        } catch (Exception ex) {
            System.out.println(
                "error: " + ex.getMessage());
        }
    }
}
