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
