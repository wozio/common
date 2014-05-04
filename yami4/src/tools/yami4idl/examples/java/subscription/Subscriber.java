import com.inspirel.yami.Agent;

public class Subscriber {

    private static class SubscriberImpl
        extends subscription.SubscriberServer {

        @Override
        public void subscriptionUpdate(subscription.Payload p)
            throws Exception {

            System.out.println("received update " + p.value);
        }
    }

    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println(
                "expecting one parameter: " +
                "publisher destination");
            return;
        }

        String publisherAddress = args[0];

        try {
            Agent subscriberAgent = new Agent();

            // prepare subscription update callback

            final String updateObjectName =
                "update_handler";
            
            SubscriberImpl mySubscriber = new SubscriberImpl();

            subscriberAgent.registerObject(
                updateObjectName, mySubscriber);

            // subscribe to the producer
            
            subscription.Publisher myPublisher =
                new subscription.Publisher(
                    subscriberAgent, publisherAddress,
                    "random_number");

            subscription.SubscriptionInfo s =
                new subscription.SubscriptionInfo();
               
            s.destinationObject = updateObjectName;

            myPublisher.subscribe(s);

            System.out.println(
                "subscribed, waiting for updates");

            // block forever
            // and receive updates in background
            while (true) {
                Thread.sleep(10000);
            }
        } catch (Exception ex) {
            System.out.println(
                "error: " + ex.getMessage());
        }
    }
}
