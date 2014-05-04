import com.inspirel.yami.Agent;
import com.inspirel.yami.IncomingMessage;
import com.inspirel.yami.IncomingMessageCallback;
import com.inspirel.yami.Parameters;

public class Subscriber {
    private static class UpdateHandler
        implements IncomingMessageCallback {

        @Override
        public void call(IncomingMessage im)
            throws Exception {

            Parameters content = im.getParameters();

            int value = content.getInteger("value");

            System.out.println("received update " + value);
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

            subscriberAgent.registerObject(
                updateObjectName, new UpdateHandler());

            // subscribe to the producer

            Parameters params = new Parameters();
            params.setString(
                "destination_object", updateObjectName);

            subscriberAgent.sendOneWay(publisherAddress,
                "random_number", "subscribe", params);

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
