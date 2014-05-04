import com.inspirel.yami.Agent;
import com.inspirel.yami.IncomingMessage;
import com.inspirel.yami.IncomingMessageCallback;
import com.inspirel.yami.Parameters;

public class Server {
    private static class Consumer
        implements IncomingMessageCallback {

        @Override
        public void call(IncomingMessage im)
            throws Exception {

            int index =
                im.getParameters().getInteger("index");

            System.out.println(
                "processing message " + index +
                " from " + im.getSource());
        }
    }

    public static void main(String[] args) {
        if (args.length != 1 && args.length != 3) {
            System.out.println(
                "need 1 or 3 parameters:\n" +
                "   - server address\n" +
                "   - incoming high water mark\n" +
                "   - incoming low water mark\n" +
                "If only server address is given," +
                " the limits will have default values");
            return;
        }

        String serverAddress = args[0];

        Parameters options = new Parameters();
        if (args.length == 3) {
            int incomingHighWaterMark;
            int incomingLowWaterMark;

            try {
                incomingHighWaterMark =
                    Integer.parseInt(args[1]);
                incomingLowWaterMark =
                    Integer.parseInt(args[2]);
            } catch (NumberFormatException ex) {
                System.out.println("invalid arguments");
                return;
            }

            options.setInteger("incoming_high_water_mark",
                incomingHighWaterMark);
            options.setInteger("incoming_low_water_mark",
                incomingLowWaterMark);
        }

        try {
            Agent serverAgent = new Agent(options);

            String resolvedAddress =
                serverAgent.addListener(serverAddress);

            System.out.println(
                "The server is listening on " +
                resolvedAddress);

            serverAgent.registerObject(
                "object", new Consumer());

            // block
            while (true) {
                Thread.sleep(10000);
            }
        } catch (Exception ex) {
            System.out.println(
                "error: " + ex.getMessage());
        }
    }
}
