import com.inspirel.yami.Agent;
import com.inspirel.yami.Parameters;

public class Client {
    public static void main(String[] args) {
        if (args.length != 1 && args.length != 4) {
            System.out.println(
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

        String serverAddress = args[0];
        int numOfIterations = -1;

        Parameters options = new Parameters();
        if (args.length == 4) {
            int outgoingHighWaterMark;
            int outgoingLowWaterMark;

            try {
                outgoingHighWaterMark =
                    Integer.parseInt(args[1]);
                outgoingLowWaterMark =
                    Integer.parseInt(args[2]);
                numOfIterations =
                    Integer.parseInt(args[3]);
            } catch (NumberFormatException ex) {
                System.out.println("invalid arguments");
                return;
            }

            options.setInteger("outgoing_high_water_mark",
                outgoingHighWaterMark);
            options.setInteger("outgoing_low_water_mark",
                outgoingLowWaterMark);
        }

        try {
            Agent clientAgent = new Agent(options);

            Parameters params = new Parameters();

            int index = 1;
            while (true) {
                params.setInteger("index", index);

                clientAgent.sendOneWay(serverAddress,
                    "object", "message", params);

                System.out.println(
                    "posted message " + index);

                if (numOfIterations > 0) {
                    if (index == numOfIterations) {
                        break;
                    }
                }

                ++index;
            }

            clientAgent.close();

        } catch (Exception ex) {
            System.out.println(
                "error: " + ex.getMessage());
        }
    }
}
