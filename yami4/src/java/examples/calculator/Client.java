import com.inspirel.yami.Agent;
import com.inspirel.yami.OutgoingMessage;
import com.inspirel.yami.Parameters;

public class Client {
    public static void main(String[] args) {
        if (args.length != 3) {
            System.out.println(
                "expecting three parameters: " +
                "server destination and two integers");
            return;
        }

        String serverAddress = args[0];

        int a;
        int b;
        try {
            a = Integer.parseInt(args[1]);
            b = Integer.parseInt(args[2]);
        } catch (NumberFormatException ex) {
            System.out.println(
                "cannot parse the parameters");
            return;
        }

        try {
            Agent clientAgent = new Agent();

            Parameters params = new Parameters();
            params.setInteger("a", a);
            params.setInteger("b", b);

            OutgoingMessage message =
                clientAgent.send(serverAddress,
                    "calculator", "calculate", params);

            message.waitForCompletion();
            OutgoingMessage.MessageState state =
                message.getState();
            if (state == OutgoingMessage.MessageState.REPLIED) {
                Parameters reply = message.getReply();

                int sum =
                    reply.getInteger("sum");
                int difference =
                    reply.getInteger("difference");
                int product =
                    reply.getInteger("product");
                int ratio = 0;

                Parameters.Entry ratioEntry =
                    reply.find("ratio");
                boolean ratioDefined = ratioEntry != null;
                if (ratioDefined) {
                    ratio = ratioEntry.getInteger();
                }

                System.out.println("sum        = " + sum);
                System.out.println("difference = " +
                    difference);
                System.out.println("product    = " + product);

                System.out.print("ratio      = ");
                if (ratioDefined) {
                    System.out.println(ratio);
                } else {
                    System.out.println("<undefined>");
                }
            } else if (state ==
                OutgoingMessage.MessageState.REJECTED) {

                System.out.println(
                    "The message has been rejected: " +
                    message.getExceptionMsg());
            } else {
                System.out.println(
                    "The message has been abandoned.");
            }

            message.close();
            clientAgent.close();

        } catch (Exception ex) {
            System.out.println(
                "error: " + ex.getMessage());
        }
    }
}
