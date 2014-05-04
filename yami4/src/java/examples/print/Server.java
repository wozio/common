import com.inspirel.yami.Agent;
import com.inspirel.yami.IncomingMessage;
import com.inspirel.yami.IncomingMessageCallback;
import com.inspirel.yami.Parameters;

public class Server {
    private static class Printer
        implements IncomingMessageCallback {

        @Override
        public void call(IncomingMessage im) {
            Parameters params = im.getParameters();

            // extract the content field
            // and print it on standard output

            System.out.println(
                params.getString("content"));
        }
    }

    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println(
                "expecting one parameter: server destination");
            return;
        }

        String serverAddress = args[0];

        try {
            Agent serverAgent = new Agent();

            String resolvedAddress =
                serverAgent.addListener(serverAddress);

            System.out.println(
                "The server is listening on " +
                resolvedAddress);

            serverAgent.registerObject(
                "printer", new Printer());

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
