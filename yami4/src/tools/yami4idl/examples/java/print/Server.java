import com.inspirel.yami.Agent;
import com.inspirel.yami.IncomingMessage;
import com.inspirel.yami.IncomingMessageCallback;
import com.inspirel.yami.Parameters;

public class Server {

    private static class PrinterImpl
        extends print.PrinterServer {

        @Override
        public void print(print.Text t) {
            System.out.println(t.content);
        }

        @Override
        public void printSynchronously(print.Text t) {
            System.out.println(t.content);
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

            PrinterImpl myServer = new PrinterImpl();
            
            serverAgent.registerObject("printer", myServer);

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
