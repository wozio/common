import com.inspirel.yami.Agent;

import java.io.BufferedReader;
import java.io.InputStreamReader;

public class ClientSynchronous {
    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println(
                "expecting one parameter: server destination");
            return;
        }

        String serverAddress = args[0];

        try {
            Agent clientAgent = new Agent();
            
            print.Printer myPrinter = new print.Printer(
                clientAgent, serverAddress, "printer");

            // read lines of text from standard input
            // and post each one for transmission

            BufferedReader reader = new BufferedReader(
                new InputStreamReader(System.in));
            String inputLine;
            while (
                (inputLine = reader.readLine()) != null) {

                print.Text msg = new print.Text();
                msg.content = inputLine;

                // the "content" field name is arbitrary,
                // but needs to be recognized at the server side

                myPrinter.printSynchronously(msg);
            }

            clientAgent.close();

        } catch (Exception ex) {
            System.out.println(
                "error: " + ex.getMessage());
        }
    }
}
