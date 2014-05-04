import com.inspirel.yami.Agent;
import com.inspirel.yami.Parameters;

import java.io.BufferedReader;
import java.io.InputStreamReader;

public class Client {
    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println(
                "expecting one parameter: server destination");
            return;
        }

        String serverAddress = args[0];

        try {
            Agent clientAgent = new Agent();

            // read lines of text from standard input
            // and post each one for transmission

            BufferedReader reader = new BufferedReader(
                new InputStreamReader(System.in));
            String inputLine;
            while (
                (inputLine = reader.readLine()) != null) {

                Parameters params = new Parameters();

                // the "content" field name is arbitrary,
                // but needs to be recognized at the server side

                params.setString("content", inputLine);

                clientAgent.sendOneWay(serverAddress,
                    "printer", "print", params);
            }

            clientAgent.close();

        } catch (Exception ex) {
            System.out.println(
                "error: " + ex.getMessage());
        }
    }
}
