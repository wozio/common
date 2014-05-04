import com.inspirel.yami.Agent;

public class Server {

    private static class CalculatorImpl
        extends calculator.OperationsServer {

        @Override
        public void calculate(
            calculator.Operands ops, calculator.Results res) {

            // prepare the answer
            // with results of four calculations

            res.sum = ops.a + ops.b;
            res.difference = ops.a - ops.b;
            res.product = ops.a * ops.b;

            // if the ratio cannot be computed,
            // it is not included in the response
            // the client will interpret that fact properly
            if (ops.b != 0) {
                res.ratio = ops.a / ops.b;
                res.ratioValid = true;
            }

            System.out.println("got message with parameters " +
                ops.a + " and " + ops.b +
                ", response has been sent back");
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

            CalculatorImpl myServer = new CalculatorImpl();

            serverAgent.registerObject("calculator", myServer);

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
