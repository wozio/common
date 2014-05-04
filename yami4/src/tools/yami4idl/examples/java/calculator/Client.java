import com.inspirel.yami.Agent;

public class Client {
    public static void main(String[] args) {
        if (args.length != 3) {
            System.out.println(
                "expecting three parameters: " +
                "server destination and two integers");
            return;
        }

        String serverAddress = args[0];

        calculator.Operands ops = new calculator.Operands();
        
        try {
            ops.a = Integer.parseInt(args[1]);
            ops.b = Integer.parseInt(args[2]);
        } catch (NumberFormatException ex) {
            System.out.println(
                "cannot parse the parameters");
            return;
        }

        try {
            Agent clientAgent = new Agent();
            
            calculator.Operations myCalculator =
                new calculator.Operations(
                   clientAgent, serverAddress, "calculator");

            calculator.Results res = new calculator.Results();
            
            myCalculator.calculate(ops, res);

            System.out.println("sum        = " + res.sum);
            System.out.println("difference = " + res.difference);
            System.out.println("product    = " + res.product);

            System.out.print("ratio      = ");
            if (res.ratioValid) {
                System.out.println(res.ratio);
            } else {
                System.out.println("<undefined>");
            }

            clientAgent.close();

        } catch (Exception ex) {
            System.out.println(
                "error: " + ex.getMessage());
        }
    }
}
