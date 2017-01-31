// Copyright Maciej Sobczak 2008-2015.
// This file is part of YAMI4.
//
// YAMI4 is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// YAMI4 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

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
