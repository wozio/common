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
