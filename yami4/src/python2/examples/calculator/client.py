# Copyright Maciej Sobczak 2008-2015.
# This file is part of YAMI4.
#
# YAMI4 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# YAMI4 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

import sys
import yami

if len(sys.argv) != 4:
    print "expecting three parameters:", \
        "server destination and two integers"
    exit()

server_address = sys.argv[1]

try:
    a = int(sys.argv[2])
    b = int(sys.argv[3])
except ValueError:
    print "cannot parse the second or third parameter"
    exit()

try:
    client_agent = yami.Agent()
    try:

        params = {"a":a, "b":b}

        msg = client_agent.send(
            server_address, "calculator", "calculate", params)
        try:

            msg.wait_for_completion()
            state = msg.get_state()
            if state[0] == yami.OutgoingMessage.REPLIED:
                reply = msg.get_reply()

                sum = reply["sum"]
                difference = reply["difference"]
                product = reply["product"]

                if "ratio" in reply:
                    ratio = reply["ratio"]
                    ratio_defined = True
                else:
                    ratio_defined = False

                print "sum        =", sum
                print "difference =", difference
                print "product    =", product

                if ratio_defined:
                    print "ratio      =", ratio
                else:
                    print "ratio      = <undefined>"

            elif state[0] == yami.OutgoingMessage.REJECTED:
                print "The message has been rejected:", \
                    msg.get_exception_msg()
            else:
                print "The message has been abandoned."
        finally:
            msg.close()
    finally:
        client_agent.close()

except Exception, e:
    print "error:", e
