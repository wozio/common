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

if len(sys.argv) != 2:
    print "expecting one parameter: name server address"
    exit()

name_server_address = sys.argv[1]

def call(msg):

    # extract the parameters for calculations

    params = msg.get_parameters()

    a = params["a"]
    b = params["b"]

    # prepare the answer with results of four calculations

    reply_params = {"sum":a+b, "difference":a-b, "product":a*b}

    # if the ratio cannot be computed,
    # it is not included in the response
    # the client will interpret that fact properly
    if b != 0:
        reply_params["ratio"] = int(a / b)

    msg.reply(reply_params)

    print "got message with parameters", a, "and", b, \
        ", response has been sent back"


try:
    server_agent = yami.Agent()

    try:
        # prepare the server and bind its address
        # to the name server

        resolved_address = server_agent.add_listener("tcp://*:*")

        print "The server is listening on", resolved_address

        bind_params = {"object":"calculator",
                       "location":resolved_address}

        ns_bind = server_agent.send(
            name_server_address, "names", "bind", bind_params)

        ns_bind.wait_for_completion()
        if ns_bind.get_state() != yami.OutgoingMessage.REPLIED:
            print "error:", ns_bind.get_exception_msg()
            exit

        ns_bind.close()

        print "Address bound by name server."

        server_agent.register_object("calculator", call)

        # block
        dummy = sys.stdin.read()
    finally:
        server_agent.close()

except Exception, e:
    print "error:", e
