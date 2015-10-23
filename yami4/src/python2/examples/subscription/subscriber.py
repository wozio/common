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

def update_handler(message):
    content = message.get_parameters()
    value = content["value"]
    print "received update:", value


if len(sys.argv) != 2:
    print "expecting one parameter: publisher destination"
    exit()

publisher_address = sys.argv[1]

try:
    subscriber_agent = yami.Agent()
    try:

        # prepare subscription update callback

        update_object_name = "update_handler"

        subscriber_agent.register_object(
            update_object_name, update_handler)

        # subscribe to the producer

        params = {"destination_object":update_object_name}

        subscriber_agent.send_one_way(publisher_address,
            "random_number", "subscribe", params)

        print "subscribed, waiting for updates"

        # block forever and receive updates in background

        dummy = sys.stdin.read()
    finally:
        subscriber_agent.close()

except Exception, e:
    print "error:", e
