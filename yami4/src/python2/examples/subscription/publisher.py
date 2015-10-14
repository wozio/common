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

import random
import sys
import time
import yami

if len(sys.argv) != 2:
    print "expecting one parameter: publisher destination"
    exit()

publisher_address = sys.argv[1]

try:
    random_value = yami.ValuePublisher()

    publisher_agent = yami.Agent()
    try:
        resolved_address = publisher_agent.add_listener(
            publisher_address)

        print "The publisher is listening on", resolved_address

        publisher_agent.register_value_publisher(
            "random_number", random_value)

        # publish random numbers forever
        while True:
            rnd = random.randint(0, 99)
            content = {"value":rnd}

            print "publishing value", rnd

            random_value.publish(content)

            time.sleep(1)
    finally:
        publisher_agent.close()

except Exception, e:
    print "error:", e
