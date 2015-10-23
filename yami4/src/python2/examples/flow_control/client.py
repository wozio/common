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

argc = len(sys.argv)
if argc != 2 and argc != 5:
    print "need 1 or 4 parameters:"
    print "   - server address"
    print "   - outgoing high water mark"
    print "   - outgoing low water mark"
    print "   - number of iterations"
    print "If only server address is given," + \
        " the limits will have default values" \
        " and the loop will be infinite"
    exit()

server_address = sys.argv[1]
num_of_iterations = -1

options = {}
if argc == 5:
    try:
        outgoing_high_water_mark = int(sys.argv[2])
        outgoing_low_water_mark = int(sys.argv[3])
        num_of_iterations = int(sys.argv[4])
    except ValueError:
        print "invalid arguments"
        exit()

    options[yami.Agent.OptionNames.OUTGOING_HIGH_WATER_MARK] = \
        outgoing_high_water_mark
    options[yami.Agent.OptionNames.OUTGOING_LOW_WATER_MARK] = \
        outgoing_low_water_mark


try:
    client_agent = yami.Agent(options)
    try:
    
        index = 1
        while True:
            params = {"index":index}

            client_agent.send_one_way(
                server_address, "object", "message", params)

            print "posted message", index

            if num_of_iterations > 0:
                if index == num_of_iterations:
                    break

            index = index + 1
    finally:
        client_agent.close()

except Exception, e:
    print "error:", e
