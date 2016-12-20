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
    print("expecting one parameter: server destination")
    exit()

server_address = sys.argv[1]

try:
    with yami.Agent() as client_agent:

        # read lines of text from standard input
        # and post each one for transmission

        for line in sys.stdin:

            # the "content" field name is arbitrary,
            # but needs to be recognized at the server side

            parameters = {"content":line.rstrip()}

            client_agent.send_one_way(
                server_address, "printer", "print", parameters)

except Exception as e:
    print("error:", e)
