import sys
import yami

if len(sys.argv) != 2:
    print "expecting one parameter: server destination"
    exit()

server_address = sys.argv[1]

try:
    client_agent = yami.Agent()
    try:

        # read lines of text from standard input
        # and post each one for transmission

        for line in sys.stdin:

            # the "content" field name is arbitrary,
            # but needs to be recognized at the server side

            parameters = {"content":line.rstrip()}

            client_agent.send_one_way(
                server_address, "printer", "print", parameters)
    finally:
        client_agent.close()

except Exception, e:
    print "error:", e
