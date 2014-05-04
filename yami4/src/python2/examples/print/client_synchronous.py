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

            msg = client_agent.send(
                server_address, "printer", "print", parameters)

            msg.wait_for_transmission()
            msg.close()
    finally:
        client_agent.close()

except Exception, e:
    print "error:", e
