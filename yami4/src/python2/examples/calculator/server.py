import sys
import yami

if len(sys.argv) != 2:
    print "expecting one parameter: server destination"
    exit()

server_address = sys.argv[1]

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

        resolved_address = server_agent.add_listener(server_address)

        print "The server is listening on", resolved_address

        server_agent.register_object("calculator", call)

        # block
        dummy = sys.stdin.read()
    finally:
        server_agent.close()

except Exception, e:
    print "error:", e
