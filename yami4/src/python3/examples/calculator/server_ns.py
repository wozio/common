import sys
import yami

if len(sys.argv) != 2:
    print("expecting one parameter: name server address")
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

    print("got message with parameters", a, "and", b, 
          ", response has been sent back")


try:
    with yami.Agent() as server_agent:

        # prepare the server and bind its address
        # to the name server

        resolved_address = server_agent.add_listener("tcp://*:*")

        print("The server is listening on", resolved_address)

        bind_params = {"object":"calculator",
                       "location":resolved_address}

        with server_agent.send(
            name_server_address,
            "names", "bind", bind_params) as ns_bind:

            ns_bind.wait_for_completion()
            if ns_bind.get_state() != yami.OutgoingMessage.REPLIED:
                print("error:", ns_bind.get_exception_msg())
                exit

        print("Address bound by name server.")

        server_agent.register_object("calculator", call)

        # block
        dummy = sys.stdin.read()

except Exception as e:
    print("error:", e)
