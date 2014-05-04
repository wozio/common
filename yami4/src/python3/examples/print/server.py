import sys
import yami

if len(sys.argv) != 2:
    print("expecting one parameter: server destination")
    exit()

server_address = sys.argv[1]

def call(msg):
    # extract the content field
    # and print it on standard output

    params = msg.get_parameters()
    print(params["content"])


try:
    with yami.Agent() as server_agent:

        resolved_address = server_agent.add_listener(server_address)

        print("The server is listening on", resolved_address)

        server_agent.register_object("printer", call)

        # block
        dummy = sys.stdin.read()

except Exception as e:
    print("error:", e)
