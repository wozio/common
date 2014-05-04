import sys
import yami

if len(sys.argv) != 4:
    print("expecting three parameters:",
          "name server address and two integers")
    exit()

name_server_address = sys.argv[1]

try:
    a = int(sys.argv[2])
    b = int(sys.argv[3])
except ValueError:
    print("cannot parse the second or third parameter")
    exit()

try:
    with yami.Agent() as client_agent:

        # obtain the address of calculator server

        params = {"object":"calculator"}

        with client_agent.send(
            name_server_address,
            "names", "resolve", params) as ns_query:

            ns_query.wait_for_completion()
            if ns_query.get_state() != yami.OutgoingMessage.REPLIED:
                print("error:", ns_query.get_exception_msg())
                exit

            resolve_reply = ns_query.get_reply()
            calculator_address = resolve_reply["location"]

        # send message to the calculator object

        params = {"a":a, "b":b}

        with client_agent.send(
            calculator_address,
            "calculator", "calculate", params) as msg:

            msg.wait_for_completion()
            state = msg.get_state()
            if state[0] == yami.OutgoingMessage.REPLIED:
                reply = msg.get_reply()

                sum = reply["sum"]
                difference = reply["difference"]
                product = reply["product"]

                if "ratio" in reply:
                    ratio = reply["ratio"]
                    ratio_defined = True
                else:
                    ratio_defined = False

                print("sum        =", sum)
                print("difference =", difference)
                print("product    =", product)

                print("ratio      = ", end="")
                if ratio_defined:
                    print(ratio)
                else:
                    print("<undefined>")

            elif state[0] == yami.OutgoingMessage.REJECTED:
                print("The message has been rejected:",
                      msg.get_exception_msg())
            else:
                print("The message has been abandoned.")

except Exception as e:
    print("error:", e)
