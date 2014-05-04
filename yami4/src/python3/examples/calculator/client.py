import sys
import yami

if len(sys.argv) != 4:
    print("expecting three parameters:",
          "server destination and two integers")
    exit()

server_address = sys.argv[1]

try:
    a = int(sys.argv[2])
    b = int(sys.argv[3])
except ValueError:
    print("cannot parse the second or third parameter")
    exit()

try:
    with yami.Agent() as client_agent:

        params = {"a":a, "b":b}

        with client_agent.send(
            server_address, "calculator", "calculate", params) as msg:

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
