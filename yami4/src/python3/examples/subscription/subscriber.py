import sys
import yami

def update_handler(message):
    content = message.get_parameters()
    value = content["value"]
    print("received update:", value)


if len(sys.argv) != 2:
    print("expecting one parameter: publisher destination")
    exit()

publisher_address = sys.argv[1]

try:
    with yami.Agent() as subscriber_agent:

        # prepare subscription update callback

        update_object_name = "update_handler"

        subscriber_agent.register_object(
            update_object_name, update_handler)

        # subscribe to the producer

        params = {"destination_object":update_object_name}

        subscriber_agent.send_one_way(publisher_address,
            "random_number", "subscribe", params)

        print("subscribed, waiting for updates")

        # block forever and receive updates in background

        dummy = sys.stdin.read()

except Exception as e:
    print("error:", e)
