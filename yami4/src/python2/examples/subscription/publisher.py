import random
import sys
import time
import yami

if len(sys.argv) != 2:
    print "expecting one parameter: publisher destination"
    exit()

publisher_address = sys.argv[1]

try:
    random_value = yami.ValuePublisher()

    publisher_agent = yami.Agent()
    try:
        resolved_address = publisher_agent.add_listener(
            publisher_address)

        print "The publisher is listening on", resolved_address

        publisher_agent.register_value_publisher(
            "random_number", random_value)

        # publish random numbers forever
        while True:
            rnd = random.randint(0, 99)
            content = {"value":rnd}

            print "publishing value", rnd

            random_value.publish(content)

            time.sleep(1)
    finally:
        publisher_agent.close()

except Exception, e:
    print "error:", e
