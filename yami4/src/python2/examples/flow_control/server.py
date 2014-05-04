import sys
import yami

argc = len(sys.argv)
if argc != 2 and argc != 4:
    print "need 1 or 3 parameters:"
    print "   - server address"
    print "   - incoming high water mark"
    print "   - incoming low water mark"
    print "If only server address is given," + \
        " the limits will have default values"
    exit()

server_address = sys.argv[1]

def call(message):

    index = message.get_parameters()["index"]
    print "processing message", index, "from", message.get_source()


options = {}
if argc == 4:
    try:
        incoming_high_water_mark = int(sys.argv[2])
        incoming_low_water_mark = int(sys.argv[3])
    except ValueError:
        print "invalid arguments"
        exit()

    options[yami.Agent.OptionNames.INCOMING_HIGH_WATER_MARK] = \
        incoming_high_water_mark
    options[yami.Agent.OptionNames.INCOMING_LOW_WATER_MARK] = \
        incoming_low_water_mark


try:
    server_agent = yami.Agent(options)
    try:

        resolved_address = server_agent.add_listener(server_address)

        print "The server is listening on", resolved_address

        server_agent.register_object("object", call)

        # block
        dummy = sys.stdin.read()
    finally:
        server_agent.close()

except Exception, e:
    print "error:", e
