# Copyright Maciej Sobczak 2008-2015.
# This file is part of YAMI4.
#
# YAMI4 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# YAMI4 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

import yami

import time

local_listener = "tcp://*:*"

def test_1():
    """Attempt to send a message to non-existing agent."""

    client_agent = yami.Agent()

    try:
        client_agent.send_one_way("tcp://nosuchaddress:12345",
                                  "nosuchobject", "badmessage")

        assert False

    except yami.YAMIError, e:
        assert str(e) == "I/O error."

    try:
        # a bit dodgy, but 4 is an unassigned port in the list
        # of well-known services, so there is a chance that
        # no existing process uses it on the machine where this test
        # is executed
        # - if this test fails then it is a sign that some process
        # has a listening socket on port 4 - pick another dummy number
        
        client_agent.send_one_way("tcp://localhost:4",
                                  "nosuchobject", "badmessage")

        assert False

    except yami.YAMIError, e:
        assert str(e) == "I/O error."

    client_agent.close()


def test_2():
    """Message sent to nonexisting object."""

    server_agent = yami.Agent()
    server_address = server_agent.add_listener(local_listener)

    client_agent = yami.Agent()

    # one-way message does not report any error
    client_agent.send_one_way(server_address, "nosuchobject", "badmessage")

    # two-way message is rejected
    message = client_agent.send(server_address, "nosuchobject", "badmessage")

    message.wait_for_completion()

    assert message.get_state()[0] == yami.OutgoingMessage.REJECTED

    assert message.get_exception_msg() == "Unknown destination object."

    message.close()
    client_agent.close()
    server_agent.close()


def test_2a():
    """Message sent to nonexisting object, explicit connection management."""

    server_agent = yami.Agent()
    server_address = server_agent.add_listener(local_listener)

    client_agent = yami.Agent()

    dummy_content = {}
    priority = 0
    auto_connect = False

    # message fails if there is no channel and no auto-connect
    try:
        message = client_agent.send(
            server_address, "nosuchobject", "badmessage",
            dummy_content, priority, auto_connect)
        assert False
    except yami.YAMIError, e:
        assert str(e) == "I/O error."

    # explicitly open the channel

    client_agent.open_connection(server_address)

    # message is successfully sent over existing channel,
    # but later rejected by server

    message = client_agent.send(
        server_address, "nosuchobject", "badmessage",
        dummy_content, priority, auto_connect)

    message.wait_for_completion()

    assert message.get_state()[0] == yami.OutgoingMessage.REJECTED

    assert message.get_exception_msg() == "Unknown destination object."

    message.close()
    client_agent.close()
    server_agent.close()


object_name = "object"
message_name = "message"

class ObjectTypeForTest3(object):

    def __init__(self):
        self.got_message = False

    def __call__(self, message):
        self.got_message = True

        assert message.get_object_name() == object_name
        assert message.get_message_name() == message_name

        content = message.get_parameters()
        assert len(content) == 1
        assert content["value"] == "ping"

        content["value"] = "pong"
        message.reply(content)

def test_3():
    """Message sent and replied to."""

    server_agent = yami.Agent()
    server_address = server_agent.add_listener(local_listener)

    my_object = ObjectTypeForTest3()
    server_agent.register_object(object_name, my_object)

    client_agent = yami.Agent()

    content = {"value":"ping"}

    message = client_agent.send(server_address,
                                object_name, message_name, content)

    message.wait_for_transmission()

    # after transmission the whole message is pushed out
    state, sent_bytes, total_byte_count = message.get_state()

    assert sent_bytes == total_byte_count

    message.wait_for_completion()

    assert my_object.got_message

    assert message.get_state()[0] == yami.OutgoingMessage.REPLIED

    content = message.get_reply()
    assert content["value"] == "pong"

    message.close()
    client_agent.close()
    server_agent.close()


class ObjectTypeForTest4(object):

    def __init__(self):
        self.got_message = False

    def __call__(self, message):
        self.got_message = True

        # expect empty parameters if no content is sent
        content = message.get_parameters()
        assert len(content) == 0

        message.reject("some reason")

def test_4():
    """Message rejected by server."""

    server_agent = yami.Agent()
    server_address = server_agent.add_listener(local_listener)

    my_object = ObjectTypeForTest4()
    server_agent.register_object(object_name, my_object)

    client_agent = yami.Agent()

    message = client_agent.send(server_address, object_name, message_name)

    message.wait_for_completion()

    assert my_object.got_message

    assert message.get_state()[0] == yami.OutgoingMessage.REJECTED
    assert message.get_exception_msg() == "some reason"

    message.close()
    client_agent.close()
    server_agent.close()


class ObjectTypeForTest5(object):

    def __init__(self):
        self.got_message = False

    def __call__(self, message):
        self.got_message = True

        raise Exception("something bad happened")

def test_5():
    """Message rejected due to exception in user code at the server side."""

    server_agent = yami.Agent()
    server_address = server_agent.add_listener(local_listener)

    my_object = ObjectTypeForTest5()
    server_agent.register_object(object_name, my_object)

    client_agent = yami.Agent()

    message = client_agent.send(server_address, object_name, message_name)

    message.wait_for_completion()

    assert my_object.got_message

    assert message.get_state()[0] == yami.OutgoingMessage.REJECTED
    assert message.get_exception_msg() == "something bad happened"

    message.close()
    client_agent.close()
    server_agent.close()


num_of_messages_in_test6 = 10

class ObjectTypeForTest6(object):

    def __init__(self, big_string):
        self.got_message = []
        for i in range(num_of_messages_in_test6):
            self.got_message.append(False)
        self.big_string = big_string

    def __call__(self, message):
        content = message.get_parameters()
        id = content["id"]
        self.got_message[id] = True

        # uncomment it to see how the messages get reordered
        #print("received message", id)

        # verify the big value
        value = content["big"]
        assert value == self.big_string

        message.reply()

def test_6():
    """Big messages sent with different priorities."""

    # Note:
    # The messages are sent with different priorities, which means
    # that they might complete in the order that is different from the
    # order of posting them to the outgoing queue.
    # The messages are posted with increasing priorities (first message
    # is sent with lowest priority, last message with highest),
    # so it is *very likely* that they will be received by server
    # in the reversed order, but this cannot be guaranteed as there is
    # no relation between the speed of posting and the speed
    # of transmission.

    size_of_big_string = 1000000

    big_string = size_of_big_string * "x"

    server_agent = yami.Agent()
    server_address = server_agent.add_listener(local_listener)

    my_object = ObjectTypeForTest6(big_string)
    server_agent.register_object(object_name, my_object)

    client_agent = yami.Agent()

    content = {"big":big_string}

    messages = []

    # send all messages with different ids and priorities
    for i in range(num_of_messages_in_test6):
        id = i
        priority = i # increasing priority

        content["id"] = id

        messages.append(client_agent.send(
                server_address, object_name, message_name, content, priority))

    # wait for all messages to complete
    for message in messages:
        message.wait_for_completion()

    for i in range(num_of_messages_in_test6):
        assert my_object.got_message[i]
        messages[i].close()

    client_agent.close()
    server_agent.close()


class ObjectTypeForTest_7_8(object):

    def __init__(self):
        self.got_message = False

    def __call__(self, message):
        self.got_message = True

        message.reply()


def test_7():
    """Message sent to load-balanced pair of destinations."""

    server_agent_1 = yami.Agent()
    server_address_1 = server_agent_1.add_listener(local_listener)

    server_agent_2 = yami.Agent()
    server_address_2 = server_agent_2.add_listener(local_listener)

    load_balanced_target = "failover:(" + \
        server_address_1 + "|" + server_address_2 + ")"

    my_object_1 = ObjectTypeForTest_7_8()
    server_agent_1.register_object(object_name, my_object_1)

    my_object_2 = ObjectTypeForTest_7_8()
    server_agent_2.register_object(object_name, my_object_2)

    client_agent = yami.Agent()

    message = client_agent.send(load_balanced_target,
                                object_name, message_name)

    # since this is a load-balanced (and failover) target,
    # the message is implicitly waited for completion

    assert message.get_state()[0] == yami.OutgoingMessage.REPLIED

    # exactly one of two servers got the message
    assert my_object_1.got_message and not my_object_2.got_message or \
        (not my_object_1.got_message and my_object_2.got_message)

    message.close()
    client_agent.close()
    server_agent_1.close()
    server_agent_2.close()


def test_8():
    """Message sent to failover pair of destinations."""

    # the failover pair consists of one proper address and one
    # that is certainly not working

    server_agent = yami.Agent()
    server_address = server_agent.add_listener(local_listener)

    my_object = ObjectTypeForTest_7_8()
    server_agent.register_object(object_name, my_object)

    client_agent = yami.Agent()

    broken_target = "tcp://nosuchhost:4"

    load_balanced_target = "failover:(" + \
        server_address + "|" + broken_target + ")"

    message = client_agent.send(load_balanced_target,
                                object_name, message_name)

    # since this is a failover target,
    # the message is implicitly waited for completion

    assert message.get_state()[0] == yami.OutgoingMessage.REPLIED

    # the working server in the failover pair got the message
    assert my_object.got_message

    message.close()
    client_agent.close()
    server_agent.close()


def test_9():
    """Empty failover group is an error."""

    client_agent = yami.Agent()

    try:
        client_agent.send_one_way("failover:()",
                                  object_name, message_name)

        assert False

    except yami.YAMIError, e:
        assert str(e) == "Empty failover group is not allowed."

    client_agent.close()


class EventCallbackTypeForTest11(object):

    def __init__(self):
        self.events = ""

    def __call__(self, name, event):
        if event == yami.Agent.NEW_INCOMING_CONNECTION:
            self.events += "incoming "
        elif event == yami.Agent.NEW_OUTGOING_CONNECTION:
            self.events += "outgoing "
        else:
            self.events += "closed "

def test_11():
    """Connection event notifications."""

    server_callback = EventCallbackTypeForTest11()
    server_agent = yami.Agent({}, server_callback)

    client_callback = EventCallbackTypeForTest11()
    client_agent = yami.Agent({}, client_callback)

    server_address = server_agent.add_listener(local_listener)

    # no communication yet -> no connections
    assert server_callback.events == ""
    assert client_callback.events == ""

    message = client_agent.send(server_address, "no_such_object", "hello")
    message.wait_for_completion()
    message.close()

    # in Python connection events are not reported synchronously,
    # but reuse the dispatching threads - this means that
    # there might be some time between connection event getting
    # recognized at the low level and having it delivered to user code
    time.sleep(1)

    # one connection open
    assert server_callback.events == "incoming "
    assert client_callback.events == "outgoing "

    client_agent.close_connection(server_address)

    # one connection open and one closed,
    # but it is a race for both the client and the server
    assert (server_callback.events == "incoming " or
            server_callback.events == "incoming closed ")
    assert (client_callback.events == "outgoing " or
            client_callback.events == "outgoing closed ")

    client_agent.close()
    server_agent.close()


def test_12():
    """Frame size border conditions - messages with all possible lengths."""

    server_agent = yami.Agent()
    server_address = server_agent.add_listener(local_listener)

    client_agent = yami.Agent()

    max_string_size = 10000
    for string_size in range(max_string_size):
        s  = string_size * 'x'
        params = {"value":s}

        message = client_agent.send(
            server_address, "nosuchobject", "hello", params)

        message.wait_for_completion()

        assert message.get_state()[0] == yami.OutgoingMessage.REJECTED

        message.close()

    client_agent.close()
    server_agent.close()


test_1()
test_2()
test_2a()
test_3()
test_4()
test_5()
test_6()
test_7()
test_8()
test_9()
test_11()
test_12()
