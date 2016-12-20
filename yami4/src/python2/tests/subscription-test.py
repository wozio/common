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

# Note: this test cannot be easily implemented "properly",
# because it also checks that the message was *not* received.
# This is done with artificial sleep calls with the assumption
# that the interaction between agents awlays takes place
# within the period of 1s.
# Since the value publisher feature does not involve any
# system-level services and is entirely implemented in terms of
# YAMI4 API, a POSIX version of this test is sufficient.

local_listener = "tcp://*:*"

class HandlerForTest1(object):

    def __init__(self):
        self.got_update = False

    def __call__(self, message):
        assert message.get_message_name() == "subscription_update"
        self.got_update = True

def test_1():
    """Simple subscribe and unsubscribe."""

    # set up the publisher side

    publisher_agent = yami.Agent()

    publisher_address = publisher_agent.add_listener(local_listener)

    value = yami.ValuePublisher()
    publisher_agent.register_value_publisher("my_value", value)

    # no subscribers yet

    assert value.get_number_of_subscribers() == 0
    assert value.get_subscribers() == []

    # set up the subscriber side

    subscriber_agent = yami.Agent()
    update_handler = HandlerForTest1()
    subscriber_agent.register_object("my_update_handler", update_handler)

    # subscribe

    params = {"destination_object":"my_update_handler"}
    subscribe_msg = subscriber_agent.send(
        publisher_address, "my_value", "subscribe", params)

    subscribe_msg.wait_for_completion()

    # there should be one subscriber, as seen at the publisher side

    assert value.get_number_of_subscribers() == 1

    subscribers = value.get_subscribers()
    assert len(subscribers) == 1
    assert subscribers[0][1] == "my_update_handler"

    # publish some value

    dummy = {}
    value.publish(dummy)

    # check if the listener got it

    time.sleep(1)

    assert update_handler.got_update

    # unsubscribe

    unsubscribe_msg = subscriber_agent.send(
        publisher_address, "my_value", "unsubscribe")

    unsubscribe_msg.wait_for_completion()

    # there should be no subscribers

    assert value.get_number_of_subscribers() == 0
    assert value.get_subscribers() == []

    # check that the updates do not arrive any longer

    update_handler.got_update = False
    value.publish(dummy)

    time.sleep(1)
    assert not update_handler.got_update

    unsubscribe_msg.close()
    subscribe_msg.close()
    subscriber_agent.close()
    publisher_agent.close()


class UnknownCommandHandler(object):

    def __init__(self):
        self.got_update = False

    def __call__(self, message):
        assert message.get_message_name() == "unknown"
        self.got_update = True
        message.reply()

def test_2():
    """Dispatch of unknown commands."""

    # set up the publisher side

    publisher_agent = yami.Agent()

    publisher_address = publisher_agent.add_listener(local_listener)

    unknown_command_handler = UnknownCommandHandler()
    value = yami.ValuePublisher(unknown_command_handler)
    publisher_agent.register_value_publisher("my_value", value)

    # set up the subscriber side

    subscriber_agent = yami.Agent()

    # send unknown command

    unknown_msg = subscriber_agent.send(
        publisher_address, "my_value", "unknown")
    
    unknown_msg.wait_for_completion()

    assert unknown_command_handler.got_update

    unknown_msg.close()
    subscriber_agent.close()
    publisher_agent.close()


test_1()
test_2()
