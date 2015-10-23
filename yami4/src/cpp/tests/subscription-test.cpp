// Copyright Maciej Sobczak 2008-2015.
// This file is part of YAMI4.
//
// YAMI4 is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// YAMI4 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

#include "../yami.h"
#include <cassert>

#include <unistd.h>

// Note: this test cannot be easily implemented "properly",
// because it also checks that the message was *not* received.
// This is done with artificial sleep calls with the assumption
// that the interaction between agents awlays takes place
// within the period of 1s.
// Since the value publisher feature does not involve any
// system-level services and is entirely implemented in terms of
// YAMI4 API, a POSIX version of this test is sufficient.

const std::string local_address = "tcp://*:*";

// simple subscribe and unsubscribe

bool got_update_1 = false;

void call_1(yami::incoming_message & im)
{
    assert(im.get_message_name() == "subscription_update");
    got_update_1 = true;
}

void test1()
{
    // set up the publisher side

    yami::agent publisher_agent;

    const std::string publisher_address =
        publisher_agent.add_listener(local_address);

    yami::value_publisher value;
    publisher_agent.register_value_publisher("my_value", value);

    // no subscribers yet

    assert(value.get_number_of_subscribers() == 0);
    assert(value.get_subscribers().empty());

    // set up the subscriber side

    yami::agent subscriber_agent;
    subscriber_agent.register_object("my_update_handler", call_1);

    // subscribe

    yami::parameters params;
    params.set_string("destination_object", "my_update_handler");
    std::auto_ptr<yami::outgoing_message> subscribe_msg(
        subscriber_agent.send(publisher_address,
            "my_value", "subscribe", params));

    subscribe_msg->wait_for_completion();

    // there should be one subscriber, as seen at the publisher side

    assert(value.get_number_of_subscribers() == 1);
    {
        const std::vector<std::pair<std::string, std::string> > subscribers =
            value.get_subscribers();

        assert(subscribers.size() == 1);
        assert(subscribers[0].second == "my_update_handler");
    }

    // publish some value

    yami::parameters dummy;
    value.publish(dummy);

    // check if the listener got it

    sleep(1);

    assert(got_update_1);

    // unsubscribe

    std::auto_ptr<yami::outgoing_message> unsubscribe_msg(
        subscriber_agent.send(publisher_address,
            "my_value", "unsubscribe"));

    unsubscribe_msg->wait_for_completion();

    // there should be no subscribers

    assert(value.get_number_of_subscribers() == 0);
    assert(value.get_subscribers().empty());

    // check that the updates do not arrive any longer

    got_update_1 = false;
    value.publish(dummy);

    sleep(1);
    assert(got_update_1 == false);
}

// dispatch of unknown commands

bool got_unknown_2;
void unknown_command_handler(yami::incoming_message & im)
{
    assert(im.get_message_name() == "unknown");
    got_unknown_2 = true;

    im.reply();
}

void test2()
{
    // set up the publisher side

    yami::agent publisher_agent;

    const std::string publisher_address =
        publisher_agent.add_listener(local_address);

    yami::value_publisher value(unknown_command_handler);

    publisher_agent.register_value_publisher("my_value", value);

    // set up the subscriber side

    yami::agent subscriber_agent;

    // send unknown command

    std::auto_ptr<yami::outgoing_message> unknown_msg(
        subscriber_agent.send(publisher_address,
            "my_value", "unknown"));

    unknown_msg->wait_for_completion();

    assert(got_unknown_2);
}

int main()
{
    test1();
    test2();
}
