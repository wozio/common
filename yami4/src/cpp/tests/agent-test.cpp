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
#include <cstring>

const char * local_listener = "tcp://*:*";

// attempt to send a message to non-existing agent
void test1()
{
    yami::agent client_agent;

    try
    {
        client_agent.send_one_way("tcp://nosuchaddress:12345",
            "nosuchobject", "badmessage");

        assert(false);
    }
    catch (const yami::yami_runtime_error & e)
    {
        assert(e.what() == std::string("I/O error."));
    }

    try
    {
        // a bit dodgy, but 4 is an unassigned port in the list
        // of well-known services, so there is a chance that
        // no existing process uses it on the machine where this test
        // is executed
        // - if this test fails then it is a sign that some process
        // has a listening socket on port 4 - pick another dummy number
        
        client_agent.send_one_way("tcp://localhost:4",
            "nosuchobject", "badmessage");

        assert(false);
    }
    catch (const yami::yami_runtime_error & e)
    {
        assert(e.what() == std::string("I/O error."));
    }
}

// message sent to nonexisting object
void test2()
{
    yami::agent server_agent;

    const std::string server_address =
        server_agent.add_listener(local_listener);

    yami::agent client_agent;

    // one-way message does not report any error
    client_agent.send_one_way(server_address, "nosuchobject", "badmessage");

    // two-way message is rejected
    std::auto_ptr<yami::outgoing_message> message(
        client_agent.send(server_address, "nosuchobject", "badmessage"));

    message->wait_for_completion();

    assert(message->get_state() == yami::rejected);
    assert(message->get_exception_msg() == "Unknown destination object.");
}

// message sent to nonexisting object, explicit connection management
void test2a()
{
    yami::agent server_agent;

    const std::string server_address =
        server_agent.add_listener(local_listener);

    yami::agent client_agent;

    std::size_t priority = 0;
    bool auto_connect = false;

    // message fails if there is no channel and no auto-connect
    try
    {
        std::auto_ptr<yami::outgoing_message> message(
            client_agent.send(server_address, "nosuchobject", "badmessage",
                yami::parameters(), priority, auto_connect));

        assert(false);
    }
    catch (const yami::yami_runtime_error & e)
    {
        assert(e.what() == std::string("I/O error."));
    }

    // explicitly open the channel

    client_agent.open_connection(server_address);

    // message is successfully sent over existing channel,
    // but later rejected by server
    std::auto_ptr<yami::outgoing_message> message(
        client_agent.send(server_address, "nosuchobject", "badmessage",
            yami::parameters(), priority, auto_connect));

    message->wait_for_completion();

    assert(message->get_state() == yami::rejected);
    assert(message->get_exception_msg() == "Unknown destination object.");
}

const std::string object_name = "object";
const std::string message_name = "message";

class object_type_for_test3
{
public:
    object_type_for_test3() : got_message(false) {}

    void operator()(yami::incoming_message & message)
    {
        got_message = true;

        assert(message.get_object_name() == object_name);
        assert(message.get_message_name() == message_name);

        yami::parameters content(message.get_parameters());
        assert(content.size() == 1);
        assert(content.get_string("value") == "ping");

        content.set_string("value", "pong");
        message.reply(content);
    }

    bool got_message;

private:
    object_type_for_test3(const object_type_for_test3 &);
    void operator=(const object_type_for_test3 &);
};

// message sent and replied to
void test3()
{
    yami::agent server_agent;

    const std::string server_address =
        server_agent.add_listener(local_listener);

    object_type_for_test3 my_object;
    server_agent.register_object(object_name, my_object);

    yami::agent client_agent;

    yami::parameters content;
    content.set_string("value", "ping");

    std::auto_ptr<yami::outgoing_message> message(
        client_agent.send(server_address,
            object_name, message_name, content));

    message->wait_for_transmission();

    // after transmission the whole message is pushed out
    std::size_t sent_bytes;
    std::size_t total_byte_count;
    yami::message_state state =
        message->get_state(sent_bytes, total_byte_count);

    assert(sent_bytes == total_byte_count);

    message->wait_for_completion();

    assert(my_object.got_message);

    assert(message->get_state() == yami::replied);

    content = message->get_reply();
    assert(content.get_string("value") == "pong");
}

// additonal variant of test3 for message object reinitialized in-place
void test3a()
{
    yami::agent server_agent;

    const std::string server_address =
        server_agent.add_listener(local_listener);

    object_type_for_test3 my_object;
    server_agent.register_object(object_name, my_object);

    yami::agent client_agent;

    yami::parameters content;
    content.set_string("value", "ping");

    yami::outgoing_message message;

    client_agent.send(message,
        server_address, object_name, message_name, content);

    message.wait_for_transmission();

    // after transmission the whole message is pushed out
    std::size_t sent_bytes;
    std::size_t total_byte_count;
    yami::message_state state =
        message.get_state(sent_bytes, total_byte_count);

    assert(sent_bytes == total_byte_count);

    message.wait_for_completion();

    assert(my_object.got_message);

    assert(message.get_state() == yami::replied);

    content = message.get_reply();
    assert(content.get_string("value") == "pong");

    // again the same message

    content.set_string("value", "ping");
    my_object.got_message = false;

    client_agent.send(message,
        server_address, object_name, message_name, content);

    message.wait_for_transmission();

    // after transmission the whole message is pushed out
    state = message.get_state(sent_bytes, total_byte_count);

    assert(sent_bytes == total_byte_count);

    message.wait_for_completion();

    assert(my_object.got_message);

    assert(message.get_state() == yami::replied);

    content = message.get_reply();
    assert(content.get_string("value") == "pong");
}

class object_type_for_test4
{
public:
    object_type_for_test4() : got_message(false) {}

    void operator()(yami::incoming_message & message)
    {
        got_message = true;

        // expect empty parameters if no content is sent
        yami::parameters content(message.get_parameters());
        assert(content.size() == 0);

        message.reject("some reason");
    }

    bool got_message;

private:
    object_type_for_test4(const object_type_for_test4 &);
    void operator=(const object_type_for_test4 &);
};

// message rejected by server
void test4()
{
    yami::agent server_agent;

    const std::string server_address =
        server_agent.add_listener(local_listener);

    object_type_for_test4 my_object;
    server_agent.register_object(object_name, my_object);

    yami::agent client_agent;

    std::auto_ptr<yami::outgoing_message> message(
        client_agent.send(server_address, object_name, message_name));

    message->wait_for_completion();

    assert(my_object.got_message);

    assert(message->get_state() == yami::rejected);
    assert(message->get_exception_msg() == "some reason");
}

class object_type_for_test5
{
public:
    object_type_for_test5() : got_message(false) {}

    void operator()(yami::incoming_message & message)
    {
        got_message = true;

        throw std::runtime_error("something bad happened");
    }

    bool got_message;

private:
    object_type_for_test5(const object_type_for_test5 &);
    void operator=(const object_type_for_test5 &);
};

// message rejected due to exception in user code at the server side
void test5()
{
    yami::agent server_agent;

    const std::string server_address =
        server_agent.add_listener(local_listener);

    object_type_for_test5 my_object;
    server_agent.register_object(object_name, my_object);

    yami::agent client_agent;

    std::auto_ptr<yami::outgoing_message> message(
        client_agent.send(server_address, object_name, message_name));

    message->wait_for_completion();

    assert(my_object.got_message);

    assert(message->get_state() == yami::rejected);
    assert(message->get_exception_msg().find("something bad happened") !=
        std::string::npos);
}

const std::size_t num_of_messages_in_test6 = 10;

class object_type_for_test6
{
public:
    object_type_for_test6(const std::string & big_string)
        : big_string_(big_string)
    {
        for (std::size_t i = 0; i != num_of_messages_in_test6; ++i)
        {
            got_messages[i] = false;
        }
    }

    void operator()(yami::incoming_message & message)
    {
        yami::parameters content(message.get_parameters());

        const int id = content.get_integer("id");
        got_messages[id] = true;

        // uncomment it to see how the messages get reordered
        //std::cout << "received message " << id << std::endl;

        // verify the big value
        std::size_t value_length;
        const char * value = content.get_string("big", value_length);
        assert(std::strncmp(big_string_.c_str(), value, value_length) == 0);

        message.reply();
    }

    bool got_messages[num_of_messages_in_test6];

private:
    object_type_for_test6(const object_type_for_test6 &);
    void operator=(const object_type_for_test6 &);

    const std::string & big_string_;
};

// big messages sent with different priorities
void test6()
{
    // Note:
    // The messages are sent with different priorities, which means
    // that they might complete in the order that is different from the
    // order of posting them to the outgoing queue.
    // The messages are posted with increasing priorities (first message
    // is sent with lowest priority, last message with highest),
    // so it is *very likely* that they will be received by server
    // in the reversed order, but this cannot be guaranteed as there is
    // no relation between the speed of posting and the speed
    // of transmission.

    const std::size_t size_of_big_string = 1000000;

    const std::string big_string(size_of_big_string, 'x');
    assert(big_string.size() == size_of_big_string);

    yami::agent server_agent;

    const std::string server_address =
        server_agent.add_listener(local_listener);

    object_type_for_test6 my_object(big_string);
    server_agent.register_object(object_name, my_object);

    yami::agent client_agent;

    yami::parameters content;
    content.set_string_shallow("big", big_string.c_str(), big_string.size());

    std::auto_ptr<yami::outgoing_message> messages[num_of_messages_in_test6];

    // send all messages with different ids and priorities
    for (std::size_t i = 0; i != num_of_messages_in_test6; ++i)
    {
        int id = static_cast<int>(i);
        std::size_t priority = i; // increasing priority

        content.set_integer("id", id);

        messages[i] = client_agent.send(server_address,
            object_name, message_name, content, priority);
    }

    // wait for all messages to complete
    for (std::size_t i = 0; i != num_of_messages_in_test6; ++i)
    {
        messages[i]->wait_for_completion();
    }

    for (std::size_t i = 0; i != num_of_messages_in_test6; ++i)
    {
        assert(my_object.got_messages[i]);
    }
}

class object_type_for_tests_7_8
{
public:
    object_type_for_tests_7_8() : got_message(false) {}

    void operator()(yami::incoming_message & message)
    {
        got_message = true;

        message.reply();
    }

    bool got_message;

private:
    object_type_for_tests_7_8(const object_type_for_tests_7_8 &);
    void operator=(const object_type_for_tests_7_8 &);
};

// message sent to load-balanced pair of destinations
void test7()
{
    yami::agent server_agent_1;
    const std::string server_address_1 =
        server_agent_1.add_listener(local_listener);

    yami::agent server_agent_2;
    const std::string server_address_2 =
        server_agent_2.add_listener(local_listener);

    const std::string load_balanced_target = "failover:(" +
        server_address_1 + "|" + server_address_2 + ")";

    object_type_for_tests_7_8 my_object_1;
    server_agent_1.register_object(object_name, my_object_1);

    object_type_for_tests_7_8 my_object_2;
    server_agent_2.register_object(object_name, my_object_2);

    yami::agent client_agent;

    std::auto_ptr<yami::outgoing_message> message(
        client_agent.send(load_balanced_target, object_name, message_name));

    // since this is a load-balanced (and failover) target,
    // the message is implicitly waited for completion

    assert(message->get_state() == yami::replied);

    // exactly one of two servers got the message
    assert((my_object_1.got_message && my_object_2.got_message == false) ||
        (my_object_1.got_message == false && my_object_2.got_message));
}

// message sent to failover pair of destinations
void test8()
{
    // the failover pair consists of one proper address and one
    // that is certainly not working

    yami::agent server_agent;
    const std::string server_address =
        server_agent.add_listener(local_listener);

    object_type_for_tests_7_8 my_object;
    server_agent.register_object(object_name, my_object);

    yami::agent client_agent;

    const std::string broken_target = "tcp://nosuchhost:4";
    const std::string failover_target = "failover:(" +
        server_address + "|" + broken_target + ")";

    std::auto_ptr<yami::outgoing_message> message(
        client_agent.send(failover_target, object_name, message_name));

    // since this is a failover target,
    // the message is implicitly waited for completion

    assert(message->get_state() == yami::replied);

    // the working server in the failover pair got the message
    assert(my_object.got_message);
}

// empty failover group is an error
void test9()
{
    yami::agent client_agent;

    try
    {
        client_agent.send_one_way("failover:()", object_name, message_name);

        assert(false);
    }
    catch (const yami::yami_logic_error & e)
    {
        assert(e.what() ==
            std::string("Empty failover group is not allowed."));
    }
}

const char message_content_for_test10[] = "Hello this is raw binary messag";
const char reply_content_for_test10[] = "Hi and this is raw binary responsee";

class object_type_for_test10
{
public:

    object_type_for_test10() : got_message(false) {}

    void operator()(yami::incoming_message & message)
    {
        got_message = true;

        const std::vector<char> & raw_body = message.get_raw_content();
        assert(std::strcmp(&raw_body[0], message_content_for_test10) == 0);

        yami::raw_buffer_data_source raw_reply(
            reply_content_for_test10, sizeof(reply_content_for_test10));
        message.reply(raw_reply);
    }

    bool got_message;

private:
    object_type_for_test10(const object_type_for_test10 &);
    void operator=(const object_type_for_test10 &);
};

// raw binary message and reply
void test10()
{
    yami::parameters options;
    options.set_boolean("deliver_as_raw_binary", true);

    yami::agent server_agent(options);

    const std::string server_address =
        server_agent.add_listener(local_listener);

    object_type_for_test10 my_object;
    server_agent.register_object(object_name, my_object);

    yami::agent client_agent(options);

    yami::raw_buffer_data_source raw_content(
        message_content_for_test10, sizeof(message_content_for_test10));

    std::auto_ptr<yami::outgoing_message> message(
        client_agent.send(server_address, object_name, "hello", raw_content));

    message->wait_for_completion();

    assert(my_object.got_message);
    assert(message->get_state() == yami::replied);

    const std::vector<char> & raw_reply = message->get_raw_reply();
    assert(std::strcmp(&raw_reply[0], reply_content_for_test10) == 0);
}

class callback_type_for_test11
{
public:
    void operator()(
        const std::string & /* name */, yami::connection_event event)
    {
        switch (event)
        {
        case yami::new_incoming_connection:
            events += "incoming ";
            break;
        case yami::new_outgoing_connection:
            events += "outgoing ";
            break;
        case yami::connection_closed:
            events += "closed ";
            break;
        }
    }

    std::string events;
};

// connection event notifications
void test11()
{
    callback_type_for_test11 server_connection_event_callback;
    yami::agent server_agent;

    server_agent.register_connection_event_monitor(
        server_connection_event_callback);

    callback_type_for_test11 client_connection_event_callback;
    yami::agent client_agent;

    client_agent.register_connection_event_monitor(
        client_connection_event_callback);


    const std::string server_address =
        server_agent.add_listener(local_listener);

    // no communication yet -> no connections
    assert(server_connection_event_callback.events.empty());
    assert(client_connection_event_callback.events.empty());

    std::auto_ptr<yami::outgoing_message> message(
        client_agent.send(server_address, "no_such_object", "hello"));
    message->wait_for_completion();

    // one connection open
    assert(server_connection_event_callback.events == "incoming ");
    assert(client_connection_event_callback.events == "outgoing ");

    client_agent.close_connection(server_address);

    // one connection open and one closed,
    // but it is a race for both the client and the server
    assert(server_connection_event_callback.events == "incoming " ||
        server_connection_event_callback.events == "incoming closed ");
    assert(client_connection_event_callback.events == "outgoing " ||
        client_connection_event_callback.events == "outgoing closed ");
}

// frame size border conditions - messages with all possible lengths
void test12()
{
    yami::agent server_agent;

    const std::string server_address =
        server_agent.add_listener(local_listener);

    yami::agent client_agent;

    const std::size_t max_string_size = 10000;

    for (std::size_t string_size = 1; string_size != max_string_size;
         ++string_size)
    {
        std::string s(string_size, 'x');
        assert(s.size() == string_size);

        yami::parameters params;
        params.set_string("value", s);

        std::auto_ptr<yami::outgoing_message> message(
            client_agent.send(server_address, "no_such_object",
                "hello", params));

        message->wait_for_completion();

        assert(message->get_state() == yami::rejected);
    }
}

int main()
{
    test1();
    test2();
    test2a();
    test3();
    test3a();
    test4();
    test5();
    test6();
    test7();
    test8();
    test9();
    test10();
    test11();
    test12();
}
