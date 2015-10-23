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
#include "../event_callback.h"
#include <cassert>
#include <cstring>
#include <sstream>

#include <iostream>

const char * local_listener = "tcp://*:*";

class event_listener : public yami::event_callback
{
public:
    virtual void agent_created()
    {
        e << "agent created,";
    }

    virtual void agent_closed()
    {
        e << "agent closed,";
    }

    virtual void listener_added(const char * target)
    {
        e << "listener added,";
    }

    virtual void listener_removed(const char * target)
    {
        e << "listener removed,";
    }

    virtual void incoming_connection_open(const char * target)
    {
        e << "incoming connection open,";
    }

    virtual void outgoing_connection_open(const char * target)
    {
        e << "outgoing connection open,";
    }

    virtual void connection_closed(const char * target)
    {
        e << "connection closed,";
    }

    virtual void connection_error(const char * target)
    {
        e << "connection error,";
    }

    virtual void object_registered(const char * name)
    {
        e << "object " << name << " registered,";
    }

    virtual void object_unregistered(const char * name)
    {
        e << "object " << name << " unregistered,";
    }

    virtual void message_sent(const char * target, std::size_t size)
    {
        e << "message sent,";
    }

    virtual void message_received(const char * target, std::size_t size)
    {
        e << "message received,";
    }

    std::ostringstream e;
};

// attempt to send a message to non-existing agent
void test1()
{
    event_listener events;
    assert(events.e.str().empty());

    {
    yami::agent client_agent(events);

    assert(events.e.str() == "agent created,");

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
    assert(events.e.str() == "agent created,agent closed,");
}

// message sent to nonexisting object
void test2()
{
    event_listener srv_events;
    event_listener cli_events;
    {
    yami::agent server_agent(srv_events);

    const std::string server_address =
        server_agent.add_listener(local_listener);

    assert(srv_events.e.str() == "agent created,listener added,");

    yami::agent client_agent(cli_events);

    // one-way message does not report any error
    client_agent.send_one_way(server_address, "nosuchobject", "badmessage");

    // two-way message is rejected
    std::auto_ptr<yami::outgoing_message> message(
        client_agent.send(server_address, "nosuchobject", "badmessage"));

    message->wait_for_completion();

    assert(message->get_state() == yami::rejected);
    assert(message->get_exception_msg() == "Unknown destination object.");
    }

    assert(cli_events.e.str() ==
        "agent created,"
        "outgoing connection open,"
        "message sent,"
        "message sent,"
        "message received," // silent rejection of the one-way message
        "message received," // rejection of the one-way message
        "connection closed,"
        "agent closed," ||

        cli_events.e.str() ==
        "agent created,"
        "outgoing connection open,"
        "message sent,"
        "message received," // silent rejection of the one-way message
        "message sent,"
        "message received," // rejection of the one-way message
        "connection closed,"
        "agent closed,"
    );
    assert(srv_events.e.str() ==
        "agent created,"
        "listener added,"
        "incoming connection open,"
        "message received,"
        "message received,"
        "message sent,"
        "message sent,"
        "connection closed,"
        "listener removed,"
        "agent closed," ||

        srv_events.e.str() ==
        "agent created,"
        "listener added,"
        "incoming connection open,"
        "message received,"
        "message sent,"
        "message received,"
        "message sent,"
        "connection closed,"
        "listener removed,"
        "agent closed,");
}

int main()
{
    test1();
    test2();
}
