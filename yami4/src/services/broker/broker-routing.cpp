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

#include "broker-routing.h"
#include "patterns.h"
#include "../common/log.h"

#include <string>
#include <vector>

using namespace broker;

namespace // unnamed
{

// The Subscription type handles both regular subscriptions
// as well as forward channels.
// Notes about forward channels:
// 1. forward channels are never removed
//    (not even after failure or overflow)
// 2. the original list of tags in the message
//    is preserved as object name when the message is forwarded

struct subscription
{
    subscription()
        : active(false), pending_messages(0), sent_messages(0), sent_bytes(0)
    {
    }

    bool active;

    std::size_t pending_messages;
    std::size_t sent_messages;
    std::size_t sent_bytes;

    std::string tag_patterns;
    std::string target_object;
    std::string target_location;
};

bool is_duplicate(const std::string & tags, const std::string & target_object,
    const std::string & target_location,
    const subscription & s)
{
    return (s.tag_patterns == tags) &&
        (s.target_object == target_object) &&
        (s.target_location == target_location);
}

extern "C" void message_progress(
    void * hint,
    std::size_t sent_bytes,
    std::size_t total_byte_count)
{
    subscription * sub = static_cast<subscription *>(hint);

    sub->sent_bytes += sent_bytes;

    if (sent_bytes == total_byte_count)
    {
        ++(sub->sent_messages);
        --(sub->pending_messages);
    }
}

std::vector<subscription> subscription_table_;
std::size_t client_queue_limit_;
overflow_policy sub_overflow_policy_;

} // unnamed namespace

void broker::init_routing(std::size_t max_subscriptions,
    std::size_t max_client_queue,
    overflow_policy subscription_overflow_policy)
{
    subscription_table_.resize(max_subscriptions);
    client_queue_limit_ = max_client_queue;
    sub_overflow_policy_ = subscription_overflow_policy;
}

void broker::subscribe(
    const std::string & tags, const std::string & target_object,
    const std::string & target_location)
{
    std::size_t insert_point;
    bool found_insert_point = false;
    bool found_duplicate = false;

    for (std::size_t i = 0; i != subscription_table_.size(); ++i)
    {
        subscription & sub = subscription_table_[i];

        if (sub.active == false)
        {
            if (found_insert_point == false)
            {
                insert_point = i;
                found_insert_point = true;
            }
        }
        else
        {
            // do not duplicate identical subscriptions

            if (is_duplicate(tags, target_object, target_location, sub))
            {
                logger::put(logger::subscriptions,
                    target_location + " refreshed subscription to " + tags);

                found_duplicate= true;

                break;
            }
        }
    }

    if (found_duplicate == false)
    {
        if (found_insert_point)
        {
            if (target_object == "*")
            {
                logger::put(logger::subscriptions,
                    "forwarding " + tags + " to " + target_location);
            }
            else
            {
                logger::put(logger::subscriptions,
                    target_location + " subscribed to " + tags);
            }

            subscription & sub = subscription_table_[insert_point];

            sub.active = true;
            sub.tag_patterns = tags;
            sub.target_object = target_object;
            sub.target_location = target_location;
        }
        else
        {
            logger::put(logger::subscriptions,
                target_location + " cannot subscribe due to overflow");
        }
    }
}

void broker::set_forwarding(
    const std::string & tags, const std::string & target_location)
{
    // forward channel is installed as a subcription with "magic"
    // target object name

    subscribe(tags, "*", target_location);
}

void broker::iterate_matching_subscriptions(
    const std::string & tags,
    const yami::core::serializable & body,
    bool (*process)(
        const std::string & target_object,
        const std::string & target_location,
        const std::string & tags,
        const yami::core::serializable & body,
        yami::core::message_progress_function progress_handler,
        void * progress_hint),
    bool & out_overflow)
{
    out_overflow = false;

    for (std::size_t i = 0; i != subscription_table_.size(); ++i)
    {
        subscription & sub = subscription_table_[i];

        if (sub.active &&
            patterns::patterns_multi_hierarchic_match(tags, sub.tag_patterns))
        {
            if (sub.pending_messages >= client_queue_limit_)
            {
                out_overflow = true;

                if (sub_overflow_policy_ == unsubscribe)
                {
                    logger::put(logger::subscriptions,
                        "queue full, abandoned subscription to " +
                        sub.target_location);

                    sub.active = false;
                }
                else
                {
                    logger::put(logger::subscriptions,
                        "queue full, dropped update for " +
                        sub.target_location);
                }
            }
            else
            {
                // call back message sender in messages module
                bool success = process(sub.target_object, sub.target_location,
                    tags, body, &message_progress, &sub);
                
                if (success)
                {
                    ++sub.pending_messages;
                }
                else
                {
                    // in case of any error take down this subscription
                    // (unless it is a forwarding channel)

                    logger::put(logger::subscriptions,
                        "abandoned subscription to " + sub.target_location);

                    sub.active = false;
                }
            }
        }
    }
}

yami::core::result broker::fill_detailed_stats(yami::core::parameters & params)
{
    std::size_t active_subscription_count = 0;

    // first count active subscriptions
    for (std::size_t i = 0; i != subscription_table_.size(); ++i)
    {
        subscription & sub = subscription_table_[i];

        if (sub.active)
        {
            ++active_subscription_count;
        }
    }

    yami::core::result res = yami::core::ok;

    if (active_subscription_count != 0)
    {
        // create arrays and fill details

        bool * overflows;
        long long * sent_messages;
        long long * sent_bytes;

        res = params.create_string_array(
            "locations", active_subscription_count);
        if (res != yami::core::ok)
        {
            return res;
        }

        res = params.create_boolean_array("overflows",
            active_subscription_count, overflows);
        if (res != yami::core::ok)
        {
            return res;
        }

        res = params.create_long_long_array("sent_messages",
            active_subscription_count, sent_messages);
        if (res != yami::core::ok)
        {
            return res;
        }

        res = params.create_long_long_array("sent_bytes",
            active_subscription_count, sent_bytes);
        if (res != yami::core::ok)
        {
            return res;
        }

        std::size_t j = 0;
        for (std::size_t i = 0; i != subscription_table_.size(); ++i)
        {
            subscription & sub = subscription_table_[i];

            if (sub.active)
            {
                overflows[j] = (sub.pending_messages == client_queue_limit_);

                sent_messages[j] = sub.sent_messages;

                sent_bytes[j] = sub.sent_bytes;

                res = params.set_string_in_array(
                    "locations", j, sub.target_location.c_str());
                if (res != yami::core::ok)
                {
                    return res;
                }

                ++j;
            }
        }
    }

    return res;
}
