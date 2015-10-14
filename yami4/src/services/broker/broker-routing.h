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

#ifndef YAMI4BROKER_ROUTING_H
#define YAMI4BROKER_ROUTING_H

#include "broker.h"

#include <yami4-core/parameters.h>
#include <yami4-core/serializable.h>

#include <string>

namespace broker
{

void init_routing(std::size_t max_subscriptions,
    std::size_t max_client_queue,
    overflow_policy subscription_overflow_policy);

void subscribe(
    const std::string & tags, const std::string & target_object,
    const std::string & target_location);

void set_forwarding(
    const std::string & tags, const std::string & target_location);

void iterate_matching_subscriptions(
    const std::string & tags,
    const yami::core::serializable & body,
    bool (*process)(
        const std::string & target_object,
        const std::string & target_location,
        const std::string & tags,
        const yami::core::serializable & body,
        yami::core::message_progress_function progress_handler,
        void * progress_hint),
    bool & out_overflow);

yami::core::result fill_detailed_stats(yami::core::parameters & params);

} // namespace broker

#endif // YAMI4BROKER_ROUTING_H
