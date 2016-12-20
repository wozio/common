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

#ifndef YAMI4QUEUE_ROUTING_H
#define YAMI4QUEUE_ROUTING_H

#include "queue.h"

#include <yami4-core/parameters.h>
#include <yami4-core/serializable.h>

#include <string>

namespace queue
{

void init_routing(std::size_t max_queue_num_of_messages,
    std::size_t max_queue_size,
    creation_policy queue_creation_policy);

void create_queue(const std::string & queue_name);

bool put_to_queue(
    const std::string & queue_name,
    const yami::core::serializable & body,
    bool (*process)(
        const char * target,
        long long incoming_msg_id,
        const yami::core::serializable & body),
    bool & out_no_such_queue);

bool get_from_queue(const std::string & source,
    const std::string & queue_name,
    long long incoming_msg_id,
    bool (*process)(
        const char * target,
        long long incoming_msg_id,
        const yami::core::serializable & body),
    bool block);

yami::core::result fill_simple_stats(yami::core::parameters & params);
yami::core::result fill_detailed_stats(yami::core::parameters & params);

} // namespace queue

#endif // YAMI4QUEUE_ROUTING_H
