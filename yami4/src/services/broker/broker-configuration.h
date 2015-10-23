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

#ifndef YAMI4BROKER_CONFIGURATION_H
#define YAMI4BROKER_CONFIGURATION_H

#include "broker.h"
#include "../common/log.h"

#include <string>

namespace broker
{

// Initializes all configuration options from the config file
// that is named in the first command-line parameter or
// from the yami4broker.cfg file if no command-line argument is given.
bool init_config(int argc, char * argv[]);

// Returns the listener endpoint for the messaging part.
std::string listener();

// Returns the warmup time
// (time between start to when the broker accepts messages).
std::size_t warmup_time();

// Max size of subscription table.
std::size_t max_subscriptions();

// Max queue length for any given subscriber.
std::size_t max_client_queue();

// Queue overflow policy.
overflow_policy subscription_overflow_policy();

// Initial log level.
bool log_enabled(logger::module m);

// Push channels configuration.
std::string forward_target(std::size_t index);
std::string forward_filter(std::size_t index);

} // namespace broker

#endif // YAMI4BROKER_CONFIGURATION_H
