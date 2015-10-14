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

#ifndef YAMI4QUEUE_CONFIGURATION_H
#define YAMI4QUEUE_CONFIGURATION_H

#include "queue.h"
#include "../common/log.h"

#include <string>

namespace queue
{

// Initializes all configuration options from the config file
// that is named in the first command-line parameter or
// from the yamiqueue.cfg file if no command-line argument is given.
bool init_config(int argc, char * argv[]);

// Returns the listener endpoint for the messaging part.
std::string listener();

// Max number of messages and total capacity of any given queue.
std::size_t max_queue_num_of_messages();
std::size_t max_queue_size();

// Max number of clients waiting on any given queue.
std::size_t max_waiting_clients();

// Queue creation policy.
creation_policy queue_creation_policy();

// Initial log level.
bool log_enabled(logger::module m);

// Push channels configuration.
std::string initial_queue_name(std::size_t index);

} // namespace queue

#endif // YAMI4QUEUE_CONFIGURATION_H
