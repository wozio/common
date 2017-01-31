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

#ifndef YAMI4BROKER_MESSAGING_H
#define YAMI4BROKER_MESSAGING_H

#include "broker.h"

#include <string>

namespace broker
{

// Initializes the messaging part with the given listener endpoint.
// The broker will be initialized in the "warm-up" mode.
void init_messaging(const std::string & listener,
    overflow_policy subscription_overflow_policy);

// Finishes the warm-up mode.
void allow_incoming();

} // namespace broker

#endif // YAMI4BROKER_MESSAGING_H
