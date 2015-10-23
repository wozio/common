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

package com.inspirel.yami;

/**
 * The callback interface for communicating log events.
 */
public interface LogCallback {
    
    enum LogLevel { LOW, MEDIUM, HIGH }

    /**
     * Callback function that is called by the agent whenever
     * some event of interest occurs.
     *
     * <p>
     * <b>Note:</b> Implementations of this interface should be
     * thread-safe.
     * </p>
     * 
     * @param level the level of this event
     * @param message event description
     */
    void log(LogLevel level, String message);
}
