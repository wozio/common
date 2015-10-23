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
 * Exception class, used to indicate
 * that a given object is in the inappropriate state
 * for the requested operation.
 */
@SuppressWarnings("serial")
public class BadStateException extends RuntimeException {
    public BadStateException(String name) {
        super("The object '" + name +
                "' is not in the appropriate state.");
    }
}
