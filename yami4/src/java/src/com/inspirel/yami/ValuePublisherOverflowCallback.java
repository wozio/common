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
 * The callback interface that is used to notify the user code
 * that the message cannot be published without delay due to the
 * overflow condition in one of the subscribers.
 * This callback allows the user to customize the decision that has to be
 * taken in the overflow situation.
 */
public interface ValuePublisherOverflowCallback {
    
    /**
     * Enumeration type defining possible decisions in the case of
     * notification overflow.
     */
    enum OverflowHandling {
        WAIT_FOR_PREVIOUS_MESSAGE,
        ABANDON_MESSAGE,
        ABANDON_SUBSCRIPTION
    }
    
    /**
     * Callback function that is automatically called by the value publisher
     * whenever there is an overflow condition for one of the subscribers.
     * 
     * <b>Note:</b> This function is called in the context of the same thread
     * that initiated the publication of new value.
     * 
     * @param serverName target location of the subscriber
     * @param objectName target object name of the subscriber
     * @param value content of the message
     * @return the user decision on how to handle the overflow condition
     * @throws Exception any exception, which will be interpreted as
     *         the "abandon message" decision
     */
    OverflowHandling subscriptionOverflow(
            String serverName, String objectName,
            YAMISerializable value) throws Exception;
}
