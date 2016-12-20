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

package com.inspirel.yami.beans;

import com.inspirel.yami.OutgoingMessage;
import com.inspirel.yami.Parameters;
import com.inspirel.yami.YAMIIOException;

/**
 * Bean utility for remote object access.
 */
public class ObjectProxy {

    private AgentWrapper agentWrapper;
    private String target;
    private String objectName;

    /**
     * Default constructor.
     */
    public ObjectProxy() {
    }

    /**
     * Property for defining (referencing) the agent wrapper.
     */
     public void setAgent(AgentWrapper agentWrapper) {
         this.agentWrapper = agentWrapper;
     }

     /**
      * Property for defining the remote target name.
      */
     public void setTarget(String target) {
         this.target = target;
     }

     /**
      * Property for defining the service name, as seen by clients.
      */
     public void setObjectName(String name) {
         this.objectName = name;
     }

    /**
     * Sends the new outgoing message to the given destination.
     *
     * <p>
     * See the documentation for the send method
     * in the Agent class for details.
     * </p>
     */
    public OutgoingMessage send(String messageName,
            Parameters content) throws YAMIIOException {

        return agentWrapper.getAgent().send(
                target, objectName, messageName, content);
    }

    /**
     * Sends the new outgoing message to the given destination.
     *
     * <p>
     * See the documentation for the send method
     * in the Agent class for details.
     * </p>
     */
    public OutgoingMessage send(String messageName,
            Parameters content, int priority) throws YAMIIOException {

        return agentWrapper.getAgent().send(
                target, objectName, messageName, content, priority);
    }

    /**
     * Sends the new outgoing message to the given destination, without
     * the possibility to track its progress.
     *
     * <p>
     * See the documentation for the sendOneWay method
     * in the Agent class for details.
     * </p>
     */
    public void sendOneWay(String messageName,
            Parameters content) throws YAMIIOException {

        agentWrapper.getAgent().sendOneWay(
                target, objectName, messageName, content);
    }

    /**
     * Sends the new outgoing message to the given destination, without
     * the possibility to track its progress.
     *
     * <p>
     * See the documentation for the sendOneWay method
     * in the Agent class for details.
     * </p>
     */
    public void sendOneWay(String messageName,
            Parameters content, int priority) throws YAMIIOException {

        agentWrapper.getAgent().sendOneWay(
                target, objectName, messageName, content, priority);
    }

    /**
     * Closes the given connection with default (lowest) priority.
     *
     * <p>
     * See the documentation for the closeConnection method
     * in the Agent class for details.
     * </p>
     */
    public void closeConnection() {

        agentWrapper.getAgent().closeConnection(target);
    }

    /**
     * Closes the given connection.
     *
     * <p>
     * See the documentation for the closeConnection method
     * in the Agent class for details.
     * </p>
     */
    public void closeConnection(int priority) {

        agentWrapper.getAgent().closeConnection(target, priority);
    }
}
