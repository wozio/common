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

import com.inspirel.yami.IncomingMessageCallback;

/**
 * Bean utility for binding YAMI object implementations
 * with the agent wrapper.
 */
public class ObjectExporter {

    private AgentWrapper agentWrapper;
    private String name;
    private IncomingMessageCallback callback;

    /**
     * Default constructor.
     */
    public ObjectExporter() {
    }

    /**
     * Property for defining (referencing) the agent wrapper.
     */
     public void setAgent(AgentWrapper agentWrapper) {
         this.agentWrapper = agentWrapper;
         if (name != null && callback != null) {
             doRegister();
         }
     }

     /**
      * Property for defining the service (object implementation).
      */
     public void setObject(IncomingMessageCallback callback) {
         this.callback = callback;
         if (agentWrapper != null && name != null) {
             doRegister();
         }
     }

     /**
      * Property for defining the service name, as seen by clients.
      */
     public void setObjectName(String name) {
         this.name = name;
         if (agentWrapper != null && callback != null) {
             doRegister();
         }
     }

     private void doRegister() {
         agentWrapper.getAgent().registerObject(name, callback);
     }
}
