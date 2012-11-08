/*
 * BEGIN_HEADER - DO NOT EDIT
 *
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)$Id: HL7ManagementService.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7.mgmt.api;

import com.sun.esb.management.common.ImplementationMap;
import com.sun.esb.management.common.ManagementRemoteException;
import com.sun.esb.management.common.Service;
import com.sun.jbi.hl7.mgmt.impl.HL7ManagementServiceImpl;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/**
 *
 * @author ylee
 */
@ImplementationMap(implementationClass = HL7ManagementServiceImpl.class,
                   clientAPIVersion = "1.0")
public interface HL7ManagementService extends Service {

    /**
     * Suspend a consuming endpoint No-op for provisioning endpoints and
     * operation will return false
     *
     * @param consumingEndpointName
     *            an unique consuming endpoint identifier
     * @param targetName
     * @param targetInstanceName
     * @return boolean indicating if suspend succeeds
     * @throws ManagementRemoteException
     */
    boolean suspend(String consumingEndpointName, String targetName,
            String targetInstanceName) throws ManagementRemoteException;

    /**
     * Resume a consuming endpoint No-op for provisioning endpoints and
     * operation will return false.
     *
     * @param consumingEndpointName
     *            an unique consuming endpoint identifier
     * @param targetName
     * @param targetInstanceName
     * @return boolean indicating if resume succeeds
     * @throws ManagementRemoteException
     */
    boolean resume(String consumingEndpointName, String targetName,
            String targetInstanceName) throws ManagementRemoteException;

    /**
     * To get the list of all endpoints deployed in the HL7BC
     * @param targetName
     * @param targetInstanceName
     * @return
     * @throws com.sun.esb.management.common.ManagementRemoteException
     */
    List<String> listEndpoints(String targetName, String targetInstanceName)
            throws ManagementRemoteException;


    /**
     * To get the list of endpoints in the provided service unit.
     * @param serviceUnit
     * @param targetName
     * @param targetInstanceName
     * @return
     * @throws com.sun.esb.management.common.ManagementRemoteException
     */
    List<String> getEndpoints(String serviceUnit, String targetName, String targetInstanceName)
            throws ManagementRemoteException;

    /**
     * get a list of  service units
     * @param targetName
     * @param targetInstanceName
     * @return
     * @throws com.sun.esb.management.common.ManagementRemoteException
     */
    List<String> listServiceUnits(String targetName, String targetInstanceName)
            throws ManagementRemoteException;

    /**
     * Returns true if an consuming endpoint is active, or false if it is
     * suspended Always return true for provisioning endpoints.
     *
     * @param consumingEndpointNamean
     *            unique consuming endpoint identifier
     * @param targetName
     * @param targetInstanceName
     * @return true if endpoint is active, false if it is suspended
     * @throws ManagementRemoteException
     */
    boolean isEndpointActive(String consumingEndpointName, String targetName,
            String targetInstanceName) throws ManagementRemoteException;

    /**
     * Returns an array of unique endpoint names for active consuming endpoints.
     *
     * @param targetName
     * @param targetInstanceName
     * @return an array of unique endpoint names for active consuming endpoints
     * @throws ManagementRemoteException
     */
    List<String> listActiveEndpoints(String targetName, String targetInstanceName)
            throws ManagementRemoteException;

    /**
     * Returns an array of unique endpoint names for inactive (suspended)
     * consuming endpoints.
     *
     * @param targetName
     * @param targetInstanceName
     * @return an array of unique endpoint names for inactive(suspended)
     *         consuming endpoints
     * @throws ManagementRemoteException
     */
    List<String> listInactiveEndpoints(String targetName, String targetInstanceName)
            throws ManagementRemoteException;

    List<String> getHl7Servers(String targetName, String targetInstanceName)
            throws ManagementRemoteException;

    /**
    * Gets the list of client connection status for the given server IP and Port  (HL7 Inbound)
    * @param serverLocation The location of the server
    * @param serverPort The port number of the serer
    * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
    * @return  a map containing the client connection address, and its status
    *
    */
    ConcurrentHashMap<String, String> getExternalSystemConnectionStatus(String serverLocation, long serverPort,
            String targetName, String targetInstanceName) throws ManagementRemoteException;


    /**
     * list of external system connections
     * @param targetName
     * @param targetInstanceName
     * @return
     */
    List<String> listExternalSystemConnections(String targetName, String targetInstanceName) throws ManagementRemoteException;


    /**
     * To get a list of last message sent timestamps. Both in inbound & outbound modes.
     * @param targetName
     * @param targetInstanceName
     * @return
     * @throws com.sun.esb.management.common.ManagementRemoteException
     */
    ConcurrentHashMap<String, String> getMessageSentTimestamps(String targetName, String targetInstanceName) throws ManagementRemoteException;


    /**
     * get a list of last message received timestamps. Both in inbound & outbound modes.
     * @param targetName
     * @param targetInstanceName
     * @return
     * @throws com.sun.esb.management.common.ManagementRemoteException
     *
     */
    ConcurrentHashMap<String, String> getMessageReceivedTimestamps(String targetName, String targetInstanceName) throws ManagementRemoteException;


    /**
     * Returns the current sequence number of the given HL7 Endpoint
     * @param serviceUnit The name of the service unit
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @return  the current sequence number of the HL7 endpoint
     * @throws ManagementRemoteException
     */
    long getSequenceNumber(String serviceUnit, String targetName, String targetInstanceName) throws ManagementRemoteException;


}


