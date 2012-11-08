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
 * @(#)HL7Server.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.server;

import java.util.Map;

import com.sun.jbi.hl7bc.ApplicationException;
import com.sun.jbi.hl7bc.Endpoint;
import com.sun.jbi.hl7bc.configuration.RuntimeConfiguration;
import com.sun.jbi.hl7bc.extservice.ProtocolInfo;
import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnectionFactory;
import com.sun.jbi.hl7bc.extservice.persist.dbo.DBObjectFactory;
import javax.jbi.component.ComponentContext;

import org.apache.mina.common.IoSession;

/**
 * Transport Protocol specific classes extends this interface to create and destory HL7 Service
 * 
 * @author S. Nageswara Rao, Raghunadh
 */
public interface HL7Server {

    public static final String UP = "Up";

    public static final String DOWN = "Down";

    /**
     * creates HL7Service
     * 
     * @param hl7Listener The listener that receives the hl7 messages from transport protocol
     * @param transportProtoolInfo Map of Information that is used during HL7 Service creation
     * @throws ApplicationException
     */
    void createHL7Service(HL7Listener hl7Listener, ProtocolInfo transportProtocolInfo) throws ApplicationException;

    /**
     * creates HL7Service
     * 
     * @param hl7Listeners The Map contains listeners per message type that receives the hl7
     *            messages from transport protocol
     * @param transportProtoolInfo Map of Information that is used during HL7 Service creation
     * @throws ApplicationException
     */
    void createHL7Service(Map hl7Listeners, ProtocolInfo transportProtocolInfo) throws ApplicationException;

    /**
     * destroys HL7Service
     * 
     * @param transportProtoolInfo Map of Information that is used during HL7 Service creation
     * @throws ApplicationException
     */
    void destoryHL7Service(ProtocolInfo transportProtocolInfo) throws ApplicationException;

    /**
     * Suspends the service from message processing
     * 
     * @param transportProtocolInfo
     * @throws ApplicationException
     */
    void suspendHL7Service(ProtocolInfo transportProtocolInfo) throws ApplicationException;

    /**
     * Resume the service for processing the messages
     * 
     * @param transportProtocolInfo
     * @throws ApplicationException
     */
    void resumeHL7Service(ProtocolInfo transportProtocolInfo) throws ApplicationException;

    /**
     * Stop all the running services
     */
    void stopAllServices() throws ApplicationException;

    /**
     * Provide the component context for HL7 Server
     * 
     * @param ctx
     */
    void setComponentContext(ComponentContext ctx);

    /**
     * Provide the RuntimeConfiguration for HL7Server
     * 
     * @param runtimeConfig
     */
    void setRuntimeConfiguration(RuntimeConfiguration runtimeConfig);

    /**
     * Set the endpoint object
     * 
     * @param endpoint
     */
    void setEndpoint(Endpoint endpoint);

    /**
     * Set the MBeanName
     * 
     * @param mbname
     */
    void setMonitorExtSysConnMBeanName(String mbname);

    /**
     * notifies about new connection creation
     * 
     * @param connID
     */
    void notifyNewConnetion(IoSession session);

    /**
     * notifies about a lost connection
     * 
     * @param connID
     */
    void notifyLostConnection(IoSession session);

    /**
     * notifies about last Msg received time stamp
     * 
     * @param connID
     */
    void notifyLastMessageReceivedTimeStamp(String connID);

    /**
     * notifies about a ACK message sent time stamp
     * 
     * @param connID
     */
    void notifyLastACKMessageSentTimeStamp(String connID);

    /**
     * Return the DBConnectionFactory
     * 
     * @return
     */
    // DBConnectionFactory getDBConnectionFactory();
    /**
     * Return the DBObjectFactory
     * 
     * @return
     */
    // /DBObjectFactory getDBObjectFactory();
}
