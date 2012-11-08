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
 *
 * @(#)EventProperties.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.alerter;


/**
 * EM constants and properties.
 * @author  Yoke Lee
 */
public interface EventProperties {
 
    /**
     * Process MBean's "start" operation name
     */
    public static final String START_OPERATION = "start";

    /**
     * Process MBean's "stop" operation name
     */
    public static final String STOP_OPERATION = "stop";
    
    
    ////////////  Event Management //////////////////
    /**
     * EventPublisher MBean <code>ObjectName</code>
     */
    public static final String EVENTPUBLISHER_MBEAN_NAME =
            "EventManagement:name=EventPublisher";    

    /**
     * EventForwarder MBean <code>ObjectName</code>
     */
    public static final String EVENTFORWARDER_MBEAN_NAME =
            "EventManagement:name=EventForwarderMBean";    
    
    
	/**
	 * Publish a text message
    */
	 public static final String PUBLISH_TEXTMESSAGE_OPERATION = "publishTextMessage";

	/**
	 * Publish an object message
    */
     public static final String EVENT_FORWARD_OPERATION = "forwardEvent";    
     public static final String EVENT_CREATEFORWARD_OPERATION = "createAndForwardEvent";    
	 public static final String PUBLISH_OBJECTMESSAGE_OPERATION = "publishObjectMessage";    
     public static final String PUBLISH_OBJECTMESSAGE_WITH_INSPECTION_OPERATION = "publishObjectMessageWithInspection";    
     public static final String UPDATE_SENTINEL_INFORMATION = "updateSentinelInformation";    

	 public static final String SETUP_OPERATION = "setup";    
    
    public static final String CONFIGURE_OPERATION = "configure";  
    
    /** the jms host key name used by this publisher */
    public static final String JMSHOST = "JMSHOST";
    /** the jms port key name used by this publisher */
    public static final String JMSPORT = "JMSPORT";
    /** the jms topic key name used by this publisher */
    public static final String JMSTOPICNAME = "JMSTOPICNAME";
    public static final String JMSCONNECTIONFACTORYNAME = "JMSCONNECTIONFACTORYNAME";

    public static final String TRANSACTED_FLAG = "TRANSACTED";
    public static final String DELIVERY_MODE = "DELIVERYMODE";
    public static final String PRIORITY = "PRIORITY";
    public static final String TIME_TO_LIVE = "TIMETOLIVE";
    
    public static final String ENVIRONMENT = "ENVIRONMENT";
    public static final String LOGICALHOSTNAME = "LOGICALHOSTNAME";
    public static final String PHYSICALHOSTNAME = "PHYSICALHOSTNAME";
    public static final String SERVER_TYPE = "SERVERTYPE";
    public static final String SERVER_NAME = "SERVERNAME";
    
    
    // values
    public static final String INTEGRATION_SERVER_TYPE = "INTEGRATION";
    public static final String TOPICNAME = "Events";
    public static final String NOTIFICATION_ALERT = "Notification.Alert";
    
   //////////////////////////////////////////////////////////
}
