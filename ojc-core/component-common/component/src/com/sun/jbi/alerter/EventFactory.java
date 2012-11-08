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
 * @(#)EventFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.alerter;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.util.Date;

import java.util.logging.Logger;

/**
 * this factory creates a TopicConnectionFactory based on classname and properties
 *
 * @author Yoke Lee
 * @version 1.0
 * 
 */
public class EventFactory implements Serializable {
   
    private static Logger mLogger = 
        Logger.getLogger(EventFactory.class.getName());
   

    
    /** empty constructor
     * @return Event concrete class
     */
    public Event getEvent(){             
                          
        Event event =  new EventImpl();
        return event;
    }

    
    /**
     * Event Constructor.
     * @param physicalHostName         the name of the physical host
     * @param environmentName          the name of the environment
     * @param logicalHostName          the name of the logical host
     * @param serverType               the type of the server (INTEGRATION,
     *                                 MESSAGE)
     * @param serverName               the name of the server
     * @param componentType            the type of the component
     *                                 (COLLABORATION_COMPONENT)
     * @param componentProjectPathName the full path name of the project in
     *                                 which the component is in. Note the names
     *                                 of the project and its sub-projects, if
     *                                 any, are separated by '/'.
     * @param componentName            the name of the component
     * @param timeStamp                the timestamp of when the event was
     *                                 generated
     * @return Event concrete class
     */
    public Event getEvent(
                 String physicalHostName,
                 String environmentName,
                 String logicalHostName,
                 String serverType,
                 String serverName,
                 String componentType,
                 String componentProjectPathName,
                 String componentName,
                 long timeStamp ) {             
                          
        
        Event event = new EventImpl();
        // the rationale behind this approach is that the client can pass in <null> and primitives as parameters
        // which would present a problem if we try to use reflection to find the right constructor 
        event.setPhysicalHostName(physicalHostName);
        event.setEnvironmentName(environmentName);
        event.setLogicalHostName(logicalHostName);
        event.setServerType(serverType);
        event.setServerName(serverName);
        event.setComponentType(componentType);
        event.setComponentProjectPathName(componentProjectPathName);
        event.setComponentName(componentName);
        event.setTimeStamp(timeStamp);
        
        return event;
    }
    

    /**
     * Event Constructor.
     *
     * @param componentType            the type of the component
     *                                 (COLLABORATION_COMPONENT)
     * @param componentProjectPathName the full path name of the project in
     *                                 which the component is in. Note the names
     *                                 of the project and its sub-projects, if
     *                                 any, are separated by '/'.
     * @param componentName            the name of the component
     * @param timeStamp                the timestamp of when the event was
     *                                 generated
     * @return Event concrete class
     */
    public Event getEvent(
                 String componentType,
                 String componentProjectPathName,
                 String componentName,
                 long timeStamp ) {                     
                          
        // instantiate the event object
        Event event = new EventImpl();
        event.setComponentType(componentType);
        event.setComponentProjectPathName(componentProjectPathName);
        event.setComponentName(componentName);
        event.setTimeStamp(timeStamp);
        
        return event;
    }    
    
    
    /** empty constructor
     * @return Notification Event concrete class
     */
    public NotificationEvent getNotificationEvent() {             
                          
        NotificationEvent event = new NotificationEventImpl();
        return event;
    }
   
    /** 
     * @return Notification Event concrete class
     */
    public NotificationEvent getNotificationEvent(
                 String componentType,
                 String componentProjectPathName,
                 String componentName,
                 long timeStamp)  {    
                 
        // instantiate the event object
        NotificationEvent event = new NotificationEventImpl();
        event.setComponentType(componentType);
        event.setComponentProjectPathName(componentProjectPathName);
        event.setComponentName(componentName);
        event.setTimeStamp(timeStamp);
        
        return event;
    }
    
	/** 
	 * @return Notification Event concrete class
	 */
	public NotificationEvent getNotificationEvent(
				 String componentType,
				 String deploymentName,
				 String componentProjectPathName,
				 String componentName,
				 long timeStamp){    
                 
		// instantiate the event object
                 NotificationEvent event = new NotificationEventImpl();
		event.setComponentType(componentType);
		event.setDeploymentName(deploymentName);
		event.setComponentProjectPathName(componentProjectPathName);
		event.setComponentName(componentName);
		event.setTimeStamp(timeStamp);
        
		return event;
	}

	/** 
	 * @return Notification Event concrete class
	 */
	public NotificationEvent getNotificationEvent(
		String physicalHostName,
		String deploymentName,
		String environmentName,
		String logicalHostName,
		String serverType,
		String serverName,
		String componentType,
		String componentProjectPathName,
		String componentName,
		String type,
		int severity,
		int operationalState,
		String messageCode,
		String[] messageCodeArgs,
		String messageDetails ) {
                 
       
		// instantiate the event object
                NotificationEvent event = new NotificationEventImpl();
		event.setPhysicalHostName(physicalHostName);
		event.setDeploymentName(deploymentName);
		event.setEnvironmentName(environmentName);
		event.setLogicalHostName(logicalHostName);
		event.setServerType(serverType);
		event.setServerName(serverName);
		event.setComponentType(componentType);
		event.setComponentProjectPathName(componentProjectPathName);
		event.setComponentName(componentName);
		event.setType(type);
		event.setSeverity(severity);
		event.setOperationalState(operationalState);
		event.setMessageCode(messageCode);
		event.setMessageCodeArgs(messageCodeArgs);
		event.setMessageDetails(messageDetails);
		event.setTimeStamp(new Date().getTime());
		return event;
	}       
    
    /** 
     * @return Notification Event concrete class
     */
    public NotificationEvent getNotificationEvent(
        String physicalHostName,
        String environmentName,
        String logicalHostName,
        String serverType,
        String serverName,
        String componentType,
        String componentProjectPathName,
        String componentName,
        String type,
        int severity,
        int operationalState,
        String messageCode,
        String[] messageCodeArgs,
        String messageDetails ){    
                 
        
        // instantiate the event object
        NotificationEvent event = new NotificationEventImpl();
        event.setPhysicalHostName(physicalHostName);
        event.setEnvironmentName(environmentName);
        event.setLogicalHostName(logicalHostName);
        event.setServerType(serverType);
        event.setServerName(serverName);
        event.setComponentType(componentType);
        event.setComponentProjectPathName(componentProjectPathName);
        event.setComponentName(componentName);
        event.setType(type);
        event.setSeverity(severity);
        event.setOperationalState(operationalState);
        event.setMessageCode(messageCode);
        event.setMessageCodeArgs(messageCodeArgs);
        event.setMessageDetails(messageDetails);
        event.setTimeStamp(new Date().getTime());
        return event;
    } 
        
}
