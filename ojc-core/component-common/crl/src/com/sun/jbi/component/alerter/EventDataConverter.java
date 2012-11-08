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
 * @(#)EventDataConverter.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.alerter;

import javax.management.openmbean.CompositeData;

/**
 * Defines interface for converting {@link NotificationEvent} 
 * instances into MBean-compliant data.
 * 
 * @author Kevan Simpson
 */
public interface EventDataConverter {
	public CompositeData convertEvent(NotificationEvent evt) throws Exception;
//	public NotificationEvent convertCompositeData(CompositeData data);
	
	public enum EventField {
	    EVENT_DATE("EVENTDATE", "The timestamp when the event was generated"),
	    EVENT_ID("EVENTID", "Internal unique ID"),
	    EVENT_ENVIRONMENT("EVENTENVIRONMENT", "The environment in which the event was generated"),
	    EVENT_LOGICAL_HOST("EVENTLOGICALHOST", "The logical host in which the event was generated"),
	    EVENT_SERVER("EVENTSERVER", "The server in which the event was generated"),         
	    EVENT_COMPONENT_PROJECT_PATH("EVENTCOMPONENTPROJECTPATH", "JBI component path"),
	    EVENT_DEPLOYMENT("EVENTDEPLOYMENT", "JBI deployment unit name"),     
	    EVENT_COMPONENT_NAME("EVENTCOMPONENTNAME", "JBI component name"),
	    EVENT_SEVERITY("EVENTSEVERITY", "The severity of the event"),       
	    EVENT_TYPE("EVENTTYPE", "The type of the event"),
	    EVENT_STATUS("EVENTSTATUS", "The status of the event"),         
	    EVENT_STATE("EVENTSTATE", "The component generating this event"),          
	    EVENT_PHYSICAL_HOST("EVENTPHYSICALHOST", "The name of the server on which the event was generated"),   
	    EVENT_MESSAGE_CODE("EVENTMESSAGECODE", "The message code associated with the event"),    
	    EVENT_MESSAGE_DETAILS("EVENTMESSAGEDETAILS", "The details of the event message"), 
	    EVENT_SERVER_TYPE("EVENTSERVERTYPE", "The server type"),      
	    EVENT_COMPONENT_TYPE("EVENTCOMPONENTTYPE", "The component type"),
	    DB_EVENT_ID("DBEVENTID", "The unique id associated with this event when peristed in the database"),  
	    SUBSCRIBER_ID("SUBSCRIBERID", "The unique id associated with the client that subscribe to receive this event");  
	   
	    private String mName, mDescription;
	    
	    private EventField(String name, String desc) {
	        mName = name;
	        mDescription = desc;
	    }

	    public String getName() {
	        return mName;
	    }
	    public String getDescription() {
	        return mDescription;
	    }
	}
}
