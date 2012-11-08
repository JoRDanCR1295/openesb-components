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
 * @(#)EventDataConverterImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.alerter.impl;

import java.util.HashMap;
import java.util.Map;

import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;

import com.sun.jbi.component.alerter.EventDataConverter;
import com.sun.jbi.component.alerter.NotificationEvent;

/**
 * 
 * @author Kevan Simpson
 */
public class EventDataConverterImpl implements EventDataConverter {
    private final String[] mItemNames, mItemDescriptions;
    private final OpenType[] mItemTypes;
    
    public EventDataConverterImpl() {
        EventField[] fields = EventField.values();
        int len = fields.length;
        mItemNames = new String[len];
        mItemDescriptions = new String[len];
        mItemTypes = new OpenType[len];
        
        for (int i = 0; i < len; i++) {
            mItemNames[i] = fields[i].getName();
            mItemDescriptions[i] = fields[i].getDescription();
            mItemTypes[i] = SimpleType.STRING;
        }
    }
    
	/** @see com.sun.jbi.component.alerter.EventDataConverter#convertEvent(com.sun.jbi.component.alerter.Event) */
	public CompositeData convertEvent(NotificationEvent evt) throws Exception {
        CompositeType compositeType = 
        		new CompositeType("event", "A single event instance",
        						  mItemNames, mItemDescriptions, mItemTypes); 
        Map<String,String> map = populateEventMap(evt);
        CompositeData compositeData = new CompositeDataSupport(compositeType, map);
        return compositeData;
	}

	protected Map<String, String> populateEventMap(NotificationEvent event) {
		Map<String, String> map = new HashMap<String, String>();
		map.put(EventField.EVENT_DATE.getName(), String.valueOf(event.getTimeStamp()));
		map.put(EventField.EVENT_ID.getName(), String.valueOf(event.getId())); 
		map.put(EventField.EVENT_ENVIRONMENT.getName(), event.getEnvironmentName());
		map.put(EventField.EVENT_LOGICAL_HOST.getName(), event.getLogicalHostName());
		map.put(EventField.EVENT_SERVER.getName(), event.getServerName());
		map.put(EventField.EVENT_COMPONENT_PROJECT_PATH.getName(), event.getComponentProjectPathName());
		map.put(EventField.EVENT_DEPLOYMENT.getName(), event.getDeploymentName());
		map.put(EventField.EVENT_COMPONENT_NAME.getName(), event.getComponentName());
		map.put(EventField.EVENT_SEVERITY.getName(), String.valueOf(event.getSeverity().ordinal()));
		map.put(EventField.EVENT_TYPE.getName(), "Alert");    // only support Alerts for now
		map.put(EventField.EVENT_STATUS.getName(), "1");   // defaults to OBSERVED
		map.put(EventField.EVENT_STATE.getName(), String.valueOf(event.getOperationalState().ordinal()));
		map.put(EventField.EVENT_PHYSICAL_HOST.getName(), event.getPhysicalHostName());
		map.put(EventField.EVENT_MESSAGE_CODE.getName(), event.getMessageCode());
		map.put(EventField.EVENT_MESSAGE_DETAILS.getName(), event.getMessage());
		map.put(EventField.EVENT_SERVER_TYPE.getName(), event.getServerType());      
		map.put(EventField.EVENT_COMPONENT_TYPE.getName(), event.getComponentType().toString());
		map.put(EventField.DB_EVENT_ID.getName(), "");    // can't see where this is used
		map.put(EventField.SUBSCRIBER_ID.getName(), "");  // no clue what this is for...
		return map;
	}
	
//	protected void populateEvent(NotificationEvent event, Object[] data) {
//		NOT SURE IF THIS IS NEEDED		
//		if (data[0] != null) event.setTimeStamp(Long.parseLong(String.valueOf(data[0])));
//	}
}
