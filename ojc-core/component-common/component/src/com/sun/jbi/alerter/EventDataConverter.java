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
package com.sun.jbi.alerter;

import java.util.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;


import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;




/*
 *  the class serialize and de-serialize event instance content.
 *  current implementation use CompositeData as the means to serialize
 *  it. 
 */
public class EventDataConverter {
    
    private final String[] itemNames = {CompositeDataEventElementType.EVENTDATE.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTID.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTENVIRONMENT.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTLOGICALHOST.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTSERVER.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTCOMPONENTPROJECTPATH.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTDEPLOYMENT.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTCOMPONENTNAME.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTSEVERITY.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTTYPE.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTSTATUS.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTSTATE.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTPHYSICALHOST.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTMESSAGECODE.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTMESSAGEDETAILS.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTSERVERTYPE.getCompositeDataElement(),
            CompositeDataEventElementType.EVENTCOMPONENTTYPE.getCompositeDataElement(),
            CompositeDataEventElementType.DBEVENTID.getCompositeDataElement()};
    private final String[] itemDescriptions = {
            CompositeDataEventElementType.EVENTDATE.getDescription(),
            CompositeDataEventElementType.EVENTID.getDescription(),
            CompositeDataEventElementType.EVENTENVIRONMENT.getDescription(),
            CompositeDataEventElementType.EVENTLOGICALHOST.getDescription(),
            CompositeDataEventElementType.EVENTSERVER.getDescription(),
            CompositeDataEventElementType.EVENTCOMPONENTPROJECTPATH.getDescription(),
            CompositeDataEventElementType.EVENTDEPLOYMENT.getDescription(),
            CompositeDataEventElementType.EVENTCOMPONENTNAME.getDescription(),
            CompositeDataEventElementType.EVENTSEVERITY.getDescription(),
            CompositeDataEventElementType.EVENTTYPE.getDescription(),
            CompositeDataEventElementType.EVENTSTATUS.getDescription(),
            CompositeDataEventElementType.EVENTSTATE.getDescription(),
            CompositeDataEventElementType.EVENTPHYSICALHOST.getDescription(),
            CompositeDataEventElementType.EVENTMESSAGECODE.getDescription(),
            CompositeDataEventElementType.EVENTMESSAGEDETAILS.getDescription(),
            CompositeDataEventElementType.EVENTSERVERTYPE.getDescription(),
            CompositeDataEventElementType.EVENTCOMPONENTTYPE.getDescription(),
            CompositeDataEventElementType.DBEVENTID.getDescription()};
    private final OpenType[] itemTypes = { SimpleType.STRING, SimpleType.STRING,
            SimpleType.STRING,SimpleType.STRING,SimpleType.STRING,
            SimpleType.STRING,SimpleType.STRING,SimpleType.STRING,
            SimpleType.STRING,SimpleType.STRING,SimpleType.STRING,
            SimpleType.STRING,SimpleType.STRING,SimpleType.STRING,
            SimpleType.STRING,SimpleType.STRING,SimpleType.STRING,
            SimpleType.STRING};
    
    public EventDataConverter() {
    }
    
    public CompositeData getEventCompositeData(NotificationEvent event,String aDBEventID) throws Exception{
        CompositeType compositeType = new CompositeType("event","The Object Represent single event instance",
                itemNames,itemDescriptions,itemTypes); 
        Map<String,String> map = getEventMap(event,aDBEventID);
        CompositeData compositeData = new CompositeDataSupport(compositeType,map);
        return compositeData;
    }
    
    public CompositeData getEventCompositeData(ResultSet aResultSet,String aDBEventID) throws Exception{
        CompositeType compositeType = new CompositeType("event","The Object Represent single event row in the persistence DB",
                itemNames,itemDescriptions,itemTypes); 
        Map<String,String> map = getEventRowMap(aResultSet,aDBEventID);
        CompositeData compositeData = new CompositeDataSupport(compositeType,map);
        return compositeData;
    }
    
    public NotificationEvent getEventFromCompositeData(CompositeData compositeData) {
        NotificationEvent event = new NotificationEventImpl();
        Object[] eventData= compositeData.getAll(itemNames);
        populateEventObject(event,eventData);       
        return  event;
    }
    
    private Map<String,String> getEventRowMap(ResultSet aResultSet,String aDBEventID) throws SQLException {

        
        
         Map<String,String> properties = new HashMap<String,String>();
         properties.put(CompositeDataEventElementType.EVENTDATE.getCompositeDataElement(),
                 aResultSet.getLong(2)+"");
         properties.put(CompositeDataEventElementType.EVENTID.getCompositeDataElement(),
                 aResultSet.getInt(1)+"");
         properties.put(CompositeDataEventElementType.EVENTENVIRONMENT.getCompositeDataElement(),
                 aResultSet.getString(4));
         properties.put(CompositeDataEventElementType.EVENTLOGICALHOST.getCompositeDataElement(),
                 aResultSet.getString(5));
         properties.put(CompositeDataEventElementType.EVENTSERVER.getCompositeDataElement(),
                 aResultSet.getString(7));
         properties.put(CompositeDataEventElementType.EVENTCOMPONENTPROJECTPATH.getCompositeDataElement(),
                 aResultSet.getString(9));
         properties.put( CompositeDataEventElementType.EVENTDEPLOYMENT.getCompositeDataElement(),
                 aResultSet.getString(17));
         properties.put(CompositeDataEventElementType.EVENTCOMPONENTNAME.getCompositeDataElement(),
                 aResultSet.getString(10));
         properties.put(CompositeDataEventElementType.EVENTSEVERITY.getCompositeDataElement(),
                 aResultSet.getInt(12)+"");
         properties.put( CompositeDataEventElementType.EVENTTYPE.getCompositeDataElement(),
                 aResultSet.getString(11));
         properties.put(CompositeDataEventElementType.EVENTSTATUS.getCompositeDataElement(),
                 aResultSet.getInt(16)+"");
         properties.put(CompositeDataEventElementType.EVENTSTATE.getCompositeDataElement(),
                 aResultSet.getInt(13)+"");
         properties.put(CompositeDataEventElementType.EVENTPHYSICALHOST.getCompositeDataElement(),
                 aResultSet.getString(3));
         properties.put(CompositeDataEventElementType.EVENTMESSAGECODE.getCompositeDataElement(),
                 aResultSet.getString(14));
         properties.put( CompositeDataEventElementType.EVENTMESSAGEDETAILS.getCompositeDataElement(),
                 aResultSet.getString(15));
         properties.put(CompositeDataEventElementType.EVENTSERVERTYPE.getCompositeDataElement(),
                 aResultSet.getString(6));
         properties.put(CompositeDataEventElementType.EVENTCOMPONENTTYPE.getCompositeDataElement(),
                 aResultSet.getString(8));
     
         return properties;
     }

    private Map<String,String> getEventMap(NotificationEvent event,String aDBEventID) {
       Map<String,String> properties = new HashMap<String,String>();
        properties.put(CompositeDataEventElementType.EVENTDATE.getCompositeDataElement(),
                event.getTimeStamp()+"");
        properties.put(CompositeDataEventElementType.EVENTID.getCompositeDataElement(),
                event.getId()+"");
        properties.put(CompositeDataEventElementType.EVENTENVIRONMENT.getCompositeDataElement(),
                event.getEnvironmentName());
        properties.put(CompositeDataEventElementType.EVENTLOGICALHOST.getCompositeDataElement(),
                event.getLogicalHostName());
        properties.put(CompositeDataEventElementType.EVENTSERVER.getCompositeDataElement(),
                event.getServerName());
        properties.put(CompositeDataEventElementType.EVENTCOMPONENTPROJECTPATH.getCompositeDataElement(),
                event.getComponentProjectPathName());
        properties.put( CompositeDataEventElementType.EVENTDEPLOYMENT.getCompositeDataElement(),
                event.getDeploymentName());
        properties.put(CompositeDataEventElementType.EVENTCOMPONENTNAME.getCompositeDataElement(),
                event.getComponentName());
        properties.put(CompositeDataEventElementType.EVENTSEVERITY.getCompositeDataElement(),
                event.getSeverity()+"");
        properties.put( CompositeDataEventElementType.EVENTTYPE.getCompositeDataElement(),
                event.getType());
        properties.put(CompositeDataEventElementType.EVENTSTATUS.getCompositeDataElement(),
                event.getObservationalState()+"");
        properties.put(CompositeDataEventElementType.EVENTSTATE.getCompositeDataElement(),
                event.getOperationalState()+"");
        properties.put(CompositeDataEventElementType.EVENTPHYSICALHOST.getCompositeDataElement(),
                event.getPhysicalHostName());
        properties.put(CompositeDataEventElementType.EVENTMESSAGECODE.getCompositeDataElement(),
                event.getMessageCode());
        properties.put( CompositeDataEventElementType.EVENTMESSAGEDETAILS.getCompositeDataElement(),
                event.getMessageDetails());
        properties.put(CompositeDataEventElementType.EVENTSERVERTYPE.getCompositeDataElement(),
                event.getServerType());
        properties.put(CompositeDataEventElementType.EVENTCOMPONENTTYPE.getCompositeDataElement(),
                event.getComponentType());
        properties.put(CompositeDataEventElementType.DBEVENTID.getCompositeDataElement(),
                aDBEventID);
   
        return properties;
    }
    
    private void populateEventObject(NotificationEvent event,Object[] eventData) {
        
        for (int index = 0; index < eventData.length; index++) {
            Object value = eventData[index];
            if(value == null) {
                continue;
            }
            String eventEntryValue = (String)value;
            switch (index) {
                //DATE
                case 0:
                    event.setTimeStamp(Long.parseLong(eventEntryValue));
                    break;
                //ID
                case 1:
                    // id will be assigned by the DB (this column must have auto-increment attribute).
                    break;
                //ENVIRONMENT    
                case 2:
                    event.setEnvironmentName(eventEntryValue);
                    break;
                //LOGICALHOST
                case 3:
                    event.setLogicalHostName(eventEntryValue);
                    break;
                //SERVER
                case 4:
                    event.setServerName(eventEntryValue);
                    break;
                //COMPONENTPROJECTPATH
                case 5:
                    event.setComponentProjectPathName(eventEntryValue);
                    break;
                //DEPLOYMENT
                case 6:
                    event.setDeploymentName(eventEntryValue);
                    break;
                //COMPONENTNAME
                case 7:
                    event.setComponentName(eventEntryValue);
                    break;
                //SEVERITY
                case 8:
                    event.setSeverity(Integer.parseInt(eventEntryValue));
                    break;
                //TYPE
                case 9:
                    event.setType(eventEntryValue);
                    break;
                //STATUS    
                case 10:
                    event.setObservationalState(Integer.parseInt(eventEntryValue));
                    break;
                //STATE
                case 11:
                    event.setOperationalState(Integer.parseInt(eventEntryValue));
                    break;
                //PHYSICALHOST
                case 12:
                    event.setPhysicalHostName(eventEntryValue);
                    break;
                //MESSAGECODE
                case 13:
                    event.setMessageCode(eventEntryValue);
                    break;
                //MESSAGEDETAILS
                case 14:
                    event.setMessageDetails(eventEntryValue);
                    break;
                //SERVERTYPE    
                case 15:
                    event.setServerType(eventEntryValue);
                    break;
                //COMPONENTTYPE    
                case 16:
                    event.setComponentType(eventEntryValue);
                   break;
            }
        }
    }
    

    public Date getEventDateFromString(String eventDate) {
        SimpleDateFormat sdf = 
                new SimpleDateFormat("EEE MMM dd HH:mm:ss z yyyy");
         return sdf.parse(eventDate,new ParsePosition(0));
    }


}
