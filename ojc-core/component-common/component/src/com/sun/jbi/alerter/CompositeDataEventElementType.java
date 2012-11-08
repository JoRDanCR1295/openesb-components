/*
 * BEGIN_HEADER - DO NOT EDIT
 *
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)CompositeDataEventElementType.java
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.alerter;

public enum CompositeDataEventElementType {
    EVENTDATE("EVENTDATE"),
    EVENTID("EVENTID"),
    EVENTENVIRONMENT("EVENTENVIRONMENT"),
    EVENTLOGICALHOST("EVENTLOGICALHOST"),
    EVENTSERVER("EVENTSERVER"),         
    EVENTCOMPONENTPROJECTPATH("EVENTCOMPONENTPROJECTPATH"),
    EVENTDEPLOYMENT("EVENTDEPLOYMENT"),     
    EVENTCOMPONENTNAME("EVENTCOMPONENTNAME"),
    EVENTSEVERITY("EVENTSEVERITY"),       
    EVENTTYPE("EVENTTYPE"),           
    EVENTSTATUS("EVENTSTATUS"),         
    EVENTSTATE("EVENTSTATE"),          
    EVENTPHYSICALHOST("EVENTPHYSICALHOST"),   
    EVENTMESSAGECODE("EVENTMESSAGECODE"),    
    EVENTMESSAGEDETAILS("EVENTMESSAGEDETAILS"), 
    EVENTSERVERTYPE("EVENTSERVERTYPE"),      
    EVENTCOMPONENTTYPE("EVENTCOMPONENTTYPE"),
    DBEVENTID("DBEVENTID"),  
    SUBSCRIBERID("SUBSCRIBERID");  
   
    private String ElementName;
    
    private CompositeDataEventElementType(String filterElementName) {
        this.ElementName = filterElementName;
    }

    public String getDescription() {
        switch (this) {
            case EVENTDATE:
                 return "The timestamp when the event was genereted";
            case EVENTID:
                return  "Internal unique ID";
            case EVENTENVIRONMENT:
                return  "The environment the event generated in";
            case EVENTLOGICALHOST:
                return  "The logical host  the event generated in";
            case EVENTSERVER:
                return  "The server the event generated in";
            case EVENTCOMPONENTPROJECTPATH:
                return  "The component path"; 
            case EVENTDEPLOYMENT:
                return  "The deployemnt";
            case EVENTCOMPONENTNAME:
                return  "The component name";
            case EVENTSEVERITY:
                return  "The severity of the event";
            case EVENTTYPE:
                return  "The type of the event"; 
            case EVENTSTATUS:
                return  "The status of the event";
            case EVENTSTATE:
                return  "The component generating this event state ";
            case EVENTPHYSICALHOST:
                return  "The name of the server the event generated in";
            case EVENTMESSAGECODE:
                return  "The message code assoc. with the event";
            case EVENTMESSAGEDETAILS:
                return  "The details of the event message "; 
            case EVENTSERVERTYPE:
                return "The server type"; 
            case EVENTCOMPONENTTYPE:
                return "The component type"; 
            case DBEVENTID:    
                return "The unique id associated with this event when peristed in the database"; 
            case SUBSCRIBERID:    
                return "The unique id associated with the client that subscribe to receive this event"; 
        }
        return "";
    }

    
    
    public String getCompositeDataElement() {
        return ElementName;
    }

}
