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
 * @(#)Alerter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.alerter;

/** 
 * The Alerter class is basically an interface class that exposes convenient methods for sending 
 * out alerts for various severity levels. A client can then use this class to send out an alert to the 
 * event management framework. The alert is first routed to the CollabMBean and then out to the 
 * event management framework
 *
 * @author Yoke Lee
 * @version 1.0
 */
public interface Alerter {

    /** 
     * send out an alert with severity set to fatal
     * @param msg               the alert message
     * @param componentName     name of component 
     *                          ex. sun-bpel-engine, sun-http-binding
     * @param deploymentName   The Service Unit name can be passed in if 
     *                         this corresponds to a deployment. Can have
     *                         null value.
     * @param serverType       the server type hosting the component. Can have
     *                         null value. ex. Glassfish. 
     * @param componentType    component type Can have null value.
     *                         ex. ServiceEngine ot BindingComponent
     * @param componentState   the state of the component when this event
     *                         was generated. See NotificationEvent class
     *                         opeartional state constants for possible values.
     * @param eventType        the event type See NotificationEvent class
     *                         opeartional state constants for possible values.
     * @param messageCode      code assocaited with this event message.
     *                         Can have null value.
     */    
    public void fatal(String msg, String componentName,
            String deploymentName,String serverType,
            String componentType,int componentState,
            String eventType,String messageCode);


    /** 
	 * send out an alert with severity set to critical
	 * @param msg	the alert message
     * @param componentName     name of component 
     *                          ex. sun-bpel-engine, sun-http-binding
     * @param deploymentName   The Service Unit name can be passed in if 
     *                         this corresponds to a deployment. Can have
     *                         null value.
     * @param serverType       the server type hosting the component. Can have
     *                         null value. ex. Glassfish. 
     * @param componentType    component type Can have null value.
     *                         ex. ServiceEngine ot BindingComponent
     * @param componentState   the state of the component when this event
     *                         was generated. See NotificationEvent class
     *                         opeartional state constants for possible values.
     * @param eventType        the event type See NotificationEvent class
     *                         opeartional state constants for possible values.
     * @param messageCode      code assocaited with this event message.
     *                         Can have null value.
     */    
    public void critical(String msg,String componentName,
            String deploymentName,String serverType,
            String componentType,int componentState,String eventType,
            String messageCode);
   
    /** 
	 * send out an alert with severity set to major
	 * @param msg	the alert message
     * @param componentName     name of component 
     *                          ex. sun-bpel-engine, sun-http-binding
     * @param deploymentName   The Service Unit name can be passed in if 
     *                         this corresponds to a deployment. Can have
     *                         null value.
     * @param serverType       the server type hosting the component. Can have
     *                         null value. ex. Glassfish. 
     * @param componentType    component type Can have null value.
     *                         ex. ServiceEngine ot BindingComponent
     * @param componentState   the state of the component when this event
     *                         was generated. See NotificationEvent class
     *                         opeartional state constants for possible values.
     * @param eventType        the event type See NotificationEvent class
     *                         opeartional state constants for possible values.
      * @param messageCode      code assocaited with this event message.
     *                         Can have null value.
    */    
    public void major(String msg,String componentName,
            String deploymentName,String serverType,
            String componentType,int componentState,String eventType,
            String messageCode);                  

    /** 
	 * send out an alert with severity set to minor
	 * @param msg	the alert message
     * @param componentName     name of component 
     *                          ex. sun-bpel-engine, sun-http-binding
     * @param deploymentName   The Service Unit name can be passed in if 
     *                         this corresponds to a deployment. Can have
     *                         null value.
     * @param serverType       the server type hosting the component. Can have
     *                         null value. ex. Glassfish. 
     * @param componentType    component type Can have null value.
     *                         ex. ServiceEngine ot BindingComponent
     * @param componentState   the state of the component when this event
     *                         was generated. See NotificationEvent class
     *                         opeartional state constants for possible values.
     * @param eventType        the event type See NotificationEvent class
     *                         opeartional state constants for possible values.
     * @param messageCode      code assocaited with this event message.
     *                         Can have null value.
	 */    
    public void minor(String msg,String componentName,
            String deploymentName,String serverType,
            String componentType,int componentState,String eventType,
            String messageCode);                      

    /** 
	 * send out an alert with severity set to warning
	 * @param msg	the alert message
     * @param componentName     name of component 
     *                          ex. sun-bpel-engine, sun-http-binding
     * @param deploymentName   The Service Unit name can be passed in if 
     *                         this corresponds to a deployment. Can have
     *                         null value.
     * @param serverType       the server type hosting the component. Can have
     *                         null value. ex. Glassfish. 
     * @param componentType    component type Can have null value.
     *                         ex. ServiceEngine ot BindingComponent
     * @param componentState   the state of the component when this event
     *                         was generated. See NotificationEvent class
     *                         opeartional state constants for possible values.
     * @param eventType        the event type See NotificationEvent class
     *                         opeartional state constants for possible values.
     * @param messageCode      code assocaited with this event message.
     *                         Can have null value.
	 */    
    public void warning(String msg,String componentName,
            String deploymentName,String serverType,
            String componentType,int componentState,String eventType,
            String messageCode);                       

    /** 
	 * send out an alert with severity set to info
	 * @param msg	the alert message
     * @param componentName     name of component 
     *                          ex. sun-bpel-engine, sun-http-binding
     * @param deploymentName   The Service Unit name can be passed in if 
     *                         this corresponds to a deployment. Can have
     *                         null value.
     * @param serverType       the server type hosting the component. Can have
     *                         null value. ex. Glassfish. 
     * @param componentType    component type Can have null value.
     *                         ex. ServiceEngine ot BindingComponent
     * @param componentState   the state of the component when this event
     *                         was generated. See NotificationEvent class
     *                         opeartional state constants for possible values.
     * @param eventType        the event type See NotificationEvent class
     *                         opeartional state constants for possible values.
     * @param messageCode      code assocaited with this event message.
     *                         Can have null value.
	 */    
    public void info(String msg,String componentName,
            String deploymentName,String serverType,
            String componentType,int componentState,String eventType,
            String messageCode);                       


    /** 
     * send out an alert with severity set to info
     * @param eventType	    the customer alert type matching the one defined 
     *                  in the enterprise alert subsystem.
     * @param severity	the alert severity. See NotificationEvent class
     * 
     * @param msg	    the alert message
     * @param componentName     name of component 
     *                          ex. sun-bpel-engine, sun-http-binding
     * @param deploymentName   The Service Unit name can be passed in if 
     *                         this corresponds to a deployment. Can have
     *                         null value.
     * @param serverType       the server type hosting the component. Can have
     *                         null value. ex. Glassfish. 
     * @param componentType    component type Can have null value.
     *                         ex. ServiceEngine ot BindingComponent
     * @param componentState   the state of the component when this event
     *                         was generated. See NotificationEvent class
     *                         opeartional state constants for possible values.
     * @param eventType        the event type See NotificationEvent class
     *                         opeartional state constants for possible values.
     * @param messageCode      code assocaited with this event message.
     *                         Can have null value.
	 */    
    public void custom(String eventType,int severity,String msg,
            String componentName, String deploymentName,
            String serverType, String componentType,int componentState,
            String messageCode);                      



}
