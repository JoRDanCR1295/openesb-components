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
 * @(#)AlerterImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */package com.sun.jbi.alerter;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.text.MessageFormat;
import java.util.Calendar;
import java.util.List;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import javax.management.MBeanException;
import javax.management.MalformedObjectNameException;
import javax.management.ReflectionException;
import javax.management.ObjectName;
import javax.management.MBeanServer;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanServerFactory;
import javax.management.openmbean.CompositeData;


import java.util.logging.Logger;
import javax.rmi.CORBA.Util;

/** 
 * The AlerterImpl class is implementation class of the Alert nterface. A client can then use this class to send out an alert to the 
 * event management framework. The alert is first routed to the CollabMBean and then out to the 
 * event management framework
 *
 * @author Yoke Lee
 * @version 1.0
 */
public class AlerterImpl implements Alerter {

    public final static String PROCESSING = "PROCESSING";

    public final static String STOPPED = "STOPPED";

    public final static String RUNNING = "RUNNING";

    public final static String STOPPING = "STOPPING";

    public final static String REMOVED = "REMOVED";

    public final static String SHUTTING_DOWN = "SHUTTINGDOWN";
    public final static String SHUTDOWN = "SHUTDOWN";
    
    public final static String UNKNOWN = "UNKNOWN";
    public final static String UP = "UP";
    public final static String CONNECTING = "CONNECTING";
    public final static String CONNECTED = "CONNECTED";
    public final static String DOWN = "DOWN";
    private String[] severityStrings = 
        {"FATAL","CRITICAL","MAJOR","MINOR","WARNING","INFO"};
    
    /**
    * the MBeanServer
    */
    private MBeanServer mMBeanServer = null;    

    /**
    * the ObjectName of the CollabMonitorMBean
    */
	private ObjectName mObjectName = null;

    private static Logger mLogger =
        Logger.getLogger(AlerterImpl.class.getName());

    private EventFactory eventFactory;
    private ResourceBundle mBundle;
    
    /**
     * @param  mbs		MBeanServer of the App Server
	 * @param  objName	ObjectName of the CollabMonitorMBean
     */
    public AlerterImpl() {
        loadBundle("com.sun.jbi.alerter");
        try {
            mObjectName = 
                    new javax.management.ObjectName(EventProperties.EVENTFORWARDER_MBEAN_NAME);
        } catch (MalformedObjectNameException ex) {
            Object[] args = new String[] {mObjectName.toString()};
            mLogger.severe(getMessage("Object.Name.is.Invalid",args));
        } catch (NullPointerException ex) {
           // will never happen EVENTFORWARDER_MBEAN_NAME is defined as
           // static  in EventProperties class.
        }
        List<MBeanServer> mbeanservers = 
                (List<MBeanServer>)MBeanServerFactory.findMBeanServer(null);
        
        if(mbeanservers.size() > 0) {
            // get default mbean server
            mMBeanServer = mbeanservers.get(0);
        }
   }

    
    /** 
     * send out an alert with severity set to critical
     * @param msg               the alert message
     * @param componentName     name of component
     */    
    public void fatal(String msg, String componentName,
            String deploymentName,String serverType,
            String componentType,int componentState,
            String eventType, String messageCode)  {                       
         sendAlert(msg,NotificationEvent.SEVERITY_TYPE_FATAL,componentName,
                 deploymentName,serverType,componentType,componentState,
                 eventType,messageCode);
    }
    
    /** 
	 * send out an alert with severity set to critical
	 * @param msg	the alert message
     */    
    public void critical(String msg,String componentName,
            String deploymentName,String serverType,
            String componentType,int componentState,
            String eventType,String messageCode) {                       
            sendAlert(msg,NotificationEvent.SEVERITY_TYPE_CRITICAL,
               componentName,deploymentName,serverType,componentType,
               componentState,eventType,messageCode);
	}

   
    /** 
	 * send out an alert with severity set to major
	 * @param msg	the alert message
     */    
    public void major(String msg,String componentName,
            String deploymentName,String serverType,
            String componentType,int componentState,
            String eventType, String messageCode) {                       
		 sendAlert(msg,NotificationEvent.SEVERITY_TYPE_MAJOR,
                 componentName, deploymentName,serverType,componentType,
                         componentState,eventType,messageCode);
	}

    /** 
	 * send out an alert with severity set to minor
	 * @param msg	the alert message
	 */    
    public void minor(String msg,String componentName,
            String deploymentName,String serverType,
            String componentType,int componentState,
            String eventType,String messageCode) {       
		 sendAlert(msg,NotificationEvent.SEVERITY_TYPE_MINOR,
                 componentName, deploymentName,serverType,componentType,
                         componentState,eventType,messageCode);
    }

    /** 
	 * send out an alert with severity set to warning
	 * @param msg	the alert message
	 */    
    public void warning(String msg,String componentName,
            String deploymentName,String serverType,
            String componentType,int componentState,
            String eventType,String messageCode) {                       
		 sendAlert(msg,NotificationEvent.SEVERITY_TYPE_WARNING,
                 componentName, deploymentName,serverType,componentType,
                         componentState,eventType,messageCode);
                 
    }

    /** 
	 * send out an alert with severity set to info
	 * @param msg	the alert message
	 */    
    public void info(String msg,String componentName,
            String deploymentName,String serverType,
            String componentType,int componentState,
            String eventType, String messageCode) {                       
		 sendAlert(msg,NotificationEvent.SEVERITY_TYPE_INFO,
                 componentName, deploymentName,serverType,componentType,
                 componentState,eventType,messageCode);
    }

    /** 
	 * send out an alert with severity set to info
	 * @param msg	the alert message
	 */    
    public void custom(String eventType,int severity,String msg,
            String componentName, String deploymentName,String serverType,
            String componentType,int componentState,
            String messageCode) {                       
		 sendCustomAlert(msg,severity,eventType,
                 componentName, deploymentName,serverType,
                 componentType, componentState,messageCode);
                         
    }

	/**
	 * send alert to CollabMonitorMBean
	 * @param msg		the alert message
	 * @param severity	alert severity
	 */
    public void sendAlert(String msg, int severity,
           String componentName, String deploymentName,
           String serverType,String componentType, int componentState,
           String eventType,String messageCode)  {

        if(mMBeanServer == null) {
            // in case of junit test mbean server is not avialable
//            mLogger.warning( getMessage("alert.not.sent.Mbean.server.not.found",null));
            return;
        }
        
        if(mMBeanServer.isRegistered(mObjectName) == false) {
            // in case  the server installation does not installed eventmanagement.rar
//            mLogger.warning( getMessage("event.management.Mbean.not.found",null));
            return;
        }

        try {
            NotificationEvent event = createEventObject(msg,
                                             severity,
                                             componentName,
                                             deploymentName, 
                                             serverType, 
                                             componentType,
                                             componentState,
                                             eventType,
                                             messageCode);
            EventDataConverter converter = new EventDataConverter();
            CompositeData eventData = converter.getEventCompositeData(event,"");
            String[] paramsSig = new String[] { "javax.management.openmbean.CompositeData"};
            Object[] params = new Object[] {eventData};

	    mMBeanServer.invoke(mObjectName,
                                EventProperties.EVENT_FORWARD_OPERATION,
                                params,paramsSig);

        } catch(InstanceNotFoundException e) {
            Object[] args = new String[] {mObjectName.toString()};
            mLogger.severe( getMessage("Mbean.Instance.not.found",args));
        } catch(Exception e) {
            Object[] args = new String[] { e.getMessage()};
            mLogger.severe(getMessage("Send.Alert.Failed",args));
        }       
    }
    

    public void sendCustomAlert(String msg, int severity, String eventType,
            String componentName, String deploymentName,String serverType,
            String componentType, int componentState, String messageCode) {
        
        if(mMBeanServer == null) {
            // in case of junit tests mbean server is not avialable
 //           mLogger.warning( getMessage("alert.not.sent.Mbean.server.not.found",null));
            return;
        }
        
        if(mMBeanServer.isRegistered(mObjectName) == false) {
            // in case  the server installation does not installed eventmanagement.rar
 //           mLogger.warning( getMessage("event.management.Mbean.not.found",null));
            return;
        }
        

        try {
            NotificationEvent event = createEventObject(
                        msg,
                        severity,
                        componentName,
                        deploymentName,
                        serverType,
                        componentType,
                        componentState,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        messageCode);
            EventDataConverter converter = new EventDataConverter();
            CompositeData eventData = converter.getEventCompositeData(event,"");
            String[] paramsSig = new String[] { "javax.management.openmbean.CompositeData"};
            Object[] params = new Object[] {eventData};
            mMBeanServer.invoke(mObjectName,
                    EventProperties.EVENT_FORWARD_OPERATION,params,paramsSig);
         } catch(InstanceNotFoundException e) {
            Object[] args = new String[] {mObjectName.toString()};
            mLogger.severe( getMessage("Mbean.Instance.not.found",args));
        } catch(Exception e) {
            Object[] args = new String[] { e.getMessage()};
            mLogger.severe(getMessage("Send.Custom.Alert.Failed",args));
        }       
    }



    private Integer getSeverityValue(String severity) throws Exception{
        int len = severityStrings.length;
        
        for (int lIndex = 0; lIndex < len; lIndex++) {
            if(severity.toUpperCase().equals(
                    severityStrings[lIndex])) {
                return new Integer(lIndex);
            }
        }
        return new Integer(NotificationEvent.SEVERITY_TYPE_INFO);
    }
    
    
    private String mapState(String state) {
        String mappedState = UNKNOWN;
        if ( RUNNING.equalsIgnoreCase(state) || UP.equalsIgnoreCase(state) ||
                        CONNECTING.equalsIgnoreCase(state) || PROCESSING.equalsIgnoreCase(state)) {
                mappedState = RUNNING;
        } else if ( STOPPED.equalsIgnoreCase(state) || STOPPING.equalsIgnoreCase(state) || 
                        DOWN.equalsIgnoreCase(state) ) {
                mappedState = STOPPED;
        } 
        return mappedState;
    }	

    private int mapOpState(String state) {
        int mappedState = NotificationEvent.OPERATIONAL_STATE_UNKNOWN;
        if ( RUNNING.equalsIgnoreCase(state) || UP.equalsIgnoreCase(state) ||
                        CONNECTING.equalsIgnoreCase(state) || PROCESSING.equalsIgnoreCase(state)) {
                mappedState = NotificationEvent.OPERATIONAL_STATE_RUNNING;
        } else if ( STOPPED.equalsIgnoreCase(state) || STOPPING.equalsIgnoreCase(state) || 
                        DOWN.equalsIgnoreCase(state) ) {
                mappedState = NotificationEvent.OPERATIONAL_STATE_STOPPED;
        } else if ( SHUTTING_DOWN.equalsIgnoreCase(state) || SHUTDOWN.equalsIgnoreCase(state)) {
                mappedState = NotificationEvent.OPERATIONAL_STATE_SHUTDOWN;
        }
        return mappedState;
    }
    
    
    private NotificationEvent createEventObject(String msg, int severity,
           String componentName, String deploymentName,
           String serverType,String componentType, int status,
           String eventType,String messageCode) {
        
        if (eventFactory == null) {
            eventFactory = new EventFactory();
        }
        
        String physicalHostName = getPhysicalHostName();
        NotificationEvent event = eventFactory.getNotificationEvent(
                            physicalHostName, // physical host name
                            deploymentName, // deployment name
                            null, // environment name
                            physicalHostName, // logical host name
                            serverType, // server type
                            physicalHostName, // server name
                            componentType, // component type
                            null, // component project
                            componentName, // component name
                            eventType, // type
                            severity, // severity
                            status,
                            messageCode,
                            new String[] { physicalHostName, null,
                                    null, null, null,
                                    null }, // message code
                            // args
                            msg // message details
                    );
        // set the alert time to current time
        event.setTimeStamp(Calendar.getInstance().getTime().getTime());
        return event;
        
    }

    private String getPhysicalHostName() {
        String hostName = null;
       try {
            hostName = InetAddress.getLocalHost().getCanonicalHostName();
        } catch (UnknownHostException e) {
        }
        if(hostName == null) {
            try {
                hostName = InetAddress.getLocalHost().getHostName();
            } catch (UnknownHostException e1) {
                e1.printStackTrace();
            }
        }
        if(hostName == null) {
            hostName = "127.0.0.1";
        }
        return hostName;
    }
    
      private void loadBundle(String packageName) {

        String bundleBaseName = packageName + ".bundle";
        ResourceBundle resBundle = null;
        try {
            resBundle = ResourceBundle.getBundle(bundleBaseName);
        } catch (MissingResourceException ex) {
            // Try with locale independent defaultBundle
            try {
                resBundle = ResourceBundle.getBundle(bundleBaseName,
                        new Locale(""));
            } catch (Exception anyEx) {
                mLogger.finer(anyEx.getMessage());
            }
        }

        if (resBundle != null) {
            this.mBundle = resBundle;
        }
    }
      
    private String getMessage(String aI18NKey, Object[] aArgs) {
        String i18nMessage = "";
        if(mBundle != null) {
            String msgCode = mBundle.getString(aI18NKey + ".ID");
            i18nMessage = mBundle.getString(aI18NKey);
            i18nMessage  = msgCode + " : " + i18nMessage;
        }
        if ((aArgs != null) && (i18nMessage != null) && (i18nMessage.length() > 0)) {
           try {
                MessageFormat mf = new MessageFormat(i18nMessage);
                String formattedI18NMsg = mf.format(aArgs);

                return formattedI18NMsg;
            } catch (Exception ex) {
            }
        } 
        return i18nMessage;
    }
  

}
