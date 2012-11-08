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
 */

package com.sun.jbi.component.toolkit.util.alerter.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.management.InstanceNotFoundException;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.RuntimeMBeanException;
import javax.management.openmbean.CompositeData;

import com.sun.jbi.component.toolkit.util.I18n;
import com.sun.jbi.component.toolkit.util.alerter.Alerter;
import com.sun.jbi.component.toolkit.util.alerter.EventDataConverter;
import com.sun.jbi.component.toolkit.util.alerter.EventFactory;
import com.sun.jbi.component.toolkit.util.alerter.NotificationEvent;
import com.sun.jbi.component.toolkit.util.alerter.Event.ComponentType;
import com.sun.jbi.component.toolkit.util.alerter.NotificationEvent.OperationalState;
import com.sun.jbi.component.toolkit.util.alerter.NotificationEvent.Severity;

/**
 * Default implementation of the {@link Alerter} interface.
 * @author Kevan Simpson
 */
public class AlerterImpl implements Alerter {
    private static Logger mLogger = Logger.getLogger(Alerter.class.getName());
    
    /* Copied from componentsl EventProperties interface. */
    private static final String EVENT_FORWARD_OPERATION = "forwardEvent";    

	private MBeanServer mMBeanServer = null;    
	private ObjectName mObjectName = null;
	private EventFactory mFactory = new EventFactoryImpl();
	
	public AlerterImpl(MBeanServer server, ObjectName objectName) {
		mMBeanServer = server;
		mObjectName = objectName;
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Alerter#critical(java.lang.String, java.lang.String, java.lang.String, java.lang.String, com.sun.jbi.component.toolkit.util.alerter.Event.ComponentType, com.sun.jbi.component.toolkit.util.alerter.NotificationEvent.OperationalState, java.lang.String) */
	public void critical(String msg, String componentName, String deploymentName, 
						 String serverType, ComponentType componentType, 
						 OperationalState state, String messageCode) {
		custom(Severity.CRITICAL, msg, componentName, deploymentName, serverType,
			   componentType, state, messageCode);
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Alerter#custom(com.sun.jbi.component.toolkit.util.alerter.NotificationEvent.Severity, java.lang.String, java.lang.String, java.lang.String, java.lang.String, com.sun.jbi.component.toolkit.util.alerter.Event.ComponentType, com.sun.jbi.component.toolkit.util.alerter.NotificationEvent.OperationalState, java.lang.String) */
	public void custom(Severity severity, String msg, String componentName,
					   String deploymentName, String serverType,
					   ComponentType componentType, OperationalState state,
					   String messageCode) {
        if (mMBeanServer == null) {
            // in case of junit tests mbean server is not available
        	if (mLogger.isLoggable(Level.FINEST)) {
        		mLogger.finest("COMPTK-1001: MBeanServer is not initialized, skipping alert!");
        	}
            return;
        }
        
        if (!mMBeanServer.isRegistered(mObjectName)) {
            // in case  the server installation does not installed eventmanagement.rar
        	if (mLogger.isLoggable(Level.FINEST)) {
        		mLogger.finest("COMPTK-1002: Alert MBean is not registered, skipping alert!");
        	}
            return;
        }

        try {
        	NotificationEvent event = mFactory.createNotificationEvent();
        	event.setMessage(msg);
        	event.setSeverity(severity);
        	event.setComponentName(componentName);
        	event.setDeploymentName(deploymentName);
        	event.setServerType((serverType == null) ? "Glassfish" : serverType);
        	event.setComponentType(componentType);
        	event.setOperationalState(state);
        	// try to parse msg code if colon is present...
        	if (messageCode == null && msg != null) {
        		int ix = msg.indexOf(":");
        		if (ix > 0) {
        			event.setMessageCode(msg.substring(0, ix));
        		}
        	}
        	else {
        		event.setMessageCode(messageCode);
        	}
        	
            EventDataConverter converter = mFactory.createDataConverter();
            CompositeData eventData = converter.convertEvent(event);
            mMBeanServer.invoke(mObjectName,
                    			EVENT_FORWARD_OPERATION,
                    			new Object[] { eventData },
                    			new String[] { "javax.management.openmbean.CompositeData" });
        } 
        catch (InstanceNotFoundException e) {
        	// The Event Management mbean {0} not found. Please make sure eventmanagement rar is installed and started.
        	mLogger.warning(I18n.loc(
        			"COMPTK-6025: Event Management mbean \"{0}\" not found ({1})! "+
        			"Please verify appropriate RAR is installed and started.", 
        			String.valueOf(mObjectName), e.getMessage()));
        } 
        catch(Exception e) {
            String err = e.getMessage();
            // find actual thrown exception
            if (e instanceof RuntimeMBeanException) {
                RuntimeMBeanException rme = (RuntimeMBeanException) e;
                if (rme.getTargetException() != null) {
                    err = rme.getTargetException().getMessage();
                }
            }
        	mLogger.warning(I18n.loc(
        			"COMPTK-6026: Sending {0} Alert failed: {1}", 
        			String.valueOf(severity), err));
        	if (mLogger.isLoggable(Level.FINEST)) {
        		mLogger.finest("COMPTK-1003: Failed Alert message: "+ messageCode +" - "+ msg);
        	}
        }       
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Alerter#fatal(java.lang.String, java.lang.String, java.lang.String, java.lang.String, com.sun.jbi.component.toolkit.util.alerter.Event.ComponentType, com.sun.jbi.component.toolkit.util.alerter.NotificationEvent.OperationalState, java.lang.String) */
	public void fatal(String msg, String componentName, String deploymentName,
					  String serverType, ComponentType componentType,
					  OperationalState state, String messageCode) {
		custom(Severity.FATAL, msg, componentName, deploymentName, serverType,
			   componentType, state, messageCode);
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Alerter#info(java.lang.String, java.lang.String, java.lang.String, java.lang.String, com.sun.jbi.component.toolkit.util.alerter.Event.ComponentType, com.sun.jbi.component.toolkit.util.alerter.NotificationEvent.OperationalState, java.lang.String) */
	public void info(String msg, String componentName, String deploymentName,
					 String serverType, ComponentType componentType,
					 OperationalState state, String messageCode) {
		custom(Severity.INFO, msg, componentName, deploymentName, serverType,
			   componentType, state, messageCode);
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Alerter#major(java.lang.String, java.lang.String, java.lang.String, java.lang.String, com.sun.jbi.component.toolkit.util.alerter.Event.ComponentType, com.sun.jbi.component.toolkit.util.alerter.NotificationEvent.OperationalState, java.lang.String) */
	public void major(String msg, String componentName, String deploymentName,
					  String serverType, ComponentType componentType,
					  OperationalState state, String messageCode) {
		custom(Severity.MAJOR, msg, componentName, deploymentName, serverType,
			   componentType, state, messageCode);
	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Alerter#minor(java.lang.String, java.lang.String, java.lang.String, java.lang.String, com.sun.jbi.component.toolkit.util.alerter.Event.ComponentType, com.sun.jbi.component.toolkit.util.alerter.NotificationEvent.OperationalState, java.lang.String) */
	public void minor(String msg, String componentName, String deploymentName,
					  String serverType, ComponentType componentType,
					  OperationalState state, String messageCode) {
		custom(Severity.MINOR, msg, componentName, deploymentName, serverType,
			   componentType, state, messageCode);

	}

	/** @see com.sun.jbi.component.toolkit.util.alerter.Alerter#warning(java.lang.String, java.lang.String, java.lang.String, java.lang.String, com.sun.jbi.component.toolkit.util.alerter.Event.ComponentType, com.sun.jbi.component.toolkit.util.alerter.NotificationEvent.OperationalState, java.lang.String) */
	public void warning(String msg, String componentName, String deploymentName, 
						String serverType, ComponentType componentType, 
						OperationalState state, String messageCode) {
		custom(Severity.WARNING, msg, componentName, deploymentName, serverType,
			   componentType, state, messageCode);
	}
}
