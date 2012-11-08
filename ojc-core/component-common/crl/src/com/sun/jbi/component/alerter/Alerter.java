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

package com.sun.jbi.component.alerter;

import com.sun.jbi.component.alerter.Event.ComponentType;
import com.sun.jbi.component.alerter.NotificationEvent.OperationalState;
import com.sun.jbi.component.alerter.NotificationEvent.Severity;

/**
 * Defines the interface for a component alert notification utility. 
 * @author Kevan Simpson
 */
public interface Alerter {
    /* No EventType because only Alerts are sent. */

	/**
	 * Send out an Alert with {@link Severity#FATAL}.
	 * 
	 * @param msg The alert message.
	 * @param componentName The name of the component sending the alert.
	 * @param deploymentName The name of the service unit, if applicable, or <code>null</code>.
	 * @param serverType The server type (e.g. Glassfish) or <code>null</code>.
	 * @param componentType The {@link ComponentType} of the component sending the alert.
	 * @param state The {@link OperationalState} of the component when alert is sent.
	 * @param messageCode A unique message identifier code or <code>null</code>.
	 */
    public void fatal(String msg, String componentName, 
    				  String deploymentName, String serverType, 
    				  ComponentType componentType, 
    				  OperationalState state,
    				  String messageCode);
	
	/**
	 * Send out an Alert with {@link Severity#CRITICAL}.
	 * 
	 * @param msg The alert message.
	 * @param componentName The name of the component sending the alert.
	 * @param deploymentName The name of the service unit, if applicable, or <code>null</code>.
	 * @param serverType The server type (e.g. Glassfish) or <code>null</code>.
	 * @param componentType The {@link ComponentType} of the component sending the alert.
	 * @param state The {@link OperationalState} of the component when alert is sent.
	 * @param messageCode A unique message identifier code or <code>null</code>.
	 */
    public void critical(String msg, String componentName, 
    				  	 String deploymentName, String serverType, 
    				  	 ComponentType componentType, 
    				  	 OperationalState state,
    				  	 String messageCode);
    
	/**
	 * Send out an Alert with {@link Severity#MAJOR}.
	 * 
	 * @param msg The alert message.
	 * @param componentName The name of the component sending the alert.
	 * @param deploymentName The name of the service unit, if applicable, or <code>null</code>.
	 * @param serverType The server type (e.g. Glassfish) or <code>null</code>.
	 * @param componentType The {@link ComponentType} of the component sending the alert.
	 * @param state The {@link OperationalState} of the component when alert is sent.
	 * @param messageCode A unique message identifier code or <code>null</code>.
	 */
    public void major(String msg, String componentName, 
    				  String deploymentName, String serverType, 
    				  ComponentType componentType, 
    				  OperationalState state,
    				  String messageCode);
    
	/**
	 * Send out an Alert with {@link Severity#MINOR}.
	 * 
	 * @param msg The alert message.
	 * @param componentName The name of the component sending the alert.
	 * @param deploymentName The name of the service unit, if applicable, or <code>null</code>.
	 * @param serverType The server type (e.g. Glassfish) or <code>null</code>.
	 * @param componentType The {@link ComponentType} of the component sending the alert.
	 * @param state The {@link OperationalState} of the component when alert is sent.
	 * @param messageCode A unique message identifier code or <code>null</code>.
	 */
    public void minor(String msg, String componentName, 
    				  String deploymentName, String serverType, 
    				  ComponentType componentType, 
    				  OperationalState state,
    				  String messageCode);
    
	/**
	 * Send out an Alert with {@link Severity#WARNING}.
	 * 
	 * @param msg The alert message.
	 * @param componentName The name of the component sending the alert.
	 * @param deploymentName The name of the service unit, if applicable, or <code>null</code>.
	 * @param serverType The server type (e.g. Glassfish) or <code>null</code>.
	 * @param componentType The {@link ComponentType} of the component sending the alert.
	 * @param state The {@link OperationalState} of the component when alert is sent.
	 * @param messageCode A unique message identifier code or <code>null</code>.
	 */
    public void warning(String msg, String componentName, 
    				    String deploymentName, String serverType, 
    				    ComponentType componentType, 
    				    OperationalState state,
    				    String messageCode);
    
	/**
	 * Send out an Alert with {@link Severity#INFO}.
	 * 
	 * @param msg The alert message.
	 * @param componentName The name of the component sending the alert.
	 * @param deploymentName The name of the service unit, if applicable, or <code>null</code>.
	 * @param serverType The server type (e.g. Glassfish) or <code>null</code>.
	 * @param componentType The {@link ComponentType} of the component sending the alert.
	 * @param state The {@link OperationalState} of the component when alert is sent.
	 * @param messageCode A unique message identifier code or <code>null</code>.
	 */
    public void info(String msg, String componentName, 
    				 String deploymentName, String serverType, 
    				 ComponentType componentType, 
    				 OperationalState state,
    				 String messageCode);
    
	/**
	 * Send out a custom Alert.
	 * 
	 * @param severity The {@link Severity} of the alert.
	 * @param msg The alert message.
	 * @param componentName The name of the component sending the alert.
	 * @param deploymentName The name of the service unit, if applicable, or <code>null</code>.
	 * @param serverType The server type (e.g. Glassfish) or <code>null</code>.
	 * @param componentType The {@link ComponentType} of the component sending the alert.
	 * @param state The {@link OperationalState} of the component when alert is sent.
	 * @param messageCode A unique message identifier code or <code>null</code>.
	 */
    public void custom(Severity severity,
    				   String msg, String componentName, 
    				   String deploymentName, String serverType, 
    				   ComponentType componentType, 
    				   OperationalState state,
    				   String messageCode);
}
