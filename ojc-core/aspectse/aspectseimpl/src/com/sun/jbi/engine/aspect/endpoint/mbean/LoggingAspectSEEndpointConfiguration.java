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
 * @(#)LoggingAspectSEEndpointConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.mbean;

import com.sun.jbi.engine.aspect.endpoint.handler.AspectConstants;

import javax.management.AttributeChangeNotification;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;

/**
 * 
 * @author Manish
 */
public class LoggingAspectSEEndpointConfiguration extends AspectSEEndpointConfiguration implements LoggingAspectSEEndpointConfigurationMbean {

	private String loglevel = null, logFileName = null, rotationPolicy = null;

	/**
	 * 
	 * @param level 
	 * @param log_file_name 
	 * @param rotationPolicy 
         * @throws javax.management.NotCompliantMBeanException 
	 */
	public LoggingAspectSEEndpointConfiguration(String level, String log_file_name, String rotationPolicy)throws NotCompliantMBeanException
        {
		super(LoggingAspectSEEndpointConfigurationMbean.class);
		this.loglevel = level;
                this.logFileName = log_file_name;
                this.rotationPolicy = rotationPolicy;
	}

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * 
	 * @return 
	 * @throws javax.management.InvalidAttributeValueException 
	 * @throws javax.management.MBeanException 
	 */
	public String getVerbosity() throws InvalidAttributeValueException,
			MBeanException {
		// TODO Auto-generated method stub
		return this.loglevel;
	}

	/**
	 * 
	 * @param levelStr 
	 * @throws javax.management.InvalidAttributeValueException 
	 * @throws javax.management.MBeanException 
	 */
	public void setVerbosity(String levelStr)
			throws InvalidAttributeValueException, MBeanException {

		String newValue = levelStr;
		String oldValue = this.getVerbosity();

		try {
			this.loglevel = levelStr;
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		// Notify listeners of this change
		long sequenceNumber = 0L;
		String msg = "LoggingAspectSEEndpoint.LoggingLevel_Attribute_changed";
		Notification notification = new AttributeChangeNotification(this,
				sequenceNumber, System.currentTimeMillis(), msg, AspectConstants.LOG_TAG_VERBOSITY,
				String.class.getName(), oldValue, newValue);
		notificationBroadcaster.sendNotification(notification);
	}
        
        
	/**
	 * 
	 * @return 
	 * @throws javax.management.InvalidAttributeValueException 
	 * @throws javax.management.MBeanException 
	 */
	public String getLogFileName() throws InvalidAttributeValueException, MBeanException
        {
		return this.logFileName;
	}

	/**
	 * 
	 * @param logFileName 
	 * @throws javax.management.InvalidAttributeValueException 
	 * @throws javax.management.MBeanException 
	 */
	public void setLogFileName(String loggingFileName) throws InvalidAttributeValueException, MBeanException
        {

		String newValue = loggingFileName;
		String oldValue = this.getLogFileName();

		try {
			this.logFileName = loggingFileName;
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		// Notify listeners of this change
		long sequenceNumber = 0L;
		String msg = "LoggingAspectSEEndpoint.LoggingFileName_Attribute_changed";
		Notification notification = new AttributeChangeNotification(this,
				sequenceNumber, System.currentTimeMillis(), msg, AspectConstants.LOG_TAG_FILE,
				String.class.getName(), oldValue, newValue);
		notificationBroadcaster.sendNotification(notification);
	}
        
	/**
	 * 
	 * @return 
	 * @throws javax.management.InvalidAttributeValueException 
	 * @throws javax.management.MBeanException 
	 */
	public String getRotationPolicy() throws InvalidAttributeValueException, MBeanException
        {
		return this.rotationPolicy;
	}

	public void setRotationPolicy(String rotationPolicy) throws InvalidAttributeValueException, MBeanException
        {

		String newValue = rotationPolicy;
		String oldValue = this.getRotationPolicy();

		try {
			this.rotationPolicy = rotationPolicy;
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		// Notify listeners of this change
		long sequenceNumber = 0L;
		String msg = "LoggingAspectSEEndpoint.LoggingRotationPolicy_Attribute_changed";
		Notification notification = new AttributeChangeNotification(this,
				sequenceNumber, System.currentTimeMillis(), msg, AspectConstants.LOG_TAG_ROTATIONPOLICY,
				String.class.getName(), oldValue, newValue);
		notificationBroadcaster.sendNotification(notification);
	}

}
