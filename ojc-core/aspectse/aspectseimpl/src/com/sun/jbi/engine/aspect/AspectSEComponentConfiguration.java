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
 * @(#)AspectSEComponentConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.util.Properties;
import java.util.logging.Logger;

import javax.management.AttributeChangeNotification;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanNotificationInfo;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.StandardMBean;


/**
 * @author Sujit Biswas
 *
 */
public class AspectSEComponentConfiguration extends StandardMBean implements
        AspectSEComponentConfigurationMBean, NotificationEmitter, Serializable {
    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private static Logger logger = Logger.getLogger(AspectSEComponentConfiguration.class.getName());
	private static final String MAX_THREAD_COUNT = "MaxThreadCount";
	private static final Integer DEFAULT_MAX_THREAD_COUNT = Integer.valueOf(10);

	//
	NotificationBroadcasterSupport notificationBroadcaster = new NotificationBroadcasterSupport();
	String propFile;
	private Integer maxThreadCount = DEFAULT_MAX_THREAD_COUNT;
        private String mConfigurationDisplaySchema;
        private String mConfigurationDisplayData;

	
	public AspectSEComponentConfiguration(String propFile) throws NotCompliantMBeanException {
		super(AspectSEComponentConfigurationMBean.class);
		this.propFile = propFile;
		restore(this.propFile);
	}

	public AspectSEComponentConfiguration(String displaySchema, String displayData, String propFile) throws NotCompliantMBeanException {
		super(AspectSEComponentConfigurationMBean.class);
                mConfigurationDisplaySchema = displaySchema;
                mConfigurationDisplayData = displayData;
		this.propFile = propFile;
		restore(this.propFile);
	}

	public AspectSEComponentConfiguration() throws NotCompliantMBeanException{
		super(AspectSEComponentConfigurationMBean.class);
	}

	public String getMaxThreadCount() throws InvalidAttributeValueException, MBeanException {
		return maxThreadCount.toString();
	}

	public void setMaxThreadCount(String count) throws InvalidAttributeValueException,
			MBeanException {
		String newValue = count;
		String oldValue = getMaxThreadCount();

		try {
			maxThreadCount = Integer.valueOf(Integer.parseInt(newValue.trim()));
			if (maxThreadCount.intValue() < 1) {
				maxThreadCount = DEFAULT_MAX_THREAD_COUNT;
			}
		} catch (NumberFormatException e) {
			logger.info(e.getMessage());
			maxThreadCount = DEFAULT_MAX_THREAD_COUNT;
			e.printStackTrace();
		}

		save(propFile);

		long seqNo = 0;
		String msg = "AspectSEComponentConfiguration.Attribute_changed";
		String attrType = String.class.getName();
		Notification notif = new AttributeChangeNotification(this, seqNo, System
				.currentTimeMillis(), msg, MAX_THREAD_COUNT, attrType, oldValue, newValue);
		notificationBroadcaster.sendNotification(notif);

	}

    /**
     * Retrieves the Configuration Display Schema
     */
    public String retrieveConfigurationDisplaySchema() {
        return this.mConfigurationDisplaySchema;
    }
    
    /**
     * Retrieves the Configuration Display Data
     */
    public String retrieveConfigurationDisplayData() {
        return this.mConfigurationDisplayData;
	}

	public boolean restore(String propFile) {
		File fin = new File(propFile);

		try {
			FileInputStream fis = new FileInputStream(fin);
			Properties prop = new Properties();
			prop.load(fis);

			setMaxThreadCount(prop.getProperty(MAX_THREAD_COUNT));

	
			

		} catch (Exception ex) {
			logger.info(ex.getMessage());
			return false;
		}

		return true;
	}
	
	public void restore(Properties prop) {
		try {
			setMaxThreadCount(prop.getProperty(MAX_THREAD_COUNT));
		} catch (Exception ex) {
			logger.info(ex.getMessage());
		}
		
	}

	/**
	 * DOCUMENT ME!
	 * 
	 * @param propFile
	 *            DOCUMENT ME!
	 * 
	 * @return DOCUMENT ME!
	 */
	public boolean save(String propFile) {
		File fin = new File(propFile);
		Properties prop = new Properties();

		try {
			FileInputStream fis = new FileInputStream(fin);
			prop.load(fis);
		} catch (Exception ex) {
			logger.info(ex.getMessage());
			return false;
		}

		try {
			prop.setProperty(MAX_THREAD_COUNT, getMaxThreadCount());
		} catch (Exception e) {
			logger.info(e.getMessage());
			e.printStackTrace();
		}

		File fout = new File(propFile);

		try {
			FileOutputStream fos = new FileOutputStream(fout);
			prop.store(fos, "aspectse.properties");
		} catch (FileNotFoundException ex) {
			logger.info(ex.getMessage());
			return false;
		} catch (IOException ex) {
			logger.info(ex.getMessage());
			return false;
		}

		return true;
	}

	public void addNotificationListener(NotificationListener listener, NotificationFilter filter,
			Object handback) throws IllegalArgumentException {
		notificationBroadcaster.addNotificationListener(listener, filter, handback);

	}

	public MBeanNotificationInfo[] getNotificationInfo() {
		return new MBeanNotificationInfo[] { new MBeanNotificationInfo(
				new String[] { AttributeChangeNotification.ATTRIBUTE_CHANGE },
				AttributeChangeNotification.class.getName(),
				"AspectSEComponentConfiguration.Attribute_changed") };
	}

	public void removeNotificationListener(NotificationListener listener)
			throws ListenerNotFoundException {
		notificationBroadcaster.removeNotificationListener(listener);

	}

	public void removeNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback) throws ListenerNotFoundException {
		// TODO Auto-generated method stub
		
	}

	

	

}
