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
 * @(#)JMSBCRuntimeConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.mbeans;

import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.management.AttributeChangeNotification;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanNotificationInfo;
import javax.management.Notification;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;

import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.internationalization.Messages;

/**
 * Runtime configuration MBean, allow configuration to be changed at run-time
 */
public class JMSBCRuntimeConfiguration extends RuntimeConfiguration implements JMSBCRuntimeConfigurationMBean, NotificationEmitter {

    private static final Messages mMessages =
        Messages.getMessages(JMSBCRuntimeConfiguration.class);
    private static final Logger mLogger =
        Messages.getLogger(JMSBCRuntimeConfiguration.class);

    // Attribute names
    public static final String CONFIG_THREADS = "Threads";
    public static final String ENVIRONMENT_VARIABLES= "EnvironmentVariables";
    
    // Default values in the absence of configuration settings
    private static final String DEFAULT_THREADS = "16";
    private static final String DEFAULT_MAX_CONCURRENT_CONSUMERS = "-1";

    // Configuration validation settings    
    private static long MIN_THREADS = 1;
    private static long MAX_THREADS = 10000;
    private static int MIN_MAX_CONCURRENT_CONSUMERS = -1;
    private static int MAX_MAX_CONCURRENT_CONSUMERS = 10000;
    
    // Configuration 
    Properties mConfig;
    String mWorkspaceRoot;

    // Use delegation to support notification
    NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();    
    
    public JMSBCRuntimeConfiguration(String workspaceRoot, KeyStoreUtilClient keystoreUtilClient) throws JBIException {
    	super(workspaceRoot, keystoreUtilClient);
        mWorkspaceRoot = workspaceRoot;        
        // Load the persisted configuration
        mConfig = ConfigPersistence.loadConfig(mWorkspaceRoot);
    }
    
    public Integer getThreads() {
        String val = mConfig.getProperty(CONFIG_THREADS, DEFAULT_THREADS);
        return Integer.valueOf(val);
    }
    
    public void setThreads(Integer val) throws InvalidAttributeValueException, MBeanException {
        String attrName = CONFIG_THREADS;
        
        // Validate the attribute value
        Integer newVal = val;
        if (newVal.intValue() < MIN_THREADS || newVal.intValue() > MAX_THREADS) {
            String errMsg = mMessages.getString("JMSBC-E0301.ValueNotWithinRange", 
                        new Object[]{newVal,
                                     attrName,
                                     new Long(MIN_THREADS),
                                     new Long(MAX_THREADS)});                        
            throw new InvalidAttributeValueException(errMsg);
        }

        Integer oldVal = getThreads();
        
        // Apply and save the changes
        mConfig.put(CONFIG_THREADS, val.toString());
        persistConfiguration();
        
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "JMSBC-C0301.AttributeChangedDetail",
                    new Object[] { CONFIG_THREADS, oldVal.toString(), newVal.toString() });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = mMessages.getString("JMSBC-C0302.AttributeChanged");
        String attrType = Integer.class.getName();
        Notification notif = 
                new AttributeChangeNotification(this, 
                                                seqNo, 
                                                System.currentTimeMillis(), 
                                                msg, 
                                                attrName, 
                                                attrType, 
                                                oldVal, 
                                                newVal);
        broadcasterSupport.sendNotification(notif);
    }

    void persistConfiguration() throws MBeanException {
        // Persist the changed configuration        
        try {
            ConfigPersistence.persistConfig(mWorkspaceRoot, mConfig);
        } catch (JBIException ex) {
            String errMsg = mMessages.getString("JMSBC-E0302.ConfigPersistFailed",
                        new Object[]{mWorkspaceRoot});            
            throw new MBeanException(ex, errMsg);
        }
    }
    
    // Support notifying about config changes
    public MBeanNotificationInfo[] getNotificationInfo(){
        return new MBeanNotificationInfo[] {new MBeanNotificationInfo(new String[] {AttributeChangeNotification.ATTRIBUTE_CHANGE}, AttributeChangeNotification.class.getName(), "Attribute changed")};
    }

    public void addNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback){
        broadcasterSupport.addNotificationListener(listener, filter, handback);
    }

    public void removeNotificationListener(NotificationListener listener) throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener);
    }

    public void removeNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback) throws ListenerNotFoundException{
        broadcasterSupport.removeNotificationListener(listener, filter, handback);
    }
    
    public void dump(StringBuffer msgBuf) {
    	msgBuf.append("JMSBC Configuration").append(": { Threads=");
    	msgBuf.append(mConfig.getProperty(CONFIG_THREADS, DEFAULT_THREADS));
        msgBuf.append(", DefaultRedeliveryHandling=");
        msgBuf.append(mConfig.getProperty(CONFIG_DEFAULT_REDELIVERY, ""));
        msgBuf.append(" }\n");
        super.dump(msgBuf);
    }

	@Override
	protected String[] getApplicationConfigRowNames() {
		return APPLICATION_CONFIG_ROW_NAMES;
	}

	@Override
	protected Object[] encryptIfRrequired(Object[] objs, boolean base64){
		Object[] copy = new Object[objs.length];
		System.arraycopy(objs, 0, copy, 0, objs.length);
		copy[7] = getEncryptedValue((String)copy[7], base64); //Password   
		copy[5] = getEncryptedValue((String)copy[5], base64); //Security Credentials  
		return copy;
	}

	private String getEncryptedValue(String str, boolean base64) {
		if(str == null){
			str = "";
		}else{
			str = str.trim();
			if(str.length() != 0){
				if(base64){
					str = encrypt(str);
				}else{
					str = "*****";
				}
			}
		}
		return str;
	}

	@Override
	protected Object[] decryptIfRrequired(Object[] objs){
		Object[] copy = new Object[objs.length];
		System.arraycopy(objs, 0, copy, 0, objs.length);
		copy[7] = decrypt((String)copy[7]); //Password   
		copy[5] = decrypt((String)copy[5]); //Security Credentials  
		return copy;
	}
	
	@Override
	protected String[] getApplicationConfigRowDesc() {
		return APPLICATION_CONFIG_ROW_DESCS;
	}

	@Override
	protected OpenType[] getApplicationConfigRowTypes() {
		return APPLICATION_CONFIG_ROW_TYPES;
	}

	@Override
	protected void validateApplicationConfigData(Object[] objs)
			throws InvalidAttributeValueException {
	}

    public String getDefaultRedeliveryHandling() {
        String val = mConfig.getProperty(CONFIG_DEFAULT_REDELIVERY, "");
        return val;
    }

    public void setDefaultRedeliveryHandling(String val) throws InvalidAttributeValueException, MBeanException {
        String attrName = CONFIG_DEFAULT_REDELIVERY;

        String oldVal = getDefaultRedeliveryHandling();
        
        // Apply and save the changes
        mConfig.put(CONFIG_DEFAULT_REDELIVERY, val);
        persistConfiguration();
        
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "JMSBC-C0301.AttributeChangedDetail",
                    new Object[] { CONFIG_DEFAULT_REDELIVERY, oldVal, val });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = mMessages.getString("JMSBC-C0302.AttributeChanged");
        String attrType = Integer.class.getName();
        Notification notif = 
                new AttributeChangeNotification(this, 
                                                seqNo, 
                                                System.currentTimeMillis(), 
                                                msg, 
                                                attrName, 
                                                attrType, 
                                                oldVal, 
                                                val);
        broadcasterSupport.sendNotification(notif);
        }

    public String getForceConcurrencyMode() {
        String val = mConfig.getProperty(CONFIG_FORCE_CONCURRENCY_MODE, "");
        return val;
    }

    public void setForceConcurrencyMode(String val) throws InvalidAttributeValueException, MBeanException {
        String attrName = CONFIG_FORCE_CONCURRENCY_MODE;

        String oldVal = getForceConcurrencyMode();
        
        if (!"cc".equals(val) && !"sync".equals(val) && !"serial".equals(val)) {
            String errMsg = mMessages.getString("JMSBC-E0336.ValueNotValid", 
                        new Object[]{val,
                                     attrName,
                                     "'cc', 'sync', 'serial'"});                        
            throw new InvalidAttributeValueException(errMsg);
        }
        
        
        // Apply and save the changes
        mConfig.put(attrName, val);
        persistConfiguration();
        
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "JMSBC-C0301.AttributeChangedDetail",
                    new Object[] { attrName, oldVal, val });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = mMessages.getString("JMSBC-C0302.AttributeChanged");
        String attrType = Integer.class.getName();
        Notification notif = 
                new AttributeChangeNotification(this, 
                                                seqNo, 
                                                System.currentTimeMillis(), 
                                                msg, 
                                                attrName, 
                                                attrType, 
                                                oldVal, 
                                                val);
        broadcasterSupport.sendNotification(notif);
    }

    public Integer getForceMaxConcurrentConsumers() {
        String val = mConfig.getProperty(CONFIG_FORCE_MAX_CONCURRENT_CONSUMERS, DEFAULT_MAX_CONCURRENT_CONSUMERS.toString());
        try {
            int retVal = Integer.parseInt(val);
            if (retVal <= 0) {
                retVal = -1;
            }
            return retVal;
        } catch (NumberFormatException e) {
            return -1;
        }
    }

    public void setForceMaxConcurrentConsumers(Integer newVal) throws InvalidAttributeValueException, MBeanException {
        String attrName = CONFIG_FORCE_MAX_CONCURRENT_CONSUMERS;

        String oldVal = getForceConcurrencyMode();
        
        if (newVal.intValue() < MIN_THREADS || newVal.intValue() > MAX_THREADS) {
            String errMsg = mMessages.getString("JMSBC-E0301.ValueNotWithinRange", 
                        new Object[]{newVal,
                                     attrName,
                                     new Long(MIN_MAX_CONCURRENT_CONSUMERS),
                                     new Long(MAX_MAX_CONCURRENT_CONSUMERS)});                        
            throw new InvalidAttributeValueException(errMsg);
        }

        // 0 doesn't make sense
        if (newVal == 0) {
            newVal = -1;
        }
        
        // Apply and save the changes
        mConfig.put(attrName, newVal.toString());
        persistConfiguration();
        
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "JMSBC-C0301.AttributeChangedDetail",
                    new Object[] { attrName, oldVal, newVal });
        }
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = mMessages.getString("JMSBC-C0302.AttributeChanged");
        String attrType = Integer.class.getName();
        Notification notif = 
                new AttributeChangeNotification(this, 
                                                seqNo, 
                                                System.currentTimeMillis(), 
                                                msg, 
                                                attrName, 
                                                attrType, 
                                                oldVal, 
                                                newVal);
        broadcasterSupport.sendNotification(notif);
    }
}
