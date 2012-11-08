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
 * @(#)MQClientConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;

import java.math.BigDecimal;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.mqbc.messaging.MessageProperties;

/**
 */
public final class MQClientConfiguration implements Cloneable {

    /**
     * Event m_Logger
     */
     private static final Messages mMessages = Messages.getMessages( MQClientConfiguration.class );
     private static Logger m_logger = Messages.getLogger( MQClientConfiguration.class );
     
  
    /**
     * eWay + environment configuration parameters.
     */
    private Map<String, Object> m_config;
    
    /**
     * Create a blank MQClientConfiguration.
     */
    public MQClientConfiguration() {
        super();
        m_config = new TreeMap<String, Object>();
        
        m_logger.log(Level.FINE,
                mMessages.getString(
                     "DEBUG_NEW_OBJECT",
                     MQClientConfiguration.class.getName()));
    }


    public String getQueueName() {
        return getStringProperty("queue.name"); // no i18n
    }
    
    public void setQueueName(String val) {
       putStringProperty("queue.name",val); // no i18n   
    }

    public String getSecurityExit() {
        return getStringProperty("security.exit"); // no i18n
    }

    public String getSecurityExitPath() {
        return getStringProperty("security.exit.jar"); // no i18n
    }

    public String getCipherSuite() {
        return getStringProperty("ciphersuite"); // no i18n
    }
    
    public void setCipherSuite(String suite) {
        putStringProperty("ciphersuite", suite); // no i18n
    }
    
    public String getSslPeerName() {
        return getStringProperty("sslpeername"); // no i18n
    }
    
    public void setSslPeerName(String name) {
        putStringProperty("sslpeername", name);
    }

    public String getQueueManagerName() {
        return getStringProperty("queuemgr.name"); // no i18n
    }

    public void setQueueManagerName(String name) {
        putStringProperty("queuemgr.name", name);
    }

    public String getUser() {
        return getStringProperty("queuemgr.userid"); // no i18n
    }

    public void setUser(String name) {
        putStringProperty("queuemgr.userid", name);
    }

    public String getPassword() {
        return getStringProperty("queuemgr.password"); // no i18n
    }

    public void setPassword(String name) {
        putStringProperty("queuemgr.password", name);
    }

    public String getHost() {
        return getStringProperty("queuemgr.hostname"); // no i18n
    }

    public void setHost(String name) {
        putStringProperty("queuemgr.hostname", name); // no i18n
    }

    public void setPort(int port) {
        putIntProperty("queuemgr.port", port); // no i18n
    }

    public Number getPort() {
        Number val = getNumberProperty("queuemgr.port"); // no i18n
        return (val != null ? val : 0);
    }

    public Boolean getXAMode() {
        return getBooleanProperty("queuemgr.usexa"); // no i18n
    }
    
    public void setXAMode(boolean xaMode) {
        putBooleanProperty("queuemgr.usexa",xaMode); // no i18n
    }
    
    public void setReceiveMode(boolean recvmode) {
       putBooleanProperty("mqrecv",recvmode); // no i18n
    }
    
    public Boolean getReceiveMode() {
        return getBooleanProperty("mqrecv"); // no i18n
    }

    public String getChannelName() {
        return getStringProperty("channel.name"); // no i18n
    }

    public void setChannelName(String name) {
        putStringProperty("channel.name", name);
    }

    /**
     * Logs the configuration data to the object's m_Logger as a debug
     * event.
     */
    public final synchronized void logConfiguration() {
        StringBuffer logBuffer;
        String key;
        String value;
        Map.Entry prop;
        Iterator props;

       

        logBuffer = new StringBuffer(((m_config.size() + 1) * 65));
        logBuffer.append('\n');

        props = m_config.entrySet().iterator();
        while (props.hasNext()) {
            prop = (Map.Entry) props.next();
            key = (String) prop.getKey();

            // keep passwords hidden
            if (key.indexOf("password") != -1) {
                continue;
            }

            value = (String) prop.getValue();
            logBuffer.append("        ")
                    .append(key)
                    .append(" = <")
                    .append(value)
                    .append(">\n");
        }

        m_logger.log(Level.INFO, logBuffer.toString());
        logBuffer.delete(0, logBuffer.length());
    }

    /**
     * Retrieve the value of a string-type property.  If a non string-type
     * property is read using this method, the value returned is a string
     * representation of the original property obtained by invoking its {@link
     * Object#toString} method.
     *
     * @param key Property key
     *
     * @return String value of the property, or <code>null</code> if the
     *         property does not exist.
     */
    private synchronized String getStringProperty(String key) {
        final Object value = m_config.get(key);
        return null != value ? value.toString() : "";
    }

    /**
     * Retrieve the value of a number-type property.
     *
     * @param key Property key
     *
     * @return Number value of the property, or <code>null</code> if the
     *         property does not exist.
     *
     * @throws NumberFormatException if a non number-type property is accessed.
     */
    private synchronized Number getNumberProperty(String key) {
        final Object value = m_config.get(key);
        if (null != value) {
            return new BigDecimal(value.toString());
        } else {
            return null;
        }
    }

    /**
     * Retrieve the value of a boolean-type property.  A non-boolean property
     * read using this method evaluates to {@link Boolean#FALSE}. If the
     * property is non-existent, this method also evaluates to Boolean.FALSE.
     *
     * @param key Property key
     *
     * @return {@link Boolean#TRUE} or {@link Boolean#FALSE}.
     */
    private synchronized Boolean getBooleanProperty(String key) {
        final Object value = m_config.get(key);
        return (value != null ? Boolean.valueOf(value.toString()) : Boolean.FALSE);
    }

    private synchronized void putStringProperty(String key, String value) {
        m_config.put(key, (value != null ? value : ""));
    }

    private synchronized void putIntProperty(String aKey, int aValue) {
        if (aValue != Integer.MIN_VALUE) {
            m_config.put(aKey, String.valueOf(aValue));
        }
    }

    private synchronized void putLongProperty(String aKey, long aValue) {
        if (aValue != Long.MIN_VALUE) {
            m_config.put(aKey, String.valueOf(aValue));
        }
    }

    private synchronized void putBooleanProperty(String aKey, boolean aValue) {
        m_config.put(aKey, String.valueOf(aValue));
    }

    public QueueAccessOptions getQueueAccessOptions() {
        return new QueueAccessOptions();
    }
    
    public void adopt(MessageProperties properties) {
        Object value;
        
        // These null checks are necessary even though the setters
        // also check against them. The setters validate the property values;
        // these checks validate the properties themselves (whether they
        // are specified).
        value = properties.property(MessageProperties.PROPERTY_QMHOST);
        if (value != null) {
            setHost(value.toString());
        }
        
        value = properties.property(MessageProperties.PROPERTY_QMPORT);
        if (value != null) {
            setPort(Integer.parseInt(value.toString()));
        }
        
        value = properties.property(MessageProperties.PROPERTY_QM);
        if (value != null) {
            setQueueManagerName(value.toString());
        }
        
        value = properties.property(MessageProperties.PROPERTY_QUEUE);
        if (value != null) {
            setQueueName(value.toString());
        }
        
        value = properties.property(MessageProperties.PROPERTY_CHANNEL);
        if (value != null) {
            setChannelName(value.toString());
        }
        
        value = properties.property(MessageProperties.PROPERTY_USERNAME);
        if (value != null) {
            setUser(value.toString());
        }
        
        value = properties.property(MessageProperties.PROPERTY_PASSWORD);
        if (value != null) {
            setPassword(value.toString());
        }
        
        value = properties.property(MessageProperties.PROPERTY_SYNCPOINT);
        if (value != null) {
            setXAMode(Boolean.valueOf(value.toString()));
        }
    }
    
    /**
     * Create a new Properties instance containing all this object's data.
     * @return a Property object.
     */
    public synchronized Properties toProperties() {
        Properties props = new Properties();
        for (Map.Entry<String, Object> entry : m_config.entrySet()) {
            props.put(entry.getKey(), entry.getValue());
        }
        return props;
    }

    /**
     * Create a separate copy of this object.
     */
    protected synchronized MQClientConfiguration clone() {
        MQClientConfiguration copy = null;
        try {
            copy = (MQClientConfiguration) super.clone();
        } catch (CloneNotSupportedException e) {
            // finalized class; exception will never be raised.
        }
        assert copy != null;
        Map<String, Object> oldMap = copy.m_config;
        copy.m_config = new TreeMap<String, Object>();
        for (Map.Entry<String, Object> entry : oldMap.entrySet()) {
            copy.m_config.put(entry.getKey(), entry.getValue());
        }
        return copy;
    }
}
