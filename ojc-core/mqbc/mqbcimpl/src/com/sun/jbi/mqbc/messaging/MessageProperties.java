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
 * @(#)$Id: MessageProperties.java,v 1.1 2008/12/12 18:20:57 noel_ang Exp $
 *
 * Copyright 2008-2011 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.messaging;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import javax.jbi.messaging.NormalizedMessage;

import com.sun.jbi.mqbc.extservices.MQClientConfiguration;

/**
 * Represents the set of component-specific properties used in various internal
 * messaging arrangements with the JBI runtime.
 *
 * @author Noel.Ang@sun.com
 */
public final class MessageProperties {

    public static final String PROPERTY_QMHOST =
            "org.glassfish.openesb.mq.queuemanagerhost";
    public static final String PROPERTY_QMPORT =
            "org.glassfish.openesb.mq.queuemanagerport";
    public static final String PROPERTY_QM =
            "org.glassfish.openesb.mq.queuemanager";
    public static final String PROPERTY_QUEUE =
            "org.glassfish.openesb.mq.queue";
    public static final String PROPERTY_CHANNEL =
            "org.glassfish.openesb.mq.channel";
    public static final String PROPERTY_USERNAME =
            "org.glassfish.openesb.mq.username";
    public static final String PROPERTY_PASSWORD =
            "org.glassfish.openesb.mq.password";
    public static final String PROPERTY_SYNCPOINT =
            "org.glassfish.openesb.mq.syncpoint";

    private static final Set<String> propertyNames = new HashSet<String>();

    static {
        propertyNames.add(PROPERTY_QMHOST);
        propertyNames.add(PROPERTY_QMPORT);
        propertyNames.add(PROPERTY_QM);
        propertyNames.add(PROPERTY_QUEUE);
        propertyNames.add(PROPERTY_CHANNEL);
        propertyNames.add(PROPERTY_USERNAME);
        propertyNames.add(PROPERTY_PASSWORD);
        propertyNames.add(PROPERTY_SYNCPOINT);
    }

    private final Map<String, Object> properties;

    public MessageProperties() {
        properties = new HashMap<String, Object>();
    }

    /**
     * Adopt the information contained in a MQClientConfiguration object.
     *
     * @param cfg MQClientConfiguration containing the information to adopt.
     * 
     * @throws NullPointerException if cfg is null.
     */
    public void load(MQClientConfiguration cfg) {
        if (cfg == null) {
            throw new NullPointerException("cfg");
        }
        synchronized (properties) {
            properties.put(PROPERTY_QMHOST, cfg.getHost());
            properties.put(PROPERTY_QMPORT, cfg.getPort().toString());
            properties.put(PROPERTY_QM, cfg.getQueueManagerName());
            properties.put(PROPERTY_QUEUE, cfg.getQueueName());
            properties.put(PROPERTY_CHANNEL, cfg.getChannelName());
            properties.put(PROPERTY_USERNAME, cfg.getUser());
            properties.put(PROPERTY_PASSWORD, cfg.getPassword());
        }
    }

    /**
     * Copy recognized properties from a map to this object. Unrecognized
     * properties are ignored.
     *
     * @param properties A map of property names to property values.
     * 
     * @throws NullPointerException if properties is null.
     */
    public void load(Map<String, Object> properties) {
        if (properties == null) {
            throw new NullPointerException("properties");
        }
        synchronized (this.properties) {
            for (Map.Entry<String, Object> entry : properties.entrySet()) {
                String name = entry.getKey();
                if (propertyNames.contains(name)) {
                    // properties that need special treatment:

                    // 1) boolean properties: null values not acceptable
                    // because these map to
                    // http://www.w3.org/2001/XMLSchema/boolean
                    if (PROPERTY_SYNCPOINT.equals(name)) {
                        this.properties.put(name, string(bool(entry.getValue())));
                    } else {
                        this.properties.put(name, string(entry.getValue()));
                    }
                }
            }
        }
    }

    /**
     * Copy recognized properties from a NormalizedMessage to this object.
     * Unrecognized properties are ignored.
     *
     * @param message A NormalizedMessage.
     *
     * @throws NullPointerException if message is null.
     */
    public void load(NormalizedMessage message) {
        if (message == null) {
            throw new NullPointerException("message");
        }

        Map<String, Object> messageProps = new HashMap<String, Object>();
        for (Object nameObj : message.getPropertyNames()) {
            String name = nameObj.toString();
            Object value = message.getProperty(name);
            messageProps.put(name, value);
        }
        load(messageProps);
    }

    /**
     * Use the specified value to initialize the component Property indicating
     * syncpoint state.
     *
     * @param syncpoint Boolean value indicating whether syncpoint is to have an
     * effect.
     */
    public void loadSyncPoint(boolean syncpoint) {
        synchronized (properties) {
            properties.put(PROPERTY_SYNCPOINT, Boolean.toString(syncpoint));
        }
    }

    /**
     * Produce a Properties object that represents the Property values
     * represented by this object.
     *
     * @return A Properties object with zero or more component-specific
     *         properties.
     */
    public Properties toProperties() {
        synchronized (properties) {
            Properties props = new Properties();
            for (Map.Entry<String, Object> entry : properties.entrySet()) {
                // Exclude values not set
                if (entry.getValue() != null) {
                    props.put(entry.getKey(), entry.getValue());
                }
            }
            return props;
        }
    }

    /**
     * Produce the value of the named property.
     *
     * @param key The property name.
     *
     * @return The property value, if defined, otherwise null.
     * @throws IllegalArgumentException if the specified property is not
     * recognized.
     */
    public Object property(String key) {
        if (!propertyNames.contains(key)) {
            throw new IllegalArgumentException("unknown key '" + key + "'");
        }
        
        Object value = null;
        
        synchronized (properties) {
            if (properties.containsKey(key)) {
                value = properties.get(key);
            }
        }

        return value;
    }

    private static String string(Object value) {
        return (value != null ? value.toString().trim() : "");
    }

    private static boolean bool(Object value) {
        if (value == null) {
            return Boolean.FALSE;
        } else if (value instanceof Boolean) {
            return (Boolean) value;
        } else {
            String str = value.toString().toLowerCase();
            // http://www.w3.org/TR/xmlschema-2/#boolean
            return ("true".equals(str) || "1".equals(str));
        }
    }
}
