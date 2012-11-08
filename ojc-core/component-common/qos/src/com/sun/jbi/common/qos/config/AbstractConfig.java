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
 * @(#)AbstractConfig.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.config;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.management.AttributeChangeNotification;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.Notification;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.qos.config.Property.Type;

/**
 * Base implementation for {@link Property}-based configuration utilities.
 *  
 * @author Kevan Simpson
 */
public abstract class AbstractConfig implements Broadcaster {
    private Map<String, Property> mProps;
    private NotificationBroadcasterSupport mBroadcastSupport;

    protected AbstractConfig() {
        mProps = new TreeMap<String, Property>();
        mBroadcastSupport = new NotificationBroadcasterSupport();
    }

    public abstract String getName();
    
    /** @see com.sun.jbi.common.qos.config.Broadcaster#addNotificationListener(javax.management.NotificationListener, java.lang.Object[]) */
    public void addNotificationListener(NotificationListener nl, Object... cfg) {
        if (nl != null) {
            if (configArgs(cfg)) {
                getBroadcastSupport().addNotificationListener(
                        nl, (NotificationFilter) cfg[0], cfg[1]);
            }
            else {
                getBroadcastSupport().addNotificationListener(nl, null, null);
            }
        }
    }

    /** @see com.sun.jbi.common.qos.config.Broadcaster#broadcast(javax.management.Notification) */
    public void broadcast(Notification note) {
        if (note != null) {
            getBroadcastSupport().sendNotification(note);
        }
    }

    /** @see com.sun.jbi.common.qos.config.Broadcaster#removeNotificationListener(javax.management.NotificationListener, java.lang.Object[]) */
    public void removeNotificationListener(NotificationListener nl, Object... cfg) 
            throws ListenerNotFoundException {
        if (nl != null) {
            if (configArgs(cfg)) {
                getBroadcastSupport().removeNotificationListener(
                        nl, (NotificationFilter) cfg[0], cfg[1]);
            }
            else {
                getBroadcastSupport().removeNotificationListener(nl);
            }
        }
    }

    private boolean configArgs(Object... cfg) {
        return (cfg != null && 
                cfg.length == 2 && 
                (cfg[0] instanceof NotificationFilter));
    }
    
    /**
     * Fetches a {@link Property} by name, which is guaranteed to not be <code>null</code>.
     * 
     * @param name The property's name.
     * @return A non-null <code>Property</code>.
     */
    public Property getProperty(String name) {
        Property p =  mProps.get(name);
        return (p == null) ? new Property(name) : p;
    }
    
    /**
     * Returns a {@link Set} of {@link Property} instances, sorted by name.
     * @return a {@link Set} of {@link Property} instances, sorted by name.
     */
    public Set<Property> propertySet() {
        return new TreeSet<Property>(mProps.values());
    }
    
    /**
     * Modifies the specified {@link Property} with the new value and broadcasts
     * an {@link AttributeChangeNotification} to all registered {@link NotificationListener}s.
     * 
     * @param propName The name of the property to modify.
     * @param newValue The new value of the property.
     * @throws MBeanException if an error occurs modifying property or broadcasting change.
     */
    public void modifyAndBroadcast(String propName, Object newValue) throws MBeanException {
        Property prop  = getProperty(propName);
        try {
            if (propertySet().contains(prop)) {
                Type type = Type.toType(prop.getType());
                Object oldValue = type.getValue(prop.getValue());
                prop.setValue((newValue == null) ? null : String.valueOf(newValue));
                // Notify listeners of this change
                Notification note = new AttributeChangeNotification(
                        this, 0, System.currentTimeMillis(), propName +" changed",
                        propName, type.getJavaType(), oldValue, newValue);
        
                broadcast(note);
            }
        }
        catch (Exception e) {
            String msg = I18n.loc(
                    "QOS-6078: Failed to modify configuration property {0} with new value {1}: {2}", 
                    prop.getName(), String.valueOf(newValue), e.getMessage());
            Logger.getLogger(Broadcaster.class.getName())
                    .log(Level.WARNING, msg, e);
            throw new MBeanException(e, msg);
        }
    }

    /**
     * Adds the specified {@link Property} to this configuration.
     * 
     * @param prop The new <code>Property</code>.
     */
    protected void addProperty(Property prop) {
        if (prop != null && prop.getName() != null) {
            mProps.put(prop.getName(), prop);
        }
    }

    /** 
     * Returns the broadcastSupport.
     * @return the broadcastSupport. 
     */
    protected NotificationBroadcasterSupport getBroadcastSupport() {
        return mBroadcastSupport;
    }

    /**
     * Removes the specified {@link Property} from this configuration.
     * 
     * @param name The property's name.
     * @return The removed <code>Property</code>.
     */
    protected Property removeProperty(String name) {
        return (name == null) ? null : mProps.remove(name);
    }
}
