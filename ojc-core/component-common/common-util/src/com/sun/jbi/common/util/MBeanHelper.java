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
 * @(#)MBeanHelper.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.util;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.MBeanNames;
import javax.management.MBeanServer;
import javax.management.ObjectName;

/**
 * Simple utility to assist in the registering and unregistering of MBeans.
 * @author Kevan Simpson
 */
public class MBeanHelper {
    private static Logger mLogger = Logger.getLogger(MBeanHelper.class.getName());

    private MBeanServer mServer;
    private MBeanNames mNames;
    private Map<String, ObjectName> mMBeanMap;
    
    /**
     * Constructs an <code>MBeanHelper</code> from a component's JBI context.
     * @param ctx The component's JBI context.
     */
    public MBeanHelper(ComponentContext ctx) {
        this(ctx.getMBeanServer(), ctx.getMBeanNames());
    }
    
    /**
     * Constructs an <code>MBeanHelper</code>.
     * @param server The MBean server to which this utility will connect.
     * @param mbeanNames MBean names.
     */
    public MBeanHelper(MBeanServer server, MBeanNames mbeanNames) {
        mServer = server;
        mNames = mbeanNames;
        mMBeanMap = new HashMap<String, ObjectName>();
    }
    
    /**
     * Returns the object name for the specified MBean name.
     * @param mbeanName The specified MBean name.
     * @return the MBean's object name.
     */
    public ObjectName getObjectName(String mbeanName) {
        return mMBeanMap.get(mbeanName);
    }
    
    /**
     * Registers an MBean with the server.
     * @param mbeanName The MBean name.
     * @param mbean The MBean object.
     * @throws JBIException if an error occurs registering MBean.
     */
    public void registerMBean(String mbeanName, Object mbean) throws JBIException {
        registerMBean(mbeanName, mbean, false);
    }
    
    /**
     * Registers an MBean with the server. If <code>force</code> is <code>true</code>,
     * the MBean will be unregistered from the server prior to registering the
     * specified MBean.
     * 
     * @param mbeanName The MBean name.
     * @param mbean The MBean object.
     * @param force If <code>true</code>, the MBean will be unregistered before
     *              registering the specified MBean.
     * @throws JBIException if an error occurs registering MBean.
     */
    public void registerMBean(String mbeanName, Object mbean, boolean force) throws JBIException {
        if (mbean != null && !Util.isEmpty(mbeanName)) {
            try {
                ObjectName objName = mNames.createCustomComponentMBeanName(mbeanName);
                if (force && mServer.isRegistered(objName)) {
                    try {
                        unregisterMBean(mbeanName);
                    }
                    catch (JBIException je) {
                        // this exception's already been logged...don't do it twice
                        throw error(null, 
                                    "UTIL-6012: Unable to register MBean, forced unregister failed for: {0}", 
                                    mbeanName);
                    }
                }
                
                if (!mServer.isRegistered(objName)) {
                    mServer.registerMBean(mbean, objName);
                    // only store objectName if register is successful...
                    mMBeanMap.put(mbeanName, objName);
                }
            }
            catch (JBIException je) {
                throw je;   // already logged and rethrow
            }
            catch (Exception e) {
                throw error(e, "UTIL-6013: Failed to register MBean {0}: {1}",
                            mbeanName, e.getMessage());
            }
        }
    }
    
    /**
     * Unregisters an MBean from the server.
     * @param mbeanName The name of the MBean to unregister.
     * @throws JBIException if an error occurs unregistering.
     */
    public void unregisterMBean(String mbeanName) throws JBIException {
        if (!Util.isEmpty(mbeanName)) {
            try {
                ObjectName objName = mMBeanMap.get(mbeanName);
                if (objName == null) {
                    throw error(null, "No MBean associated with specified name: {0}", mbeanName);
                }
                
                if (mServer.isRegistered(objName)) {
                    mServer.unregisterMBean(objName);
                    // only remove objectName if unregister is successful...
                    mMBeanMap.remove(mbeanName);
                }
            }
            catch (JBIException je) {
                throw je;   // already logged and rethrow
            }
            catch (Exception e) {
                throw error(e, "UTIL-6014: Failed to unregister MBean {0}: {1}",
                            mbeanName, e.getMessage());
            }
        }
    }
    
    /**
     * Fetches the {@link Logger} for this utility.
     * @return the <code>Logger</code> for this utility.
     */
    protected Logger log() {
        return mLogger;
    }
    
    // Utility method to create localized JBIExceptions.
    private JBIException error(Exception e, String msg, Object... params) {
        String err = I18n.loc(msg, params);
        if (e == null) {
            log().warning(err);
            return new JBIException(err);
        }
        else {
            log().log(Level.WARNING, err, e);
            return new JBIException(err, e);
        }
    }
}
