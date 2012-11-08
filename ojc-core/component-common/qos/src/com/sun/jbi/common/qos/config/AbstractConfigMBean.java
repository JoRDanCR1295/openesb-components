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
 * @(#)AbstractConfigMBean.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.config;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.util.Util;


/**
 * Base class for component configuration MBean implementations.
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractConfigMBean {
    private Logger mLogger;
    private ComponentConfig mConfig;
    private String mWorkspaceRoot;

    public AbstractConfigMBean(ComponentContext ctx, 
                               ComponentConfig config) throws DeploymentException {
        mLogger = Util.getLogger(ctx, this.getClass().getName());
        mWorkspaceRoot = ctx.getWorkspaceRoot();
        if (config == null) {
            mConfig = ComponentConfig.parse(ctx.getInstallRoot());
            //  will not fail if config.properties in workspace root doesn't exist
            ConfigPersistence.loadConfig(mConfig, mWorkspaceRoot);
            // load Application configuration and variables, won't fail if no config-app-persistence.xml
            ConfigPersistence.loadApplicationConfig(mConfig, mWorkspaceRoot);
        }
        else {
            mConfig = config;
        }
    }

    /**
     * Persists application variables and configuration instances.
     * @see ConfigPersistence
     */
    protected void persistApplicationConfig() throws MBeanException {
        try {
            ConfigPersistence.persistApplicationConfig(getConfig(), getWorkspaceRoot());
        } 
        catch (DeploymentException de) {
            throw mbeanError(de, I18n.loc(
                    "QOS-6027: Failed to persist {0} application configuration: {1}", 
                    getConfig().getName(), de.getMessage()));
        }
    }

    protected void persistConfiguration() throws MBeanException {
        try {
            ConfigPersistence.persistConfig(getConfig(), getWorkspaceRoot());
        } 
        catch (DeploymentException de) {
            throw mbeanError(de, I18n.loc(
                    "QOS-6028: Failed to persist {0} component configuration: {1}", 
                    getConfig().getName(), de.getMessage()));
        }
    }

    protected ComponentConfig getConfig() {
        return mConfig;
    }
    protected String getWorkspaceRoot() {
        return mWorkspaceRoot;
    }
    protected Logger log() {
        return mLogger;
    }
    
    protected DeploymentException deployError(Exception e, String msg) {
        if (e == null) {
            log().warning(msg);
            return new DeploymentException(msg);
        }
        else {
            log().log(Level.WARNING, msg, e);
            return new DeploymentException(msg, e);
        }
    }
    
    protected MBeanException invalidProperty(String prop, String badValue) {
        String msg = I18n.loc(
                "QOS-6079: Invalid value {0} for property {1}", badValue, prop);
        return mbeanError(null, msg);
    }
    protected MBeanException mbeanError(Exception e, String msg) {
        if (e == null) {
            log().warning(msg);
            return new MBeanException(new Exception(msg));
        }
        else {
            log().log(Level.WARNING, msg, e);
            return new MBeanException(e, msg);
        }
    }

    InvalidAttributeValueException invalidAttr(String msg, Object... params) {
        String err = I18n.loc(msg, params);
        log().warning(err);
        return new InvalidAttributeValueException(err);
    }
}
