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
 * @(#)ComponentConfigFilter.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.config;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.jbi.management.DeploymentException;
import javax.jbi.management.MBeanNames;
import javax.management.ObjectName;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.common.util.MBeanHelper;
import com.sun.jbi.crl.util.I18n;
import com.sun.jbi.crl.util.LogUtil;

/**
 * Abstract bootstrap implementation to register the 
 * MBean related to component configuration.
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractConfigBootstrap implements Bootstrap {
    private ComponentConfig mConfig;
    private MBeanHelper mMBeanHelper;
    private Object mMBean;
    private String mWorkspaceRoot;
    private Logger mLogger;
    
    /**
     * Sets component-specific default property values on the bootstrap MBean 
     * using a {@link ComponentConfig} with default values parsed from the 
     * component descriptor.
     * 
     * @param ctx The component context.
     * @throws JBIException if an error occurs initializing property values.
     */
    protected abstract Object initializeMBean(ComponentContext ctx) throws JBIException;
    
    /** @see com.sun.jbi.crl.lifecycle.AbstractBootstrapFilter#cleanUp() */
    public void cleanUp() throws JBIException {
        // unregister MBean
        if (mMBeanHelper != null) {
            mMBeanHelper.unregisterMBean(MBeanNames.BOOTSTRAP_EXTENSION);
        }
    }

    /** @see javax.jbi.component.Bootstrap#getExtensionMBeanName() */
    public ObjectName getExtensionMBeanName() {
        return mMBeanHelper.getObjectName(MBeanNames.BOOTSTRAP_EXTENSION);
    }

    /** @see com.sun.jbi.crl.lifecycle.AbstractBootstrapFilter#init(javax.jbi.component.InstallationContext) */
    public void init(InstallationContext installContext) throws JBIException {
        mLogger = LogUtil.getLogger(installContext.getContext(), 
                                    this.getClass().getName());        
        try {
            mMBeanHelper = new MBeanHelper(installContext.getContext());
            mWorkspaceRoot = installContext.getContext().getWorkspaceRoot();
            // parse component descriptor
            mConfig = ComponentConfig.parse(installContext.getInstallRoot());
            // set properties onto MBean instance
            setMBean(initializeMBean(installContext.getContext()));
            // register MBean
            mMBeanHelper.registerMBean(MBeanNames.BOOTSTRAP_EXTENSION, 
                                       getMBean(), 
                                       true);   // force unregister, in case prior attempt failed
        }
        catch (DeploymentException de) {
            throw error(de, "CRL-6060: Failed to parse component descriptor: {0}", de.getMessage());
        }
        catch (JBIException je) {
            throw error(je, "CRL-6061: Failed to initialize component bootstrap: {0}", je.getMessage());
        }
        catch (Exception e) {
            throw error(e, "CRL-6062: Failed to register component bootstrap: {0}", e.getMessage());
        }
    }

    /** @see com.sun.jbi.crl.lifecycle.AbstractBootstrapFilter#onInstall() */
    public void onInstall() throws JBIException {
        // persist properties to local file
        ConfigPersistence.persistConfig(getConfig(), getWorkspaceRoot());
        ConfigPersistence.persistApplicationConfig(getConfig(), getWorkspaceRoot());
    }

    /** @see javax.jbi.component.Bootstrap#onUninstall() */
    public void onUninstall() throws JBIException {
        // TODO Auto-generated method stub
        
    }

    protected Object getMBean() {
        return mMBean;
    }
    protected void setMBean(Object mbean) {
        mMBean = mbean;
    }
    
    protected ComponentConfig getConfig() {
        return mConfig;
    }
    protected String getWorkspaceRoot() {
        return mWorkspaceRoot;
    }
    
    /** Fetches a non-null {@link Logger} for this bootstrap. */ 
    protected Logger log() {
        return ((mLogger == null) ? Logger.getLogger(this.getClass().getName()) 
                                  : mLogger);
    }
    protected JBIException error(Exception e, String msg, Object... params) {
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
