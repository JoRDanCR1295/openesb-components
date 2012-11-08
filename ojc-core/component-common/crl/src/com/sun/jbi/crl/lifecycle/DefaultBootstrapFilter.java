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
 * @(#)DefaultBootstrapFilter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.lifecycle;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.jbi.management.MBeanNames;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;

import com.sun.jbi.crl.util.I18n;

/**
 * Simple bootstrap filter capable of un/registering an installer
 * configuration MBean, during {@link #init(InstallationContext)} and
 * {@link #cleanUp()}, respectively.
 * 
 * @author Kevan Simpson
 */
public class DefaultBootstrapFilter extends AbstractBootstrapFilter {
    private StandardMBean mInstallerConfigMBean = null;
    
    /**
     * Constructs bootstrap decorator with bootstrap to decorate and an
     * installer configuration MBean.
     * 
     * @param bootstrap The bootstrap to decorate, may be <code>null</code>.
     * @param installerConfigMBean An installer configuration MBean.
     */
    public DefaultBootstrapFilter(BootstrapFilterChain chain, 
                                  StandardMBean installerConfigMBean) {
        super(chain);
        setInstallerConfigMBean(installerConfigMBean);
    }

    /**
     * Sets installation and component contexts and registers an installer
     * configuration MBean, if one has been provided, <i>prior</i> to delegating
     * initialization to the decorated bootstrap.
     * 
     * @see javax.jbi.component.Bootstrap#init(javax.jbi.component.InstallationContext) 
     */
    public void init(InstallationContext installContext) throws JBIException {
        registerConfigurationMBean(mInstallerConfigMBean);
    }

    /**
     * Unregisters installer configuration MBean, if one has been registered with
     * the extension MBean name of this instance, <i>prior</i> to delegating 
     * clean up to the decorated bootstrap.
     * 
     * @see javax.jbi.component.Bootstrap#getExtensionMBeanName()
     * @see javax.jbi.component.Bootstrap#cleanUp()
     */
    public void cleanUp() throws JBIException {
        unregisterConfigurationMBean(getExtensionMBeanName());
    }

    /**
     * @return Returns the installer configuration MBean.
     */
    public StandardMBean getInstallerConfigMBean() {
        return mInstallerConfigMBean;
    }
    
    /**
     * @param installerConfigMBean The installer configuration MBean to set.
     */
    public void setInstallerConfigMBean(StandardMBean installerConfigMBean) {
        mInstallerConfigMBean = installerConfigMBean;
    }
    
    /**
     * Registers specified MBean with this decorator's extension MBean name.
     * 
     * @param mbean The MBean to register.
     * @throws JBIException if an error occurs registering MBean.
     */
    protected void registerConfigurationMBean(StandardMBean mbean) throws JBIException {
        if (mbean != null) {
            try {
                ComponentContext ctx = getChain().getComponentContext();
                MBeanNames mbeanNames = ctx.getMBeanNames();
                MBeanServer mbeanServer = ctx.getMBeanServer();
                getChain().setExtensionMBeanName(
                        mbeanNames.createCustomComponentMBeanName(
                                MBeanNames.BOOTSTRAP_EXTENSION));
                if (!mbeanServer.isRegistered(getExtensionMBeanName())) {
                    mbeanServer.registerMBean(mbean, 
                                              getExtensionMBeanName());
                }
            }
            catch (Exception e) {
                String error = I18n.loc(
                		"CRL-6026: Failed to register configuration MBean {0}: {1}",
                		String.valueOf(getExtensionMBeanName()), e.getMessage());
                Logger.getLogger(this.getClass().getPackage().getName())
                		.log(Level.WARNING, error, e);
                throw new JBIException(error, e);
            }
        }

    }

    /**
     * Unregisters an MBean using the specified object name.
     * 
     * @param mbean The object name of the MBean to unregister.
     * @throws JBIException if an error occurs unregistering MBean.
     */
    protected void unregisterConfigurationMBean(ObjectName objName) throws JBIException {
        // TODO can this be used for any MBean?
        // TODO should there be a utility for un/registering MBeans?
        try {
            if (objName != null && getChain().getComponentContext() != null) {
                MBeanServer mbeanServer = getChain().getComponentContext().getMBeanServer();
                if (mbeanServer.isRegistered(objName)) {
                    mbeanServer.unregisterMBean(objName);
                }
            }
        } 
        catch (Exception e) {
            String error = I18n.loc(
            		"CRL-6027: Failed to unregister configuration MBean {0}: {1}",
            		String.valueOf(getExtensionMBeanName()), e.getMessage());
            Logger.getLogger(this.getClass().getPackage().getName())
            		.log(Level.WARNING, error, e);
            throw new JBIException(error, e);
        }
    }
}
