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
 * @(#)SAPBindingBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.bootstrap;

import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.internationalization.Messages;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Properties;

import javax.jbi.component.Bootstrap;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.jbi.JBIException;
import javax.jbi.management.MBeanNames;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;

import org.w3c.dom.DocumentFragment;

/**
 * Install/uninstall processor for SAP Binding Component.
 */
public class SAPBindingBootstrap implements Bootstrap {
    
    public SAPBindingBootstrap() {
        mLogger = Messages.getLogger(SAPBindingBootstrap.class);
    }
    
    public ObjectName getExtensionMBeanName() {
        mLogger.info("SAPBindingBootstrap.Extension_mbean_name" + mInstallerExtName);
        return mInstallerExtName;
    }

    public void init(InstallationContext installContext)
        throws JBIException {
        mContext = installContext;
        ComponentContext ctx = installContext.getContext();
        
        Messages.registerContext(ctx);
        mLogger = Messages.getLogger(SAPBindingBootstrap.class);
        mLogger.log(Level.INFO, "SAPBindingBootstrap.Calling_init");        
                
        MBeanServer mbServer = ctx.getMBeanServer();
        MBeanNames mbNames = ctx.getMBeanNames();
        
        mInstallerExtName = mbNames.createCustomComponentMBeanName(
                MBeanNames.BOOTSTRAP_EXTENSION);
        
        InstallerExtMBean installerExt = new InstallerExt();
                    
        try {
            if (!mbServer.isRegistered(mInstallerExtName)) {
                StandardMBean installerExtMBean =
                        new StandardMBean(installerExt, InstallerExtMBean.class);
                mbServer.registerMBean(installerExtMBean, mInstallerExtName);
            }
        } catch (Exception ex) {
            mLogger.log(Level.WARNING,
                    "SAPBindingBootstrap.Failed_register_mbean",
                    ex);
            throw new JBIException(
                    messages.getString(
                        "SAPBindingBootstrap.Failed_register_mbean"), ex);
        }
        mLogger.info("SAPBindingBootstrap.Registered_mbean" + mInstallerExtName);
    }

    public void onInstall() throws JBIException {
        mLogger.info("SAPBindingBootstrap.Calling_onInstall");
        
        if (mContext == null) {
            mLogger.warning("SAPBindingBootstrap.Calling_onInstall_without_context");
            return;
        }
        
        ComponentContext ctx = mContext.getContext();
        MBeanServer mbServer = ctx.getMBeanServer();        
        DocumentFragment descriptorExtension =
                mContext.getInstallationDescriptorExtension();
        Properties defaultProperties =
                ConfigPersistence.parseDefaultConfig(descriptorExtension);
        ConfigPersistence.persistInitialConfig(
                mbServer,
                mInstallerExtName,
                ctx.getWorkspaceRoot(),
                defaultProperties);

        mLogger.info("SAPBindingBootstrap.Completed_install");
    }

    public void onUninstall() throws JBIException {
        mLogger.info("SAPBindingBootstrap.Calling_onUninstall");
        mLogger.info("SAPBindingBootstrap.Completed_uninstall");
    }
    
    public void cleanUp() throws JBIException {
        try {
            if (mInstallerExtName != null) {
                ComponentContext ctx = mContext.getContext();
                if (ctx != null) {
                    if (ctx.getMBeanServer().isRegistered(mInstallerExtName)) {
                        ctx.getMBeanServer().unregisterMBean(mInstallerExtName);
                        mLogger.info("SAPBindingBootstrap.Unregistered_mbean"
                                + mInstallerExtName);
                    }
                }
            }
        } catch (Exception ex) {
            throw new JBIException(
                    messages.getString("SAPBindingBootstrap.Failed_unregister_mbean")
                            + mInstallerExtName,
                    ex);
        }
    }
    
    /** 
      * Package protected method.
      * Used solely for JUnit test purposes
      */
    void setInstallationContext (InstallationContext context) {
        mContext = context;
    }
    
    /** 
      * Package protected method.
      * Used solely for JUnit test purposes
      */
    void setMBeanObjectName (ObjectName objectName) {
        mInstallerExtName = objectName;
    }
    
    private static final Messages messages =
            Messages.getMessages(SAPBindingBootstrap.class);
    
    private static Logger mLogger;
            
    private InstallationContext mContext;
    
    private ObjectName mInstallerExtName;    
}
