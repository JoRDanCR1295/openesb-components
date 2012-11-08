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
 * @(#)JMSBindingBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc;

import java.util.Properties;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.jbi.management.MBeanNames;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;

import org.w3c.dom.DocumentFragment;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.jmsbc.mbeans.InstallerExt;
import com.sun.jbi.jmsbc.mbeans.InstallerExtMBean;

/**
 * JMS Binding Component bootstrap implementation for the JBI framework
 *
 */
public class JMSBindingBootstrap implements Bootstrap {

    private static final Messages mMessages =
        Messages.getMessages(JMSBindingBootstrap.class);
    private static Logger mLogger = null;
    
    private InstallationContext mContext;
    
    private ObjectName mInstallerExtName;    

    public JMSBindingBootstrap() {
    }
    
    public ObjectName getExtensionMBeanName() {
        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG, 
                    "JMSBindingBootstrap_EXTENSION_MBEAN",
                    new Object[]{mInstallerExtName});
        }
        return mInstallerExtName;
    }

    public void init(InstallationContext installContext)
        throws JBIException {
        mContext = installContext;
        
        ComponentContext ctx = installContext.getContext();
        Messages.registerContext(ctx);        
        mLogger = Messages.getLogger(JMSBindingBootstrap.class);
        MBeanServer mbServer = ctx.getMBeanServer();
        MBeanNames mbNames = ctx.getMBeanNames();
        
        mInstallerExtName = mbNames.createCustomComponentMBeanName(MBeanNames.BOOTSTRAP_EXTENSION);
        
        InstallerExtMBean installerExt = new InstallerExt();
                    
        try {
            if (!ctx.getMBeanServer().isRegistered(mInstallerExtName)) {
                StandardMBean installerExtMBean = new StandardMBean(installerExt, InstallerExtMBean.class);
                ctx.getMBeanServer().registerMBean(installerExtMBean, mInstallerExtName);
            }

            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG, "JMSBindingBootstrap_EXTENSION_MBEAN_REG_SUCCEEDED",
                            new Object[]{mInstallerExtName});
            }
        } catch (Exception ex) {
                throw new JBIException(
                            mMessages.getString("JMSBC-E0717.RegisterInstallerXtensionMBeanFailed",
                                new Object[] {mInstallerExtName}),
                            ex);
        }
        
        mLogger.log(Level.INFO, "JMSBC-I0701.BootstrapInitialize");
    }

    public void onInstall() throws JBIException {        
        ComponentContext ctx = mContext.getContext();
        MBeanServer mbServer = ctx.getMBeanServer();        
        DocumentFragment descriptorExtension = mContext.getInstallationDescriptorExtension();
        Properties defaultProperties = ConfigPersistence.parseDefaultConfig(descriptorExtension);
        ConfigPersistence.persistInitialConfig(mbServer, mInstallerExtName, ctx.getWorkspaceRoot(), defaultProperties);
        mLogger.log(Level.INFO, "JMSBC-I0702.BootstrapInstall");
    }

    public void onUninstall() throws JBIException {
        mLogger.log(Level.INFO, "JMSBC-I0703.BootstrapUninstall");
    }

    public void cleanUp() throws JBIException {
        try {
            if (mInstallerExtName != null) {
                ComponentContext ctx = mContext.getContext();
                if (ctx != null) {
                    if (ctx.getMBeanServer().isRegistered(mInstallerExtName)) {
                        ctx.getMBeanServer().unregisterMBean(mInstallerExtName);
                        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                            mLogger.log(LogSupport.LEVEL_DEBUG,
                                    "JMSBindingBootstrap_EXTENSION_MBEAN_UNREG_SUCCEEDED",
                                    new Object[]{mInstallerExtName});
                        }
                    }
                }
            }
        } catch (Exception ex) {
                throw new JBIException(mMessages.getString("JMSBC-E0718.UnregisterInstallerXtensionMBeanFailed",
                                          new Object[]{mInstallerExtName}),
                                       ex);
        }

        mLogger.log(Level.INFO, "JMSBC-I0704.BootstrapCleanup");
    }
}
