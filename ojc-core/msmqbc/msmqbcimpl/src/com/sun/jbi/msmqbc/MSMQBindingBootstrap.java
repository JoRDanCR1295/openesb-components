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
 * @(#)MSMQBindingBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc;

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

import com.sun.jbi.msmqbc.mbeans.InstallerExt;
import com.sun.jbi.msmqbc.mbeans.InstallerExtMBean;

/**
 * MSMQ Binding Component bootstrap implementation for the JBI framework
 * 
 * @author Sun Microsystems
 */
public class MSMQBindingBootstrap implements Bootstrap {

    private static final Messages mMessages = Messages.getMessages(MSMQBindingBootstrap.class);

    private static final Logger mLogger = Messages.getLogger(MSMQBindingBootstrap.class);

    private InstallationContext mContext;

    private ObjectName mInstallerExtName;

    public MSMQBindingBootstrap() {
    }

    public ObjectName getExtensionMBeanName() {
        mLogger.log(Level.INFO, "MSMQBindingBootstrap_EXTENSION_MBEAN", new Object[] { mInstallerExtName });
        return mInstallerExtName;
    }

    public void init(InstallationContext installContext) throws JBIException {
        mLogger.log(Level.FINEST, "MSMQBindingBootstrap_INIT_CALLED");
        mContext = installContext;

        ComponentContext ctx = installContext.getContext();
        MBeanServer mbServer = ctx.getMBeanServer();
        MBeanNames mbNames = ctx.getMBeanNames();

        mInstallerExtName = mbNames.createCustomComponentMBeanName(MBeanNames.BOOTSTRAP_EXTENSION);

        InstallerExtMBean installerExt = new InstallerExt();

        try {
            if (!ctx.getMBeanServer().isRegistered(mInstallerExtName)) {
                StandardMBean installerExtMBean = new StandardMBean(installerExt, InstallerExtMBean.class);
                ctx.getMBeanServer().registerMBean(installerExtMBean, mInstallerExtName);
            }
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "MSMQBindingBootstrap_EXTENSION_MBEAN_REG_FAILED",
                    new Object[] { ex.getLocalizedMessage() });

            throw new JBIException(mMessages.getString("MSMQBindingBootstrap_EXTENSION_MBEAN_REG_FAILED",
                    new Object[] { ex }));
        }

        mLogger.log(Level.INFO, "MSMQBindingBootstrap_EXTENSION_MBEAN_REG_SUCCEEDED",
                new Object[] { mInstallerExtName });
    }

    public void onInstall() throws JBIException {
        mLogger.log(Level.INFO, "MSMQBindingBootstrap_ON_INSTALL_CALLED");

        ComponentContext ctx = mContext.getContext();
        MBeanServer mbServer = ctx.getMBeanServer();
        DocumentFragment descriptorExtension = mContext.getInstallationDescriptorExtension();
        Properties defaultProperties = ConfigPersistence.parseDefaultConfig(descriptorExtension);
        ConfigPersistence.persistInitialConfig(mbServer, mInstallerExtName, ctx.getWorkspaceRoot(), defaultProperties);

        mLogger.log(Level.INFO, "MSMQBindingBootstrap_ON_INSTALL_DONE");
    }

    public void onUninstall() throws JBIException {
        mLogger.log(Level.INFO, "MSMQBindingBootstrap_ON_UNINSTALL_CALLED");
    }

    public void cleanUp() throws JBIException {
        try {
            if (mInstallerExtName != null) {
                ComponentContext ctx = mContext.getContext();
                if (ctx != null) {
                    if (ctx.getMBeanServer().isRegistered(mInstallerExtName)) {
                        ctx.getMBeanServer().unregisterMBean(mInstallerExtName);
                        mLogger.log(Level.INFO, "MSMQBindingBootstrap_EXTENSION_MBEAN_UNREG_SUCCEEDED",
                                new Object[] { mInstallerExtName });
                    }
                }
            }
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "MSMQBindingBootstrap_EXTENSION_MBEAN_UNREG_FAILED", new Object[] {
                    mInstallerExtName, ex.getLocalizedMessage() });

            throw new JBIException(mMessages.getString("MSMQBindingBootstrap_EXTENSION_MBEAN_UNREG_FAILED",
                    new Object[] { mInstallerExtName, ex }));
        }
    }
}
