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
 * @(#)FileBindingBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.bootstrap;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.internationalization.Messages;

import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.component.Bootstrap;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.jbi.JBIException;
import javax.jbi.management.MBeanNames;

import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;

/**
 * File binding component bootstrap implementation for the JBI framework
 *
 * @author Sherry Weng
 * @author Nitin Nahata
 */
public class FileBindingBootstrap implements Bootstrap {

    private static final Messages messages = Messages.getMessages(FileBindingBootstrap.class);
    private static Logger mLogger;
    private InstallationContext mContext;
    private ObjectName mInstallerExtName;
    private InstallerExtMBean mInstallerMBean;

    public FileBindingBootstrap() {
    }

    public ObjectName getExtensionMBeanName() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Extension MBean name: " + mInstallerExtName);
        }
        return mInstallerExtName;
    }

    public void init(InstallationContext installContext)
            throws JBIException {
        mContext = installContext;
        ComponentContext ctx = installContext.getContext();

        Messages.registerContext(ctx);
        mLogger = Messages.getLogger(FileBindingBootstrap.class);
        mLogger.log(Level.FINE, "Calling init method");

        MBeanServer mbServer = ctx.getMBeanServer();
        MBeanNames mbNames = ctx.getMBeanNames();

        mInstallerExtName = mbNames.createCustomComponentMBeanName(MBeanNames.BOOTSTRAP_EXTENSION);

//        InstallerExtMBean installerExt = new InstallerExt();
        mInstallerMBean = new InstallerExt();

        try {
            if (!mbServer.isRegistered(mInstallerExtName)) {
                StandardMBean installerExtMBean = new StandardMBean(mInstallerMBean, InstallerExtMBean.class);
                mbServer.registerMBean(installerExtMBean, mInstallerExtName);

                ComponentConfig defaultProperties = ComponentConfig.parse(mContext.getInstallRoot());
                mInstallerMBean.setInitialConfigurations(defaultProperties);

            }
        } catch (Exception ex) {
            String msg = messages.getString("FILEBC-W00101", ex.getLocalizedMessage());
            mLogger.log(Level.FINE, msg, ex);
            throw new JBIException(msg, ex);
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "MBean registered for " + mInstallerExtName);
        }

    }

    public void onInstall() throws JBIException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "entering onInstall...");
        }

        ComponentContext ctx = mContext.getContext();
        MBeanServer mbServer = ctx.getMBeanServer();
//        DocumentFragment descriptorExtension = mContext.getInstallationDescriptorExtension();
//        Properties defaultProperties = ConfigPersistence.parseDefaultConfig(descriptorExtension);
//        ConfigPersistence.persistInitialConfig(mbServer, mInstallerExtName, ctx.getWorkspaceRoot(), defaultProperties);

        ComponentConfig installProperties = mInstallerMBean.getInstallationConfigurationProperties();
        ConfigPersistence.persistConfig(installProperties, ctx.getWorkspaceRoot());

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "onInstall completed");
        }
    }

    public void onUninstall() throws JBIException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "entering onUninstall...");
            mLogger.log(Level.FINE, "onUninstall completed");
        }
    }

    public void cleanUp() throws JBIException {
        try {
            if (mInstallerExtName != null) {
                ComponentContext ctx = mContext.getContext();
                if (ctx != null) {
                    if (ctx.getMBeanServer().isRegistered(mInstallerExtName)) {
                        ctx.getMBeanServer().unregisterMBean(mInstallerExtName);
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.FINE, mInstallerExtName + " MBean unregistered");
                        }
                    }
                }
            }
        } catch (Exception ex) {
            throw new JBIException("Failed_unregister_mbean" + mInstallerExtName, ex);
        }
    }

    /**
     * Package protected method.
     * Used solely for JUnit test purposes
     */
    void setInstallationContext(InstallationContext context) {
        mContext = context;
    }

    /**
     * Package protected method.
     * Used solely for JUnit test purposes
     */
    void setMBeanObjectName(ObjectName objectName) {
        mInstallerExtName = objectName;
    }
}
