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
 * @(#)SwiftBindingBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.bootstrap;

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

import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.internationalization.Messages;

/**
 * Swift Binding Component bootstrap implementation for the JBI framework
 * 
 * @author Sriram, S. Nageswara Rao
 */
public class SwiftBindingBootstrap implements Bootstrap {

    private static final Messages messages = Messages.getMessages(SwiftBindingBootstrap.class);

    private static Logger mLogger = null;

    private InstallationContext mContext;

    private ObjectName mInstallerExtName;

    public SwiftBindingBootstrap() {
    }

    public ObjectName getExtensionMBeanName() {
        mLogger.info("Extension MBean name: " + mInstallerExtName);
        return mInstallerExtName;
    }

    public void init(InstallationContext installContext) throws JBIException {

        mContext = installContext;
        ComponentContext ctx = installContext.getContext();

        Messages.registerContext(ctx);
        mLogger = Messages.getLogger(SwiftBindingBootstrap.class);
        mLogger.log(Level.INFO, "Calling_init_method");

        MBeanServer mbServer = ctx.getMBeanServer();
        MBeanNames mbNames = ctx.getMBeanNames();

        mInstallerExtName = mbNames.createCustomComponentMBeanName(MBeanNames.BOOTSTRAP_EXTENSION);

        InstallerExtMBean installerExt = new InstallerExt();

        try {
            if (!mbServer.isRegistered(mInstallerExtName)) {
                StandardMBean installerExtMBean = new StandardMBean(installerExt, InstallerExtMBean.class);
                mbServer.registerMBean(installerExtMBean, mInstallerExtName);
            }
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, "Failed_register_mbean", ex);
            throw new JBIException("Failed_register_mbean", ex);
        }
        mLogger.info("Register_mbean" + mInstallerExtName);

    }

    public void onInstall() throws JBIException {
        mLogger.info("Call_onInstall");

        ComponentContext ctx = mContext.getContext();
        MBeanServer mbServer = ctx.getMBeanServer();
        DocumentFragment descriptorExtension = mContext.getInstallationDescriptorExtension();
        Properties defaultProperties = ConfigPersistence.parseDefaultConfig(descriptorExtension);
        ConfigPersistence.persistInitialConfig(mbServer, mInstallerExtName, ctx.getWorkspaceRoot(), defaultProperties);

        mLogger.info("Complete_install");
    }

    public void onUninstall() throws JBIException {
        mLogger.info("Call_onUninstall");
        mLogger.info("Complete_uninstall");
    }

    public void cleanUp() throws JBIException {
        try {
            if (mInstallerExtName != null) {
                ComponentContext ctx = mContext.getContext();
                if (ctx != null) {
                    MBeanServer mbServer = ctx.getMBeanServer();
                    if (mbServer.isRegistered(mInstallerExtName)) {
                        mbServer.unregisterMBean(mInstallerExtName);
                        mLogger.info("Unregister_mbean" + mInstallerExtName);
                    }
                }
            }
        } catch (Exception ex) {
            throw new JBIException("Failed_unregister_mbean" + mInstallerExtName, ex);
        }
    }

    /**
     * Used solely for JUnit test purposes
     */
    public void setInstallationContext(InstallationContext context) {
        mContext = context;
    }

    /**
     * Used solely for JUnit test purposes
     */
    public void setMBeanObjectName(ObjectName objectName) {
        mInstallerExtName = objectName;
    }
}
