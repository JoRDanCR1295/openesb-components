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
 * @(#)FTPBindingBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc.bootstrap;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.internationalization.Messages;

import javax.jbi.component.Bootstrap;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.jbi.JBIException;
import javax.jbi.management.MBeanNames;

import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;

import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * FTP BC bootstrap implementation for the JBI framework
 *
 * @author jfu
 */
public class FTPBindingBootstrap implements Bootstrap {

    private static final Messages mMessages = Messages.getMessages(FTPBindingBootstrap.class);
    private static Logger mLogger;
    private InstallationContext mContext;
    private ObjectName mInstallerExtName;
    private InstallerExtMBean mInstallerMBean;

    /**
     *
     */
    public FTPBindingBootstrap() {
    }

    /**
     *
     * @return
     */
    public ObjectName getExtensionMBeanName() {
        return mInstallerExtName;
    }

    /**
     *
     * @param installContext
     * @throws javax.jbi.JBIException
     */
    public void init(InstallationContext installContext)
            throws JBIException {

        mContext = installContext;

        ComponentContext ctx = installContext.getContext();

        Messages.registerContext(ctx);

        mLogger = Messages.getLogger(FTPBindingBootstrap.class);

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-R001001.Calling_init_method"));
        }

        MBeanServer mbServer = ctx.getMBeanServer();
        MBeanNames mbNames = ctx.getMBeanNames();

        mInstallerExtName = mbNames.createCustomComponentMBeanName(MBeanNames.BOOTSTRAP_EXTENSION);

        mInstallerMBean = new InstallerExt();

        try {
            if (!mbServer.isRegistered(mInstallerExtName)) {
                StandardMBean installerExtMBean = new StandardMBean(mInstallerMBean, InstallerExtMBean.class);
                mbServer.registerMBean(installerExtMBean, mInstallerExtName);
            }
            ComponentConfig defaultProperties = ComponentConfig.parse(mContext.getInstallRoot());
            mInstallerMBean.setInitialConfigurations(defaultProperties);
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W001001.Failed_register_mbean", ex));
            throw new JBIException(mMessages.getString("FTPBC-W001001.Failed_register_mbean", ex), ex);
        }
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R001002.Register_mbean", mInstallerExtName));
        }

    }

    /**
     *
     * @throws javax.jbi.JBIException
     */
    public void onInstall() throws JBIException {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R001003.Call_onInstall"));
        }

        ConfigPersistence.persistConfig(mInstallerMBean.getInstallationConfigurationProperties(),
                mContext.getContext().getWorkspaceRoot());

        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R001004.Complete_install"));
        }
    }

    /**
     *
     * @throws javax.jbi.JBIException
     */
    public void onUninstall() throws JBIException {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R001005.Call_onUninstall"));
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R001006.Complete_uninstall"));
        }
    }

    /**
     *
     * @throws javax.jbi.JBIException
     */
    public void cleanUp() throws JBIException {
        try {
            if (mInstallerExtName != null) {
                ComponentContext ctx = mContext.getContext();
                if (ctx != null) {
                    if (ctx.getMBeanServer().isRegistered(mInstallerExtName)) {
                        ctx.getMBeanServer().unregisterMBean(mInstallerExtName);
                        if (mLogger.isLoggable(Level.INFO)) {
                            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R001007.Unregister_mbean", mInstallerExtName));
                        }
                    }
                }
            }
        } catch (Exception ex) {
            throw new JBIException(mMessages.getString("FTPBC-E001001.Failed_unregister_mbean", mInstallerExtName), ex);
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
