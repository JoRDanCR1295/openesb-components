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
 * @(#)JDBCBindingBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc.bootstrap;

import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
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
 * HTTP SOAP binding component bootstrap implementation for the JBI framework
 */
public class JDBCBindingBootstrap implements Bootstrap {
    private static final Messages mMessages = Messages.getMessages(JDBCBindingBootstrap.class);
    public static Logger mLogger;
    private InstallationContext mContext;
    ObjectName mInstallerExtName;

    public JDBCBindingBootstrap() {
    }

    /**
     *
     * @return
     */
    //@Override
    public ObjectName getExtensionMBeanName() {
        JDBCBindingBootstrap.mLogger.info("Extension MBean name: " + mInstallerExtName);

        return mInstallerExtName;
    }

    /**
     *
     * @param installContext
     * @throws JBIException
     */
    //@Override
    public void init(final InstallationContext installContext)
        throws JBIException {
        mContext = installContext;

        final ComponentContext ctx = installContext.getContext();

        Messages.registerContext(ctx);
        mLogger = Messages.getLogger(JDBCBindingBootstrap.class);
        mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00214.Calling_init_method") );

        final MBeanServer mbServer = ctx.getMBeanServer();
        final MBeanNames mbNames = ctx.getMBeanNames();
        mInstallerExtName = mbNames.createCustomComponentMBeanName(MBeanNames.BOOTSTRAP_EXTENSION);

        final InstallerExtMBean installerExt = new InstallerExt();

        try {
            if (!ctx.getMBeanServer().isRegistered(mInstallerExtName)) {
                final StandardMBean installerExtMBean = new StandardMBean(installerExt,
                        InstallerExtMBean.class);
                ctx.getMBeanServer()
                   .registerMBean(installerExtMBean, mInstallerExtName);
            }
        } catch (final Exception ex) {
            mLogger.log(Level.WARNING, mMessages.getString("SQLSE_E00103.JDBCBLC_Failed_register_mbean"), ex);
            throw new JBIException(mMessages.getString("SQLSE_E00103.JDBCBLC_Failed_register_mbean"), ex);
        }
        //if(mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.INFO,mMessages.getString("SQLSE_R00221.Register_mbean"), mInstallerExtName);
    }

    /**
     *
     * @throws JBIException
     */
   //@Override
    public void onInstall() throws JBIException {
        mLogger.log(Level.INFO,mMessages.getString("SQLSE_R00216.Call_onInstall") );

        final ComponentContext ctx = mContext.getContext();
        final MBeanServer mbServer = ctx.getMBeanServer();
        final DocumentFragment descriptorExtension = mContext.getInstallationDescriptorExtension();
        final Properties defaultProperties = ConfigPersistence.parseDefaultConfig(descriptorExtension);
        ConfigPersistence.persistInitialConfig(mbServer, mInstallerExtName,
            ctx.getWorkspaceRoot(), defaultProperties);
        //if(mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.INFO,mMessages.getString("SQLSE_R00217.Complete_install"));
    }

    /**
     *
     * @throws JBIException
     */
    //@Override
    public void onUninstall() throws JBIException {
        //if(mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.INFO,mMessages.getString("SQLSE_R00218.Call_onUninstall") );
        //if(mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.INFO,mMessages.getString("SQLSE_R00219.Complete_uninstall") );
    }

    /**
     *
     * @throws JBIException
     */
    //@Override
    public void cleanUp() throws JBIException {
        try {
            if (mInstallerExtName != null) {
                final ComponentContext ctx = mContext.getContext();

                if ((ctx != null) &&
                        (ctx.getMBeanServer().isRegistered(mInstallerExtName))) {
                    ctx.getMBeanServer().unregisterMBean(mInstallerExtName);
                    //if(mLogger.isLoggable(Level.INFO))
                    mLogger.log(Level.INFO,mMessages.getString("SQLSE_R00102.JDBCBLC_Register_mbean") + mInstallerExtName);
                }
            }
        } catch (final Exception ex) {
            mLogger.log(Level.SEVERE,mMessages.getString("SQLSE_E00111.JDBCBLC_Failed_unregister_mbean"),ex);
            throw new JBIException("SQLSE_E00111.JDBCBLC_Failed_unregister_mbean" +
                mInstallerExtName, ex);
        }
    }
}
