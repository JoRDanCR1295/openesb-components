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
 * @(#)SMTPBindingBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc;

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
import com.sun.jbi.smtpbc.mbeans.InstallerExt;
import com.sun.jbi.smtpbc.mbeans.InstallerExtMBean;

/**
 * SMTP Binding Component bootstrap implementation for the JBI framework
 * @author aegloff
 */
public class SMTPBindingBootstrap implements Bootstrap {
    private static final Messages mMessages =
           Messages.getMessages(SMTPBindingBootstrap.class);
    private static final Logger mLogger = Messages.getLogger(SMTPBindingBootstrap.class);
    
    private InstallationContext mContext;
    
    private ObjectName mInstallerExtName;    

    public SMTPBindingBootstrap() {
    }
    
    public ObjectName getExtensionMBeanName() {
        if (SMTPBindingBootstrap.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingBootstrap.mLogger.log(Level.INFO, "SMTPBS_MBeanName", mInstallerExtName);
        }
        return mInstallerExtName;
    }

    public void init(final InstallationContext installContext)
        throws JBIException {

        mContext = installContext;        
        final ComponentContext ctx = installContext.getContext();
        Messages.registerContext(ctx);
        if (SMTPBindingBootstrap.mLogger.isLoggable(Level.FINEST)) {
            SMTPBindingBootstrap.mLogger.log(Level.FINEST, "SMTPBS_Called_Init");
        }
        final MBeanServer mbServer = ctx.getMBeanServer();
        final MBeanNames mbNames = ctx.getMBeanNames();
        
        mInstallerExtName = mbNames.createCustomComponentMBeanName(MBeanNames.BOOTSTRAP_EXTENSION);
        final InstallerExtMBean installerExt = new InstallerExt();
                    
        try {
            if (!ctx.getMBeanServer().isRegistered(mInstallerExtName)) {
                final StandardMBean installerExtMBean = new StandardMBean(installerExt, InstallerExtMBean.class);
                ctx.getMBeanServer().registerMBean(installerExtMBean, mInstallerExtName);
            }
        } catch (final Exception ex) {
            SMTPBindingBootstrap.mLogger.log(Level.WARNING,
                        "SMTPBS_Failed_register_installer_mbean");
            throw new JBIException(SMTPBindingBootstrap.mMessages.getString("SMTPBS_Failed_register_installer_mbean"), ex);
        }
        if (SMTPBindingBootstrap.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingBootstrap.mLogger.log(Level.INFO, "SMTPBS_Register_installer_mbean", mInstallerExtName);
        }
    }

    public void onInstall() throws JBIException {
        if (SMTPBindingBootstrap.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingBootstrap.mLogger.log(Level.INFO, "SMTPBS_onInstall");
        }
        final ComponentContext ctx = mContext.getContext();
        final MBeanServer mbServer = ctx.getMBeanServer();        
        final DocumentFragment descriptorExtension = mContext.getInstallationDescriptorExtension();
        final Properties defaultProperties = ConfigPersistence.parseDefaultConfig(descriptorExtension);
        ConfigPersistence.persistInitialConfig(mbServer, mInstallerExtName, ctx.getWorkspaceRoot(), defaultProperties);
        if (SMTPBindingBootstrap.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingBootstrap.mLogger.log(Level.INFO, "SMTPBS_onInstall_complete");
        }
    }

    public void onUninstall() throws JBIException {
        if (SMTPBindingBootstrap.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingBootstrap.mLogger.log(Level.INFO, "SMTPBS_onUninstall");
            SMTPBindingBootstrap.mLogger.log(Level.INFO, "SMTPBS_onUninstall_complete");
        }
    }
    
    public void cleanUp() throws JBIException {
        try {
            if (mInstallerExtName != null) {
                final ComponentContext ctx = mContext.getContext();
                if ((ctx != null) && (ctx.getMBeanServer().isRegistered(mInstallerExtName))) {
					ctx.getMBeanServer().unregisterMBean(mInstallerExtName);
					if (SMTPBindingBootstrap.mLogger.isLoggable(Level.INFO)) {
					    SMTPBindingBootstrap.mLogger.log(Level.INFO,
					                "SMTPBS_Unregistered_installer_mbean",
					                mInstallerExtName);
					}
				}
            }
        } catch (final Exception ex) {
            throw new JBIException(
                    SMTPBindingBootstrap.mMessages.getString("SMTPBS_Failed_unregister_mbean",
                                        mInstallerExtName),
                    ex);
            
        }
    }
}
