/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/
package com.sun.jbi.dcombc;

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

import com.sun.jbi.dcombc.mbeans.InstallerExt;
import com.sun.jbi.dcombc.mbeans.InstallerExtMBean;

/**
 * DCOM Binding Component bootstrap implementation for the JBI framework
 * 
 * @author Chandrakanth Belde
 */
public class DCOMBindingBootstrap implements Bootstrap {
	/**
	 *
	 */
    private static final Messages mMessages = Messages.getMessages(DCOMBindingBootstrap.class);

    private static final Logger mLogger = Messages.getLogger(DCOMBindingBootstrap.class);

    private InstallationContext mContext;

    private ObjectName mInstallerExtName;

    public DCOMBindingBootstrap() {
    }

    public ObjectName getExtensionMBeanName() {
        mLogger.log(Level.INFO, "DCOMBindingBootstrap.EXTENSION_MBEAN", new Object[] { mInstallerExtName });
        return mInstallerExtName;
    }

    public void init(InstallationContext installContext) throws JBIException {
        mLogger.log(Level.FINEST, "DCOMBindingBootstrap.INIT_CALLED");
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
            mLogger.log(Level.SEVERE, "DCOMBindingBootstrap.EXTENSION_MBEAN_REG_FAILED",
                    new Object[] { ex.getLocalizedMessage() });

            throw new JBIException(mMessages.getString("DCOMBindingBootstrap.EXTENSION_MBEAN_REG_FAILED",
                    new Object[] { ex }));
        }

        mLogger.log(Level.INFO, "DCOMBindingBootstrap.EXTENSION_MBEAN_REG_SUCCEEDED",
                new Object[] { mInstallerExtName });
    }

    public void onInstall() throws JBIException {
        mLogger.log(Level.INFO, "DCOMBindingBootstrap.ON_INSTALL_CALLED");

        ComponentContext ctx = mContext.getContext();
        MBeanServer mbServer = ctx.getMBeanServer();
        DocumentFragment descriptorExtension = mContext.getInstallationDescriptorExtension();
        Properties defaultProperties = ConfigPersistence.parseDefaultConfig(descriptorExtension);
        ConfigPersistence.persistInitialConfig(mbServer, mInstallerExtName, ctx.getWorkspaceRoot(), defaultProperties);

        mLogger.log(Level.INFO, "DCOMBindingBootstrap.ON_INSTALL_DONE");
    }

    public void onUninstall() throws JBIException {
        mLogger.log(Level.INFO, "DCOMBindingBootstrap.ON_UNINSTALL_CALLED");
    }

    public void cleanUp() throws JBIException {
        try {
            if (mInstallerExtName != null) {
                ComponentContext ctx = mContext.getContext();
                if (ctx != null) {
                    if (ctx.getMBeanServer().isRegistered(mInstallerExtName)) {
                        ctx.getMBeanServer().unregisterMBean(mInstallerExtName);
                        mLogger.log(Level.INFO, "DCOMBindingBootstrap.EXTENSION_MBEAN_UNREG_SUCCEEDED",
                                new Object[] { mInstallerExtName });
                    }
                }
            }
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "DCOMBindingBootstrap.EXTENSION_MBEAN_UNREG_FAILED", new Object[] {
                    mInstallerExtName, ex.getLocalizedMessage() });

            throw new JBIException(mMessages.getString("DCOMBindingBootstrap.EXTENSION_MBEAN_UNREG_FAILED",
                    new Object[] { mInstallerExtName, ex }));
        }
    }
}
