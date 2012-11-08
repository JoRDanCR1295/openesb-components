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

package com.sun.jbi.imsbc.bootstrap;

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

import com.sun.jbi.imsbc.bootstrap.InstallerExt;
import com.sun.jbi.imsbc.bootstrap.InstallerExtMBean;

/**
 * IMS Binding Component bootstrap implementation for the JBI framework
 * 
 * @author Sun Microsystems
 */
public class IMSBindingBootstrap implements Bootstrap {

    private static final Messages mMessages = Messages.getMessages(IMSBindingBootstrap.class);

    private static final Logger mLogger = Messages.getLogger(IMSBindingBootstrap.class);

    private InstallationContext mContext;

    private ObjectName mInstallerExtName;

    public IMSBindingBootstrap() {
    }

    public ObjectName getExtensionMBeanName() {
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00124.IMLC_Mbean_Extension", new Object[] { mInstallerExtName }));
        return mInstallerExtName;
    }

    public void init(InstallationContext installContext) throws JBIException {
		if (mLogger.isLoggable(Level.FINEST))
			mLogger.log(Level.FINEST, mMessages.getString("IMSBC-R00125.IMLC_Init_Called"));
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
            mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00123.IMLC_Mbean_Reg_Failed",
                    new Object[] { ex.getLocalizedMessage() }));

            throw new JBIException(mMessages.getString("IMSBC-E00123.IMLC_Mbean_Reg_Failed",
                    new Object[] { ex }));
        }
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00126.IMLC_Mbean_Reg_Succeeded",
					new Object[] { mInstallerExtName }));
    }

    public void onInstall() throws JBIException {
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00127.IMLC_On_Install_Called"));

        ComponentContext ctx = mContext.getContext();
        MBeanServer mbServer = ctx.getMBeanServer();
        DocumentFragment descriptorExtension = mContext.getInstallationDescriptorExtension();
        Properties defaultProperties = ConfigPersistence.parseDefaultConfig(descriptorExtension);
        ConfigPersistence.persistInitialConfig(mbServer, mInstallerExtName, ctx.getWorkspaceRoot(), defaultProperties);
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00128.IMLC_On_Install_Done"));
    }

    public void onUninstall() throws JBIException {
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00129.IMLC_On_Uninstall_Called"));
    }

    public void cleanUp() throws JBIException {
        try {
            if (mInstallerExtName != null) {
                ComponentContext ctx = mContext.getContext();
                if (ctx != null) {
                    if (ctx.getMBeanServer().isRegistered(mInstallerExtName)) {
                        ctx.getMBeanServer().unregisterMBean(mInstallerExtName);
						if (mLogger.isLoggable(Level.INFO))
							mLogger.log(Level.INFO,  mMessages.getString("IMSBC-R00130.IMLC_Mbean_Unreg_Succeeded",
									new Object[] { mInstallerExtName }));
                    }
                }
            }
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE,  mMessages.getString("IMSBC-E00131.IMLC_Mbean_Unreg_Failed", new Object[] {
                    mInstallerExtName, ex.getLocalizedMessage() }));

            throw new JBIException(mMessages.getString("IMSBC-E00131.IMLC_Mbean_Unreg_Failed",
                    new Object[] { mInstallerExtName, ex }));
        }
    }
}
