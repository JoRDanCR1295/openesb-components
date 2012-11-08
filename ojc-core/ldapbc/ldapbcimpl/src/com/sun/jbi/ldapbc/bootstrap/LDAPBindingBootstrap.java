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
 * @(#)LDAPBindingBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ldapbc.bootstrap;

import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;
import javax.jbi.JBIException;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import com.sun.jbi.common.qos.config.ComponentConfig;
import javax.jbi.management.MBeanNames;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;
import org.w3c.dom.DocumentFragment;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.internationalization.Messages;


/**
 * LDAP binding component bootstrap implementation for the JBI framework
 */
public class LDAPBindingBootstrap implements Bootstrap {
    private static final Messages mMessages = Messages.getMessages(LDAPBindingBootstrap.class);
    private static final Logger mLogger = Messages.getLogger(LDAPBindingBootstrap.class);
    private InstallationContext mContext;
    ObjectName mInstallerExtName;
	private InstallerExtMBean installerExt;

    public LDAPBindingBootstrap() {
    }

    /**
     *
     * @return
     */
    public ObjectName getExtensionMBeanName() {
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00109.LDLC_Mbean_Extension", new Object[] { mInstallerExtName }));
        return mInstallerExtName;
    }

    /**
     *
     * @param installContext
     * @throws JBIException
     */
    public void init(final InstallationContext installContext)
        throws JBIException {
        mContext = installContext;

        final ComponentContext ctx = installContext.getContext();

        Messages.registerContext(ctx);
		if (mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00110.LDLC_Init_Called"));

        final MBeanServer mbServer = ctx.getMBeanServer();
        final MBeanNames mbNames = ctx.getMBeanNames();
        mInstallerExtName = mbNames.createCustomComponentMBeanName(MBeanNames.BOOTSTRAP_EXTENSION);

        installerExt = new InstallerExt();

        try {
            if (!ctx.getMBeanServer().isRegistered(mInstallerExtName)) {
                final StandardMBean installerExtmBean = new StandardMBean(installerExt,
                        InstallerExtMBean.class);
                ctx.getMBeanServer()
                   .registerMBean(installerExtmBean, mInstallerExtName);
                ComponentConfig defaultProperties = ComponentConfig.parse(mContext.getInstallRoot());
				installerExt.setInitialConfigurations(defaultProperties);
            }
        } catch (final Exception ex) {
        	ex.printStackTrace();
		   mLogger.log(Level.WARNING, mMessages.getString("LDAPBC-W00101.LDLC_Mbean_Reg_Failed",
                    new Object[] { ex.getLocalizedMessage() }));
		   throw new JBIException(mMessages.getString("LDAPBC-W00101.LDLC_Mbean_Reg_Failed",
                    new Object[] { ex }));
        }

		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00111.LDLC_Mbean_Reg_Succeeded",
					new Object[] { mInstallerExtName }));
    }

    /**
     *
     * @throws JBIException
     */
    public void onInstall() throws JBIException {
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00112.LDLC_On_Install_Called"));        

        final ComponentContext ctx = mContext.getContext();
        final MBeanServer mbServer = ctx.getMBeanServer();
        /*final DocumentFragment descriptorExtension = mContext.getInstallationDescriptorExtension();
        final Properties defaultProperties = ConfigPersistence.parseDefaultConfig(descriptorExtension);
		*/
       /* ConfigPersistence.persistInitialConfig(mbServer, mInstallerExtName,
            ctx.getWorkspaceRoot(), defaultProperties);
		*/
		
		
		ConfigPersistence.persistConfig(installerExt.getInstallationConfigurationProperties(),
				ctx.getWorkspaceRoot());
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00113.LDLC_On_Install_Done"));        
    }

    /**
     *
     * @throws JBIException
     */
    public void onUninstall() throws JBIException {
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00114.LDLC_On_Uninstall_Called"));
        if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00115.LDLC_On_Unistall_Called"));        
    }

    /**
     *
     * @throws JBIException
     */
    public void cleanUp() throws JBIException {
        try {
            if (mInstallerExtName != null) {
                final ComponentContext ctx = mContext.getContext();

                if ((ctx != null) &&
                        (ctx.getMBeanServer().isRegistered(mInstallerExtName))) {
                    ctx.getMBeanServer().unregisterMBean(mInstallerExtName);
					if (mLogger.isLoggable(Level.INFO))
						mLogger.log(Level.INFO,  mMessages.getString("LDAPBC-R00116.LDLC_Mbean_Unreg_Succeeded", new Object[] { mInstallerExtName }));                    
                }
            }
        } catch (final Exception ex) {
			mLogger.log(Level.SEVERE,  mMessages.getString("LDAPBC-E00101.LDLC_Mbean_Unreg_Failed", new Object[] {
					mInstallerExtName, ex.getLocalizedMessage() }));
            throw new JBIException(mMessages.getString("LDAPBC-E00101.LDLC_Mbean_Unreg_Failed",
                    new Object[] { mInstallerExtName, ex }));
        }
    }
}
