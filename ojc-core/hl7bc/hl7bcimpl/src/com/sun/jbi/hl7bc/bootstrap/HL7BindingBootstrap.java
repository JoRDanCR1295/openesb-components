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
 * @(#)HL7BindingBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.bootstrap;

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

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.hl7bc.I18n;
import com.sun.jbi.common.util.Util;

/**
 * HL7 Binding Component bootstrap implementation for the JBI framework
 * 
 * @author S. Nageswara Rao
 */
public class HL7BindingBootstrap implements Bootstrap {

	private static Logger mLogger = Logger.getLogger(HL7BindingBootstrap.class.getName());;

	private InstallationContext mContext;

	private ObjectName mInstallerExtName;

	private InstallerExtMBean mInstallerMBean;

	public HL7BindingBootstrap() {
        //mLogger = Logger.getLogger(HL7BindingBootstrap.class.getName());
	}

	public ObjectName getExtensionMBeanName() {
		mLogger.log(Level.INFO, I18n.msg(
                    "I0101: Extension MBean name: {0}.",
                    mInstallerExtName));
		return mInstallerExtName;
	}

	public void init(InstallationContext installContext) throws JBIException {

		mContext = installContext;
		ComponentContext ctx = installContext.getContext();
        mLogger = Util.getLogger(ctx, HL7BindingBootstrap.class.getName());
		if (mLogger.isLoggable(Level.FINE)) {
			mLogger.log(Level.FINE, I18n.msg("Init method has been called"));
		}

		MBeanServer mbServer = ctx.getMBeanServer();
		MBeanNames mbNames = ctx.getMBeanNames();

		mInstallerExtName = mbNames
				.createCustomComponentMBeanName(MBeanNames.BOOTSTRAP_EXTENSION);

		mInstallerMBean = new InstallerExt();

		try {
			if (!mbServer.isRegistered(mInstallerExtName)) {
				StandardMBean installerExtMBean = new StandardMBean(
						mInstallerMBean, InstallerExtMBean.class);
				mbServer.registerMBean(installerExtMBean, mInstallerExtName);
			}

			ComponentConfig defaultProperties = ComponentConfig.parse(mContext
					.getInstallRoot());
			mInstallerMBean.setInitialConfigurations(defaultProperties);
		} catch (Exception ex) {
			throw new JBIException(I18n.msg(
                        "E0100: Failed to register installer extension MBean {0}",
                        mInstallerExtName), ex);
		}
		if (mLogger.isLoggable(Level.FINE)) {
			mLogger
					.log(Level.FINE, I18n.msg("MBean registered for {0}",
							mInstallerExtName));
		}
	}

	public void onInstall() throws JBIException {
		if (mLogger.isLoggable(Level.FINE)) {
			mLogger.log(Level.FINE, I18n.msg("entering onInstall..."));
		}
		ComponentContext ctx = mContext.getContext();
		MBeanServer mbServer = ctx.getMBeanServer();
		ComponentConfig installProperties = mInstallerMBean
				.getInstallationConfigurationProperties();
		ConfigPersistence.persistConfig(installProperties, ctx
				.getWorkspaceRoot());

		if (mLogger.isLoggable(Level.FINE)) {
			mLogger.log(Level.FINE, I18n.msg("onInstall completed"));
		}
	}

	public void onUninstall() throws JBIException {
		if (mLogger.isLoggable(Level.FINE)) {
			mLogger.log(Level.FINE, I18n.msg("entering onUninstall..."));
			mLogger.log(Level.FINE, I18n.msg("onUninstall completed"));
		}
	}

	public void cleanUp() throws JBIException {
		try {
			if (mInstallerExtName != null) {
				ComponentContext ctx = mContext.getContext();
				if (ctx != null) {
					MBeanServer mbServer = ctx.getMBeanServer();
					if (mbServer.isRegistered(mInstallerExtName)) {
						mbServer.unregisterMBean(mInstallerExtName);
						if (mLogger.isLoggable(Level.FINE)) {
							mLogger.log(Level.FINE,
									I18n.msg(" MBean unregistered", mInstallerExtName));
						}
					}
				}
			}
		} catch (Exception ex) {
			throw new JBIException(I18n.msg(
                        "E0101: Failed to unregister MBean {0}",
                        mInstallerExtName), ex);
		}
	}

	/**
	 * Package protected method. Used solely for JUnit test purposes
	 */
	void setInstallationContext(InstallationContext context) {
		mContext = context;
	}

	/**
	 * Package protected method. Used solely for JUnit test purposes
	 */
	void setMBeanObjectName(ObjectName objectName) {
		mInstallerExtName = objectName;
	}
}
