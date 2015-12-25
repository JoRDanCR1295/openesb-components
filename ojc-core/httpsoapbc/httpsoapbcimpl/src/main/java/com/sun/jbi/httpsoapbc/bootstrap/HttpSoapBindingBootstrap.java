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
 * @(#)HttpSoapBindingBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.bootstrap;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.internationalization.Messages;

import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Properties;

import javax.jbi.component.Bootstrap;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.jbi.JBIException;
import javax.jbi.management.MBeanNames;

import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;

import org.w3c.dom.DocumentFragment;

/**
 * HTTP SOAP binding component bootstrap implementation for the JBI framework
 * @see BindingBootstrap
 */
public class HttpSoapBindingBootstrap implements Bootstrap {

    private static final Messages mMessages =
        Messages.getMessages(HttpSoapBindingBootstrap.class);
    private Logger mLogger;

    private InstallationContext mContext;
    private ObjectName mInstallerExtName;   
    private InstallerExtMBean mInstallerMBean;
   
    public HttpSoapBindingBootstrap() {
    }

    public ObjectName getExtensionMBeanName() {
        return mInstallerExtName;
    }

    public void init(InstallationContext installContext)
        throws JBIException {

        mContext = installContext;
        
        ComponentContext ctx = installContext.getContext();
        Messages.registerContext(ctx);
        mLogger = Messages.getLogger(HttpSoapBindingBootstrap.class);
        if (mLogger.isLoggable(Level.FINEST)) {
            mLogger.log(Level.FINEST, "Init method has been called");
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
            String text = mMessages.getString("HTTPBC-W00110.InstallerMBean_registration_failed");
            throw new JBIException(text, ex);
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Registered InstallerExtMBean " + mInstallerExtName);
        }
    }

    public void onInstall() throws JBIException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "onInstall running");
        }
        
        ComponentContext ctx = mContext.getContext();
        MBeanServer mbServer = ctx.getMBeanServer();    
        ComponentConfig installProperties = mInstallerMBean.getInstallationConfigurationProperties();
        ConfigPersistence.persistConfig(installProperties, ctx.getWorkspaceRoot());
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Completed installation");
        }
    }

    public void onUninstall() throws JBIException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"onUninstall running");
            mLogger.log(Level.FINE,"Completed uninstallation");
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
                            mLogger.log(Level.FINE, "Unregistered MBean " + mInstallerExtName);
                        }
                    }
                }
            }
        } catch (Exception ex) {
            String text = mMessages.getString("HTTPBC-W00111.InstallerMBean_deregistration_failed", mInstallerExtName);
            throw new JBIException(text, ex);
        }
    }    
}
