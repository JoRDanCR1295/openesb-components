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
 * @(#)MQBindingBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.bootstrap;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.jbi.management.MBeanNames;
import javax.management.InstanceAlreadyExistsException;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.mqbc.I18n;


/**
 * MQ Binding Component bootstrap implementation for the JBI framework.
 * 
 * @author Noel.Ang@sun.com
 */
public class MQBindingBootstrap implements Bootstrap {

    private Logger mLogger;
    private InstallationContext mContext;
    private ObjectName mInstallerExtName;    
    private boolean isInitialized = false;
    private InstallerExtMBean mInstallerMbean;

    public MQBindingBootstrap() {
        mLogger = Logger.getLogger(getClass().getName());
    }
    
    public ObjectName getExtensionMBeanName() {
        return mInstallerExtName;
    }

    public synchronized void init(InstallationContext installContext)
        throws JBIException {
        
        if (installContext == null) {
            throw new JBIException("0510: Failed bootstrap initialization -"
                    + " installation context is null!");
        }
      
        ComponentContext ctx = installContext.getContext();
        mLogger = Util.getLogger(ctx, getClass().getName());
        
        // Install bootstrap extension MBean.
        MBeanServer mbServer = ctx.getMBeanServer();
        MBeanNames mbNames = ctx.getMBeanNames();
        mInstallerExtName = mbNames.createCustomComponentMBeanName(
                MBeanNames.BOOTSTRAP_EXTENSION);
        if (!mbServer.isRegistered(mInstallerExtName)) {
            try {
                StandardMBean installerExtMBean = new StandardMBean(
                        new InstallerExt(), InstallerExtMBean.class);
                mbServer.registerMBean(installerExtMBean, mInstallerExtName);
                mLogger.log(Level.INFO, I18n.msg(
                        "0513: Bootstrap extension MBean for this component registered: {0}",
                        mInstallerExtName));
            } catch (InstanceAlreadyExistsException e) {
                // Already registered; ignore exception
                mLogger.log(Level.WARNING, I18n.msg(
                        "0511: Bootstrap extension MBean for this component"
                                + " already registered: {0}",
                        mInstallerExtName.getCanonicalName()));
            } catch (Exception e) {
                throw new JBIException(I18n.msg(
                        "0512: Failed registration of bootstrap extension MBean {0}",
                        mInstallerExtName), e);
            }
        }

        mContext = installContext;
        if (mInstallerMbean == null) {
            mInstallerMbean = new InstallerExt();
            mInstallerMbean.read(ComponentConfig.parse(mContext.getInstallRoot()));
        }
        isInitialized = true;
        
        mLogger.log(Level.INFO, I18n.msg(
                "0513: Bootstrap extension MBean for this component registered: {0}",
                mInstallerExtName));
    }

    public synchronized void onInstall() throws JBIException {
        if (!isInitialized) {
            throw new JBIException(I18n.msg(
                    "0514: onInstall invoked before successful initialization!"));
        }
        
        InstallationContext ictx = mContext;
        ComponentContext ctx = ictx.getContext();
        
        if (ctx == null) {
            throw new JBIException(I18n.msg("0515: Failed bootstrap onInstall -"
                    + " component context not available!"));
        }
        ConfigPersistence.persistInitialConfig(ctx.getMBeanServer(),
                mInstallerExtName,
                ctx.getWorkspaceRoot(),
                mInstallerMbean.toProperties()
        );
        
        mLogger.log(Level.INFO, "0516: Bootstrap initialization completed.");
    }

    public synchronized void onUninstall() throws JBIException {
        if (!isInitialized) {
            throw new JBIException(I18n.msg(
                    "0517: onUninstall invoked before successful initialization!"));
        }
        mLogger.log(Level.INFO, "0518: Bootstrap onUninstall completed.");
    }
    
    public synchronized void cleanUp() throws JBIException {
        if (!isInitialized) {
            throw new JBIException(I18n.msg(
                    "0522: cleanUp invoked before successful initialization!"));
        }
        ComponentContext ctx = mContext.getContext();
        ObjectName installerExtName = mInstallerExtName;
        try {
            if (installerExtName != null) {
                if (ctx != null) {
                    isInitialized = false;
                    if (ctx.getMBeanServer().isRegistered(mInstallerExtName)) {
                        ctx.getMBeanServer().unregisterMBean(mInstallerExtName);
                        mLogger.log(Level.INFO, I18n.msg(
                                "0519: Bootstrap extension MBean for this component unregistered: {0}",
                                mInstallerExtName));
                    }
                }
            }
        } catch (Exception ex) {
            throw new JBIException(I18n.msg(
                    "0520: Failed deregistration of bootstrap extension MBean {0}",
                    mInstallerExtName), ex);
        }
        mLogger.log(Level.INFO, "0521: Bootstrap cleanUp completed.");
    }
}
