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
 * @(#)WorkflowSEBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.jbi.management.MBeanNames;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.engine.workflow.util.I18n;

public class WorkflowSEBootstrap implements Bootstrap {

    private static final Logger mLogger = Logger.getLogger(WorkflowSEBootstrap.class.getName());

    private InstallationContext mContext;
    private ObjectName mInstallerExtName;
    private WLMSEInstallerConfiguration mConfigMBean;    
    private String workspaceRoot;
    private ComponentConfig props;

    public WorkflowSEBootstrap() {
    }

    /**
     * Initializes the installation environment for a SE. This method is
     * expected to save any information from the installation context that
     * may be needed by other methods.
     * @param installContext is the context containing information from the
     * install command and from the SE jar file.
     * @throws JBIException when there is an error requiring that the
     * installation be terminated.
     */
    public void init(InstallationContext installContext)
        throws javax.jbi.JBIException {           
        mLogger.log(Level.INFO, I18n.loc("WLM-5001: Worklist Manager Service Engine bootstrap init starts"));
        mContext = installContext;
        MBeanServer mbeanServer = installContext.getContext().getMBeanServer();
        MBeanNames mbeanNames = installContext.getContext().getMBeanNames();
        String compName = installContext.getContext().getComponentName();
        workspaceRoot =  installContext.getContext().getWorkspaceRoot();
        try {
            //read the values from component configuration
            props = ComponentConfig.parse(installContext.getInstallRoot());
            mConfigMBean = new WLMSEInstallerConfiguration();
            mConfigMBean.setValues(props);

            mInstallerExtName = mbeanNames.createCustomComponentMBeanName(MBeanNames.BOOTSTRAP_EXTENSION);
            if (!mbeanServer.isRegistered(mInstallerExtName)) {
                mbeanServer.registerMBean(mConfigMBean,  mInstallerExtName);
            }
        } catch (Exception ex) {
            throw new JBIException(I18n.loc("WLM-6002: Caught exception while creating Installatoin Configuration MBean, failed to init workflowse component"), ex);
        }
        mLogger.log(Level.INFO, I18n.loc("WLM-5002: Worklist Manager Service Engine bootstrap init done"));
    }
 
    /**
     * Obtains the optional installer configuration MBean ObjectName. If none
     * is provided by this SE, returns null.
     * @return ObjectName which represents the MBean registered by the init()
     * method. If none was registered, returns null.
     */
    public ObjectName getExtensionMBeanName() {
        return mInstallerExtName;
    }

    /**
     * Called at the beginning of installation of a SE to perform any special
     * installation tasks required by the SE.
     * @throws JBIException when there is an error requiring that the
     * installation be terminated.
     */
    public void onInstall() throws JBIException {
        mLogger.log(Level.FINE, I18n.loc("WLM-3000: Worklist Manager Service Engine bootstrap onInstall starts"));
        ComponentContext ctx = mContext.getContext();
        props = mConfigMBean.getValues();
        ConfigPersistence.persistConfig(props, workspaceRoot);        
        mLogger.log(Level.FINE, I18n.loc("WLM-3001: Worklist Manager Service Engine bootstrap onInstall ends"));        
    }

    /**
     * Called at the beginning of uninstallation of a SE to perform any
     * special uninstallation tasks required by the SE.
     * @throws JBIException when there is an error requiring that
     * the uninstallation be terminated.
     */
    public void onUninstall()
        throws javax.jbi.JBIException {
        mLogger.log(Level.FINE, I18n.loc("WLM-3002: Worklist Manager Service Engine bootstrap onUninstall starts"));
        mLogger.log(Level.FINE, I18n.loc("WLM-3003:  Worklist Manager Service Engine bootstrap onUninstall ends"));    
    }
    
    /**
     * Cleans up any resources allocated by the bootstrap implementation, 
     * including performing deregistration of the extension MBean, if applicable. 
     * This method must be called after the onInstall() or onUninstall() method 
     * is called, whether it succeeds or fails. It must be called after init() 
     * is called, if init() fails by throwing an exception. 
     *
     * @throws JBIException - if the bootstrap cannot clean up allocated resources}
     */
    public void cleanUp() throws JBIException {
        try {
            if (mInstallerExtName != null) {
                ComponentContext ctx = mContext.getContext();
                if (ctx != null) {
                    if (ctx.getMBeanServer().isRegistered(mInstallerExtName)) {
                        ctx.getMBeanServer().unregisterMBean(mInstallerExtName);
                        mLogger.log(Level.FINE, I18n.loc("WLM-3004:  Worklist Manager Service Engine unregistered install Mbean"));    
                    }
                }
            }
        } catch (Exception ex) {
            throw new JBIException(I18n.loc("WLM-6003: Caught exception while unregistering MBean {0}", mInstallerExtName), ex);
        }
    }

}
