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
package com.sun.jbi.engine.mashup;

import java.util.logging.Logger;
import javax.management.ObjectName;
import javax.management.MBeanServer;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.InstallationContext;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.MBeanNames;
import com.sun.jbi.internationalization.Messages;
import javax.management.StandardMBean;
import javax.jbi.JBIException;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.engine.mashup.mbean.MashupSEInstallerConfiguration;
import com.sun.jbi.engine.mashup.mbean.MashupSEInstallerConfigurationMBean;
import java.util.logging.Level;

public class MashupSEBootstrap implements Bootstrap {

    private static final Logger mLogger = Logger.getLogger(MashupSEBootstrap.class.getName());
    private static final Messages mMessages = Messages.getMessages(MashupSEBootstrap.class);
    private InstallationContext mContext;
    private ObjectName mInstallerExtName;
    private MashupSEInstallerConfigurationMBean installerExt;
    public MashupSEBootstrap() {
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
        mContext = installContext;
        MBeanServer mbeanServer = installContext.getContext().getMBeanServer();
        MBeanNames mbeanNames = installContext.getContext().getMBeanNames();
        try {
            installerExt = new MashupSEInstallerConfiguration();
            mInstallerExtName = mbeanNames.createCustomComponentMBeanName(MBeanNames.BOOTSTRAP_EXTENSION);
            if (!mbeanServer.isRegistered(mInstallerExtName)) {
                StandardMBean stdBean = new StandardMBean(installerExt, MashupSEInstallerConfigurationMBean.class);
                mbeanServer.registerMBean(stdBean,  mInstallerExtName);
            }
            ComponentConfig defaultProperties = ComponentConfig.parse(mContext.getInstallRoot());
            installerExt.setInitialConfigurations(defaultProperties);
        } catch (Exception ex) {
            throw new JBIException(mMessages.getString("EDMSE-E0101.Installatoin_Configuration_MBean"), ex);
        }
        mLogger.info(mMessages.getString("EDMSE-I0301.calling_initMethod_Success"));
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
        ComponentContext ctx = mContext.getContext();
        ComponentConfig installProperties = installerExt.getInstallationConfigurationProperties();
        ConfigPersistence.persistConfig(installProperties, ctx.getWorkspaceRoot());
        mLogger.info(mMessages.getString("EDMSE-I0303.calling_onInstallMethod_Success"));
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("EDMSE-F0212.installation_Complete"));
        }
    }

    /**
     * Called at the beginning of uninstallation of a SE to perform any
     * special uninstallation tasks required by the SE.
     * @throws JBIException when there is an error requiring that
     * the uninstallation be terminated.
     */
    public void onUninstall()
        throws javax.jbi.JBIException {
        //mLogger.info(mMessages.getString("EDMSE-I0304.calling_onUninstallMethod"));
        mLogger.info(mMessages.getString("EDMSE-I0305.calling_onUninstallMethod_Success"));
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
                        //mLogger.info(mMessages.getString("EDMSE-I0306.unregistered_MBean"));
                    }
                }
            }
        } catch (Exception ex) {
            throw new JBIException(mMessages.getString("EDMSE-E0102.unregisterMBean_Fail")+ mInstallerExtName + ": " + ex.getMessage(), ex);
        }
    }
}
