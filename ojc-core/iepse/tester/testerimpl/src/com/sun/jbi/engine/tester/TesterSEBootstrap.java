/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.tester;

import com.sun.jbi.configuration.ConfigPersistence;
import java.util.Properties;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.jbi.management.MBeanNames;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;

/**
 *
 * @author radval
 */
public class TesterSEBootstrap implements Bootstrap {

    private Logger mLogger = Logger.getLogger(TesterSEBootstrap.class.getName());
    
    private InstallationContext mInstallationContext;
    
    private ObjectName mInstallerExtName;
    
    public void init(InstallationContext installContext) throws JBIException {
        
        mInstallationContext = installContext;
        MBeanServer mbeanServer = installContext.getContext().getMBeanServer();
        MBeanNames mbeanNames = installContext.getContext().getMBeanNames();
        String compName = installContext.getContext().getComponentName();
        try {
            TesterInstallerConfigurationMBean installerExt = new TesterInstallerConfigurationMBean();
            mInstallerExtName = mbeanNames.createCustomComponentMBeanName(MBeanNames.BOOTSTRAP_EXTENSION);
            if (!mbeanServer.isRegistered(mInstallerExtName)) {
                StandardMBean stdBean = new StandardMBean(installerExt, TesterInstallerConfiguration.class);
                mbeanServer.registerMBean(stdBean,  mInstallerExtName);
            }
        } catch (Exception ex) {
            throw new JBIException("Caught exception while creating Installatoin Configuration MBean, failed to init tester component", ex);
        }
        mLogger.info("init method has been called");
        
    }

    public void cleanUp() throws JBIException {
        
    }

    public ObjectName getExtensionMBeanName() {
        return null;
    }

    public void onInstall() throws JBIException {
        mLogger.info("Calling onInstall method");
        ComponentContext ctx = mInstallationContext.getContext();
        MBeanServer mbServer = ctx.getMBeanServer();
        org.w3c.dom.DocumentFragment descriptorExtension = mInstallationContext.getInstallationDescriptorExtension();
        Properties defaultProperties = ConfigPersistence.parseDefaultConfig(descriptorExtension);
        ConfigPersistence.persistInitialConfig(mbServer, mInstallerExtName, ctx.getWorkspaceRoot(), defaultProperties);
        mLogger.info("onInstall method has been called");
    }

    public void onUninstall() throws JBIException {
        
    }

}
