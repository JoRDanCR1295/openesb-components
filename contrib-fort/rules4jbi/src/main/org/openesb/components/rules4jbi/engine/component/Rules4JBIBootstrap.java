/*
 * @(#)Rules4JBIBootstrap.java        $Revision: 1.5 $ $Date: 2009/01/14 02:53:12 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.engine.component;

import java.io.File;

import javax.jbi.JBIException;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import org.w3c.dom.DocumentFragment;

import nu.xom.Element;
import nu.xom.Nodes;
import nu.xom.XPathContext;

import com.google.inject.Guice;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Stage;
import com.google.inject.name.Named;

import org.openesb.components.rules4jbi.shared.logging.Logger;
import org.openesb.components.rules4jbi.shared.util.XOMUtils;

import org.openesb.components.rules4jbi.engine.guice.modules.LoggerModule;
import org.openesb.components.rules4jbi.engine.util.DOMUtils;

/**
 * Component bootstrap implementation. Note that according to the JBI spec,
 * there is no guarantee that the same instance of its Bootstrap implementation will be used
 * during both install and uninstall operations of the component. Data that need to be retained
 * between installation-time and uninstallation-time must be persisted in such a fashion that
 * a separate instance of the bootstrap class can find them, despite component or system shutdown.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.5 $ $Date: 2009/01/14 02:53:12 $
 * 
 * @since 0.1
 */
public class Rules4JBIBootstrap implements Bootstrap {
    
    private static final String JBI_CONFIGURATION_NAMESPACE = "http://www.sun.com/jbi/Configuration/V1.0";
    
    @Inject @Named("Bootstrap")
    private Logger logger = null;
    
//    @Inject
//    private Injector injector;
    
    @Inject
    private InstallationConfiguration installationConfiguration;
    
    private ObjectName extensionMBeanName;
    
    private InstallationContext installationContext;
    
    public void init(InstallationContext installationContext) throws JBIException {
        if (installationContext == null) {
            throw new JBIException("Null installation context received during bootstrap");
        }
        
        Injector injector =
                Guice.createInjector(Stage.PRODUCTION, new LoggerModule(installationContext.getContext()));
        
        injector.injectMembers(this);
        
        if (logger == null) {
            throw new JBIException("Unable to properly inject depencencies");
        }

        logger.entering(this.getClass(), "init");
        
        this.installationContext = installationContext;
        
        if (installationContext.isInstall()) {
            logger.fine("NEW INSTALLATION: %1$td.%1$tm.%1$tY at %1$tT", System.currentTimeMillis());
        }
        
        logInstallationContext(installationContext);
        
        parseInstallationDescriptorExtension(installationContext.getInstallationDescriptorExtension());
        
        registerExtensionMBean();
        
        logger.exiting(this.getClass(), "init");
    }

    private void registerExtensionMBean() throws JBIException {
        logger.fine("Registering installation configuration MBean");
        try {
            final ComponentContext componentContext = installationContext.getContext();
            
//            extensionMBeanName = new ObjectName("com.milanfort.rules4jbi", "type", "InstallationConfiguration");
        
            extensionMBeanName =
                    componentContext.getMBeanNames().createCustomComponentMBeanName("Configuration");
            
            MBeanServer mbeanServer = componentContext.getMBeanServer();
            mbeanServer.registerMBean(installationConfiguration, extensionMBeanName);
            
            logger.fine("Installation configuration MBean registration sucessfull");
            
        } catch (Exception e) {
            logger.severe("Installation configuration MBean registration failed");
            
            throw new JBIException(e);
        }
    }

    public ObjectName getExtensionMBeanName() {
        logger.entering(this.getClass(), "getExtensionMBeanName");
        
        return extensionMBeanName;
    }
    
    public void onInstall() throws JBIException {
        logger.entering(this.getClass(), "onInstall");
        
        installationConfiguration.save(new File(installationContext.getContext().getWorkspaceRoot(),
                                                InstallationConfiguration.CONFIG_FILE_NAME));
    }

    public void onUninstall() throws JBIException {
        logger.entering(this.getClass(), "onUninstall");
        
        /* 
         * No need to delete the configuration file here;
         * the JBI runtime will delete the whole workspace anyway.
         */
    }

    public void cleanUp() throws JBIException {
        logger.entering(this.getClass(), "cleanUp");

        unregisterExtensionMBean();
    }

    private void unregisterExtensionMBean() throws JBIException {
        logger.fine("Unregistering installation configuration MBean");
        try {
            MBeanServer mbeanServer = installationContext.getContext().getMBeanServer();
            mbeanServer.unregisterMBean(extensionMBeanName);

            logger.fine("Installation configuration MBean unregistration sucessfull");

        } catch (Exception e) {
            logger.severe("Installation configuration MBean unregistration failed");

            throw new JBIException(e);
        }
    }
    
    private void logInstallationContext(InstallationContext installationContext) {
        logger.config("--- Begin Installation Context Info ---");
        
        logger.config("Component_name: %s; %s", 
                installationContext.getComponentName(), 
                installationContext.isInstall() ? "installing" : "not installing");
        
        logger.config("Component_class_name: %s", installationContext.getComponentClassName());
        logger.config("Class_path_elements: %s", installationContext.getClassPathElements());
        logger.config("Installation_root_directory: %s", installationContext.getInstallRoot());
        logger.config("Workspace_root_directory: %s", installationContext.getContext().getWorkspaceRoot());

        org.w3c.dom.DocumentFragment doc = installationContext.getInstallationDescriptorExtension();

        String configString = DOMUtils.documentFragmentToString(doc);
        logger.config("Descriptor_extension: %s", configString);
        
        logger.config("--- End Installation Context Info ---");
    }
    
    private void parseInstallationDescriptorExtension(DocumentFragment extension) {
        logger.fine("Parsing installation descriptor extension");
        
        try {
            Element configuration = XOMUtils.toElement(DOMUtils.documentFragmentToString(extension));
            
            int poolSize = getConfigurationValue(
                    configuration, "PoolSize", InstallationConfiguration.DEFAULT_POOL_SIZE);
           
            int maxServiceUnits = getConfigurationValue(
                    configuration, "MaxServiceUnits", InstallationConfiguration.DEFAULT_MAX_SERVICE_UNITS);
            
            logger.config("Retrieved pool size: %d", poolSize);
            logger.config("Retrieved max service units: %d", maxServiceUnits);
            
            installationConfiguration.setPoolSize(poolSize);
            installationConfiguration.setMaxServiceUnits(maxServiceUnits);
            
        } catch (Exception e) {
            logger.severe("Unable to parse installation descriptor extension - using default values");
        }
    }

    static int getConfigurationValue(Element configuration, String propertyName, int defaultValue) {
        XPathContext context = new XPathContext("config", JBI_CONFIGURATION_NAMESPACE);

        try {
            Nodes query = configuration.query(
                    "//config:Property[@name='" + propertyName + "']/@defaultValue", context);
            
            return Integer.parseInt(query.get(0).getValue());    
            
        } catch (Exception e) {
            return defaultValue;
        }
    }
}
