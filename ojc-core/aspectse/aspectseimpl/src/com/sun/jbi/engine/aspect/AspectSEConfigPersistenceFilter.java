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
 * @(#)AspectSEConfigPersistenceFilter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect;

import java.io.File;
import java.util.Properties;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.jbi.management.MBeanNames;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import org.w3c.dom.DocumentFragment;

import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.crl.lifecycle.BootstrapFilterChain;
import com.sun.jbi.crl.lifecycle.ConfigPersistenceFilter;
import com.sun.jbi.engine.aspect.utils.AnnotatedStandardMBean;




/**
 * @author Sujit Biswas
 *
 */
public class AspectSEConfigPersistenceFilter extends ConfigPersistenceFilter {
    static final String PROP_CONFIG_FILE = "config.properties";

    ObjectName extensionMBeanName;

    private static final Logger logger = Logger
            .getLogger(AspectSEConfigPersistenceFilter.class.getName());

    private InstallationContext context;

    private AspectSEComponentConfiguration installerConfigMBean;

    /**
     * Creates a new instance of AspectSEConfigPersistenceFilter
     */
    public AspectSEConfigPersistenceFilter(BootstrapFilterChain chain) {
        super(chain);
    }

    public void setInstallerConfigMBean(
            AspectSEComponentConfiguration installerMBean) {
        this.installerConfigMBean = installerMBean;
    }

    /** @see javax.jbi.component.Bootstrap#getExtensionMBeanName() */
    public ObjectName getExtensionMBeanName() {
        return extensionMBeanName;
    }

    public void setExtensionMBeanName(ObjectName objectName) {
        this.extensionMBeanName = objectName;
    }

    /** @see com.sun.jbi.crl.lifecycle.AbstractBootstrapFilter#onInstall() */
    public void onInstall() throws JBIException {
        ComponentContext ctx = getChain().getComponentContext();
        MBeanServer mbServer = ctx.getMBeanServer();
        DocumentFragment descriptorExtension = getChain()
                .getInstallationContext().getInstallationDescriptorExtension();
        Properties defaultProperties = ConfigPersistence
                .parseDefaultConfig(descriptorExtension);
        ConfigPersistence.persistInitialConfig(mbServer,
                getExtensionMBeanName(), ctx.getWorkspaceRoot(),
                defaultProperties);

        String workspaceRoot = ctx.getWorkspaceRoot();
        String configFile = workspaceRoot + File.separator + PROP_CONFIG_FILE;
        if (!installerConfigMBean.save(configFile)) {
            throw new JBIException(
                    "AspectSEConfigPersistenceFilter.Caught_exception_while_storing_configuration_properties_file");
        }
        logger
                .info("AspectSEConfigPersistenceFilter.onInstall_method_has_been_called");

    }

    /** @see com.sun.jbi.crl.lifecycle.BootstrapFilter#init(javax.jbi.component.InstallationContext) */
    public void init(InstallationContext installContext) throws JBIException {
        context = installContext;

        try {
            // Read default properties from jbi.xml
            org.w3c.dom.DocumentFragment descriptorExtension = context
                    .getInstallationDescriptorExtension();
            Properties defaultProperties = ConfigPersistence
                    .parseDefaultConfig(descriptorExtension);

            // create installer config. mbean and initialize it with default
            // properteis
            installerConfigMBean = new AspectSEComponentConfiguration();
            installerConfigMBean.restore(defaultProperties);

            // publish installer config. mbean for user to configure at
            // installation time.
            MBeanServer mbeanServer = context.getContext().getMBeanServer();
            MBeanNames mbeanNames = context.getContext().getMBeanNames();
            this.extensionMBeanName = mbeanNames
                    .createCustomComponentMBeanName(MBeanNames.BOOTSTRAP_EXTENSION);
            if (!mbeanServer.isRegistered(this.extensionMBeanName)) {
                Object stdBean = new AnnotatedStandardMBean(
                        installerConfigMBean,
                        AspectSEComponentConfigurationMBean.class);
                mbeanServer.registerMBean(stdBean, this.extensionMBeanName);
            }
        } catch (Exception ex) {
            throw new JBIException(
                    "AspectSEConfigPersistenceFilter.Caught_exception_while_creating_Installatoin_Configuration_MBean_failed_to_init_component",
                    ex);
        }

        logger
                .info("AspectSEConfigPersistenceFilter.init_method_has_been_called");
    }

    /**
     * Cleans up any resources allocated by the bootstrap implementation,
     * including performing deregistration of the extension MBean, if
     * applicable. This method must be called after the onInstall() or
     * onUninstall() method is called, whether it succeeds or fails. It must be
     * called after init() is called, if init() fails by throwing an exception.
     * 
     * @throws JBIException -
     *             if the bootstrap cannot clean up allocated resources}
     */
    public void cleanUp() throws JBIException {
        try {
            if (this.extensionMBeanName != null) {
                ComponentContext ctx = context.getContext();
                if (ctx != null) {
                    if (ctx.getMBeanServer().isRegistered(
                            this.extensionMBeanName)) {
                        ctx.getMBeanServer().unregisterMBean(
                                this.extensionMBeanName);
                        logger
                                .info("AspectSEConfigPersistenceFilter.Unregistered_MBean: "
                                        + this.extensionMBeanName);
                    }
                }
            }
        } catch (Exception ex) {
            throw new JBIException(
                    "AspectSEConfigPersistenceFilter.Failed_to_unregister_MBean: "
                            + this.extensionMBeanName, ex);
        }
    }

}
