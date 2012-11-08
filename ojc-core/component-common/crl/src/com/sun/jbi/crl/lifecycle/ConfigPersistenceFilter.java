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
 * @(#)ConfigPersistenceFilter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.lifecycle;

import java.util.Properties;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.management.MBeanServer;

import org.w3c.dom.DocumentFragment;

import com.sun.jbi.component.config.ConfigPersistence;
import com.sun.jbi.crl.util.I18n;

/**
 * Bootstrap filter which persists component configuration during installation.
 * 
 * @author Kevan Simpson
 */
public class ConfigPersistenceFilter extends AbstractBootstrapFilter {
    public ConfigPersistenceFilter(BootstrapFilterChain chain) {
        super(chain);
    }

    /** @see com.sun.jbi.crl.lifecycle.AbstractBootstrapFilter#onInstall() */
    public void onInstall() throws JBIException {
        ComponentContext ctx = getChain().getComponentContext();
        MBeanServer mbServer = ctx.getMBeanServer();        
        DocumentFragment descriptorExtension = 
                getChain().getInstallationContext().getInstallationDescriptorExtension();
        Properties defaultProperties = ConfigPersistence.parseDefaultConfig(descriptorExtension);
        ConfigPersistence.persistInitialConfig(mbServer, 
                                               getExtensionMBeanName(), 
                                               ctx.getWorkspaceRoot(), 
                                               defaultProperties);
        if (log().isLoggable(Level.CONFIG)) {
        	log().config(I18n.loc(
        			"CRL-4002: Initial component configuration for {0} persisted successfully: {1}", 
        			getChain().getComponentContext().getComponentName(),
        			String.valueOf(defaultProperties)));
        }
    }
}
