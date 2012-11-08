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
 * @(#)SNMPBindingBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc;

import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.internationalization.Messages;

import org.w3c.dom.DocumentFragment;

import javax.jbi.JBIException;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.management.ObjectName;

import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * SNMP BC bootstrap implementation
 *
 * @author echou
 */
public class SNMPBindingBootstrap implements Bootstrap {

    private static final Messages mMessages =
        Messages.getMessages(SNMPBindingBootstrap.class);
    private static final Logger mLogger = Logger.getLogger(SNMPBindingBootstrap.class.getName());
    
    private InstallationContext mContext;
    
    public SNMPBindingBootstrap() {
    }
    
    public ObjectName getExtensionMBeanName() {
        return null;
    }

    public void init(InstallationContext installContext) throws JBIException {
        mContext = installContext;
    }

    public void onInstall() throws JBIException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "SNMPBC_R101.ON_INSTALL_CALLED");
        }

        ComponentContext ctx = mContext.getContext();   
        DocumentFragment descriptorExtension = mContext.getInstallationDescriptorExtension();
        Properties defaultProperties = ConfigPersistence.parseDefaultConfig(descriptorExtension);
        ConfigPersistence.persistConfig(ctx.getWorkspaceRoot(), defaultProperties);

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "SNMPBC_R102.ON_INSTALL_DONE");
        }
    }

    public void onUninstall() throws JBIException {
    }
    
    public void cleanUp() throws JBIException {
    }
}
