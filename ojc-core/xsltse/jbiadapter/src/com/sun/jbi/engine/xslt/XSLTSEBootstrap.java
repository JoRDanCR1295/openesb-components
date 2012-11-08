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
 * @(#)XSLTSEBootstrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.xslt;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;

import com.sun.jbi.component.toolkit.lifecycle.AbstractBootstrap;
import com.sun.jbi.component.toolkit.lifecycle.impl.AcceptPoller;

/**
 * Bootstrap implementation for XSLT Service Engine.
 * 
 * @author Kevan Simpson
 */
public class XSLTSEBootstrap extends AbstractBootstrap {
    public XSLTSEBootstrap() {
    }
    
    
    /** @see com.sun.jbi.component.toolkit.lifecycle.AbstractBootstrap#initInstallMBean(javax.jbi.component.ComponentContext) */
    protected Object initInstallMBean(ComponentContext ctx) throws JBIException {
        try {
            // initialize config to avoid broadcasting changes unnecessarily
            getConfig().getProperty(AcceptPoller.POLLER_COUNT_PROPERTY)
                    .setValue(String.valueOf(AcceptPoller.DEFAULT_POLLER_COUNT));
            // pass config to avoid duplicate initialization from descriptor by AcceptConfig
            return new XsltSEConfig(ctx, getConfig());
        }
        catch (Exception e) {
            throw error(e, "XSLTSE-6003: XsltSEBootstrap installation MBean failed to initialize: {0}",
                        e.getMessage());
        }
    }

    /*
     * The version of initializeMBean is for when XsltSE supports AppVars + AppConfig
     */
//    protected AcceptManagerMBean initializeMBean(ComponentContext ctx)
//            throws JBIException {
//        try {
//            // initialize config to avoid broadcasting changes unnecessarily
//            getConfig().getProperty(PollerConfigMBean.POLLER_COUNT_PROPERTY)
//                    .setValue(String.valueOf(PollerConfigMBean.DEFAULT_POLLER_COUNT));
//            // pass config to avoid duplicate initialization from descriptor by AcceptConfig
//            return new AcceptConfig(ctx, getConfig());
//        }
//        catch (Exception e) {
//            throw error(e, "XSLTSE-6011: XsltSEBootstrap installation MBean failed to initialize: {0}",
//                        e.getMessage());
//        }
//    }
}
