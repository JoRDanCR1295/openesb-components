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
 * Copyright 2007-2008 ZAZ Consulting, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.zaz.jbi.engine.screenscrapingse;

import com.sun.jbi.crl.lifecycle.BootstrapFilterChain;
import com.sun.jbi.crl.lifecycle.ConfigPersistenceFilter;
import com.sun.jbi.crl.lifecycle.DefaultBootstrapFilter;

import javax.management.NotCompliantMBeanException;
import javax.management.StandardMBean;

/**
 * Bootstrap implementation for Telnet Service Engine.
 * @author ZAZ Consulting
 */
public class ScreenScrapingseBootstrap extends BootstrapFilterChain { //implements Bootstrap {


    /**
     * Creates a new ScreenScrapingseBootstrap object.
     */
    public ScreenScrapingseBootstrap() {
        StandardMBean mbean = null;

        try {
            mbean = new StandardMBean(new ScreenScrapingseInstallerConfigurationMBean(),
                    ScreenScrapingseConfigurationMBean.class);
        } catch (NotCompliantMBeanException ncme) {
            // log error
        }

        addFilter(new DefaultBootstrapFilter(this, mbean));
        addFilter(new ConfigPersistenceFilter(this));
    }
}
