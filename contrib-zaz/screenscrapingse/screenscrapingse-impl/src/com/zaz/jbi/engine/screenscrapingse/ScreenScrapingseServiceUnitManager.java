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

import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.component.lifecycle.impl.AbstractServiceUnitManager;
import com.sun.jbi.crl.mep.ManagerContext;

import com.zaz.jbi.engine.screenscrapingse.process.ParseXsds;


import java.util.logging.Level;

import javax.jbi.management.DeploymentException;

public class ScreenScrapingseServiceUnitManager
        extends AbstractServiceUnitManager {
    //    private StatusProviderHelper mStatusProviderHelper;

    /**
     * DOCUMENT ME!
     *
     * @param componentCtx
     * @param emgr
     */
    public ScreenScrapingseServiceUnitManager(ManagerContext componentCtx,
            EndpointManager emgr) {
        super(componentCtx, emgr);
    }

    //    void initialize(StatusProviderHelper statusProviderHelper) {
    //        mStatusProviderHelper = statusProviderHelper;
    //    }
    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractServiceUnitManager#init(java.lang.String,
     *      java.lang.String)
     */
    public void init(String serviceUnitName, String serviceUnitRootPath)
            throws DeploymentException {
        log().info("Initializing service unit " + serviceUnitName);
        log().info("Initializing service unit:: ServiceUnitRoot " +
                serviceUnitRootPath);

        super.init(serviceUnitName, serviceUnitRootPath);

        //Trying to intialize the JAR FILE NAME HERE..
        if (log().isLoggable(Level.INFO)) {
            log().info("Initialized service unit " + serviceUnitName +
                    " serviceUnitRootPath: " + serviceUnitRootPath +
                    " successfully.");
        }
    }

    public String deploy(String serviceUnitName, String serviceUnitRootPath)
            throws DeploymentException {
        try {
            ParseXsds parse = new ParseXsds();
            parse.parseForXsds(serviceUnitRootPath);
        } catch (Exception e) {
        }

        return super.deploy(serviceUnitName, serviceUnitRootPath);
    }
}
