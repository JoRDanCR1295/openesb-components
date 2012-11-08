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
package com.sun.jbi.engine.scriptse.jbiadapter;

import javax.jbi.management.DeploymentException;

import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.jbi.component.toolkit.lifecycle.impl.DefaultServiceUnitManager;

import com.sun.jbi.engine.scriptsecore.process.ParseXsds;


import java.util.logging.Level;

import javax.jbi.management.DeploymentException;

public class ScriptseServiceUnitManager
        extends DefaultServiceUnitManager  {
    //    private StatusProviderHelper mStatusProviderHelper;

    /**
     * DOCUMENT ME!
     *
     * @param componentCtx
     * @param emgr
     */
    public ScriptseServiceUnitManager(ManagerContext mgrCtx ){
        super(mgrCtx);
    }

    public void init(String serviceUnitName, String serviceUnitRootPath)
            throws DeploymentException {        //initialize the service class loader. 
        getManagerContext().getCustomClassLoaderUtil()
                .registerServiceClassLoader(serviceUnitName, serviceUnitRootPath);
        super.init(serviceUnitName, serviceUnitRootPath);
        
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

    /** @see javax.jbi.component.ServiceUnitManager#shutDown(java.lang.String) */
    public void shutDown(String serviceUnitName) throws DeploymentException {
        // remove any initialized service class-loader.
        getManagerContext().getCustomClassLoaderUtil()
                .unregisterServiceClassLoader(serviceUnitName);
        super.shutDown(serviceUnitName);
    }    
}
