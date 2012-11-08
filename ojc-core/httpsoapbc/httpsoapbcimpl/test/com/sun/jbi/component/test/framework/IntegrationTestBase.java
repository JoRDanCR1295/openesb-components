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
 * @(#)BaseIntegrationTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.test.framework;

import java.util.Properties;
import junit.framework.*;

import com.sun.jbi.component.test.framework.container.AdministrationService;
import com.sun.jbi.component.test.framework.container.AdministrationServiceFactory;

/**
 *
 * Base class for all JUnit "integration" tests
 */
public abstract class IntegrationTestBase extends TestCase{
    
    private AdministrationService adminService;
    private String jbiID = AdministrationServiceFactory.JBI_ID_OPENESB;

    public IntegrationTestBase() {
    }
    
    public IntegrationTestBase(String testName) {
        super(testName);
    }

    protected void setjbiID (String jbiID) throws Exception {
        if (!AdministrationServiceFactory.supportedJBIRuntime(jbiID)) {
            throw new Exception (jbiID + " is not a supported JBI runtime type");
        }
        
        this.jbiID = jbiID;
    }

    protected String getjbiID () {
        return jbiID;
    }
    
    /**
     * Runs the test case except if a similarly named method prefixed with
     * skip_ or disabled_ exists or JBI runtime is not supported
     */
    public void run(TestResult result) {
        try {
            getClass().getMethod("skip_" + getName(), new Class[] {});
            // Specially marked method apparently exists; skip it
            return;
        } catch (Exception ignore) {
        }

        try {
            // Specially marked method apparently exists; skip it
            getClass().getMethod("disabled_" + getName(), new Class[] {});
            return;
        } catch (Exception ignore) {
        }
        
        System.out.println("*** Now running test " + getName());
        super.run(result);
    }
    
    protected AdministrationService getAdministrationService() throws Exception {
        if (adminService == null) {
            Properties connectionProps = getConnectionProperties();
            adminService = AdministrationServiceFactory.getAdminService(jbiID, connectionProps);
        } 
        return adminService;
    }
    

    protected abstract Properties getConnectionProperties();
    
    // Todo: figure out how to break up SA - return WSDLs, Extensibility elements??
}
