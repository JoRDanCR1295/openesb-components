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
 * @(#)OpenESBIntegrationTestBase.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.test.framework;

import com.sun.jbi.component.test.framework.container.openesb.OpenESBAdministrationService;
import com.sun.jbi.component.test.framework.container.openesb.OpenESBAdminServiceConnectionSpec;

import java.io.File;
import java.util.Properties;
import junit.framework.*;

/**
 *
 * Base class for all JUnit "integration" tests
 */
public abstract class OpenESBIntegrationTestBase extends IntegrationTestBase {
    
    private String saZipFilename;
    private File   saZipFile;
    private File   saZipFileOrig;

    public OpenESBIntegrationTestBase() {
    }
    
    public OpenESBIntegrationTestBase(String testName) {
        super(testName);
    }

    protected void setSAZipFilename (String saZipFilename) {
        this.saZipFilename = saZipFilename;
    }

    protected String getServiceAssemblyName () {
        return saZipFilename;
    }
    
    protected Properties getConnectionProperties () {
        Properties connectionProperties = new Properties();
        connectionProperties.setProperty(OpenESBAdminServiceConnectionSpec.CONN_PROP_HOST, "localhost");
        connectionProperties.setProperty(OpenESBAdminServiceConnectionSpec.CONN_PROP_PORT, "8686");
        connectionProperties.setProperty(OpenESBAdminServiceConnectionSpec.CONN_PROP_USERNAME, "admin");
        connectionProperties.setProperty(OpenESBAdminServiceConnectionSpec.CONN_PROP_PASSWORD, "adminadmin");
        connectionProperties.setProperty(OpenESBAdminServiceConnectionSpec.TARGET_NAME, "server");
        return connectionProperties;
    }
    
    /**
     * Check to see if JBI runtime is running first...
     */
    public void run(TestResult result) {
        try {
            if (getAdministrationService() instanceof OpenESBAdministrationService && 
                !((OpenESBAdministrationService)getAdministrationService()).isJBIRuntimeEnabled()) {
                System.out.println("*** Skipping " + getName() + ": JBI runtime is not enabled");
            }
        } catch (Exception ignore) {
        }
        super.run(result);
    }
}
