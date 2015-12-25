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
 * @(#)MonitorManagementTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.integrationtest;


import java.util.Properties;

import javax.management.ObjectName;

import com.sun.jbi.component.test.framework.OpenESBIntegrationTestBase;
import com.sun.jbi.component.test.framework.OpenESBMBeanHelper;

/**
 *
 * Junit test cases for performance measurement implementation
 */
public class TestEricssonProjectDeploymentRegression extends OpenESBIntegrationTestBase {

    private static final String SUN_HTTP_BINDING = "sun-http-binding";

    // Simple echo SA
    // 0 provisioning endpoint
    // 1 consuming endoint
    private static final String SA = "C:/ojc/open-jbi-components/ojc-core/httpsoapbc/httpsoapbcimpl/test/com/sun/jbi/httpsoapbc/integrationtest/caps/AlarmIRPCompositeApp2.zip";
    private String saname;
    
    public TestEricssonProjectDeploymentRegression(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        saname = getAdministrationService().deployServiceAssembly(SA);
        getAdministrationService().startServiceAssembly(saname);
    }

    protected void tearDown() throws Exception {
        getAdministrationService().stopServiceAssembly(saname);
        getAdministrationService().shutdownServiceAssembly(saname);
        getAdministrationService().undeployServiceAssembly(saname);
    }
    
    /**
     * Test of getting the main WSDL for the endpoint
     */
    public void testGetWSDLDefinition() throws Exception {
        String endpoint = "http://services/fm,AlarmIRPProxy,AlarmIRPProxyPortTypePort,Consumer";
        String operation = "getWSDLDefinition";
        ObjectName statusProviderMBean = OpenESBMBeanHelper.getBindingStatusMBeanObjectName(SUN_HTTP_BINDING);
        Object [] paramVals  = new Object [] {endpoint};
        String [] paramTypes = new String [] {endpoint.getClass().getName()};
        Properties jmxConnProps = getConnectionProperties();
        String wsdl = (String)OpenESBMBeanHelper.invokeMBeanOperation(statusProviderMBean, operation, paramVals, paramTypes, jmxConnProps);
        System.out.println ("========== START wsdl\n" + wsdl + "\n============ END wsdl\n");
    }
}
