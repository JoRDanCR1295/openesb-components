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
 * @(#)CorrelationInClusterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.failover;

public class CorrelationSubBPTest extends BaseTestCase {

    private static final String TEST_FOLDER = "CorrelationSubBPTest";
    
    private static String enginesDirectory = "engines/" + TEST_FOLDER + "/";
    private static String deploymentConfigPropFile = "engines/" + TEST_FOLDER + "/CorrelationSubBP.properties";
    
    public CorrelationSubBPTest(String testName) {
        super(testName, enginesDirectory);
    }
    
    public void testCorrelationSubBPTestCase1() throws Exception {
        System.out.println("*** Starting JUnit testCorrelationSubBPTestCase1() ***\n");
        
        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/CorrelationSubBPTestCase1.properties";
        runTestCase(deploymentConfigPropFile, testPropFile, null);
        
        verifyResults();
        
        System.out.println("*** Exiting JUnit testCorrelationSubBPTestCase1() ***\n");
    }
    
    public void testCorrelationSubBPTestCase2() throws Exception {
        System.out.println("*** Starting JUnit testCorrelationSubBPTestCase2() ***\n");

        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/CorrelationSubBPTestCase2.properties";
        runTestCase(deploymentConfigPropFile, testPropFile, null);

        verifyResults();
        
        System.out.println("*** Exiting JUnit testCorrelationSubBPTestCase2() ***\n");
    }
}
