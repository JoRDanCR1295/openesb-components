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
 * @(#)MultipleEnginesTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.failover;

public class MultipleEnginesTest extends BaseTestCase {

    private static final String TEST_FOLDER = "MultipleEnginesTest";
    String deploymentConfigPropFile = "engines/" + TEST_FOLDER + "/MultipleEnginesTest.properties";

    public MultipleEnginesTest(String testName) {
        super(testName, TEST_FOLDER);
    }

    public void testMultipleEnginesTestCase1() throws Exception {
        System.out.println("*** Starting JUnit testMultipleEnginesTestCase1() ***\n");
        
        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/TestCase1.properties";
        runTestCase(deploymentConfigPropFile, testPropFile, null);
        
        verifyResults();
        
        System.out.println("*** Exiting JUnit testMultipleEnginesTestCase1() ***\n");
    }
}
