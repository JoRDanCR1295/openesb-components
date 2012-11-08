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

public class CorrelationInClusterTest extends BaseTestCase {

    private static final String TEST_FOLDER = "CorrelationInClusterTest";
    private static String deploymentConfigPropFile = "engines/" + TEST_FOLDER + "/CorrelationInClusterTest.properties";


    public CorrelationInClusterTest(String testName) {
        super(testName, TEST_FOLDER);
    }

    public void testCorrelationInClusterTestCase1() throws Exception {
        System.out.println("*** Starting JUnit testCorrelationInClusterTestCase1() ***\n");
        
        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/CorrelationInClusterTestCase1.properties";
        runTestCase(deploymentConfigPropFile, testPropFile, null);
        
        /* verify results */
        verifyResults();
        
        System.out.println("*** Exiting JUnit testCorrelationInClusterTestCase1() ***\n");
    }


    /**
	 * Adding Junit for clustering for regression testing of the following issue
	 * https://open-jbi-components.dev.java.net/issues/show_bug.cgi?id=365
	 * 
	 * 1. Feed multiple messages to the same engine with same correlation id. 
	 * 2. This project contains two receives. Hence two sets of messages will be sent, 
	 * time spaced. The way the test is structured that after first message 
	 * (of first set), when the execution reaches second receive, 
	 * the instance will be passivated (as per design). The second message 
	 * (to be send 5 seconds later) of the first set will activate the instance 
	 * and the instance will complete. 
	 * 3. The first message of the second set (5 seconds after the second message 
	 * of first set) with same correlation will be fed. This will cause the 
	 * instance to passivate. When the correlated message is sent (5 seconds later), the
	 * instance will be picked for further execution and the second instance
	 * should complete successfully. 
	 * 4. The test in the end will verify that there are two completed instances 
	 * in the database for success of the test.
	 * 
	 * @throws Exception
	 */
    public void testCorrelationInClusterTestCase2() throws Exception {
        System.out.println("*** Starting JUnit testCorrelationInClusterTestCase2() ***\n");
        
        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/CorrelationInClusterTestCase2.properties";
        runTestCase(deploymentConfigPropFile, testPropFile, null);
        
        /* verify results */
        verifyResults();
        
        System.out.println("*** Exiting JUnit testCorrelationInClusterTestCase2() ***\n");
    }
}