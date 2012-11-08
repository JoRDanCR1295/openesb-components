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
 * @(#)SimpleInvokeFailoverTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.failover;

import junit.framework.TestSuite;


/**
 * @author mbhasin
 *
 */
public class SimpleInvokeFailoverTest extends BaseTestCase {
    
    private static final String TEST_FOLDER = "SimpleInvokeFailoverTest";
    String deploymentConfigPropFile = "engines/" + TEST_FOLDER + "/SimpleInvokeFailoverTest.properties";
    
    public SimpleInvokeFailoverTest(String testName) {
        super(testName, TEST_FOLDER);
    }

    public static TestSuite suite() {
        return new TestSuite(SimpleInvokeFailoverTest.class);
    }

    public void testSimpleFailoverTestCase1() throws Exception {
        System.out.println("*** Starting JUnit testSimpleFailoverTestCase1() ***\n");
        
        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/TestCase1.properties";
        runTestCase(deploymentConfigPropFile, testPropFile, null);
        
        verifyResults();
        
        System.out.println("*** Exiting JUnit testSimpleFailoverTestCase1() ***\n");
    }
    
    public void testSimpleFailoverTestCase2() throws Exception {
        System.out.println("*** Starting JUnit testSimpleFailoverTestCase2() ***\n");
        
        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/TestCase2.properties";
        runTestCase(deploymentConfigPropFile, testPropFile, null);
        
        verifyResults();
        
        System.out.println("*** Exiting JUnit testSimpleFailoverTestCase2() ***\n");
    }
    
    
    /**
	 * NOTE: In order to verify failover in batches functionality you need to un
	 * comment the commented code in recoverDanglingInstances method of
	 * EngineImpl. This test would create 4 engines and feed 10 messages each.
	 * One of the engine (engine1) would be crashed and live engines would
	 * failover the instances of the failed engine. The recovery batch size is
	 * set to 3, hence the failover should happen in batches, total four batches
	 * of sizes 3, 3, 3 and 1. These batch could be picked by one or more of
	 * remaining live engines (3). The test verifies that all the 40 instances
	 * are recovered and completed at the end of the test. Also the junit will
	 * print the final instance count for each engine, it could read something
	 * like engine1=0, engine2=14, engine3=13, engine3=13 (the engine1 is
	 * crashed, engine2=14 represents that engine2 took over 4 instances of
	 * engine1 and so on).
	 * 
	 * @throws Exception
	 */
    public void testFailoverScalabilityTestCase3() throws Exception {
        System.out.println("*** Starting JUnit testFailoverScalabilityTestCase3() ***\n");
        
        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/TestCase3.properties";
        runTestCase(deploymentConfigPropFile, testPropFile, null);
        
        verifyResults();
        
        System.out.println("*** Exiting JUnit testFailoverScalabilityTestCase3() ***\n");
    }
}
