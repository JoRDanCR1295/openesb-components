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


public class CorrFlowMultIMATest extends BaseTestCase {

    private static final String TEST_FOLDER = "CorrFlowMultIMATest";
    
    private static String enginesDirectory = "engines/" + TEST_FOLDER + "/";
    private static String deploymentConfigPropFile = "engines/" + TEST_FOLDER + "/CorrFlowMultIMA.properties";

    /**
     * @param testName
     */
    public CorrFlowMultIMATest(String testName) {
        super(testName, enginesDirectory);
    }
    
    /**
     * @throws Exception
     */
    public void testCorrFlowMultIMATestCase1() throws Exception {
        System.out.println("*** Starting JUnit testCorrFlowMultIMATestCase1() ***\n");
        
        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/CorrFlowMultIMATestCase1.properties";
        runTestCase(deploymentConfigPropFile, testPropFile, null);
        
        verifyResults();
        
        System.out.println("*** Exiting JUnit testCorrFlowMultIMATestCase1() ***\n");
    }
    
    /**
     * NOTE: This test is designed to be run with 2 engines. 
     * The folowing list how the messages are being
     * distributed across the engines 
     * Receive1 (create instance=yes) -> Engine1 
     * The following IMAs are in a flow 
     * Receive2 -> Engine2 
     * Receive3 -> Engine1 
     * The instance is created in engine1 during execution of Receive1 activity. During execution 
     * of the flow branch for Receive2, no message will be found and the instance will be passivated. 
     * If however, the branch containing the invoke activity were to start before the Receive2 is reached, 
     * the instance cannot be passivated. A BPIT for passivation will be created which will be checked
     * by the failover thread perodically and when the invoke completes, the instanece will be
     * passivated. 
     * In the meanwhile the message for Receive3 one engine2 upon not finding the
     * instance will be put in mEventRequestMapForCorr (maintained at BPELProcessManger). This map
     * will be visited by the failover thread and when the instance in engine1 is passiaved, the
     * instance will be activated by engine2 and the instance would further executed by engine2. 
     * Hence, the success criterion for this test being 
     * a) the instnance is completed 
     * b) engine2 is final owner of the
     * instance. Note: It is possible that during the flow execution the Receive2 activity in
     * engine1 is played first (before the invoke activity in the same flow), and upon not finding
     * the instance the instance may be passivated. In this case, the invoke will be played by the
     * second engine. Regardless of the execution by engine1 or engine2, the outcome (as outlined
     * above, and hence the success criterion) should still be the same.
     * 
     * @throws Exception
     */
    public void testCorrFlowMultIMATestCase2() throws Exception {
        System.out.println("*** Starting JUnit testCorrFlowMultIMATestCase2() ***\n");

        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/CorrFlowMultIMATestCase2.properties";
        runTestCase(deploymentConfigPropFile, testPropFile, null);

        verifyResults();
        
        System.out.println("*** Exiting JUnit testCorrFlowMultIMATestCase2() ***\n");
    }
    
    public void testCorrFlowMultIMATestCase3() throws Exception {
        System.out.println("*** Starting JUnit testCorrFlowMultIMATestCase3() ***\n");

        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/CorrFlowMultIMATestCase3.properties";
        runTestCase(deploymentConfigPropFile, testPropFile, null);
        
        verifyResults();
        
        System.out.println("*** Exiting JUnit testCorrFlowMultIMATestCase3() ***\n");
    }
}