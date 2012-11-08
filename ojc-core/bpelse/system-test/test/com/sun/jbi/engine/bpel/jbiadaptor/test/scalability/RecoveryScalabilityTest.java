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
 * @(#)RecoveryScalabilityTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.scalability;


import java.util.ArrayList;
import java.util.List;

import junit.framework.TestSuite;

import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;

public class RecoveryScalabilityTest extends ScalabilityTestCase {

    private static final String TEST_FOLDER = "RecoveryScalability";
    String deploymentConfigPropFile = "engines/" + TEST_FOLDER + "/RecoveryScalability.properties";
    private static String enginesDirectory = "engines/" + TEST_FOLDER + "/";
    
    public RecoveryScalabilityTest(String testName) {
        super(testName, enginesDirectory);
    }
    
    public static TestSuite suite() {
        return new TestSuite(RecoveryScalabilityTest.class);
    }

    public void testRecoveryScalabilityTestCase1() throws Exception {
        System.out.println("*** Starting JUnit testPRecoveryScalabilityTestCase1() ***\n");
        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/TestCase1.properties";

        runTestCase(deploymentConfigPropFile, testPropFile, null);
        Engine engine = getEngineForRecovery(deploymentConfigPropFile, testPropFile, null);
        
        int expectedCompleteInstances = Integer.parseInt(testProp.getProperty(EXPECTEDCOMPLEINSTANCES));

        firstTest(engine, expectedCompleteInstances);
        secondTest(engine);
        thirdTest(engine, expectedCompleteInstances);
        
        System.out.println("*** Exiting JUnit testRecoveryScalabilityTestCase1() ***\n");
    }
    
    /**
     * This following will verify that no recovery is performed if the available memory 
     * is less than defined by phase 1 memory ratio. 
     * Since no recovery is performed, the success criterion would be all the instances 
     * should still be running. 
     * Setting ALL_FAIL for fail-points for BPEL Memory Monitor would result in 
     * ph1freememory check would always fail, meaning
     * 
     * @param engine
     * @param totalInstances
     */
    private void firstTest(Engine engine, int totalInstances) {
        List ph1FailPointsList = new ArrayList();
        
        ph1FailPointsList.add("ALL_FAIL");
        ((BPELMemoryMonitorTestImpl)engine.getBpelMemoryMonitor()).setPh1LowerFailureCallPoints(ph1FailPointsList);
        engine.process();
        verifyResultsSingleEngRecovery(totalInstances, 0);
    }
    
    /**
     * The following is the test to verify the functioning of batch size recovery. The test is designed such 
     * that only one batch to be scheduled for recovery.
     * Since we are sending 14 messages, the successful completion of this test would result in 
     * 4 complete instances and 10 running instances in the database.
     * 
     * First time the memory check will be made at 
     * EngineImpl -doMemoryManagement - return true here
     * Second check is made in the recovery method -return true
     * Third in doBatch recovery - this should return true also
     * Subsequent checks should return false
     * Hence the value of ph1FailPointsList should be 4, NEXT_ALL_FAIL
     * 
     * @param engine
     */
    private void secondTest(Engine engine) {
        List ph1FailPointsList = new ArrayList();
        ph1FailPointsList.add(4);
        ph1FailPointsList.add("NEXT_ALL_FAIL");
        ((BPELMemoryMonitorTestImpl)engine.getBpelMemoryMonitor()).setPh1LowerFailureCallPoints(ph1FailPointsList);
        engine.process();
        // verify results: At this point all 4 instances should be recovered and there should be 10 running instances
        // in the database
        verifyResultsSingleEngRecovery(10, 4);
    }
    
    /**
     * During processing engine would make multiple calls to BPELMemoryMonitor to check for memory levels
     * the following would return false for the fifth call. The way the test is designed for 10 instances
     * and batch of 4, the first two batches will be scheduled for recovery and during the scheduling of
     * the third batch the MemoryMonitor will return false, hence the phase 1 scalability solution will kick
     * in. After the work by scalability solution, another check will be made to see if the memory levels
     * meets the phase 1 memory criterion. We will trick engine, by passing true, hence the recovery will
     * schedule other two instances for recovery.
     * 
     * @param engine
     * @param totalInstances
     */
    private void thirdTest(Engine engine, int totalInstances) {
        List ph1FailPointsList = new ArrayList();
        ph1FailPointsList.add(6);
        ((BPELMemoryMonitorTestImpl)engine.getBpelMemoryMonitor()).setPh1LowerFailureCallPoints(ph1FailPointsList);
        engine.process();
        
        // verify results: At this point all 10 instances should be recovered. 
        verifyResultsSingleEngRecovery(0, totalInstances);
    }
}

