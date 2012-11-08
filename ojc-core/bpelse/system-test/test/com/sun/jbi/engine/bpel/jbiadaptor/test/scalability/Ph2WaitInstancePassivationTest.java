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
 * @(#)Ph2WaitInstancePassivationTest.java 
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
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.EngineInfo;

public class Ph2WaitInstancePassivationTest extends ScalabilityTestCase {

    private static final String TEST_FOLDER = "Ph2WaitInstancePassivation";
    String deploymentConfigPropFile = "engines/" + TEST_FOLDER + "/Ph2WaitInstancePassivation.properties";
    private static String enginesDirectory = "engines/" + TEST_FOLDER + "/";
    
    public Ph2WaitInstancePassivationTest(String testName) {
        super(testName, enginesDirectory);
    }
    
    public static TestSuite suite() {
        return new TestSuite(Ph2WaitInstancePassivationTest.class);
    }

    /**
     * The following test verifies the phase 2 of scalability. 
     * Test Description:
     * A. Test Phase 2 of scalability solution for Single Engine.
     * B. Will send one message and will simulate the low memory levels by 
     *    setting appropriate values to BPELMemoryMonitorTestImpl class. 
     *    These values will fool the engine to believe that the memory is low.
     * C. For this particular test, the sequence is as follows:
     * 1. Send one message, the thread will return after encountering a wait (10 secs)
     * 2. First Test: Simulate low memory, send a thread, this will cause phase 1 and 2
     * 		passivation. Verify that the instance is still running in the database.
     * 2. Second Test: Wait 5 secs, send another thread (set memory to be back to normal -
     * 		i.e no low memory), since the wait time in the process is set to 10 seconds,
     * 		this thread should not cause the instance to be activated. 
     * 		Success criterion of the test is that the instance status in the database
     * 		is still RUNNING. 
     * 3. Third Test: Wait another 10 secs, this will ensure that the wait time 
     * 		for the instance is expired. Send another thread, this should activate 
     * 		the instance and complete the instance. The success criterion is that
     * 		the status on the instance should say DONE
     *     
     * @throws Exception
     */
    public void testPh2WaitInstancePassivationTest1() throws Exception {
        System.out.println("*** Starting JUnit testPh2WaitInstancePassivationTest1() ***\n");
        
        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/TestCase1.properties";
        
        List engines = runTestCase(deploymentConfigPropFile, testPropFile, null);
        
        // since the test contain a wait activity, the thread dispatched by the engine returns
        // when it encounters wait activity. For engines in a cluster this would not be a problem
        // as the engine-alive thread for each engine would be able to pick the instance when the 
        // wait time expires, for single engine case, we need to dispatch this thread.
      
        if (engines.size() == 1) {
            Engine engine = ((EngineInfo) engines.get(0)).getEngine().getEngine();
            firstTest(engine, 1);
            secondTest(engine, 1);
            thirdTest(engine, 1);
        }
        
        /* verify results */
        verifyResults();
        
        System.out.println("*** Exiting JUnit testPh2WaitInstancePassivationTest1() ***\n");
    }
    
    /**
     * This following will verify that no recovery is performed if the available memory 
     * is less than defined by phase 1 memory ratio. 
     * Since no recovery is performed, the success criterion would be that all the instances 
     * should still be running. 
     * Setting ALL_FAIL for fail-points for BPEL Memory Monitor would result in 
     * ph1freememory check would always fail.
     * 
     * @param engine
     * @param totalInstances
     * @throws InterruptedException 
     */
    private void firstTest(Engine engine, int totalInstances) throws InterruptedException {
        List ph1FailPointsList = new ArrayList();

        // the following will cause the phase 1 and phase 2 scalability solution be called. 
        // the phase 1 will result in thrashing of the variables and phase 2 should result 
        // in passivation of the instance i.e the callframe in the bpit will be dereferenced
        //  the instance status in the database should still be RUNNING.
        ph1FailPointsList.add("ALL_FAIL");
        ((BPELMemoryMonitorTestImpl)engine.getBpelMemoryMonitor()).setPh1LowerFailureCallPoints(ph1FailPointsList);
        engine.process();
        verifyResultsSingleEngRecovery(1, 0);
    }
    
    /**
     * refer comments at the test description
     * 
     * @param engine
     * @param totalInstances
     * @throws InterruptedException
     */
    private void secondTest(Engine engine, int totalInstances) throws InterruptedException {
        Thread.currentThread().sleep(1000 * 5);
        ((BPELMemoryMonitorTestImpl)engine.getBpelMemoryMonitor()).setPh1LowerFailureCallPoints(null);
        engine.process();
        verifyResultsSingleEngRecovery(1, 0);
    }
    
    /**
     * refer comments at the test description
     * 
     * @param engine
     * @param totalInstances
     */
    private void thirdTest(Engine engine, int totalInstances) throws InterruptedException {
        Thread.currentThread().sleep(1000 * 7);
    	engine.process();
        verifyResultsSingleEngRecovery(0, 1);
    }
}
