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
 * @(#)Ph2PendCorrRecvInstPassTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.scalability;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.TestSuite;

import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.AbstractCallback;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.Callback;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.EngineInfo;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.EngineSimulator;

/**
 * @author mbhasin
 * 
 * Project Description: 
 * The BPEL Project contains a receive activity that initiates a correlation. 
 * This is followed by a pick activity that contains an on-alarm (with 10 secs wait)
 * and one onMessage activity. 
 * 
 * Test Description:
 * This following will test the scalability support for pick. By carefully choosing
 * timing of messages and memory levels the instance will be passivated and for first
 * test the instance will be activated by feeding on-Message event and for the second
 * test case the instance will be activated by expiry of onAlarm activity of pick.
 * Finally, the output will be compared. The output will be dependent upon the branch 
 * of execution, i.e. onAlarm or onMessage.
 */

public class Ph2PendCorrPickInstPassTest extends ScalabilityTestCase {

    private static final String TEST_FOLDER = "Ph2PendCorrPickInstPass";

    private static String enginesDirectory = "engines/" + TEST_FOLDER + "/";
    
    public Ph2PendCorrPickInstPassTest(String testName) {
        super(testName, enginesDirectory);
        Logger logger = Logger.getLogger("com.sun.jbi.engine.bpel.core.bpel.engine.impl.ScalabilityManager");
        logger.setLevel(Level.FINEST);
    }
    
    public static TestSuite suite() {
        return new TestSuite(Ph2PendCorrPickInstPassTest.class);
    }

    /**
     * 1. The two requests will be sent with some delay (3 secs).
     * 2. The engine will be set to simulate low memory which will cause 
     * 		phase 2 scalability solution to kick in. 
     * 3. Intercepting the status of the first receive, a thread will 
     * 		be despatched which will cause the instance to be passivated.
     * 4. The send message with 5 seconds delay will be sent (as defined in 
     * 		testcases/TestCase1.properties), which will cause the instance to be
     * 		activated and further processing to continue.
     * 5. The results will be verified in the end.
     * @throws Exception
     */
    public void testPh2PendCorrPickInstPassTestCase1() throws Exception {
        
        System.out.println("*** Starting JUnit testPPh2PendCorrPickInstPassTestCase1() ***\n");
        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/TestCase1.properties";

        Callback callback = getCallback();

        String deploymentConfigPropFile = "engines/" + TEST_FOLDER + "/Ph2PendCorrPickInstPass.properties";
        runTestCase(deploymentConfigPropFile, testPropFile, callback);
        
        verifyResults();
        
        System.out.println("*** Exiting JUnit testPh2PendCorrPickInstPassTestCase1() ***\n");
    }
    
    /**
     * Refer the notes for the class and test case 1 (above). In continuation, this test 
     * would verify the functioning of on Alarm with scalability. 
     * For this test case only one message (for first receive activity) that creates 
     * an instance will be fed.
     * Low memory situation will be simulated that will cause the instance to be passivated.
     * After waiting more than 12 secs (the timer value of on alarm as defined in the 
     * business process file the on alarm should fire and activate the instance), the instance
     * status on the database will be checked to be complete. 
     * Finally the output will be compared with expected for successful completion of the
     * test.
     * 
     * @throws Exception
     */
    public void testPh2PendCorrPickInstPassTestCase2() throws Exception {
        
        System.out.println("*** Starting JUnit testPh2PendCorrPickInstPassTestCase2() ***\n");
        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/TestCase2.properties";

        Callback callback = getCallback();
        
        String deploymentConfigPropFile = "engines/" + TEST_FOLDER + "/Ph2PendCorrPickOnAlarmInstPass.properties";

        List engines = runTestCase(deploymentConfigPropFile, testPropFile, callback);

        // wait longer than the defined (10 secs) wait in the business process.
		Thread.sleep(12000);

		// Since this test has alarm and wait causes the thread to be released, hence
		// need to despatch another thread. This thread would cause the instance activation
		// and final response (invoke) from the business process.
        if (engines.size() == 1) {
            Engine engine = ((EngineInfo) engines.get(0)).getEngine().getEngine();
            engine.process();
        }
        verifyResults();
        
        System.out.println("*** Exiting JUnit testPh2PendCorrPickInstPassTestCase2() ***\n");
    }
    
    private Callback getCallback() {
        
        Callback callback = new AbstractCallback() {

            EngineSimulator mEngineSimulator = null;
            String mMsgExId = null;
            int inonlyReqestStatusCallbackCount = 0;
            
            public void sendInOnlyRequestStatusCallback(EngineSimulator engineSimulator, String msgExchangeId) {
            	mEngineSimulator = engineSimulator;
            	mMsgExId = msgExchangeId;
            	inonlyReqestStatusCallbackCount++;
                System.out.println(Thread.currentThread().getName() + "CB-1 - "  + new Date());
                
            	if (inonlyReqestStatusCallbackCount == 1 ) {
            		/*
            		 * Since there are two receives, this method will be called two times,
            		 * We want to inject scalability phase 2 after first receive, hence
            		 * the following thread is created for first time call only.
            		 */
            		new Thread() {
            			
            			public void run() {
            				/*
            				 * Although we can use the engine alive thread to do the job of
            				 * invoking the scalability thread, but we need to make sure
            				 * that we increase the memory utilization to required levels so
            				 * that the scalability solution kicks in. Hence for this test
            				 * disabling the clustering, which would prevent the engine
            				 * alive thread to kick in scalability hence result in
            				 * unpredictability.
            				 * 
            				 * In order to invoke scalability a new thread will be created.
            				 * Also, since callback thread need to return and do the persistence, 
            				 * hence the following sleep.
            				 */
            				// NOTE: This value must be kept higher than the instance idleness criterion
            				
            				long sleepFor = 6000;
            				try {
            					sleep(sleepFor);
            				} catch (InterruptedException e) {
            					e.printStackTrace();
            				}
            				
            				List<String> ph1FailPointsList = new ArrayList<String>();
            				/*
            				 * setting ALL_FAIL for fail-points indicate that ph1freememory check would always fail
            				 * for scalability phase 1, and that would kick in scalability for by phase 2
            				 */
            				ph1FailPointsList.add("ALL_FAIL");
            				((BPELMemoryMonitorTestImpl)mEngineSimulator.getEngine().getBpelMemoryMonitor()).setPh1LowerFailureCallPoints(ph1FailPointsList);
            				// the following should cause the scalability thread to be kicked in for
            				// phase 2 solution that would passivate the instance.
            				mEngineSimulator.getEngine().process();
            			}
            		}.start();
            	}
            }
        };
        
        return callback;
    }
}
