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
import java.util.List;

import junit.framework.TestSuite;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.AbstractCallback;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.Callback;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.EngineSimulator;

/**
 * @author mbhasin
 *
 */
public class Ph2PendCorrRecvInstPassTest extends ScalabilityTestCase {

    private static final String TEST_FOLDER = "Ph2PendCorrRecvInstPass";
    String deploymentConfigPropFile = "engines/" + TEST_FOLDER + "/Ph2PendCorrRecvInstPass.properties";
    private static String enginesDirectory = "engines/" + TEST_FOLDER + "/";
    
    public Ph2PendCorrRecvInstPassTest(String testName) {
        super(testName, enginesDirectory);
    }
    
    public static TestSuite suite() {
        return new TestSuite(Ph2PendCorrRecvInstPassTest.class);
    }

    public void testPh2PendCorrRecvInstPassTestCase1() throws Exception {
        
        System.out.println("*** Starting JUnit testPendStatusVarsPassivationTestCase1() ***\n");
        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/TestCase1.properties";

        Callback callback = getCallback();

        runTestCase(deploymentConfigPropFile, testPropFile, callback);
        
        verifyResults();
        
        System.out.println("*** Exiting JUnit testPendStatusVarsPassivationTestCase1() ***\n");
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
            				//long sleepFor = 6000;
            				long sleepFor = BPELMemoryMonitorTestImpl.JUNIT_IDLE_THRESHOLD  * 1000;

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
