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
 * @(#)PendRespVarsPassivationTest.java 
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
import com.sun.jbi.engine.bpel.core.bpel.dbo.VariableScalabilityDBO;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELMemoryMonitor;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.AbstractCallback;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.Callback;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.EngineSimulator;

public class PendRespVarsPassivationTest extends ScalabilityTestCase {

    private static final String TEST_FOLDER = "PendResVarsPassivation";
    String deploymentConfigPropFile = "engines/" + TEST_FOLDER + "/PendRespVarsPassivation.properties";
    private static String enginesDirectory = "engines/" + TEST_FOLDER + "/";
    
    private long RECEIVE_VARID         = 1000000;
    private long INVOKE_INPUT_VARID    = 1000001;
    private long INVOKE_RESPONSE_VARID = 1000002;
    
    public PendRespVarsPassivationTest(String testName) {
        super(testName, enginesDirectory);
    }
    
    public static TestSuite suite() {
        return new TestSuite(PendRespVarsPassivationTest.class);
    }

    public void testPendRespVarsPassivationTestTestCase1() throws Exception {
        System.out.println("*** Starting JUnit testPendRespVarsPassivationTestTestCase1() ***\n");
        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/TestCase1.properties";

        Callback callback = getCallback();
        runTestCase(deploymentConfigPropFile, testPropFile, callback);
        
        verifyResults();
        
        System.out.println("*** Exiting JUnit testPendRespVarsPassivationTestTestCase1() ***\n");
    }
    
    private Callback getCallback() {
        
        Callback callback = new AbstractCallback() {

            EngineSimulator mEngineSimulator = null;
            MessageContainer mMessageContainer = null;
            RBPELProcess mProcess = null;
            String mMsgExId = null;

            public boolean twoWayInvokeCallback(EngineSimulator engineSimulator,
                                                MessageContainer msgCont,
                                                RBPELProcess process,
                                                String msgExId) {
                mEngineSimulator = engineSimulator;
                mMessageContainer = msgCont;
                mMsgExId = msgExId;
                mProcess = process;

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
                    	long sleepFor = BPELMemoryMonitorTestImpl.JUNIT_IDLE_THRESHOLD  * 1000;

                    	try {
                            sleep(sleepFor);
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }

                        verifyDbBeforeScalabilityCall();

                        List ph1FailPointsList = new ArrayList();

                        // We want to test phase 1 scalability here, also we do not want the phase 2
                        // to be kick in (phase 2 will passivate the instance altogether). 
                        // Also, since the begin time criterion for scalability is 20 secs (junit scalability
                        // class BPELMemoryMonitorTestImpl overrides the default value of 10 mins in the corresponding
                        // engine class), 
                        // and it is halved for each iteration (and also the memory check is done)
                        // in each of these iteration, we need to keep failing this check 3 times (we already waited
                        // 6 secs in sleep above, and three check should bring down the time criterion to less than 
                        // 6 secs (20 secs - 10 secs - 5 secs), qualifying the instance waiting on response 
                        // pending queue to be picked for instance variable trashing)  
                        
						BPELMemoryMonitor memoryMonitor = mEngineSimulator.getEngine().getBpelMemoryMonitor();
                        for(int i = 1; i < memoryMonitor.IDLENSS_EXPONENT + 1; i++) {
                            ph1FailPointsList.add(i);
                        }
                        
                        ((BPELMemoryMonitorTestImpl)mEngineSimulator.getEngine().getBpelMemoryMonitor()).setPh1LowerFailureCallPoints(ph1FailPointsList);

                        // the following should cause the scalability thread to be kicked in for
                        // phase 1 solution.
                        mEngineSimulator.getEngine().process();
                        verifyDbAfterScalabilityCallBeforeResponseMessage();

                        mEngineSimulator.sendResponseMessage(mMessageContainer, mProcess, mMsgExId);
                        verifyDbAfterResponseMessageProcessing();
                    }
                }.start();

                return false;
            }
        };
        
        return callback;
    }
    
    /**
     * Before the Scalability solution is called, verify that the persistence
     * is in order.
     */
    private void verifyDbBeforeScalabilityCall() {
        // verify that the RECEIVE_VARID is only variable persisted at this point
        // with scalability passivated flag as 'N'
        List varDbos = getVariableDBOs(RECEIVE_VARID);
        verifyVariable(RECEIVE_VARID, getScalabilityFlag(varDbos), VariableScalabilityDBO.SCALABILITYPASSIVATED_NO,
                varDbos.size(), 1);

        // verify INVOKE_INPUT_VARID is not scalability passivated at this point.
        varDbos = getVariableDBOs(INVOKE_INPUT_VARID);
        verifyVariable(INVOKE_INPUT_VARID, getScalabilityFlag(varDbos), null, varDbos.size(), 0);
        
        // verify INVOKE_RESPONSE_VARID is not scalability passivated at this point.
        varDbos = getVariableDBOs(INVOKE_RESPONSE_VARID);
        verifyVariable(INVOKE_RESPONSE_VARID, getScalabilityFlag(varDbos), null, varDbos.size(), 0);
    }
    
    /**
     * After the scalability solution is called, the dirty variable
     * need to be passivated in the database.
     */
    private void verifyDbAfterScalabilityCallBeforeResponseMessage() {
        // at this point there should be two entries in the varaibles table,
        // RECEIVE_VARID and another REPLY_VARID
        List varDbos = getVariableDBOs(RECEIVE_VARID);
        verifyVariable(RECEIVE_VARID, getScalabilityFlag(varDbos), VariableScalabilityDBO.SCALABILITYPASSIVATED_NO,
                varDbos.size(), 1);

        // check for the other variable it should
        // be scalability passivated at this point
        varDbos = getVariableDBOs(INVOKE_INPUT_VARID);
        verifyVariable(INVOKE_INPUT_VARID, getScalabilityFlag(varDbos), VariableScalabilityDBO.SCALABILITYPASSIVATED_YES, varDbos.size(),
                1);
        
        // check for the other variable it should
        // be scalability passivated at this point
        varDbos = getVariableDBOs(INVOKE_RESPONSE_VARID);
        verifyVariable(INVOKE_RESPONSE_VARID, getScalabilityFlag(varDbos), VariableScalabilityDBO.SCALABILITYPASSIVATED_YES, varDbos.size(),
                0);
    }
    
    /**
     * After the status is sent, verify the database.
     */
    private void verifyDbAfterResponseMessageProcessing() {
        // at this point there should be two entries in the variables table,
        // one with value of input message for RECEIVE_VARID and another
        // INVOKE_INPUT_VARID each with scalability passivated flag as 'N'
        List varDbos = getVariableDBOs(RECEIVE_VARID);
        verifyVariable(RECEIVE_VARID, getScalabilityFlag(varDbos), VariableScalabilityDBO.SCALABILITYPASSIVATED_NO,
                varDbos.size(), 1);

        // check for the other variable it should be scalability passivated at this point
        varDbos = getVariableDBOs(INVOKE_INPUT_VARID);
        verifyVariable(INVOKE_INPUT_VARID, getScalabilityFlag(varDbos), VariableScalabilityDBO.SCALABILITYPASSIVATED_NO, varDbos.size(),
                1);
        
        varDbos = getVariableDBOs(INVOKE_RESPONSE_VARID);
        verifyVariable(INVOKE_RESPONSE_VARID, getScalabilityFlag(varDbos), VariableScalabilityDBO.SCALABILITYPASSIVATED_NO, varDbos.size(),
                1);
    }
}
