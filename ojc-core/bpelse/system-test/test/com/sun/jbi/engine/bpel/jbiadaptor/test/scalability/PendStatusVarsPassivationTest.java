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
 * @(#)PendStatusVarsPassivationTest.java 
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
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.AbstractCallback;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.Callback;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.EngineSimulator;

public class PendStatusVarsPassivationTest extends ScalabilityTestCase {

    private static final String TEST_FOLDER = "PendStatusVarsPassivation";
    String deploymentConfigPropFile = "engines/" + TEST_FOLDER + "/PendStatusVarsPassivation.properties";
    private static String enginesDirectory = "engines/" + TEST_FOLDER + "/";
    
    private long RECEIVE_VARID = 1000000;
    private long INVOKE_INPUT_VARID   = 1000001;
    
    public PendStatusVarsPassivationTest(String testName) {
        super(testName, enginesDirectory);
    }
    
    public static TestSuite suite() {
        return new TestSuite(PendStatusVarsPassivationTest.class);
    }

    public void testPendStatusVarsPassivationTestCase1() throws Exception {
        String memorystart = System.getProperty("Xms");
        System.out.print(memorystart);
        
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
            MessageContainer mMessageContainer = null;
            InComingEventModel mModel = null;
            RBPELProcess mProcess = null;
            String mMsgExId = null;

            public boolean oneWayInvokeCallback(EngineSimulator engineSimulator,
                                                MessageContainer messageContainer,
                                                RBPELProcess process,
                                                String msgExId) {
                mEngineSimulator = engineSimulator;
                mMessageContainer = messageContainer;
                mProcess = process;
                mMsgExId = msgExId;

				new Thread() {
					

					public void run() {
						/*
						 * Although we can use the engine alive thread to do this job of
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

						BPELMemoryMonitor memoryMonitor = mEngineSimulator.getEngine().getBpelMemoryMonitor();

						/*
						 * We want to test phase 1 scalability here, also we do
						 * not want the phase 2 to be kick in (phase 2 will
						 * passivate the instance altogether). The following is a mechanism 
						 * to simulate low memory conditions. Since the idleness criterion to identify
						 * instances is powered by IDLNESS_EXPONENT, the instance will not qualify for
						 * scalability passivation in first iteration. Hence we need configure the memory
						 * manager in such a fashion that the check fails for successive iterations. 
						 * The reason why the IDLENSS_EXPONENT is incremented by 1 is because first 
						 * memory check is in EngineImpl process method, so in all we need to simulate
						 * such that for IDLENSS_EXPONENT +1 times the memory check fails.
						 */
						for (int i = 1; i < memoryMonitor.IDLENSS_EXPONENT + 1; i++) {
							ph1FailPointsList.add(i);
						}
						
						((BPELMemoryMonitorTestImpl) memoryMonitor).setPh1LowerFailureCallPoints(ph1FailPointsList);

						/*
						 * the following should cause the scalability thread to be kicked in for phase 1 solution.
						 */
						
						mEngineSimulator.getEngine().process();
						verifyDbAfterScalabilityCallBeforeStatusMessage();

						mEngineSimulator.sendDoneStatus(mMessageContainer,
								mMsgExId, mProcess);
						verifyDbAfterStatusMessage();
					}
				}.start();

                return false;
            }
        };
        
        return callback;
    }
    
    /**
	 * Before the Scalability solution is called, verify that the persistence is
	 * in order.
	 */
    private void verifyDbBeforeScalabilityCall() {
        // verify that the RECEIVE_VARID is only variable persisted at this
		// point
        // with scalability passivated flag as 'N'
        List varDbos = getVariableDBOs(RECEIVE_VARID);
        verifyVariable(RECEIVE_VARID, getScalabilityFlag(varDbos), VariableScalabilityDBO.SCALABILITYPASSIVATED_NO,
                varDbos.size(), 1);

        // verify INVOKE_INPUT_VARID is not scalability passivated at this point.
        varDbos = getVariableDBOs(INVOKE_INPUT_VARID);
        verifyVariable(INVOKE_INPUT_VARID, getScalabilityFlag(varDbos), null, varDbos.size(), 0);
    }
    
    /**
     * After the scalability solution is called, the dirty variable
     * need to be passivated in the database.
     */
    private void verifyDbAfterScalabilityCallBeforeStatusMessage() {
        // at this point there should be two entries in the variables table,
        // RECEIVE_VARID and another INVOKE_INPUT_VARID
        List varDbos = getVariableDBOs(RECEIVE_VARID);
        verifyVariable(RECEIVE_VARID, getScalabilityFlag(varDbos), VariableScalabilityDBO.SCALABILITYPASSIVATED_NO,
                varDbos.size(), 1);

        // check for the INVOKE_INPUT_VARID variable  it should be scalability passivated at this point
        varDbos = getVariableDBOs(INVOKE_INPUT_VARID);
        verifyVariable(INVOKE_INPUT_VARID, getScalabilityFlag(varDbos), VariableScalabilityDBO.SCALABILITYPASSIVATED_YES, varDbos.size(),
                1);
    }
    
    /**
     * After the status is sent, verify the database.
     */
    private void verifyDbAfterStatusMessage() {
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
    }

}
