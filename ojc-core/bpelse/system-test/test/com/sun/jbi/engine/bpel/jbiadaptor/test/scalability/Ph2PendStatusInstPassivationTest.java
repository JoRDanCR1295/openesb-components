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
 * @(#)Ph2PendRespInstPassivationTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.scalability;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.AbstractCallback;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.Callback;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.EngineSimulator;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.Utility;

import junit.framework.TestSuite;

public class Ph2PendStatusInstPassivationTest extends ScalabilityTestCase {

    private static final String TEST_FOLDER = "Ph2PendStatusInstPassivation";
    String deploymentConfigPropFile = "engines/" + TEST_FOLDER + "/Ph2PendStatusInstPassivation.properties";
    private static String enginesDirectory = "engines/" + TEST_FOLDER + "/";
    
    public Ph2PendStatusInstPassivationTest(String testName) {
        super(testName, enginesDirectory);
    }
    
    public static TestSuite suite() {
        return new TestSuite(Ph2PendStatusInstPassivationTest.class);
    }
    
    public void testPh2PendStatusInstPassivationTest1() throws Exception {
        System.out.println("*** Starting JUnit testPh2PendStatusInstPassivationTest1() ***\n");
        String testPropFile = "engines/" + TEST_FOLDER + "/testcases/TestCase1.properties";

        Callback callback = getCallback();
        runTestCase(deploymentConfigPropFile, testPropFile, callback);
        
        verifyResults();
        
        System.out.println("*** Exiting JUnit testPh2PendStatusInstPassivationTest1() ***\n");
    }
    
    private Callback getCallback() {
        
        Callback callback = new AbstractCallback() {

            int invokeCounter = 0;
            EngineSimulator mEngineSimulator = null;
            RBPELProcess mProcess = null;
            String mMsgExId = null;
            MessageContainer mMessageContainer = null;

            public boolean oneWayInvokeCallback(EngineSimulator engineSimulator,
                                                MessageContainer messageContainer,
                                                RBPELProcess process,
                                                String msgExId) {
                mEngineSimulator = engineSimulator;
                mMsgExId = msgExId;
                mMessageContainer = messageContainer;
                mProcess = process;
                invokeCounter++;

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
                    	//long sleepFor = 5000;
    					long sleepFor = BPELMemoryMonitorTestImpl.JUNIT_IDLE_THRESHOLD  * 1000;
                    	
    					try {
                            sleep(sleepFor);
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }

                        List ph1FailPointsList = new ArrayList();
                        // setting ALL_FAIL for fail-points indicate that ph1freememory check would always fail
                        // for scalability phase 1, and that would kick in scalability for by phase 2, 
                        ph1FailPointsList.add("ALL_FAIL");
                        ((BPELMemoryMonitorTestImpl)mEngineSimulator.getEngine().getBpelMemoryMonitor()).setPh1LowerFailureCallPoints(ph1FailPointsList);

                        // the following should cause the phase 2 solution of scalability to get rid of the instance 
                        // waiting on pending queue for response or outstanding request. 
                        mEngineSimulator.getEngine().process();
                        
                        // There are two invokes defined in the process. The first invoke is part enclosed by 
                        // receive-reply pair (in-out). Since we do not passivate the instance if there is any 
                        // outstanding message exchange, hence for invokeCounter value of 1, there should not
                        // be any record in OutstandingMsgEx table
                        int outstandingMsgExCount = -1;
                        
                        if (invokeCounter == 1) {
                            outstandingMsgExCount = 0;
                        } else if (invokeCounter == 2) {
                            // this indicate second invoke; this time the instance should be passivated
                            outstandingMsgExCount = 1;
                        }
                        verifyDbForOutstandingMsgEx(mMsgExId, outstandingMsgExCount); 

                        // send the done message, should cause the scalability passivated instance loaded back (recovered) from 
                        // the database. The instance then should complete.
                        mEngineSimulator.sendDoneStatus(mMessageContainer, mMsgExId, mProcess);
                    }
                }.start();

                return false;
            }
        };
        
        return callback;
    }
    
    private void verifyDbForOutstandingMsgEx(String msgExId, int expectedCount) {
        List instances = getOutstandingMsgExDBOs(msgExId);
        
        if (instances.size() != expectedCount) {
            String message = "JUNIT ASSERTION ERROR: Found " + instances.size() + " records when " + expectedCount + " record was expected"
                    + " in the OutstandingMsgEx table. Note: For invokes/wait which is "
                    + " is enclosed by in-out msg ex defination (receive-reply pair), the scalability instance passivation"
                    + " should not happen. Check the scalability logic..";
            throw new AssertionError(message);
        } else {
            String message = "";
            if (instances.size() > 1) {
                message = "JUNIT ASSERTION ERROR: Found " + instances.size() + " records in in the OutstandingMsgEx table for Msg Ex id : "
                        + msgExId + " when " + expectedCount + " expected. Check scalability logic ";
                throw new AssertionError(message);
            } else if (instances.size() == 1) {
                message = "Verified Outstanding Message Exchange Persistence for Msg Ex id : " + msgExId + " For Instance Id : " + instances.get(0);
            } else if (expectedCount == 0){
                message = "Verified no passivation happened for the instance";
            }
            System.out.println(message);
        }
    }
    
    
    protected List getOutstandingMsgExDBOs(String msgExId) {        
        String query = "Select msgexid, stateid from OUTSTANDINGMSGEX where msgexid = '" + msgExId + "'";
        Connection con = null;
        ResultSet rs = null;
        Statement stmt = null;
        List list = new ArrayList();
        
        String instanceId = null;
        
        try {
            con = Utility.getConnection();
            stmt = con.createStatement();
            rs = stmt.executeQuery(query);
            
            while (rs.next()) {
                instanceId = new String(rs.getString(2));
                list.add(instanceId);
            }
            
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            try {
                if (stmt != null) {
                    stmt.close();
                }
                if (rs != null) {
                    rs.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return list;
    }
}
