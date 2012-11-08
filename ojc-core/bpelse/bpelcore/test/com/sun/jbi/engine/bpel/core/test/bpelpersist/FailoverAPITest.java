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
 * @(#)FailoverAPITest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.bpelpersist;


import java.io.IOException;
import java.io.InputStream;
import java.rmi.server.UID;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;

import javax.naming.InitialContext;
import javax.transaction.TransactionManager;

import junit.framework.TestSuite;

import com.sun.jbi.engine.bpel.core.bpel.dbo.StateDBO;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.EngineImpl;
import com.sun.jbi.engine.bpel.core.test.common.DummyNonXATxManagerAndDataSource;
import com.sun.jbi.engine.bpel.core.test.common.DummyTxManagerAndDataSource;
import com.sun.jbi.engine.bpel.core.test.common.TestContext;
import com.sun.jbi.engine.bpel.core.test.common.Utility;


public class FailoverAPITest extends AbstractTestCase {
    
    public static final String SENDMESSAGECOUNT = "SendMessageCount";
    
    Set mEnginesSet = new HashSet();
    
    private static final String ENGINECOUNT = "EngineCount";
    private static final String ENGINE_EXPIRY_INTERVAL = "EngineExpiryInterval";
    private String mDBURL = null;
    private String mUserName = null;
    private String mPassWd = null;
    
    private int sendMessageCount = 1;
    private long engExpInterval;
    
    public FailoverAPITest(String arg0) {
        super(arg0);
    }

    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        try {
            mConnProp = new Properties();
        	String bpelsePropFile = "engines/FailoverAPITest/failover.properties";
            InputStream is = FailoverAPITest.class.getResourceAsStream(bpelsePropFile);
            mConnProp.load(is);
            
            DummyTxManagerAndDataSource dummyTMAndDS = new DummyTxManagerAndDataSource(mConnProp);
            DummyNonXATxManagerAndDataSource dummyNonXATMAndDS = 
                new DummyNonXATxManagerAndDataSource(mConnProp);
            
            doCleanupPersisteneDB();
            
            BPELSERegistry registry = BPELSERegistry.getInstance();
            registry.register(TransactionManager.class.getName(), dummyTMAndDS);
            InitialContext ic = new TestContext(dummyNonXATMAndDS, dummyTMAndDS, mConnProp);            
            
            sendMessageCount = Integer.parseInt(mConnProp.getProperty(SENDMESSAGECOUNT));
            engExpInterval = Long.parseLong(mConnProp.getProperty(ENGINE_EXPIRY_INTERVAL));
            int engineCount = Integer.parseInt(mConnProp.getProperty(ENGINECOUNT));

            System.setProperty(Engine.IS_CLUSTERED, "true");
            //System.setProperty(Engine.IS_CLUSTER_JUNIT_TEST, "true");
            //System.setProperty(Engine.INSTANCE_NAME, "instance-one");
            
            Properties engineProp = null;
            EngineWrapper engine;
            for (int i = 0; i < engineCount; i++) {
                engineProp = getEngineProperties(i+1);
                mConnProp.setProperty(Engine.JUNIT_ENGINEID, "engine" + i);
                engine = new EngineWrapper(mConnProp, engineProp, ic);
                mEnginesSet.add(engine);
            }
            
            
        } catch (Exception ex) {
            ex.printStackTrace();
        }    
    }

    private Properties getEngineProperties(int engineId) throws IOException {
        String engId = "engines/FailoverAPITest/engine" + engineId + ".properties";        
        String bpelsePropFile = System.getProperty(engId, engId);
        InputStream is = FailoverAPITest.class.getResourceAsStream(bpelsePropFile);  
        Properties engineProp = new Properties();
        engineProp.load(is);
        return engineProp;
    }
    
    /** @see junit.framework.TestCase#tearDown()
     */
    protected void tearDown() throws Exception {
        mEnginesSet = null;
        super.tearDown();
    }

    public static TestSuite suite() {
        return new TestSuite(FailoverAPITest.class);
    }
    
    public void testInsertHeartbeatAPI() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testInsertHeartbeatAPI");
        Connection conn = null;
        for (Iterator iter = mEnginesSet.iterator(); iter.hasNext(); ) {
            EngineWrapper engWrapper = (EngineWrapper) iter.next();
			try {
                conn = getConnection();
                Statement stmt = conn.createStatement();
                String query = "select count(*) from ENGINE where "
                    + "engineid = '" + engWrapper.getEngine().getId() + "'";
                ResultSet rs = stmt.executeQuery(query);
                assertTrue(rs.next());
                int engineDBOs = rs.getInt(1);
                assertEquals(1, engineDBOs);
            } finally {
                if (conn != null) {
                    conn.close();
                }
            }            
   	    }
    	Utility.logExit(getClass().getSimpleName(), "testInsertHeartbeatAPI");
    }

    public void testUpdateDanglingInstanceAPI() throws Exception {
    	Utility.logEnter(getClass().getSimpleName(), "testUpdateDanglingInstanceAPI");
        persistStateDBOs();
        Thread.sleep((engExpInterval + 1) * 1000);
        updateEngines();
        int actualFailedInstances = getFailedInstanceCount();
        Connection conn = null;
        //Now check for the dangling instances
        for (Iterator iter = mEnginesSet.iterator(); iter.hasNext(); ) {
            EngineWrapper engWrapper = (EngineWrapper) iter.next();
            if (!engWrapper.isCrashEnabled) {
                int updateCount = engWrapper.getEngine().getEngStateMgr().
                        updateDanglingInstances(engExpInterval, 50);
                if (updateCount > 0) {
                    assertEquals(actualFailedInstances, updateCount);
                	System.out.println("Found " + updateCount + " dangling instances for failover.");
                	//Also assert the total number of instances this engine owns now.
                	int actualInstancesOwned = sendMessageCount + actualFailedInstances;
                    try {
                	    conn = getConnection();
                        Statement stmt = conn.createStatement();
                        String query = "select count(*) from STATE where "
                            + "engineid = '" + engWrapper.getEngine().getId() + "'";
                        ResultSet rs = stmt.executeQuery(query);
                        assertTrue(rs.next());
                        int ownedInstances = rs.getInt(1);
                        assertEquals(actualInstancesOwned, ownedInstances);
                    } finally {
                        if (conn != null) {
                            conn.close();	
                        }
                    }
                    //Now clean up these running instances.
                    doCleanUp(engWrapper.getEngine().getId());
                    break;	
                }
            }
        }
        Utility.logExit(getClass().getSimpleName(), "testDanglingInstanceAPI");
    }
    
    /**
     * Persist information in the State Table.
     * (enginecount * sendMessageCount) no. of rows will be inserted.
     */
    void persistStateDBOs() throws Exception {
    	Connection conn = null;
    	PreparedStatement stmt = null;
    	String bpelId = new UID().toString();
        Iterator engineIter = mEnginesSet.iterator();
        while (engineIter.hasNext()) {
        	EngineWrapper engWrapper = (EngineWrapper) engineIter.next();
            try {
				/*RBPELProcess process = loadBPELModel("assign.bpel");
				 BPELProcessManager bpelMgr = new BPELProcessManagerImpl(process, engWrapper.getEngine());
				 process.setBPELProcessManager(bpelMgr);*/
                conn = getConnection();
                boolean currFlag = conn.getAutoCommit();
                conn.setAutoCommit(false);
                for (int i = 0; i < sendMessageCount; i++) {
                    String stateId = new UID().toString();

					//StateImpl state = StateFactory.getStateFactory().createState(
					//engWrapper.getEngine().getId(), bpelId, bpelInstanceId);
					//state.enterScope(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue());
					//state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2,
					//"[1]");
					//engWrapper.getEngine().getStateManager().persistState(state);
					//mark the entry as "Done" in the DB
					//state.exitScope(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue());
					//engWrapper.getEngine().getStateManager().persistState(state); 
                    String query = StateDBO.BASE_INSERT_STMT_STR;
                    stmt = conn.prepareStatement(query);

                    stmt.setString(1, stateId);
                    stmt.setString(2, bpelId);
                    stmt.setString(3, engWrapper.getEngine().getId());
                    if (engWrapper.isCrashEnabled) {
                        stmt.setString(4, StateDBO.RUNNING_STATUS);
                    } else {
                        stmt.setString(4, StateDBO.COMPLETE_STATUS);
                    }
                    stmt.execute();
                    conn.commit();
                    stmt.clearParameters();
                    stmt.close();
                }
                conn.setAutoCommit(currFlag);
                conn.close();
            } finally {
                if (conn != null) {
                    conn.close();
                }
                if (stmt != null) {
                    stmt.close();
                }	
			}    	    
        }        	
    }
    
    /**
     * Update the HeartBeat for the alive engines
     *
     */
    void updateEngines() {
        Iterator engineIter = mEnginesSet.iterator();
        while (engineIter.hasNext()) {
            EngineWrapper engWrapper = (EngineWrapper) engineIter.next();
            if (!engWrapper.isCrashEnabled) {
                engWrapper.getEngine().getEngStateMgr().updateHeartbeat(engExpInterval);    	
        	}
        }        	
    }
    
    /**
     * Count the total number of instances which should be in the failed state.
     * @return int
     */
    private int getFailedInstanceCount() {        
        int failedEngines = 0;
        for (Iterator iter = mEnginesSet.iterator(); iter.hasNext(); ) {
            EngineWrapper engWrapper = (EngineWrapper) iter.next();
            if (engWrapper.isCrashEnabled) {
                failedEngines++;    	
            }
        }
        int failedInstanceCount = failedEngines * sendMessageCount;
        return failedInstanceCount;
    }
    
    /**
     * Clean up the State Table by updating the status to 'DONE'
     * @param engineID 
     * @throws SQLException
     */
    private void doCleanUp(String engineID) throws Exception {
        Connection con = getConnection();
        con.setAutoCommit(false);
        Statement create = con.createStatement();
        create.execute("update STATE set status = 'DONE' where engineid = '" + engineID + "'");
        con.commit();
        con.close();
    }
        
    
    public class EngineWrapper {    	
    	public static final String CRASH = "crash";
    	boolean isCrashEnabled = false;
    	Engine mEng;
    	
    	public EngineWrapper(Properties connProp, Properties engineProp, InitialContext ic) {
            isCrashEnabled = Boolean.parseBoolean(engineProp.getProperty(CRASH));
            mEng = new EngineImpl(connProp, ic);
            mEng.preStart();
        }
    	
    	public Engine getEngine() {
    	    return mEng;	
    	}
    }
    
    public void doCleanupPersisteneDB() throws Exception {
        Connection con = null;
        Statement stmt = null;
        try {
            con = getConnection();
            con.setAutoCommit(true);
            stmt = con.createStatement();
            stmt.execute("delete from OUTSTANDINGMSGEX");
            stmt.execute("delete from SCOPE");
            stmt.execute("delete from CRMP");
            stmt.execute("delete from VARIABLE");
            stmt.execute("delete from EVENTHANDLER");
            stmt.execute("delete from LASTCHECKPOINT");
            stmt.execute("delete from INSTANCECORRELATION");
            stmt.execute("delete from ENGINECORRELATION");
            stmt.execute("delete from WAITINGIMA");
            stmt.execute("delete from FOREACH");
            stmt.execute("delete from SIMPLEVARIABLE");
            stmt.execute("delete from STATE");
            stmt.execute("delete from ENGINE");
        } catch (SQLException e) {
            System.out.println(e.getMessage());
        } finally {
            try {
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }
}
