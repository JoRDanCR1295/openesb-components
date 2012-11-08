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
 * @(#)DBSchemaCreationTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.bpelpersist;

import java.rmi.server.UID;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;

import javax.naming.InitialContext;
import javax.transaction.TransactionManager;
import javax.xml.namespace.QName;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.connection.AbstractDBConnection;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.dbo.StateDBO;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.BPELEventPersister;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.persist.DBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.MonitorDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.PurgeData;
import com.sun.jbi.engine.bpel.core.bpel.persist.TransactionInfo;
import com.sun.jbi.engine.bpel.core.bpel.persist.DBSchemaCreation.STATUS;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateImpl;
import com.sun.jbi.engine.bpel.core.test.common.DummyNonXATxManagerAndDataSource;
import com.sun.jbi.engine.bpel.core.test.common.DummyRBPELProcess;
import com.sun.jbi.engine.bpel.core.test.common.DummyTxManagerAndDataSource;
import com.sun.jbi.engine.bpel.core.test.common.TestContext;
import com.sun.jbi.engine.bpel.core.test.common.Utility;


/**
 * @author Sun Microsystems
 */
public class PurgeDataTest extends AbstractTestCase {
    
    protected RBPELProcess mProcess;
    static QName mBPELId1 = QName.valueOf(new UID().toString());
    
    public PurgeDataTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
        mProcess = new DummyRBPELProcess(mBPELId1);
        mEng.addModel(mProcess, null, null);
    }

    protected void tearDown() throws Exception {
        super.tearDown();
        mEng.removeModel(mBPELId1);
    }
    
    public void testPersistenceTablesPurge() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testPersistenceTablesPurge");

        DummyTxManagerAndDataSource dummyTMAndDS = new DummyTxManagerAndDataSource(mConnProp);
        DummyNonXATxManagerAndDataSource dummyNonXATMAndDS = 
            new DummyNonXATxManagerAndDataSource(mConnProp);
        BPELSERegistry registry = BPELSERegistry.getInstance();
        registry.register(TransactionManager.class.getName(), dummyTMAndDS);
        InitialContext ctx = new TestContext(dummyNonXATMAndDS, dummyTMAndDS, mConnProp);
            
        DBConnectionFactory dbConnFac = new DBConnectionFactory(mConnProp, ctx, null);

        PurgeData purge = new PurgeData();
        purge.purgePersistenceData(dbConnFac);
        
        String status = StateDBO.COMPLETE_STATUS;
        String STATE_TABLE = PersistenceDBSchemaCreation.STATE.trim();
        ResultSet resultSet = null;
        AbstractDBConnection dbConn = null;
        Statement stmt = null;
        int beforeCount;
        int beforeVarCount;
        try {
            dbConn = dbConnFac.createNonXAConnection();
            Connection conn = dbConn.getUnderlyingConnection();
            stmt = conn.createStatement();
            resultSet = stmt.executeQuery("select count(*) from "
                    + STATE_TABLE + " where " + STATE_TABLE + ".status='"
                    + status + "'");
            resultSet.next();
            beforeCount = resultSet.getInt(1);
            System.out.println("count value before insert =" + beforeCount);
            resultSet = stmt.executeQuery("select count(*) from "
                    + PersistenceDBSchemaCreation.VARIABLE);
            resultSet.next();
            beforeVarCount = resultSet.getInt(1);
        }  finally {
            if (resultSet != null) {
                resultSet.close();
            }
            if (stmt != null) {
                stmt.close();
            }
            if (dbConn != null) {
                dbConn.close(); 
            }
        }
        
        insertPersistenceData(dbConnFac);
        
        int afterInsertCount;
        int afterInsertVarCount;
        try {
            dbConn = dbConnFac.createNonXAConnection();
            Connection conn = dbConn.getUnderlyingConnection();
            stmt = conn.createStatement();
            resultSet = stmt.executeQuery("select count(*) from "
                    + STATE_TABLE + " where " + STATE_TABLE + ".status='"
                    + status + "'");
            resultSet.next();
            afterInsertCount = resultSet.getInt(1);
            System.out.println("count value after insert =" + afterInsertCount);
            assertTrue(afterInsertCount == (beforeCount + 1));
            
            resultSet = stmt.executeQuery("select count(*) from "
                    + PersistenceDBSchemaCreation.VARIABLE);
            resultSet.next();
            afterInsertVarCount = resultSet.getInt(1);
            assertTrue(afterInsertVarCount == (beforeVarCount + 2));
        }  finally {
            if (resultSet != null) {
                resultSet.close();
            }
            if (stmt != null) {
                stmt.close();
            }
            if (dbConn != null) {
                dbConn.close(); 
            }
        }
        purge.purgePersistenceData(dbConnFac);
        
        int afterPurgeCount;
        int afterPurgeVarCount;
        try {
            dbConn = dbConnFac.createNonXAConnection();
            Connection conn = dbConn.getUnderlyingConnection();
            stmt = conn.createStatement();
            resultSet = stmt.executeQuery("select count(*) from "
                    + STATE_TABLE + " where " + STATE_TABLE + ".status='"
                    + status + "'");
            resultSet.next();
            afterPurgeCount = resultSet.getInt(1);
            System.out.println("count value after purge =" + afterPurgeCount);
            assertTrue(afterPurgeCount == beforeCount);
            resultSet = stmt.executeQuery("select count(*) from "
                    + PersistenceDBSchemaCreation.VARIABLE);
            resultSet.next();
            afterPurgeVarCount = resultSet.getInt(1);
            assertTrue(afterPurgeVarCount == beforeVarCount);
        }  finally {
            if (resultSet != null) {
                resultSet.close();
            }
            if (stmt != null) {
                stmt.close();
            }
            if (dbConn != null) {
                dbConn.close(); 
            }
        }
        
        Utility.logExit(getClass().getSimpleName(), "testPersistenceTablesPurge");
    }
    
    private void insertPersistenceData(DBConnectionFactory dbConnFac) throws Exception {
        
        StateImpl state = createState(mProcess.getBPELId());
        String stateId = state.getId();
        state.enterScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);

        // simulating the receive
        state.updatePC(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue(), 2);
        FirstTest someInstance = new FirstTest("not a test");
        
        RuntimeVariable var1 = someInstance.createVariable("request");
//        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_SCOPE_ID.longValue(),
//            var1);
        state.updateVariable(RBPELProcess.DEFAULT_PROCESS_BRANCH_ID, var1);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);

        // state, Variable, lastcheckpoint tables now have entries. insert entries for this 
        // stateId for the rest of the relevant tables.
        
        AbstractDBConnection dbConn = null;
        Statement stmt = null;
        try {
            dbConn = dbConnFac.createNonXAConnection();
            Connection conn = dbConn.getUnderlyingConnection();
            stmt = conn.createStatement();
            stmt.executeUpdate("insert into " + PersistenceDBSchemaCreation.WAITINGIMA + "(stateid, " +
                    "partnerlink, operation) VALUES ('" + stateId + "', 'somePLink', 'someOper')");

            stmt.executeUpdate("INSERT INTO " + PersistenceDBSchemaCreation.FOREACH + 
            "VALUES(1000000, '" + stateId + "', 2, 3, 1, 4, 2)");
            
            stmt.executeUpdate("INSERT INTO " + PersistenceDBSchemaCreation.INSTANCECORRELATION + 
            "VALUES('" + stateId + "', 1000001, 'someCorrValue')");
            
            stmt.executeUpdate("INSERT INTO " + PersistenceDBSchemaCreation.SCOPE + 
            "VALUES('" + stateId + "', 1000002, '" + new UID().toString() + "', " +
                    "'" + new UID().toString() + "', '" + FaultHandlingContext.COMPLETED + "', " +
                            "1000003, 4, null, null, null)");
            
            stmt.executeUpdate("INSERT INTO " + PersistenceDBSchemaCreation.CRMP + 
                    " (stateid, crmpinvokeid, partnerlink, operation) VALUES('" + stateId + "'" +
                            ", 'someCRMPInvokeId', 'somePLink', 'someOper' )");
            
            stmt.executeUpdate("INSERT INTO " + PersistenceDBSchemaCreation.SIMPLEVARIABLE + 
            "(stateid, varid, stringvalue, scopeguid) " + "values ('" + stateId + "', " +
                    "1000004, 'someStringValue', '" + new UID().toString() + "')");
            
            stmt.executeUpdate("INSERT INTO " + PersistenceDBSchemaCreation.PARTNERLINK + 
                    "VALUES('" + stateId + "', 1000005, null, '" + new UID().toString() + "')");
            
            stmt.executeUpdate("INSERT INTO " + PersistenceDBSchemaCreation.OUTSTANDINGMSGEX 
                    + "VALUES ('" + new UID().toString() + "', '" + stateId + "',  " + "'" + mBPELId1.toString() + "')");
            
            String ehId = new UID().toString();
            stmt.executeUpdate("INSERT INTO " + PersistenceDBSchemaCreation.EVENTHANDLER + 
                    "(stateid, ehid) " + "VALUES('" + stateId + "', '" + ehId + "')");
            // inserting a partnerlink associated with the above ehId
            stmt.executeUpdate("INSERT INTO " + PersistenceDBSchemaCreation.PARTNERLINK + 
                    "VALUES('" + ehId + "', 1000006, null, '" + new UID().toString() + "')");
            // insert a variable associated with the above ehId
            stmt.executeUpdate("INSERT INTO " + PersistenceDBSchemaCreation.VARIABLE +
                    "VALUES('" + ehId + "', 1000007, 'N', null, '" + new UID().toString() + "')");
                    
        } catch (Exception ex) {
            ex.printStackTrace();
            throw ex;
        } finally {
            if (stmt != null) {
                stmt.close();
            }
            if (dbConn != null) {
                dbConn.close(); 
            }
        }
        
        // one more persistent point here, that either deletes or marks the instance
        // for deletion
        state.exitScope(FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
        mStateMgr.persistState(state, TransactionInfo.getLocalTxInfo(), null);
    }
    
    private void insertMonitorData(DBConnectionFactory dbConnFac) throws Exception {
        AbstractDBConnection dbConn = null;
        Statement stmt = null;
        String instanceId = new UID().toString();
        try {
            dbConn = dbConnFac.createNonXAConnection();
            Connection conn = dbConn.getUnderlyingConnection();
            stmt = conn.createStatement();
            stmt.executeUpdate("insert into MONITORBPELINSTANCE VALUES ('someEngineId', '" +
                    instanceId + "', 'someBPELId', '" + BPELEventPersister.COMPLETED + 
                    "', null, null, null)");

            stmt.executeUpdate("INSERT INTO MONITORBPELACTIVITY VALUES ('someEngineId', '" +
                    instanceId + "', 1000000, 'someActivityXPath', 0, 'STARTED', 'N', null, " +
                            "null, null, null)");
            
            stmt.executeUpdate("INSERT INTO MONITORBPELVARIABLE VALUES('" + instanceId + "', " +
                    "1000002, 2000001, 'someVarName', 'N', null)");
            
            stmt.executeUpdate("INSERT INTO MONITORBPELACTIVITYVARIABLE VALUES('" + instanceId + "', " +
            "2000001, 1000003, 'someVarName', 'N', null, 'STRING', 'someVarType')");
            
        } catch (Exception ex) {
            ex.printStackTrace();
            throw ex;
        } finally {
            if (stmt != null) {
                stmt.close();
            }
            if (dbConn != null) {
                dbConn.close(); 
            }
        }
    }
    
    public void testMonitorTablesPurge() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testMonitorTablesPurge");

        createTables();
        
        DummyTxManagerAndDataSource dummyTMAndDS = new DummyTxManagerAndDataSource(mConnProp);
        DummyNonXATxManagerAndDataSource dummyNonXATMAndDS = 
            new DummyNonXATxManagerAndDataSource(mConnProp);
        BPELSERegistry registry = BPELSERegistry.getInstance();
        registry.register(TransactionManager.class.getName(), dummyTMAndDS);
        InitialContext ctx = new TestContext(dummyNonXATMAndDS, dummyTMAndDS, mConnProp);
            
        DBConnectionFactory dbConnFac = new DBConnectionFactory(mConnProp, ctx, null);
        
        PurgeData purge = new PurgeData();
        purge.purgeMonitoringData(dbConnFac);
        
        ResultSet resultSet = null;
        AbstractDBConnection dbConn = null;
        Statement stmt = null;
        int beforeCount;
        String status = BPELEventPersister.COMPLETED;
        try {
            dbConn = dbConnFac.createNonXAConnection();
            Connection conn = dbConn.getUnderlyingConnection();
            stmt = conn.createStatement();
            resultSet = stmt.executeQuery("select count(*) from "
                    + MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + " where " +
                    MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + ".status='"
                    + status + "'");
            resultSet.next();
            beforeCount = resultSet.getInt(1);
            System.out.println("count value before insert =" + beforeCount);
        }  finally {
            if (resultSet != null) {
                resultSet.close();
            }
            if (stmt != null) {
                stmt.close();
            }
            if (dbConn != null) {
                dbConn.close(); 
            }
        }
        
        insertMonitorData(dbConnFac);
        
        int afterInsertCount;
        try {
            dbConn = dbConnFac.createNonXAConnection();
            Connection conn = dbConn.getUnderlyingConnection();
            stmt = conn.createStatement();
            resultSet = stmt.executeQuery("select count(*) from "
                    + MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + " where " +
                    MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + ".status='"
                    + status + "'");
            resultSet.next();
            afterInsertCount = resultSet.getInt(1);
            System.out.println("count value after insert =" + afterInsertCount);
            assertTrue(afterInsertCount == (beforeCount + 1));
        }  finally {
            if (resultSet != null) {
                resultSet.close();
            }
            if (stmt != null) {
                stmt.close();
            }
            if (dbConn != null) {
                dbConn.close(); 
            }
        }
        
        purge.purgeMonitoringData(dbConnFac);
        
        int afterPurgeCount;
        try {
            dbConn = dbConnFac.createNonXAConnection();
            Connection conn = dbConn.getUnderlyingConnection();
            stmt = conn.createStatement();
            resultSet = stmt.executeQuery("select count(*) from "
                    + MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + " where " +
                    MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + ".status='"
                    + status + "'");
            resultSet.next();
            afterPurgeCount = resultSet.getInt(1);
            System.out.println("count value after purge =" + afterPurgeCount);
            assertTrue(afterPurgeCount == beforeCount);
        }  finally {
            if (resultSet != null) {
                resultSet.close();
            }
            if (stmt != null) {
                stmt.close();
            }
            if (dbConn != null) {
                dbConn.close(); 
            }
        }
        
        Utility.logExit(getClass().getSimpleName(), "testMonitorTablesPurge");
    }   
    
    private void createTables() throws Exception {
        DBSchemaCreation tablesCreator = MonitorDBSchemaCreation.getInstance();

        DummyTxManagerAndDataSource dummyTMAndDS = new DummyTxManagerAndDataSource(mConnProp);
        DummyNonXATxManagerAndDataSource dummyNonXATMAndDS = 
            new DummyNonXATxManagerAndDataSource(mConnProp);
        BPELSERegistry registry = BPELSERegistry.getInstance();
        registry.register(TransactionManager.class.getName(), dummyTMAndDS);
        InitialContext ctx = new TestContext(dummyNonXATMAndDS, dummyTMAndDS, mConnProp);
            
        DBConnectionFactory dbConnFac = new DBConnectionFactory(mConnProp, ctx, null);
        
        //Check to see if the tables are already present
        STATUS tablesAlreadyPresent = tablesCreator.checkTablesIntegrity(dbConnFac);

        // If the tables are already present drop them.
        if (tablesAlreadyPresent == STATUS.EMPTY) {
            //Create tables
            tablesCreator.createTables(dbConnFac);
        }
        
    }
    
    private void dropPersistenceTables() throws Exception { 
        DBSchemaCreation tablesCreator = PersistenceDBSchemaCreation.getInstance();

        DummyTxManagerAndDataSource dummyTMAndDS = new DummyTxManagerAndDataSource(mConnProp);
        DummyNonXATxManagerAndDataSource dummyNonXATMAndDS = 
            new DummyNonXATxManagerAndDataSource(mConnProp);
        BPELSERegistry registry = BPELSERegistry.getInstance();
        registry.register(TransactionManager.class.getName(), dummyTMAndDS);
        InitialContext ctx = new TestContext(dummyNonXATMAndDS, dummyTMAndDS, mConnProp);
            
        DBConnectionFactory dbConnFac = new DBConnectionFactory(mConnProp, ctx, null);
        
        tablesCreator.dropTables(mConnProp, dbConnFac);
    }

    public void testMonitorOnlyTablesPurge() throws Exception {
        // This test case is to test purging monitor data when ther persistence is turned off.
        // to easily simulate that, used the engine with persistence on, and dropped off the 
        // persistence tables. Subsequent tests will create the persistence tables again.
        
        /** https://open-jbi-components.dev.java.net/issues/show_bug.cgi?id=309
         */
        Utility.logEnter(getClass().getSimpleName(), "testMonitorOnlyTablesPurge");
    
        dropPersistenceTables();
        createTables();
        
        DummyTxManagerAndDataSource dummyTMAndDS = new DummyTxManagerAndDataSource(mConnProp);
        DummyNonXATxManagerAndDataSource dummyNonXATMAndDS = 
            new DummyNonXATxManagerAndDataSource(mConnProp);
        BPELSERegistry registry = BPELSERegistry.getInstance();
        registry.register(TransactionManager.class.getName(), dummyTMAndDS);
        InitialContext ctx = new TestContext(dummyNonXATMAndDS, dummyTMAndDS, mConnProp);
            
        DBConnectionFactory dbConnFac = new DBConnectionFactory(mConnProp, ctx, null);
        
        PurgeData purge = new PurgeData();
        purge.purgeMonitoringData(dbConnFac);
        
        ResultSet resultSet = null;
        AbstractDBConnection dbConn = null;
        Statement stmt = null;
        int beforeCount;
        String status = BPELEventPersister.COMPLETED;
        try {
            dbConn = dbConnFac.createNonXAConnection();
            Connection conn = dbConn.getUnderlyingConnection();
            stmt = conn.createStatement();
            resultSet = stmt.executeQuery("select count(*) from "
                    + MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + " where " +
                    MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + ".status='"
                    + status + "'");
            resultSet.next();
            beforeCount = resultSet.getInt(1);
            System.out.println("count value before insert =" + beforeCount);
        }  finally {
            if (resultSet != null) {
                resultSet.close();
            }
            if (stmt != null) {
                stmt.close();
            }
            if (dbConn != null) {
                dbConn.close(); 
            }
        }
        
        insertMonitorData(dbConnFac);
        
        int afterInsertCount;
        try {
            dbConn = dbConnFac.createNonXAConnection();
            Connection conn = dbConn.getUnderlyingConnection();
            stmt = conn.createStatement();
            resultSet = stmt.executeQuery("select count(*) from "
                    + MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + " where " +
                    MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + ".status='"
                    + status + "'");
            resultSet.next();
            afterInsertCount = resultSet.getInt(1);
            System.out.println("count value after insert =" + afterInsertCount);
            assertTrue(afterInsertCount == (beforeCount + 1));
        }  finally {
            if (resultSet != null) {
                resultSet.close();
            }
            if (stmt != null) {
                stmt.close();
            }
            if (dbConn != null) {
                dbConn.close(); 
            }
        }
        
        purge.purgeMonitoringData(dbConnFac);
        
        int afterPurgeCount;
        try {
            dbConn = dbConnFac.createNonXAConnection();
            Connection conn = dbConn.getUnderlyingConnection();
            stmt = conn.createStatement();
            resultSet = stmt.executeQuery("select count(*) from "
                    + MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + " where " +
                    MonitorDBSchemaCreation.MONITOR_BPEL_INSTANCE + ".status='"
                    + status + "'");
            resultSet.next();
            afterPurgeCount = resultSet.getInt(1);
            System.out.println("count value after purge =" + afterPurgeCount);
            assertTrue(afterPurgeCount == beforeCount);
        }  finally {
            if (resultSet != null) {
                resultSet.close();
            }
            if (stmt != null) {
                stmt.close();
            }
            if (dbConn != null) {
                dbConn.close(); 
            }
        }
        
        Utility.logExit(getClass().getSimpleName(), "testMonitorOnlyTablesPurge");
    }
}
