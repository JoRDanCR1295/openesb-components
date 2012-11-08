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
 * @(#)PersistenceTesterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.persistence;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;


/**
 *
 * @author Sun Microsystems
 */
public class EventHdlrPersistenceTesterTest extends TestCase {
    
    /** Creates a new instance of PersistenceTesterTest */
    public EventHdlrPersistenceTesterTest(String testname) {
        super(testname);
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        CommonPersistenceTesterHelper.setUp();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(EventHdlrPersistenceTesterTest.class);
        return suite;
    }
    
     public void testProcessLevelEventHdlr() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/eventhandler/process", "process.properties");
        verifyAllInstancesAreComplete();
    }
  
     public void testProcessLevelNestedEventHdlr() throws Exception {
         CommonPersistenceTesterHelper.commonCode("bpels/eventhandler/processLevelNestedEH", "processLevelNestedEH.properties");
         verifyAllInstancesAreComplete();
     }
     
     public void test_eveHdlrScopeBeforeRec() throws Exception {
         CommonPersistenceTesterHelper.commonCode("bpels/eventhandler/eveHdlrScopeBeforeRec", "eveHdlrScopeBeforeRec.properties");
         verifyAllInstancesAreComplete();
     }
     
     public void test_nestedScopeLevelEveHdlr() throws Exception {
         CommonPersistenceTesterHelper.commonCode("bpels/eventhandler/nestedScopeLevelEveHdlr", "nestedScopeLevelEveHdlr.properties");
         verifyAllInstancesAreComplete();
     }
     
     public void test_flowInEveHandler() throws Exception {
         CommonPersistenceTesterHelper.commonCode("bpels/eventhandler/flowInEveHandler", "flowInEveHandler.properties");
         verifyAllInstancesAreComplete();
     }
     
     public void testOnlyFor() throws Exception {
         CommonPersistenceTesterHelper.commonCode("bpels/eventhandler/onAlarm/Basic", "For.properties");
         verifyAllInstancesAreComplete();
     }
     
     public void testOnlyFor_crashpoint3() throws Exception {
         CommonPersistenceTesterHelper.commonCode("bpels/eventhandler/onAlarm/Basic", "For_crashpoint3.properties");
         verifyAllInstancesAreComplete();
     }
     
     public void testOnlyRepeatEvery() throws Exception {
         CommonPersistenceTesterHelper.commonCode("bpels/eventhandler/onAlarm/Basic", "RepeatEvery.properties");
         verifyAllInstancesAreComplete();
     }

     public void testVariablesDefinedInEH() throws Exception {
         CommonPersistenceTesterHelper.commonCode("bpels/eventhandler/VariablesDefinedInEH", "VariablesDefinedInEH.properties");
         verifyAllInstancesAreComplete();
     }
     
     private static void verifyAllInstancesAreComplete() throws Exception {
        
//        DummyTxManagerAndDataSource tm = (DummyTxManagerAndDataSource) BPELSERegistry.getInstance().lookup(
//                TransactionManager.class.getName());
//        Connection conn = tm.getNonTxConnection();
//        Statement stmt = conn.createStatement();
//        ResultSet rs = stmt.executeQuery("select count(*) from state where status != 'DONE'");
//        rs.next();
//        int incompleteInstanceCount = rs.getInt(1);
//        assertTrue(incompleteInstanceCount == 0);        
    }
}
