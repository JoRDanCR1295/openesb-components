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
 * @(#)DebuggerTesterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.bpelmonitor;

import java.util.Properties;

import javax.naming.InitialContext;
import javax.transaction.TransactionManager;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.persist.DBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.MonitorDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.DBSchemaCreation.STATUS;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.DummyNonXATxManagerAndDataSource;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.DummyTxManagerAndDataSource;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.SetUpHelper;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.TestContext;


/**
 *
 * @author Sun Microsystems
 */
public class MonitorTesterTest extends TestCase {
    
	  protected Properties mConnProp;
	  
    /** Creates a new instance of PersistenceTesterTest */
    public MonitorTesterTest(String testname) {
        super(testname);
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        System.setProperty("bpelse.properties", "bpelsemonitor.properties");
        mConnProp = new SetUpHelper ().getEngineProperties();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(MonitorTesterTest.class);
        return suite;
    }
    
    public void testInvoke() throws Exception {
        truncateMonitorTable();
        setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/invoke", "invoke.properties");
    }
    
    private void setWait(int i) {
		// TODO Auto-generated method stub
		System.setProperty("ToWait", new Integer(i).toString());
	}

	public void testAssign() throws Exception {
    	truncateMonitorTable();
    	setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/assign1", "assign.properties");
    }
    
    public void testSimpleWait() throws Exception {
    	truncateMonitorTable();
    	setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/simpleWait", "simpleWait.properties");
    }
    
    public void testWhileWait() throws Exception {
    	truncateMonitorTable();
    	setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/whileTest", "WhileWait.properties");
    }
    
    public void testWhileInvoke() throws Exception {
    	truncateMonitorTable();
    	setWait(4000);
        CommonPersistenceTesterHelper.commonCode("bpels/whileTest/invoke", "WhileInvoke.properties");
    }
    
    public void testRepeatUntilWait() throws Exception {
    	truncateMonitorTable();
    	setWait(2000);
        CommonPersistenceTesterHelper.commonCode("bpels/repeatUntil", "RepeatUntilWait.properties");
    }

    public void testRepeatUntilInvoke() throws Exception {
    	truncateMonitorTable();
    	setWait(4000);
        CommonPersistenceTesterHelper.commonCode("bpels/repeatUntil/invoke", "RepeatUntilInvoke.properties");
    }

    public void testPickIfWhileRepeatUntil() throws Exception {
    	 truncateMonitorTable();
    	 setWait(10000);
        CommonPersistenceTesterHelper.commonCode("bpels/pickifwhilerepeatuntil", "pickifwhilerepeatuntil.properties");
    }  
    
    public void testTruncateMonitorTable () throws Exception {
    	truncateMonitorTable();
    }
    
    public void truncateMonitorTable() throws Exception {
    	  
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
        if (tablesAlreadyPresent != STATUS.EMPTY) {
            tablesCreator.truncateTables(mConnProp, dbConnFac);
        }
    }

}
