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

import javax.naming.InitialContext;
import javax.transaction.TransactionManager;

import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.persist.DBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.MonitorDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.DBSchemaCreation.STATUS;
import com.sun.jbi.engine.bpel.core.test.common.DummyNonXATxManagerAndDataSource;
import com.sun.jbi.engine.bpel.core.test.common.DummyTxManagerAndDataSource;
import com.sun.jbi.engine.bpel.core.test.common.TestContext;
import com.sun.jbi.engine.bpel.core.test.common.Utility;


/**
 * @author Sun Microsystems
 */
public class DBSchemaCreationTest extends AbstractTestCase {

    public DBSchemaCreationTest(String testName) {
        super(testName);
    }

    public void testPersistenceTablesCreation() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testTablesCreation");

        DBSchemaCreation tablesCreator = PersistenceDBSchemaCreation.getInstance();

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
            tablesCreator.dropTables(mConnProp, dbConnFac);
            //ensure that the tables are dropped
            assertTrue(tablesCreator.checkTablesIntegrity(dbConnFac) == STATUS.EMPTY);
        }
        
        //Create tables
        tablesCreator.createTables(dbConnFac);
        
        //Verify that the tables were created
        assertTrue(tablesCreator.checkTablesIntegrity(dbConnFac) == STATUS.VALID);

        tablesCreator.dropTables(mConnProp, dbConnFac);
        assertTrue(tablesCreator.checkTablesIntegrity(dbConnFac) == STATUS.EMPTY);

        //These tables are required for other tests (need to verify this)
        tablesCreator.checkAndCreateTables(mConnProp, dbConnFac);
        assertTrue(tablesCreator.checkTablesIntegrity(dbConnFac) == STATUS.VALID);
        
        Utility.logExit(getClass().getSimpleName(), "testTablesCreation");
    }
    
    public void testMonitorTablesCreation() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testTablesCreation");

        DBSchemaCreation tablesCreator = MonitorDBSchemaCreation.getInstance();

        DummyTxManagerAndDataSource dummyTMAndDS = new DummyTxManagerAndDataSource(mConnProp);
        DummyNonXATxManagerAndDataSource dummyNonXATMAndDS = 
            new DummyNonXATxManagerAndDataSource(mConnProp);
        BPELSERegistry registry = BPELSERegistry.getInstance();
        registry.register(TransactionManager.class.getName(), dummyTMAndDS);
        InitialContext ctx = new TestContext(dummyNonXATMAndDS, dummyTMAndDS, mConnProp);
            
        DBConnectionFactory dbConnFac = new DBConnectionFactory(mConnProp, ctx, null);
        
        //Check to see if the tables are already present
        STATUS status = tablesCreator.checkTablesIntegrity(dbConnFac);

        // If the tables are already present drop them.
        if (status != STATUS.EMPTY) {
            tablesCreator.dropTables(mConnProp, dbConnFac);
            //ensure that the tables are dropped
            assertTrue(tablesCreator.checkTablesIntegrity(dbConnFac) == STATUS.EMPTY);
        }
        
        //Create tables
        tablesCreator.createTables(dbConnFac);
        
        //Verify that the tables were created
        assertTrue(tablesCreator.checkTablesIntegrity(dbConnFac) == STATUS.VALID);

        tablesCreator.dropTables(mConnProp, dbConnFac);
        assertTrue(tablesCreator.checkTablesIntegrity(dbConnFac) == STATUS.EMPTY);

        //These tables are required for other tests (need to verify this)
        tablesCreator.checkAndCreateTables(mConnProp, dbConnFac);
        assertTrue(tablesCreator.checkTablesIntegrity(dbConnFac) == STATUS.VALID);
        
        Utility.logExit(getClass().getSimpleName(), "testTablesCreation");
    }   
    
 
}
