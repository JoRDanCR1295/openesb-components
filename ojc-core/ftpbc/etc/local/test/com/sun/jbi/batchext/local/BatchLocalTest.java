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
 * @(#)BatchLocalTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.local;

import junit.framework.*;
import com.sun.jbi.batchext.BatchException;
import java.io.File;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.batchext.statemanager.StateManager;
import com.sun.jbi.batchext.statemanager.StateManagerException;
import com.sun.jbi.batchext.statemanager.StatePersistenceAdapter;
import com.sun.jbi.batchext.statemanager.StatePersistenceAdapterFactory;
import com.sun.jbi.batchext.statemanager.StatePersistenceException;

/*
 * JUnit based test.
 *
 * 
 * 
 * 
 * @author Harry Liu (harry.liu@sun.com)
 *
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 */
public class BatchLocalTest extends TestCase {
    
    public BatchLocalTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(BatchLocalTest.class);
        
        return suite;
    }

    /**
     * Test of initialize method, of class com.sun.jbi.batchext.local.BatchLocal.
     */
    public void testInitialize() throws Exception {
        System.out.println("initialize");
        
        Properties p = null;
        BatchLocal instance = new BatchLocal();
        
        instance.initialize(p);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of reset method, of class com.sun.jbi.batchext.local.BatchLocal.
     */
    public void testReset() throws Exception {
        System.out.println("reset");
        
        BatchLocal instance = new BatchLocal();
        
        boolean expResult = true;
        boolean result = instance.reset();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of terminate method, of class com.sun.jbi.batchext.local.BatchLocal.
     */
    public void testTerminate() throws Exception {
        System.out.println("terminate");
        
        BatchLocal instance = new BatchLocal();
        
        instance.terminate();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getConfiguration method, of class com.sun.jbi.batchext.local.BatchLocal.
     */
    public void testGetConfiguration() {
        System.out.println("getConfiguration");
        
        BatchLocal instance = new BatchLocal();
        
        LocalFileConfiguration expResult = null;
        LocalFileConfiguration result = instance.getConfiguration();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setConfiguration method, of class com.sun.jbi.batchext.local.BatchLocal.
     */
    public void testSetConfiguration() {
        System.out.println("setConfiguration");
        
        LocalFileConfiguration cfg = null;
        BatchLocal instance = new BatchLocal();
        
        instance.setConfiguration(cfg);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getClient method, of class com.sun.jbi.batchext.local.BatchLocal.
     */
    public void testGetClient() {
        System.out.println("getClient");
        
        BatchLocal instance = new BatchLocal();
        
        LocalFileClient expResult = null;
        LocalFileClient result = instance.getClient();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getProvider method, of class com.sun.jbi.batchext.local.BatchLocal.
     */
    public void testGetProvider() {
        System.out.println("getProvider");
        
        BatchLocal instance = new BatchLocal();
        
        LocalFileProvider expResult = null;
        LocalFileProvider result = instance.getProvider();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getStateManager method, of class com.sun.jbi.batchext.local.BatchLocal.
     */
    public void testGetStateManager() {
        System.out.println("getStateManager");
        
        BatchLocal instance = new BatchLocal();
        
        StateManager expResult = null;
        StateManager result = instance.getStateManager();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPersistentState method, of class com.sun.jbi.batchext.local.BatchLocal.
     */
    public void testGetPersistentState() {
        System.out.println("getPersistentState");
        
        BatchLocal instance = new BatchLocal();
        
        LocalFileState expResult = null;
        LocalFileState result = instance.getPersistentState();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of storePersistentState method, of class com.sun.jbi.batchext.local.BatchLocal.
     */
    public void testStorePersistentState() throws Exception {
        System.out.println("storePersistentState");
        
        BatchLocal instance = new BatchLocal();
        
        instance.storePersistentState();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of createStateManager method, of class com.sun.jbi.batchext.local.BatchLocal.
     */
    public void testCreateStateManager() throws Exception {
        System.out.println("createStateManager");
        
        BatchLocal instance = new BatchLocal();
        
        instance.createStateManager();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
