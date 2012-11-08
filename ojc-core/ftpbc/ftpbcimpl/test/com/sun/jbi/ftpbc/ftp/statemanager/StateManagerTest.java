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
 * @(#)StateManagerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp.statemanager;

import java.io.File;
import java.io.FileOutputStream;
import java.io.Serializable;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

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
public class StateManagerTest extends TestCase {
    File testFile;
    StateManager instance;
    
    public StateManagerTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        // create a file with 10 bytes content
        testFile = File.createTempFile("testStateManager","");
        FileOutputStream fos = new FileOutputStream(testFile);
        byte[] tenBytes = "0123456789".getBytes();
        fos.write(tenBytes);
        fos.flush();
        fos.close();
        
        instance = new StateManager (new FileStatePersistenceAdapter (testFile.getParent(), testFile.getName()));
    }
    
    protected void tearDown() throws Exception {
        try {
            testFile.deleteOnExit();
            testFile.delete();
            instance.close();
        } catch (Exception ex) {
            ;
        }
        instance = null;
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(StateManagerTest.class);
        
        return suite;
    }
    
    /**
     * Test of close method, of class com.sun.jbi.ftpbc.statemanager.StateManager.
     */
    public void testClose() throws Exception {
        System.out.println("close");
        
        instance.close();
        
    }
    
    /**
     * Test of setState method, of class com.sun.jbi.ftpbc.statemanager.StateManager.
     */
    public void testSetState() throws Exception {
        System.out.println("setState");
        
        Serializable state = null;
        instance.setState(state);
        assertEquals(state, instance.getState());
        
        state = "some data";
        instance.setState(state);
        assertEquals(state, instance.getState());
    }
    
    /**
     * Test of getState method, of class com.sun.jbi.ftpbc.statemanager.StateManager.
     */
    public void testGetState() throws Exception {
        System.out.println("getState");
        
        Serializable expResult = null;
        Serializable result = instance.getState();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of store method, of class com.sun.jbi.ftpbc.statemanager.StateManager.
     */
    public void testStore() throws Exception {
        System.out.println("store");
        try {
            instance.store();
            fail("An exception is expected - no state is set yet");
        } catch (StateManagerException ex) {
            ex.printStackTrace();
        }
        
        instance.setState("some data for Store");
        instance.store();
        
    }
    
    /**
     * Test of load method, of class com.sun.jbi.ftpbc.statemanager.StateManager.
     */
    public void testLoad() throws Exception {
        System.out.println("load");

        try {
            instance.load();
            fail("An exception is expected - the pre-filled content is not load-able");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        //depend on status, it could be true or false:  assertNotNull(instance.getState());
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.statemanager.StateManager.
     */
    public void testStateManager () throws Exception {
        System.out.println ("StateManager");
        
        assertNotNull (new StateManager ());
        assertNotNull (new StateManager (new FileStatePersistenceAdapter (System.getProperty("java.io.tmpdir"), "testStateManager")));
        assertNotNull (new StateManager (null));
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
