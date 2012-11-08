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
 * @(#)StatePersistenceAdapterFactoryTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp.statemanager;

import java.io.File;
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
public class StatePersistenceAdapterFactoryTest extends TestCase {
    
    public StatePersistenceAdapterFactoryTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
    }
    
    protected void tearDown() throws Exception {
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(StatePersistenceAdapterFactoryTest.class);
        
        return suite;
    }
    
    /**
     * Test of createFileStatePersistenceAdapter method, of class com.sun.jbi.ftpbc.statemanager.StatePersistenceAdapterFactory.
     */
    public void testCreateFileStatePersistenceAdapter() throws Exception {
        System.out.println("createFileStatePersistenceAdapter");
        
        assertNotNull(StatePersistenceAdapterFactory.createFileStatePersistenceAdapter(System.getProperty("java.io.tmpdir"), "FileExistenceIsNotCheckedHere"));
        assertNotNull(StatePersistenceAdapterFactory.createFileStatePersistenceAdapter(System.getProperty("java.io.tmpdir"), "")); // why empty file name is allowed?
        
        assertNotNull(StatePersistenceAdapterFactory.createFileStatePersistenceAdapter(System.getProperty("java.io.tmpdir") + File.separator + "testCreateFileStatePersistenceAdapter", "FileExistenceIsNotCheckedHere"));
        
        try {
            StatePersistenceAdapterFactory.createFileStatePersistenceAdapter(System.getProperty("java.io.tmpdir"), null);
            fail("An exception is expected - null file name is not allowed");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        try {
            StatePersistenceAdapterFactory.createFileStatePersistenceAdapter(null, "abc");
            fail("An exception is expected - null dir name is not allowed");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of createFileStatePersistenceAdapterTrans method, of class com.sun.jbi.ftpbc.statemanager.StatePersistenceAdapterFactory.
     */
    public void testCreateFileStatePersistenceAdapterTrans() throws Exception {
        System.out.println("createFileStatePersistenceAdapterTrans");
        
        assertNotNull(StatePersistenceAdapterFactory.createFileStatePersistenceAdapterTrans(System.getProperty("java.io.tmpdir"), "FileExistenceIsNotCheckedHere"));
        assertNotNull(StatePersistenceAdapterFactory.createFileStatePersistenceAdapterTrans(System.getProperty("java.io.tmpdir"), "")); // why empty file name is allowed?
        
        assertNotNull(StatePersistenceAdapterFactory.createFileStatePersistenceAdapterTrans(System.getProperty("java.io.tmpdir") + File.separator + "testCreateFileStatePersistenceAdapterTrans", "FileExistenceIsNotCheckedHere"));
        
        try {
            StatePersistenceAdapterFactory.createFileStatePersistenceAdapterTrans(System.getProperty("java.io.tmpdir"), null);
            fail("An exception is expected - null file name is not allowed");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        try {
            StatePersistenceAdapterFactory.createFileStatePersistenceAdapterTrans(null, "abc");
            fail("An exception is expected - null dir name is not allowed");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.statemanager.StatePersistenceAdapterFactory.
     */
    public void testStatePersistenceAdapterFactory() throws Exception {
        System.out.println("StatePersistenceAdapterFactory");
        
        assertNotNull(new StatePersistenceAdapterFactory());
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
