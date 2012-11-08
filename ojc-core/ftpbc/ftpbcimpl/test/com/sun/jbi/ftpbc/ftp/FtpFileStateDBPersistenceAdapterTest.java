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
 * @(#)FtpFileStateDBPersistenceAdapterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp;

import com.sun.jbi.ftpbc.ftp.statemanager.StatePersistenceException;
import java.io.Serializable;
import java.sql.Connection;
import java.util.Arrays;
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
public class FtpFileStateDBPersistenceAdapterTest extends TestCase {
    private FtpFileStateDBPersistenceAdapter instance;

    public FtpFileStateDBPersistenceAdapterTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
        try {
            instance = new FtpFileStateDBPersistenceAdapter("this ftpbc");
        } catch (StatePersistenceException ex) {
            ex.printStackTrace();
            instance = null;
        }
    }

    protected void tearDown() throws Exception {
        instance = null;
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(FtpFileStateDBPersistenceAdapterTest.class);
        
        return suite;
    }
    
    /**
     * Test of save method, of class com.sun.jbi.ftpbc.ftp.FtpFileStateDBPersistenceAdapter.
     */
    public void testSave() throws Exception {
        System.out.println("save");
        if (null == instance) {
            System.out.println("Database is not available - no live test.");
            return;
        }
        
        Serializable state = new FtpFileState();
        
        instance.save(state);
        
    }

    /**
     * Test of restore method, of class com.sun.jbi.ftpbc.ftp.FtpFileStateDBPersistenceAdapter.
     */
    public void testRestore() throws Exception {
        System.out.println("restore");
        if (null == instance) {
            System.out.println("Database is not available - no live test.");
            return;
        }
        
        FtpFileState expResult = new FtpFileState();
        Arrays.fill(expResult.getSequenceNo(), 100);
        // save first
        instance.save(expResult);
        
        FtpFileState result = (FtpFileState) instance.restore();
        for (int i = 0; i < expResult.getSequenceNo().length; i++) {
            assertEquals(expResult.getSequenceNo(i), result.getSequenceNo(i));
        }
        
    }

    /**
     * Test of close method, of class com.sun.jbi.ftpbc.ftp.FtpFileStateDBPersistenceAdapter.
     */
    public void testClose() throws Exception {
        System.out.println("close");
        if (null == instance) {
            System.out.println("Database is not available - no live test.");
            return;
        }
        
        instance.close();
        
    }

    /**
     * Test of getConnection method, of class com.sun.jbi.ftpbc.ftp.FtpFileStateDBPersistenceAdapter.
     */
    public void testGetConnection() throws Exception {
        System.out.println("getConnection");
        if (null == instance) {
            System.out.println("Database is not available - no live test.");
            return;
        }
        
        Connection result = instance.getConnection();
        assertNotNull(result);
        
    }

    /**
     * Test of createTableIfNeeded method, of class com.sun.jbi.ftpbc.ftp.FtpFileStateDBPersistenceAdapter.
     */
    public void testCreateTableIfNeeded() throws Exception {
        System.out.println("createTableIfNeeded");
        if (null == instance) {
            System.out.println("Database is not available - no live test.");
            return;
        }
        
        instance.createTableIfNeeded();
        
    }

    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.FtpFileStateDBPersistenceAdapter.
     */
    public void testFtpFileStateDBPersistenceAdapter () throws Exception {
        System.out.println ("FtpFileStateDBPersistenceAdapter");
        if (null == instance) {
            System.out.println("Database is not available - no live test.");
            return;
        }
        
        assertNotNull (new FtpFileStateDBPersistenceAdapter (System.getProperty("user.home")));
        
        try {
            new FtpFileStateDBPersistenceAdapter (null);
            fail("An exception is expected - null file name key is not allowed");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
