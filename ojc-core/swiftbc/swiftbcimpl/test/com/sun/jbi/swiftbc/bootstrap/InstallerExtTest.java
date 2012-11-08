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
 * @(#)InstallerExtTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.bootstrap;

import com.sun.jbi.swiftbc.bootstrap.InstallerExt;
import junit.framework.*;


/**
 *
 * @author Raghunadh
 */
public class InstallerExtTest extends TestCase {
    InstallerExt instance = new InstallerExt();
            
    public InstallerExtTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(InstallerExtTest.class);
        
        return suite;
    }

    /**
     * Test of setThreads and getThreads method, of class com.sun.jbi.hl7bc.bootstrap.InstallerExt.
     */
    public void testSetGetThreads() {
        System.out.println("Testing setThreads and getThreads");
        
        String threads = "someNumberOfThreads";
        instance.setThreads(threads);
        String result = instance.getThreads();
        assertEquals(threads, result);
    }

	/**
     * Test of setDB_URL and getDB_URL method, of class com.sun.jbi.hl7bc.bootstrap.InstallerExt.
     */
    public void testSetGetDB_URL() {
        System.out.println("Testing setDB_URL and getDB_URL");
        
        String expResult = "jdbc:derby://localhost:1527/sun-appserv-samples";
        instance.setDB_URL(expResult);
        String result = instance.getDB_URL();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setDB_URL and getDB_URL");
    }

	/**
     * Test of setDB_Type and getDB_Type method, of class com.sun.jbi.hl7bc.bootstrap.InstallerExt.
     */
    public void testSetGetDB_Type() {
        System.out.println("Testing setDB_Type and getDB_Type");
        
        String expResult = "4";
        instance.setDB_Type(expResult);
        String result = instance.getDB_Type();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setDB_Type and getDB_Type");
    }

	/**
     * Test of setDB_UserName and getDB_UserName method, of class com.sun.jbi.hl7bc.bootstrap.InstallerExt.
     */
    public void testSetDB_UserName() {
        System.out.println("Testing setDB_UserName and getDB_UserName");
        
        String expResult = "app";
        instance.setDB_UserName(expResult);
        String result = instance.getDB_UserName();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setDB_UserName and getDB_UserName");
    }

	/**
     * Test of setDB_Password and getDB_Password method, of class com.sun.jbi.hl7bc.bootstrap.InstallerExt.
     */
    public void testSetGetDB_Password() {
        System.out.println("Testing setDB_Password and getDB_Password");
        
        String expResult = "app";
        instance.setDB_Password(expResult);
        String result = instance.getDB_Password();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setDB_Password and getDB_Password");
    }

	/**
     * Test of setDB_JNDIName and getDB_JNDIName method, of class com.sun.jbi.hl7bc.bootstrap.InstallerExt.
     */
    public void testSetGetDB_JNDIName() {
        System.out.println("Testing setDB_JNDIName and getDB_JNDIName");
        
        String expResult = "jdbc/__default";
        instance.setDB_JNDIName(expResult);
        String result = instance.getDB_JNDIName();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setDB_JNDIName and getDB_JNDIName");
    }
}
