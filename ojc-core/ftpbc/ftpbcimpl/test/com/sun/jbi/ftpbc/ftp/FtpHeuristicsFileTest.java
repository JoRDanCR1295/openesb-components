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
 * @(#)FtpHeuristicsFileTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp;

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
public class FtpHeuristicsFileTest extends TestCase {
    FtpHeuristicsFile instance;
    
    public FtpHeuristicsFileTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        instance = new FtpHeuristicsFile();
    }
    
    protected void tearDown() throws Exception {
        instance = null;
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(FtpHeuristicsFileTest.class);
        
        return suite;
    }
    
    /**
     * Test of setRawListing method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testSetRawListing() throws Exception {
        System.out.println("setRawListing");
        
        String s = "one line entry";
        instance.setRawListing(s);
        assertEquals(s, instance.getRawListing());
        
    }
    
    /**
     * Test of getRawListing method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testGetRawListing() throws Exception {
        System.out.println("getRawListing");
        
        String expResult = null;
        String result = instance.getRawListing();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isDirectory method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testIsDirectory() throws Exception {
        System.out.println("isDirectory");
        
        boolean expResult = false;
        boolean result = instance.isDirectory();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isFile method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testIsFile() throws Exception {
        System.out.println("isFile");
        
        boolean expResult = false;
        boolean result = instance.isFile();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isSymbolicLink method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testIsSymbolicLink() throws Exception {
        System.out.println("isSymbolicLink");
        
        boolean expResult = false;
        boolean result = instance.isSymbolicLink();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of isUnknown method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testIsUnknown() throws Exception {
        System.out.println("isUnknown");
        
        boolean expResult = true;
        boolean result = instance.isUnknown();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setType method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testSetType() throws Exception {
        System.out.println("setType");
        
        int i = instance.DIRECTORY_TYPE;
        instance.setType(i);
        assertEquals(i, instance.getType());
        
    }
    
    /**
     * Test of getType method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testGetType() throws Exception {
        System.out.println("getType");
        
        int expResult = instance.UNKNOWN_TYPE;
        int result = instance.getType();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setName method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testSetName() throws Exception {
        System.out.println("setName");
        
        String s = "abc";
        instance.setName(s);
        assertEquals(s, instance.getName());
        
    }
    
    /**
     * Test of getName method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testGetName() throws Exception {
        System.out.println("getName");
        
        String expResult = null;
        String result = instance.getName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setSize method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testSetSize() throws Exception {
        System.out.println("setSize");
        
        long l = 1024;
        instance.setSize(l);
        assertEquals(l, instance.getSize());
        
    }
    
    /**
     * Test of getSize method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testGetSize() throws Exception {
        System.out.println("getSize");
        
        long expResult = -1;
        long result = instance.getSize();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setLink method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testSetLink() throws Exception {
        System.out.println("setLink");
        
        String s = "admin -> ../../usr/lpp/dce";
        instance.setLink(s);
        assertEquals(s, instance.getLink());
        
    }
    
    /**
     * Test of getLink method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testGetLink() throws Exception {
        System.out.println("getLink");
        
        String expResult = null;
        String result = instance.getLink();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of toString method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testToString() throws Exception {
        System.out.println("toString");
        
        instance.setRawListing("raw listing");
        
        String result = instance.toString();
        assertEquals(instance.getRawListing(), result);
        
    }
    
    /**
     * Test of isDataAvailable method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testIsDataAvailable() throws Exception {
        System.out.println("isDataAvailable");
        
        boolean expResult = true;
        boolean result = instance.isDataAvailable();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setDataAvailable method, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testSetDataAvailable() throws Exception {
        System.out.println("setDataAvailable");
        
        boolean newDataAvailable = false;
        instance.setDataAvailable(newDataAvailable);
        assertFalse(instance.isDataAvailable());
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.ftp.FtpHeuristicsFile.
     */
    public void testFtpHeuristicsFile() throws Exception {
        System.out.println("FtpHeuristicsFile");
        
        assertNotNull(new FtpHeuristicsFile());
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
