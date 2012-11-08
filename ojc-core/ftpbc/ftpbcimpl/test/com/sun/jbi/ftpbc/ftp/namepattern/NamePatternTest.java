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
 * @(#)NamePatternTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp.namepattern;

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
public class NamePatternTest extends TestCase {

    NamePattern instance = null;
    public NamePatternTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
        instance = new NamePattern("Nothing to be expanded");
    }

    protected void tearDown() throws Exception {
        instance = null;
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(NamePatternTest.class);
        
        return suite;
    }

    /**
     * Test of main method, of class com.sun.jbi.ftpbc.NamePattern.
     */
    public void testMain() throws Exception {
        System.out.println("main");
        
        String[] args1 = {};
        
        NamePattern.main(args1);
        
        String[] args2 = {"A name with time stamps pattern - %y%y%y%y.%M%M.%d%d %G 'at' %h%h:%m%m:%s%s %z"};
        NamePattern.main(args2);
        
    }

    /**
     * Test of expand method, of class com.sun.jbi.ftpbc.NamePattern.
     */
    public void testExpand_1() throws Exception {
        System.out.println("expand_1");
        
        // test 1
        String expResult = "Nothing to be expanded";
        String result = instance.expand();
        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
        assertEquals(expResult, result);
    }

    /**
     * Test of expand method, of class com.sun.jbi.ftpbc.NamePattern.
     */
    public void testExpand_2() throws Exception {
        System.out.println("expand_2");
        
        // test 2
        String expResult = "A name with UUID pattern - %u and another %u";
        instance = new NamePattern(expResult);
        String result = instance.expand();
        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
        assertFalse("Not equal - time stamps expanded already", expResult.equals(result));
    }

    /**
     * Test of expand method, of class com.sun.jbi.ftpbc.NamePattern.
     */
    public void testExpand_3() throws Exception {
        System.out.println("expand_3");
        
        // test 3
        String expResult = "A name with time stamps pattern - %y%y%y%y%M%M%d%d.%h%h%m%m%S%S%S%S";
        instance = new NamePattern(expResult);
        String result = instance.expand();
        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
        assertFalse("Not equal - time stamps expanded already", expResult.equals(result));
    }

    /**
     * Test of expand method, of class com.sun.jbi.ftpbc.NamePattern.
     */
    public void testExpand_4() throws Exception {
        System.out.println("expand_4");
        
        // test 4
//        String expResult = "A name with sequence number pattern - %1%1%1%1%1 and %1%1%1";
//        instance = new NamePattern(expResult);
//        String result = instance.expand();
//        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
//        assertFalse("Not equal - sequence number expanded already", expResult.equals(result));
//        assertEquals("Stating from 1 by default", "A name with sequence number pattern - 00001 and 001", result);
//        
//        result = instance.expand();
//        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
//        assertEquals("Increased by 1", "A name with sequence number pattern - 00002 and 002", result);
//        
//        result = instance.expand();
//        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
//        assertEquals("Increased by 1", "A name with sequence number pattern - 00003 and 003", result);
//        
//        result = instance.expand();
//        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
//        assertEquals("Increased by 1", "A name with sequence number pattern - 00004 and 004", result);
//        
//        result = instance.expand();
//        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
//        assertEquals("Increased by 1", "A name with sequence number pattern - 00005 and 005", result);
//        
//        result = instance.expand();
//        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
//        assertEquals("Increased by 1", "A name with sequence number pattern - 00006 and 006", result);
//        
//        result = instance.expand();
//        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
//        assertEquals("Increased by 1", "A name with sequence number pattern - 00007 and 007", result);
//        
//        result = instance.expand();
//        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
//        assertEquals("Increased by 1", "A name with sequence number pattern - 00008 and 008", result);
    }

    /**
     * Test of expand method, of class com.sun.jbi.ftpbc.NamePattern.
     */
    public void testExpand_5() throws Exception {
        System.out.println("expand_5");
        
        // test 5
        String expResult = "A name with working file pattern - %f";
        instance = new NamePattern(expResult);
        String result = instance.expand();
        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
        assertEquals("Not expanded because the original file is not set", expResult, result);
        
        instance.setFileNameFromEgate("abc");
        result = instance.expand();
        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
        assertFalse("Not equal - working file expanded already", expResult.equals(result));
        assertEquals("A name with working file pattern - abc", result);
    }

    /**
     * Test of expand method, of class com.sun.jbi.ftpbc.NamePattern.
     */
    public void testExpand_6() throws Exception {
        System.out.println("expand_6");
        
        // test 6
        String expResult = "A name with multiple patterns - %f.%h%h%m%m%s%s";
        instance = new NamePattern(expResult);
        String result = instance.expand();
        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
        assertFalse("Not equal - expanded already except %f", expResult.equals(result));
        
        instance.setFileNameFromEgate("EmbeddedPattern%y%y%y%y%M%M%d%d");
        result = instance.expand();
        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
        assertFalse("Not equal - expanded already including %f and its embedded patterns", expResult.equals(result));
        
        result = instance.expand();
        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
        assertFalse("Not equal - expanded already including %f and its embedded patterns", expResult.equals(result));
        
        result = instance.expand();
        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
        assertFalse("Not equal - expanded already including %f and its embedded patterns", expResult.equals(result));
        
        result = instance.expand();
        System.out.println("Pattern is [" + expResult + "], expanded as [" + result + "]");
        assertFalse("Not equal - expanded already including %f and its embedded patterns", expResult.equals(result));
        
    }


    /**
     * Test of getMaxSeqNo method, of class com.sun.jbi.ftpbc.NamePattern.
     */
    public void testGetMaxSeqNo() throws Exception {
        System.out.println("getMaxSeqNo");
        
        long expResult = 2147483647;
        long result = instance.getMaxSeqNo();
        assertEquals(expResult, result);
        
    }

    /**
     * Test of getStartSeqNo method, of class com.sun.jbi.ftpbc.NamePattern.
     */
    public void testGetStartSeqNo() throws Exception {
        System.out.println("getStartSeqNo");
        
        long expResult = 1;
        long result = instance.getStartSeqNo();
        assertEquals(expResult, result);
        
    }

    /**
     * Test of setMaxSeqNo method, of class com.sun.jbi.ftpbc.NamePattern.
     */
    public void testSetMaxSeqNo() throws Exception {
        System.out.println("setMaxSeqNo");
        
        long newMaxSeqNo = 10;
        instance.setMaxSeqNo(newMaxSeqNo);
        assertEquals(newMaxSeqNo, instance.getMaxSeqNo());
        
    }

    /**
     * Test of setStartSeqNo method, of class com.sun.jbi.ftpbc.NamePattern.
     */
    public void testSetStartSeqNo() throws Exception {
        System.out.println("setStartSeqNo");
        
        long newStartSeqNo = 54321;
        instance.setStartSeqNo(newStartSeqNo);
        assertEquals(newStartSeqNo, instance.getStartSeqNo());
        
    }

    /**
     * Test of setFileNameFromEgate method, of class com.sun.jbi.ftpbc.NamePattern.
     */
    public void testSetFileNameFromEgate() throws Exception {
        System.out.println("setFileNameFromEgate");
        
        String newFileNameFromEgate = "any";
        instance.setFileNameFromEgate(newFileNameFromEgate);
        assertEquals(newFileNameFromEgate, instance.getFileNameFromEgate());
        
    }

    /**
     * Test of getFileNameFromEgate method, of class com.sun.jbi.ftpbc.NamePattern.
     */
    public void testGetFileNameFromEgate() throws Exception {
        System.out.println("getFileNameFromEgate");
        
        String expResult = "";
        String result = instance.getFileNameFromEgate();
        assertEquals(expResult, result);
        
    }

    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.NamePattern.
     */
    public void testNamePattern () throws Exception {
        System.out.println ("NamePattern");
        
        assertNotNull (new NamePattern ("allowed pattern"));
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
