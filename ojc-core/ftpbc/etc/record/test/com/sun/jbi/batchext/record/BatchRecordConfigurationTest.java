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
 * @(#)BatchRecordConfigurationTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.record;

import junit.framework.*;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;

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
public class BatchRecordConfigurationTest extends TestCase {
    
    public BatchRecordConfigurationTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(BatchRecordConfigurationTest.class);
        
        return suite;
    }

    /**
     * Test of getParserClassName method, of class com.sun.jbi.batchext.record.BatchRecordConfiguration.
     */
    public void testGetParserClassName() {
        System.out.println("getParserClassName");
        
        BatchRecordConfiguration instance = null;
        
        String expResult = "";
        String result = instance.getParserClassName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getRecordType method, of class com.sun.jbi.batchext.record.BatchRecordConfiguration.
     */
    public void testGetRecordType() {
        System.out.println("getRecordType");
        
        BatchRecordConfiguration instance = null;
        
        String expResult = "";
        String result = instance.getRecordType();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getRecordDelimiter method, of class com.sun.jbi.batchext.record.BatchRecordConfiguration.
     */
    public void testGetRecordDelimiter() {
        System.out.println("getRecordDelimiter");
        
        BatchRecordConfiguration instance = null;
        
        byte[] expResult = null;
        byte[] result = instance.getRecordDelimiter();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getRecordDelimiterAsString method, of class com.sun.jbi.batchext.record.BatchRecordConfiguration.
     */
    public void testGetRecordDelimiterAsString() {
        System.out.println("getRecordDelimiterAsString");
        
        BatchRecordConfiguration instance = null;
        
        String expResult = "";
        String result = instance.getRecordDelimiterAsString();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getRecordSize method, of class com.sun.jbi.batchext.record.BatchRecordConfiguration.
     */
    public void testGetRecordSize() {
        System.out.println("getRecordSize");
        
        BatchRecordConfiguration instance = null;
        
        int expResult = 0;
        int result = instance.getRecordSize();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of isDelimiterOnLast method, of class com.sun.jbi.batchext.record.BatchRecordConfiguration.
     */
    public void testIsDelimiterOnLast() {
        System.out.println("isDelimiterOnLast");
        
        BatchRecordConfiguration instance = null;
        
        boolean expResult = true;
        boolean result = instance.isDelimiterOnLast();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of isCreateMode method, of class com.sun.jbi.batchext.record.BatchRecordConfiguration.
     */
    public void testIsCreateMode() {
        System.out.println("isCreateMode");
        
        BatchRecordConfiguration instance = null;
        
        boolean expResult = true;
        boolean result = instance.isCreateMode();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getUserProperties method, of class com.sun.jbi.batchext.record.BatchRecordConfiguration.
     */
    public void testGetUserProperties() {
        System.out.println("getUserProperties");
        
        BatchRecordConfiguration instance = null;
        
        Properties expResult = null;
        Properties result = instance.getUserProperties();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of parseProperties method, of class com.sun.jbi.batchext.record.BatchRecordConfiguration.
     */
    public void testParseProperties() {
        System.out.println("parseProperties");
        
        BatchRecordConfiguration instance = null;
        
        instance.parseProperties();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setSynchronized method, of class com.sun.jbi.batchext.record.BatchRecordConfiguration.
     */
    public void testSetSynchronized() {
        System.out.println("setSynchronized");
        
        boolean b = true;
        BatchRecordConfiguration instance = null;
        
        instance.setSynchronized(b);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getSynchronized method, of class com.sun.jbi.batchext.record.BatchRecordConfiguration.
     */
    public void testGetSynchronized() {
        System.out.println("getSynchronized");
        
        BatchRecordConfiguration instance = null;
        
        boolean expResult = true;
        boolean result = instance.getSynchronized();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
