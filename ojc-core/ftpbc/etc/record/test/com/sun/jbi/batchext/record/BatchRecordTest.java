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
 * @(#)BatchRecordTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.record;

import junit.framework.*;
import com.sun.jbi.batchext.BatchException;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.batchext.streaming.InputStreamAdapter;
import com.sun.jbi.batchext.streaming.OutputStreamAdapter;
import com.sun.jbi.batchext.streaming.StreamingException;

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
public class BatchRecordTest extends TestCase {
    
    public BatchRecordTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(BatchRecordTest.class);
        
        return suite;
    }

    /**
     * Test of initialize method, of class com.sun.jbi.batchext.record.BatchRecord.
     */
    public void testInitialize() throws Exception {
        System.out.println("initialize");
        
        Properties p = null;
        BatchRecord instance = new BatchRecord();
        
        instance.initialize(p);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of reset method, of class com.sun.jbi.batchext.record.BatchRecord.
     */
    public void testReset() throws Exception {
        System.out.println("reset");
        
        BatchRecord instance = new BatchRecord();
        
        boolean expResult = true;
        boolean result = instance.reset();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of terminate method, of class com.sun.jbi.batchext.record.BatchRecord.
     */
    public void testTerminate() throws Exception {
        System.out.println("terminate");
        
        BatchRecord instance = new BatchRecord();
        
        instance.terminate();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of get method, of class com.sun.jbi.batchext.record.BatchRecord.
     */
    public void testGet() throws Exception {
        System.out.println("get");
        
        BatchRecord instance = new BatchRecord();
        
        boolean expResult = true;
        boolean result = instance.get();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of put method, of class com.sun.jbi.batchext.record.BatchRecord.
     */
    public void testPut() throws Exception {
        System.out.println("put");
        
        BatchRecord instance = new BatchRecord();
        
        instance.put();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of finish method, of class com.sun.jbi.batchext.record.BatchRecord.
     */
    public void testFinish() throws Exception {
        System.out.println("finish");
        
        BatchRecord instance = new BatchRecord();
        
        instance.finish();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setOutputStreamAdapter method, of class com.sun.jbi.batchext.record.BatchRecord.
     */
    public void testSetOutputStreamAdapter() throws Exception {
        System.out.println("setOutputStreamAdapter");
        
        OutputStreamAdapter osa = null;
        BatchRecord instance = new BatchRecord();
        
        instance.setOutputStreamAdapter(osa);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setInputStreamAdapter method, of class com.sun.jbi.batchext.record.BatchRecord.
     */
    public void testSetInputStreamAdapter() throws Exception {
        System.out.println("setInputStreamAdapter");
        
        InputStreamAdapter isa = null;
        BatchRecord instance = new BatchRecord();
        
        instance.setInputStreamAdapter(isa);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPayload method, of class com.sun.jbi.batchext.record.BatchRecord.
     */
    public void testGetPayload() throws Exception {
        System.out.println("getPayload");
        
        BatchRecord instance = new BatchRecord();
        
        byte[] expResult = null;
        byte[] result = instance.getPayload();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setPayload method, of class com.sun.jbi.batchext.record.BatchRecord.
     */
    public void testSetPayload() throws Exception {
        System.out.println("setPayload");
        
        byte[] data = null;
        BatchRecord instance = new BatchRecord();
        
        instance.setPayload(data);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getRecord method, of class com.sun.jbi.batchext.record.BatchRecord.
     */
    public void testGetRecord() throws Exception {
        System.out.println("getRecord");
        
        BatchRecord instance = new BatchRecord();
        
        byte[] expResult = null;
        byte[] result = instance.getRecord();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setRecord method, of class com.sun.jbi.batchext.record.BatchRecord.
     */
    public void testSetRecord() throws Exception {
        System.out.println("setRecord");
        
        byte[] data = null;
        BatchRecord instance = new BatchRecord();
        
        instance.setRecord(data);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getConfiguration method, of class com.sun.jbi.batchext.record.BatchRecord.
     */
    public void testGetConfiguration() {
        System.out.println("getConfiguration");
        
        BatchRecord instance = new BatchRecord();
        
        BatchRecordConfiguration expResult = null;
        BatchRecordConfiguration result = instance.getConfiguration();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setConfiguration method, of class com.sun.jbi.batchext.record.BatchRecord.
     */
    public void testSetConfiguration() {
        System.out.println("setConfiguration");
        
        BatchRecordConfiguration cfg = null;
        BatchRecord instance = new BatchRecord();
        
        instance.setConfiguration(cfg);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
