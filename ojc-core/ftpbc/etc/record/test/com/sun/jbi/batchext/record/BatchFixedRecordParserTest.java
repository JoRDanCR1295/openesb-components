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
 * @(#)BatchFixedRecordParserTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.record;

import junit.framework.*;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.InvalidParameterException;
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
public class BatchFixedRecordParserTest extends TestCase {
    
    public BatchFixedRecordParserTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(BatchFixedRecordParserTest.class);
        
        return suite;
    }

    /**
     * Test of initialize method, of class com.sun.jbi.batchext.record.BatchFixedRecordParser.
     */
    public void testInitialize() throws Exception {
        System.out.println("initialize");
        
        BatchRecordConfiguration conf = null;
        BatchFixedRecordParser instance = new BatchFixedRecordParser();
        
        instance.initialize(conf);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of get method, of class com.sun.jbi.batchext.record.BatchFixedRecordParser.
     */
    public void testGet() throws Exception {
        System.out.println("get");
        
        InputStream input = null;
        BatchFixedRecordParser instance = new BatchFixedRecordParser();
        
        byte[] expResult = null;
        byte[] result = instance.get(input);
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of put method, of class com.sun.jbi.batchext.record.BatchFixedRecordParser.
     */
    public void testPut() throws Exception {
        System.out.println("put");
        
        OutputStream output = null;
        byte[] data = null;
        BatchFixedRecordParser instance = new BatchFixedRecordParser();
        
        instance.put(output, data);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of finish method, of class com.sun.jbi.batchext.record.BatchFixedRecordParser.
     */
    public void testFinish() throws Exception {
        System.out.println("finish");
        
        OutputStream output = null;
        InputStream input = null;
        BatchFixedRecordParser instance = new BatchFixedRecordParser();
        
        instance.finish(output, input);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
