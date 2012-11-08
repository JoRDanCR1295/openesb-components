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
 * @(#)LocalFileClientImplTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.local;

import junit.framework.*;
import com.sun.jbi.batchext.BatchException;
import com.sun.jbi.batchext.TransferNamesAndCommands;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.batchext.statemanager.StateManagerException;
import com.sun.jbi.batchext.streaming.FileInputStream;
import com.sun.jbi.batchext.streaming.InputStreamAdapter;
import com.sun.jbi.batchext.streaming.OutputStreamAdapter;
import com.sun.jbi.batchext.streaming.StreamUtil;
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
public class LocalFileClientImplTest extends TestCase {
    
    public LocalFileClientImplTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(LocalFileClientImplTest.class);
        
        return suite;
    }

    /**
     * Test of initialize method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testInitialize() {
        System.out.println("initialize");
        
        BatchLocal etd = null;
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.initialize(etd);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of reset method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testReset() {
        System.out.println("reset");
        
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        boolean expResult = true;
        boolean result = instance.reset();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of allowTransfer method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testAllowTransfer() {
        System.out.println("allowTransfer");
        
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.allowTransfer();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getResolvedNamesToGet method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testGetResolvedNamesToGet() throws Exception {
        System.out.println("getResolvedNamesToGet");
        
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        TransferNamesAndCommands expResult = null;
        TransferNamesAndCommands result = instance.getResolvedNamesToGet();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getResolvedNamesToPut method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testGetResolvedNamesToPut() throws Exception {
        System.out.println("getResolvedNamesToPut");
        
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        TransferNamesAndCommands expResult = null;
        TransferNamesAndCommands result = instance.getResolvedNamesToPut();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getPayload method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testGetPayload() {
        System.out.println("getPayload");
        
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        byte[] expResult = null;
        byte[] result = instance.getPayload();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setPayload method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testSetPayload() {
        System.out.println("setPayload");
        
        byte[] payload = null;
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.setPayload(payload);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getInputStreamAdapter method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testGetInputStreamAdapter() {
        System.out.println("getInputStreamAdapter");
        
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        InputStreamAdapter expResult = null;
        InputStreamAdapter result = instance.getInputStreamAdapter();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getOutputStreamAdapter method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testGetOutputStreamAdapter() {
        System.out.println("getOutputStreamAdapter");
        
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        OutputStreamAdapter expResult = null;
        OutputStreamAdapter result = instance.getOutputStreamAdapter();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of requestInputStream method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testRequestInputStream() throws Exception {
        System.out.println("requestInputStream");
        
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        InputStream expResult = null;
        InputStream result = instance.requestInputStream();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of releaseInputStream method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testReleaseInputStream() throws Exception {
        System.out.println("releaseInputStream");
        
        boolean success = true;
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.releaseInputStream(success);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of requestOutputStream method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testRequestOutputStream() throws Exception {
        System.out.println("requestOutputStream");
        
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        OutputStream expResult = null;
        OutputStream result = instance.requestOutputStream();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of releaseOutputStream method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testReleaseOutputStream() throws Exception {
        System.out.println("releaseOutputStream");
        
        boolean success = true;
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.releaseOutputStream(success);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of get method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testGet() throws Exception {
        System.out.println("get");
        
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.get();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getIfExists method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testGetIfExists() throws Exception {
        System.out.println("getIfExists");
        
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.getIfExists();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of put method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testPut() throws Exception {
        System.out.println("put");
        
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.put();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getResumeReadingInProgress method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testGetResumeReadingInProgress() {
        System.out.println("getResumeReadingInProgress");
        
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        boolean expResult = true;
        boolean result = instance.getResumeReadingInProgress();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of executePreCommandGet method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testExecutePreCommandGet() throws Exception {
        System.out.println("executePreCommandGet");
        
        LocalFileTransferNamesAndCommandsGet tncg = null;
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.executePreCommandGet(tncg);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of undoPreCommandGet method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testUndoPreCommandGet() throws Exception {
        System.out.println("undoPreCommandGet");
        
        LocalFileTransferNamesAndCommandsGet tncg = null;
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.undoPreCommandGet(tncg);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of executePostCommandGet method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testExecutePostCommandGet() throws Exception {
        System.out.println("executePostCommandGet");
        
        LocalFileTransferNamesAndCommandsGet tncg = null;
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.executePostCommandGet(tncg);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of undoTransferGet method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testUndoTransferGet() throws Exception {
        System.out.println("undoTransferGet");
        
        LocalFileTransferNamesAndCommandsGet tncg = null;
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.undoTransferGet(tncg);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of executePreCommandPut method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testExecutePreCommandPut() throws Exception {
        System.out.println("executePreCommandPut");
        
        LocalFileTransferNamesAndCommandsPut tncp = null;
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.executePreCommandPut(tncp);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of undoPreCommandPut method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testUndoPreCommandPut() throws Exception {
        System.out.println("undoPreCommandPut");
        
        LocalFileTransferNamesAndCommandsPut tncp = null;
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.undoPreCommandPut(tncp);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of executePostCommandPut method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testExecutePostCommandPut() throws Exception {
        System.out.println("executePostCommandPut");
        
        LocalFileTransferNamesAndCommandsPut tncp = null;
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.executePostCommandPut(tncp);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of undoTransferPut method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testUndoTransferPut() throws Exception {
        System.out.println("undoTransferPut");
        
        LocalFileTransferNamesAndCommandsPut tncp = null;
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.undoTransferPut(tncp);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of notifyChanges method, of class com.sun.jbi.batchext.local.LocalFileClientImpl.
     */
    public void testNotifyChanges() {
        System.out.println("notifyChanges");
        
        LocalFileClientImpl instance = new LocalFileClientImpl();
        
        instance.notifyChanges();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
