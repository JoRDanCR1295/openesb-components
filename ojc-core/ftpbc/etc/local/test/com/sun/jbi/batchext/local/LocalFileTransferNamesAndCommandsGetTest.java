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
 * @(#)LocalFileTransferNamesAndCommandsGetTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.local;

import junit.framework.*;
import com.sun.jbi.batchext.BatchException;
import com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommands;
import java.io.FileNotFoundException;
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
public class LocalFileTransferNamesAndCommandsGetTest extends TestCase {
    
    public LocalFileTransferNamesAndCommandsGetTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(LocalFileTransferNamesAndCommandsGetTest.class);
        
        return suite;
    }

    /**
     * Test of setEtd method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommandsGet.
     */
    public void testSetEtd() {
        System.out.println("setEtd");
        
        BatchLocal etd = null;
        LocalFileTransferNamesAndCommandsGet instance = null;
        
        instance.setEtd(etd);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of resolveTargetLocation method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommandsGet.
     */
    public void testResolveTargetLocation() throws Exception {
        System.out.println("resolveTargetLocation");
        
        LocalFileTransferNamesAndCommandsGet instance = null;
        
        instance.resolveTargetLocation();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of resolvePreTransfer method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommandsGet.
     */
    public void testResolvePreTransfer() throws Exception {
        System.out.println("resolvePreTransfer");
        
        LocalFileTransferNamesAndCommandsGet instance = null;
        
        instance.resolvePreTransfer();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of resolvePostTransfer method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommandsGet.
     */
    public void testResolvePostTransfer() throws Exception {
        System.out.println("resolvePostTransfer");
        
        LocalFileTransferNamesAndCommandsGet instance = null;
        
        instance.resolvePostTransfer();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getReadPosition method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommandsGet.
     */
    public void testGetReadPosition() {
        System.out.println("getReadPosition");
        
        LocalFileTransferNamesAndCommandsGet instance = null;
        
        long expResult = 0L;
        long result = instance.getReadPosition();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setReadPosition method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommandsGet.
     */
    public void testSetReadPosition() {
        System.out.println("setReadPosition");
        
        long readPosition = 0L;
        LocalFileTransferNamesAndCommandsGet instance = null;
        
        instance.setReadPosition(readPosition);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
