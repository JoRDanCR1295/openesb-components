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
 * @(#)LocalFileTransferNamesAndCommandsPutTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.local;

import junit.framework.*;
import com.sun.jbi.batchext.BatchException;
import com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommands;
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
public class LocalFileTransferNamesAndCommandsPutTest extends TestCase {
    
    public LocalFileTransferNamesAndCommandsPutTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(LocalFileTransferNamesAndCommandsPutTest.class);
        
        return suite;
    }

    /**
     * Test of setEtd method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommandsPut.
     */
    public void testSetEtd() {
        System.out.println("setEtd");
        
        BatchLocal etd = null;
        LocalFileTransferNamesAndCommandsPut instance = null;
        
        instance.setEtd(etd);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setOriginalFileSize method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommandsPut.
     */
    public void testSetOriginalFileSize() {
        System.out.println("setOriginalFileSize");
        
        long size = 0L;
        LocalFileTransferNamesAndCommandsPut instance = null;
        
        instance.setOriginalFileSize(size);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getOriginalFileSize method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommandsPut.
     */
    public void testGetOriginalFileSize() {
        System.out.println("getOriginalFileSize");
        
        LocalFileTransferNamesAndCommandsPut instance = null;
        
        long expResult = 0L;
        long result = instance.getOriginalFileSize();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of resolveTargetLocation method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommandsPut.
     */
    public void testResolveTargetLocation() throws Exception {
        System.out.println("resolveTargetLocation");
        
        LocalFileTransferNamesAndCommandsPut instance = null;
        
        instance.resolveTargetLocation();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of resolvePreTransfer method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommandsPut.
     */
    public void testResolvePreTransfer() throws Exception {
        System.out.println("resolvePreTransfer");
        
        LocalFileTransferNamesAndCommandsPut instance = null;
        
        instance.resolvePreTransfer();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of resolvePostTransfer method, of class com.sun.jbi.batchext.local.LocalFileTransferNamesAndCommandsPut.
     */
    public void testResolvePostTransfer() throws Exception {
        System.out.println("resolvePostTransfer");
        
        LocalFileTransferNamesAndCommandsPut instance = null;
        
        instance.resolvePostTransfer();
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
