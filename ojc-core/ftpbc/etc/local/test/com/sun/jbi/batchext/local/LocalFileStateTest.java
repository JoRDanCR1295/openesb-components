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
 * @(#)LocalFileStateTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.local;

import junit.framework.*;
import java.io.Serializable;

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
public class LocalFileStateTest extends TestCase {
    
    public LocalFileStateTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(LocalFileStateTest.class);
        
        return suite;
    }

    /**
     * Test of getTnc method, of class com.sun.jbi.batchext.local.LocalFileState.
     */
    public void testGetTnc() {
        System.out.println("getTnc");
        
        LocalFileState instance = new LocalFileState();
        
        LocalFileTransferNamesAndCommandsGet expResult = null;
        LocalFileTransferNamesAndCommandsGet result = instance.getTnc();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setTnc method, of class com.sun.jbi.batchext.local.LocalFileState.
     */
    public void testSetTnc() {
        System.out.println("setTnc");
        
        LocalFileTransferNamesAndCommandsGet tnc = null;
        LocalFileState instance = new LocalFileState();
        
        instance.setTnc(tnc);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of getSequenceNumber method, of class com.sun.jbi.batchext.local.LocalFileState.
     */
    public void testGetSequenceNumber() {
        System.out.println("getSequenceNumber");
        
        LocalFileState instance = new LocalFileState();
        
        long expResult = 0L;
        long result = instance.getSequenceNumber();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of setSequenceNumber method, of class com.sun.jbi.batchext.local.LocalFileState.
     */
    public void testSetSequenceNumber() {
        System.out.println("setSequenceNumber");
        
        long sequenceNumber = 0L;
        LocalFileState instance = new LocalFileState();
        
        instance.setSequenceNumber(sequenceNumber);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
