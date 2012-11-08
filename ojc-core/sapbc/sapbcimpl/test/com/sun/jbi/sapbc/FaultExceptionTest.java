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
 * @(#)FaultExceptionTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import junit.framework.*;
import javax.jbi.messaging.MessagingException;

/**
 *
 * @author sweng
 */
public class FaultExceptionTest extends TestCase {
    FaultException instance = new FaultException(new MessagingException("Testing exception message"));
    
    public FaultExceptionTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(FaultExceptionTest.class);
        
        return suite;
    }

    /**
     * Test of getDetail method, of class com.sun.jbi.sapbc.FaultException.
     */
    public void testGetDetail() {
        System.out.println("Testing getDetail");
        
        String expResult = "Testing exception message";
        String result = instance.getDetail();
        assertEquals(expResult, result);
    }
    
    /**
     * Test of getFactor method, of class com.sun.jbi.sapbc.FaultException.
     */
    public void testGetFaultActor() {
        System.out.println("Testing getFaultActor");
        
        String expResult = null;
        String result = instance.getFaultActor();
        assertEquals(expResult, result);
    }
    
    /**
     * Test of getFaultString method, of class com.sun.jbi.sapbc.FaultException.
     */
    public void testGetFaultString() {
        System.out.println("Testing getFaultString");
        
        String expResult = null;
        String result = instance.getFaultString();
        assertEquals(expResult, result);
    }
    
    /**
     * Test of getFaultCode method, of class com.sun.jbi.sapbc.FaultException.
     */
    public void testGetFaultCode() {
        System.out.println("Testing getFaultCode");
        
        String expResult = "Server";
        String result = instance.getFaultCode();
        assertEquals(expResult, result);
    }
    
}
