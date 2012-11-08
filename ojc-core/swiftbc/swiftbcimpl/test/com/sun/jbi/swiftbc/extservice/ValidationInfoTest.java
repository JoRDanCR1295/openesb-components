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
 * @(#)ValidationInfoTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.extservice;


import junit.framework.*;

/**
 *
 * @author Raghunadh
 */
public class ValidationInfoTest extends TestCase {

    ValidationInfo instance = null;

    public ValidationInfoTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new ValidationInfo();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ValidationInfoTest.class);
        return suite;
    }

 
    /**
     * Test of setValidationStatus and getValidationStatus method, of class com.sun.jbi.swiftbc.extServices.ValidationInfo.
     */
    public void testSetGetValidationStatus() {
        System.out.println("Testing setValidationStatus and getValidationStatus");
        
        boolean exp = true;
        instance.setValidationStatus(exp);
        boolean result = instance.getValidationStatus();
        assertEquals(exp, result);
        
        System.out.println("Successfully tested setValidationStatus and getValidationStatus");
    }

	 /**
     * Test of setErrorCode and getErrorCode method, of class com.sun.jbi.swiftbc.extServices.ValidationInfo.
     */
    public void testSetGetErrorCode() {
        System.out.println("Testing setErrorCode and getErrorCode");
        
        String exp = "100";
        instance.setErrorCode(exp);
        String result = instance.getErrorCode();
        assertEquals(exp, result);
        
        System.out.println("Successfully tested setErrorCode and getErrorCode");
    }

	 /**
     * Test of setErrorMessage and getErrorMessage method, of class com.sun.jbi.swiftbc.extServices.ValidationInfo.
     */
    public void testSetGetErrorMessage() {
        System.out.println("Testing setErrorMessage and setErrorMessage");
        
        String exp = "Segment sequence error";
        instance.setErrorMessage(exp);
        String result = instance.getErrorMessage();
        assertEquals(exp, result);
        
        System.out.println("Successfully tested setErrorMessage and getErrorMessage");
    }

 }

  
