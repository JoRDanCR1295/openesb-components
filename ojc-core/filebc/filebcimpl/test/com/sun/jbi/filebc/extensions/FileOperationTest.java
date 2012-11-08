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
 * @(#)FileOperationTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.extensions;

import junit.framework.*;
import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;

/**
 *
 * @author sweng
 */
public class FileOperationTest extends TestCase {

    FileOperation instance = null;

    public FileOperationTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new FileOperation();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(FileOperationTest.class);
        return suite;
    }

    /**
     * Test of setElementType and getElementType method, of class com.sun.jbi.filebc.extensions.FileOperation.
     */
    public void testSetGetElementType() {
        System.out.println("Testing setElementType and getElementType");

        // 1. testing default element type value
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/file/", Constants.ELEM_OPERATION);
        QName result = instance.getElementType();
        assertEquals(expResult, result);

        // 2. testing setElementType
        QName val = new QName("http://file-operation-test/", Constants.ELEM_OPERATION);
        expResult = new QName("http://file-operation-test/", Constants.ELEM_OPERATION);
        instance.setElementType(val);
        result = instance.getElementType();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setElementType and getElementType");
    }

    /**
     * Test of setRequired and setgetRequired method, of class com.sun.jbi.filebc.extensions.FileOperation.
     */
    public void testSetGetRequired() {
        System.out.println("Testing setRequired and getRequired");

        Boolean expResult = Boolean.TRUE;
        instance.setRequired(Boolean.TRUE);
        Boolean result = instance.getRequired();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setRequired and getRequired");
    }

    /**
     * Test of toString method, of class com.sun.jbi.filebc.extensions.FileOperation.
     */
    public void testToString() {
        System.out.println("Testing toString");


        String expResult = "File operation ({http://file-operation-test/}operation):" +
                "\nRequired=true";
        instance.setElementType(new QName("http://file-operation-test/", Constants.ELEM_OPERATION));
        instance.setRequired(Boolean.TRUE);
        String result = instance.toString();
        System.out.println("-------------- result is [" + result + "]");
        assertTrue(result.indexOf(expResult) > 0);

        System.out.println("Successfully tested toString");
    }
}
