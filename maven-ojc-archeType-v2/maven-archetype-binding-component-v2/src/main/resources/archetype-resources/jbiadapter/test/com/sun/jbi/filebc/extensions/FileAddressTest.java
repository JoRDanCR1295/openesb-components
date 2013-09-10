#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(${symbol_pound})FileAddressTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.extensions;

import junit.framework.*;
import javax.xml.namespace.QName;

/**
 *
 * @author sweng
 */
public class FileAddressTest extends TestCase {

    FileAddress instance = null;

    public FileAddressTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new FileAddress();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(FileAddressTest.class);

        return suite;
    }

    /**
     * Test of setElementType and getElementType method, of class com.sun.jbi.filebc.extensions.FileAddress.
     */
    public void testSetGetElementType() {
        System.out.println("Testing setElementType and getElementType");

        // 1. testing the default value of element type
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/file/", "address");
        QName result = instance.getElementType();
        assertEquals(expResult, result);

        // 2. testing setElementType
        QName val = new QName("http://my-file-address-test", "address");
        expResult = new QName("http://my-file-address-test", "address");
        instance.setElementType(val);
        result = instance.getElementType();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setElementType and getElementType");
    }

    /**
     * Test of setRequired and getRequired method, of class com.sun.jbi.filebc.extensions.FileAddress.
     */
    public void testSetGetRequired() {
        System.out.println("Testing setRequired and getRequired");

        Boolean val = Boolean.TRUE;
        Boolean expResult = Boolean.TRUE;
        instance.setRequired(val);
        Boolean result = instance.getRequired();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setRequired and getRequired");
    }

    /**
     * Test of setFileDirectory and getFileDirectory method, of class com.sun.jbi.filebc.extensions.FileAddress.
     */
    public void testSetGetFileDirectory() {
        System.out.println("Testing setInputDir and getInputDir");

        String val = "c:/myfiletest/myDir";
        ;
        String expResult = "c:/myfiletest/myDir";
        instance.setFileDirectory(val);
        String result = instance.getFileDirectory();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setInputDir and getInputDir");
    }
}
