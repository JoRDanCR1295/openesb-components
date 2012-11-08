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
 * @(#)ExecAddressTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc.extensions;

import junit.framework.*;
import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import com.sun.jbi.execbc.extensions.ExecAddress;

/**
 *
 * @author sweng
 */
public class ExecAddressTest extends TestCase {
    ExecAddress instance = null;
    
    public ExecAddressTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new ExecAddress();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ExecAddressTest.class);
        
        return suite;
    }

    /**
     * Test of setElementType and getElementType method, of class com.sun.jbi.execbc.extensions.ExecAddress.
     */
    public void testSetGetElementType() {
        System.out.println("Testing setElementType and getElementType");
        
        // 1. testing the default value of element type
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/exec/", "address");
        QName result = instance.getElementType();
        assertEquals(expResult, result);
        
        // 2. testing setElementType
        QName val = new QName("http://my-exec-address-test", "address");
        expResult = new QName("http://my-exec-address-test", "address");
        instance.setElementType(val);
        result = instance.getElementType();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setElementType and getElementType");
    }
    
    /**
     * Test of setRequired and getRequired method, of class com.sun.jbi.execbc.extensions.ExecAddress.
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
     * Test of setHostName and getHostName method, of class com.sun.jbi.execbc.extensions.ExecAddress.
     */
    public void testSetGetFileDirectory() {
        System.out.println("Testing setInputDir and getInputDir");
        
        String val = "localhost";;
        String expResult = "localhost";
        instance.setHostName(val);
        String result = instance.getHostName();
        assertEquals(expResult, result);
        val = "un1";;
        expResult = "un1";
        instance.setUserName(val);
        result = instance.getUserName();
        assertEquals(expResult, result);
        val = "abcde";;
        expResult = "abcde";
        instance.setPassword(val);
        result = instance.getPassword();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested ExecAddress getters and setters.");
    }
}
