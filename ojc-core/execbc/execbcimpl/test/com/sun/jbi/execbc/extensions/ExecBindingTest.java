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
 * @(#)ExecBindingTest.java 
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

import com.ibm.wsdl.Constants;
import com.sun.jbi.execbc.extensions.ExecBinding;

/**
 *
 * @author sweng
 */
public class ExecBindingTest extends TestCase {
    ExecBinding instance = null;
    
    public ExecBindingTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new ExecBinding();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ExecBindingTest.class);
        
        return suite;
    }

    /**
     * Test of setElementType and getElementType method, of class com.sun.jbi.execbc.extensions.ExecBinding.
     */
    public void testSetGetElementType() {
        System.out.println("Testing setElementType and getElementType");
        
        // 1. testing default element type value
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/exec/", Constants.ELEM_BINDING);
        QName result = instance.getElementType();
        assertEquals(expResult, result);
        
        // 2. testing setElementType
        QName val = new QName("http://my-exec-binding/test", Constants.ELEM_BINDING);
        expResult = new QName("http://my-exec-binding/test", Constants.ELEM_BINDING);
        instance.setElementType(val);
        result = instance.getElementType();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setElementType and getElementType");
    }
    
    /**
     * Test of setRequired and getRequired method, of class com.sun.jbi.execbc.extensions.ExecBinding.
     */
    public void testSetGetRequired() {
        System.out.println("Testing setRequired and getRequired");
        
        Boolean expResult = Boolean.FALSE;
        instance.setRequired(Boolean.FALSE);
        Boolean result = instance.getRequired();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setRequired and getRequired");
    }

    /**
     * Test of toString method, of class com.sun.jbi.execbc.extensions.ExecBinding.
     */
    public void testToString() {
        System.out.println("Testing toString");
        
        String expResult = "ExecBinding {http://my-exec-binding/test}binding:" + 
                           "\nRequired=false";
        instance.setElementType(new QName("http://my-exec-binding/test", Constants.ELEM_BINDING));
        instance.setRequired(Boolean.FALSE);
        String result = instance.toString();
        assertTrue(result.indexOf(expResult) > 0);
        
        System.out.println("Successfully tested toString");
    }
    
}
