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
 * @(#)MQBCBindingTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extensions;

import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 *
 * @author rchen
 */
public class MQBCBindingTest extends TestCase {
    
    public MQBCBindingTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(MQBCBindingTest.class);
        
        return suite;
    }

    /**
     * Test of getElementType method, of class com.sun.jbi.mqbc.extensions.MQBCBinding.
     */
    public void testGetElementType() {
        System.out.println("getElementType");
        
        MQBCBinding instance = new MQBCBinding();
        
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/mq/",Constants.ELEM_BINDING);
        QName result = instance.getElementType();
        assertEquals(expResult, result);
        
       
    }

   

    /**
     * Test of getRequired method, of class com.sun.jbi.mqbc.extensions.MQBCBinding.
     */
    public void testGetRequired() {
        System.out.println("getRequired");
        
        MQBCBinding instance = new MQBCBinding();
        
        boolean expResult = false;
        boolean result = instance.getRequired();
        assertEquals(expResult, result);
        
       
    }

    /**
     * Test of setRequired method, of class com.sun.jbi.mqbc.extensions.MQBCBinding.
     */
    public void testSetRequired() {
        System.out.println("setRequired");
        
        Boolean required = new Boolean("true");
        MQBCBinding instance = new MQBCBinding();
        
        instance.setRequired(required);
        
           assertEquals(required, instance.getRequired());
    }

   
    
}
