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
 * @(#)MQBCHeaderTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extensions;

import javax.xml.namespace.QName;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 *
 * @author rchen
 */
public class MQBCHeaderTest extends TestCase {
    
    public MQBCHeaderTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
    }
    
    protected void tearDown() throws Exception {
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(MQBCHeaderTest.class);
        
        return suite;
    }
    
    /**
     * Test of getElementType method, of class com.sun.jbi.mqbc.extensions.MQBCHeader.
     */
    public void testGetElementType() {
        System.out.println("getElementType");
        
        MQBCHeader instance = new MQBCHeader(MessageDescriptors.correlationId, "foo");
        
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/mq/","header");
        QName result = instance.getElementType();
        assertEquals(expResult, result);
        
    }
    
    
    /**
     * Test of getRequired method, of class com.sun.jbi.mqbc.extensions.MQBCHeader.
     */
    public void testGetRequired() {
        System.out.println("getRequired");
        
        MQBCHeader instance = new MQBCHeader(MessageDescriptors.correlationId, "foo");
        
        boolean expResult = false;
        boolean result = instance.getRequired();
        assertEquals(expResult, result);
        
        
    }
    
    /**
     * Test of setRequired method, of class com.sun.jbi.mqbc.extensions.MQBCHeader.
     */
    public void testSetRequired() {
        System.out.println("setRequired");
        
        boolean required = true;
        MQBCHeader instance = new MQBCHeader(MessageDescriptors.correlationId, "foo");
        
        instance.setRequired(required);
        
        assertEquals(required, (boolean) instance.getRequired());
    }
    
}
