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
 * @(#)MQBCBodyTest.java 
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
public class MQBCBodyTest extends TestCase {
    
    public MQBCBodyTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(MQBCBodyTest.class);
        
        return suite;
    }

    /**
     * Test of getElementType method, of class com.sun.jbi.mqbc.extensions.MQBCBody.
     */
    public void testGetElementType() {
        System.out.println("getElementType");
        
        MQBCBody instance = new MQBCBody();
        
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/mq/","body");
        QName result = instance.getElementType();
        assertEquals(expResult, result);
        
       
    }

  
    /**
     * Test of getRequired method, of class com.sun.jbi.mqbc.extensions.MQBCBody.
     */
    public void testGetRequired() {
        System.out.println("getRequired");
        
        MQBCBody instance = new MQBCBody();
        
        boolean expResult = false;
        boolean result = instance.getRequired();
        assertEquals(expResult, result);
        
      
    }

    /**
     * Test of setRequired method, of class com.sun.jbi.mqbc.extensions.MQBCBody.
     */
    public void testSetRequired() {
        System.out.println("setRequired");
        
        boolean required = true;
        MQBCBody instance = new MQBCBody();
        
        instance.setRequired(required);   
        assertEquals(required, (boolean) instance.getRequired());
    }

    /**
     * Test of getMQMessageType method, of class com.sun.jbi.mqbc.extensions.MQBCBody.
     */
    public void testGetMQMessageType() {
        System.out.println("getMQMessageType");
        
        MQBCBody instance = new MQBCBody();
        
        String expResult = "TextMessage";
        String result = instance.getMQMessageType();
        assertEquals(expResult, result);
        
    }

    /**
     * Test of setMQMessageType method, of class com.sun.jbi.mqbc.extensions.MQBCBody.
     */
    public void testSetMQMessageType() {
        System.out.println("setMQMessageType");
        
        String val = "TextMessage";
        MQBCBody instance = new MQBCBody();
        
        instance.setMQMessageType(val);   
        assertEquals(val,  instance.getMQMessageType());
    }

    /**
     * Test of getMQMessageBody method, of class com.sun.jbi.mqbc.extensions.MQBCBody.
     */
    public void testGetMQMessageBody() {
        System.out.println("getMQMessageBody");
        
        MQBCBody instance = new MQBCBody();
        
        String expResult = "";
        String result = instance.getMQMessageBody();
        assertEquals(expResult, result);
        
   
    }

    /**
     * Test of setMQMessageBody method, of class com.sun.jbi.mqbc.extensions.MQBCBody.
     */
    public void testSetMQMessageBody() {
        System.out.println("setMQMessageBody");
        
        String val = "MQMessageBody";
        MQBCBody instance = new MQBCBody();
        
        instance.setMQMessageBody(val);
        
           assertEquals(val, instance.getMQMessageBody());
    }

    /**
     * Test of getMQSyncPoint method, of class com.sun.jbi.mqbc.extensions.MQBCBody.
     */
    public void testGetMQSyncPoint() {
        System.out.println("getMQSyncPoint");
        
        MQBCBody instance = new MQBCBody();
        
        boolean expResult = false;
        boolean result = instance.getMQSyncPoint();
        assertEquals(expResult, result);
        
       
    }

    /**
     * Test of setMQSyncPoint method, of class com.sun.jbi.mqbc.extensions.MQBCBody.
     */
    public void testSetMQSyncPoint() {
        System.out.println("setMQSyncPoint");
        
        boolean val = true;
        MQBCBody instance = new MQBCBody();
        
        instance.setMQSyncPoint(val);
        assertEquals(val,  instance.getMQSyncPoint());
    
    }
    
}
