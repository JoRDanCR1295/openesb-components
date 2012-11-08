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
 * @(#)MQBCOperationTest.java 
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
public class MQBCOperationTest extends TestCase {
    
    public MQBCOperationTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
    }
    
    protected void tearDown() throws Exception {
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(MQBCOperationTest.class);
        
        return suite;
    }
    
    /**
     * Test of getElementType method, of class com.sun.jbi.mqbc.extensions.MQBCOperation.
     */
    public void testGetElementType() {
        System.out.println("getElementType");
        
        MQBCOperation instance = new MQBCOperation();
        
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/mq/",Constants.ELEM_OPERATION);
        QName result = instance.getElementType();
        assertEquals(expResult, result);
        
        
    }
    
    
    
    /**
     * Test of getRequired method, of class com.sun.jbi.mqbc.extensions.MQBCOperation.
     */
    public void testGetRequired() {
        System.out.println("getRequired");
        
        MQBCOperation instance = new MQBCOperation();
        
        Boolean expResult = false;
        Boolean result = instance.getRequired();
        assertEquals(expResult, result);
        
        
    }
    
    /**
     * Test of setRequired method, of class com.sun.jbi.mqbc.extensions.MQBCOperation.
     */
    public void testSetRequired() {
        System.out.println("setRequired");
        
        Boolean required = new Boolean("true");
        MQBCOperation instance = new MQBCOperation();
        
        instance.setRequired(required);
        
        assertEquals(required,  instance.getRequired());
    }
    
    /**
     * Test of getQueueName method, of class com.sun.jbi.mqbc.extensions.MQBCOperation.
     */
    public void testGetQueueName() {
        System.out.println("getQueueName");
        
        MQBCOperation instance = new MQBCOperation();
        
        String expResult = "";
        String result = instance.getQueueName();
        assertEquals(expResult, result);
        
       
    }
    
    /**
     * Test of setQueueName method, of class com.sun.jbi.mqbc.extensions.MQBCOperation.
     */
    public void testSetQueueName() {
        System.out.println("setQueueName");
        
        String val = "QueueName";
        MQBCOperation instance = new MQBCOperation();
        
        instance.setQueueName(val);
        
           assertEquals(val, instance.getQueueName());
    }
    
    /**
     * Test of getTransaction method, of class com.sun.jbi.mqbc.extensions.MQBCOperation.
     */
    public void testGetTransaction() {
        System.out.println("getTransaction");
        
        MQBCOperation instance = new MQBCOperation();
        
        boolean expResult = false;
        boolean result = instance.getTransaction();
        assertEquals(expResult, result);
        
    
    }
    
    /**
     * Test of setTransaction method, of class com.sun.jbi.mqbc.extensions.MQBCOperation.
     */
    public void testSetTransaction() {
        System.out.println("setTransaction");
        
        boolean val = false;
        MQBCOperation instance = new MQBCOperation();
        
        instance.setTransaction(val);
        
            assertEquals(val, instance.getTransaction());
    }
    
    /**
     * Test of getQueueOpenOptions method, of class com.sun.jbi.mqbc.extensions.MQBCOperation.
     */
    public void testGetQueueOpenOptions() {
        System.out.println("getQueueOpenOptions");
        
        MQBCOperation instance = new MQBCOperation();
        
        Integer expResult = 0;
        Integer result = instance.getQueueOpenOptions();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setQueueOpenOptions method, of class com.sun.jbi.mqbc.extensions.MQBCOperation.
     */
    public void testSetQueueOpenOptions() {
        System.out.println("setQueueOpenOptions");
        
        int val = 1;
        MQBCOperation instance = new MQBCOperation();
        
        instance.setQueueOpenOptions(val);
        
          assertEquals(val, instance.getQueueOpenOptions());
    }
    
    /**
     * Test of getPollingInterval method, of class com.sun.jbi.mqbc.extensions.MQBCOperation.
     */
    public void testGetPollingInterval() {
        System.out.println("getPollingInterval");
        
        MQBCOperation instance = new MQBCOperation();
        
        Long expResult = 1000L;
        Long result = instance.getPollingInterval();
        assertEquals(expResult, result);
        
       
    }
    
    /**
     * Test of setPollingInterval method, of class com.sun.jbi.mqbc.extensions.MQBCOperation.
     */
    public void testSetPollingInterval() {
        System.out.println("setPollingInterval");
        
        long val = 2500L;
        MQBCOperation instance = new MQBCOperation();
        
        instance.setPollingInterval(val);
        
          assertEquals(val, instance.getPollingInterval());
    }
}
