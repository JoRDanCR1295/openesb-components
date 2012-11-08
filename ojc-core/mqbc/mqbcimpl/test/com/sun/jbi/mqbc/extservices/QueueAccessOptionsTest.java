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
 * @(#)QueueAccessOptionsTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;

import junit.framework.*;
import com.ibm.mq.MQC;

/**
 *
 * @author rchen
 */
public class QueueAccessOptionsTest extends TestCase {
    QueueAccessOptions mInstance ;
    
    public QueueAccessOptionsTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        mInstance = new QueueAccessOptions();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(QueueAccessOptionsTest.class);
        
        return suite;
    }

    /**
     * Test of getOptions method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetOptions() {
        System.out.println("getOptions");
        
        
        
        int expResult = 0;
        int result = mInstance.getOptions();
        assertEquals(expResult, result);
        
        mInstance.setMQOO_INPUT_AS_Q_DEF(true);
        mInstance.setMQOO_OUTPUT(true);
        System.out.println("***:"+mInstance.getOptions());
        
      
    }

    /**
     * Test of optionsClearAll method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testOptionsClearAll() {
        System.out.println("optionsClearAll");
        
        
        
        mInstance.optionsClearAll();
        
       
    }

    /**
     * Test of setMQOO_INPUT_AS_Q_DEF method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_INPUT_AS_Q_DEF() {
        System.out.println("setMQOO_INPUT_AS_Q_DEF");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_INPUT_AS_Q_DEF(bSet);
        
        
      
    }

    /**
     * Test of getMQOO_INPUT_AS_Q_DEF method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_INPUT_AS_Q_DEF() {
        System.out.println("getMQOO_INPUT_AS_Q_DEF");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_INPUT_AS_Q_DEF();
        assertEquals(expResult, result);
        
       
    }

    /**
     * Test of setMQOO_INPUT_SHARED method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_INPUT_SHARED() {
        System.out.println("setMQOO_INPUT_SHARED");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_INPUT_SHARED(bSet);
        
      
    }

    /**
     * Test of getMQOO_INPUT_SHARED method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_INPUT_SHARED() {
        System.out.println("getMQOO_INPUT_SHARED");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_INPUT_SHARED();
        assertEquals(expResult, result);
        
      
    }

    /**
     * Test of setMQOO_INPUT_EXCLUSIVE method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_INPUT_EXCLUSIVE() {
        System.out.println("setMQOO_INPUT_EXCLUSIVE");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_INPUT_EXCLUSIVE(bSet);
        
       
    }

    /**
     * Test of getMQOO_INPUT_EXCLUSIVE method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_INPUT_EXCLUSIVE() {
        System.out.println("getMQOO_INPUT_EXCLUSIVE");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_INPUT_EXCLUSIVE();
        assertEquals(expResult, result);
        
      
    }

    /**
     * Test of setMQOO_BROWSE method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_BROWSE() {
        System.out.println("setMQOO_BROWSE");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_BROWSE(bSet);
        
       
    }

    /**
     * Test of getMQOO_BROWSE method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_BROWSE() {
        System.out.println("getMQOO_BROWSE");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_BROWSE();
        assertEquals(expResult, result);
        
       
    }

    /**
     * Test of setMQOO_OUTPUT method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_OUTPUT() {
        System.out.println("setMQOO_OUTPUT");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_OUTPUT(bSet);
        
        
    }

    /**
     * Test of getMQOO_OUTPUT method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_OUTPUT() {
        System.out.println("getMQOO_OUTPUT");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_OUTPUT();
        assertEquals(expResult, result);
        
   
    }

    /**
     * Test of setMQOO_SAVE_ALL_CONTEXT method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_SAVE_ALL_CONTEXT() {
        System.out.println("setMQOO_SAVE_ALL_CONTEXT");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_SAVE_ALL_CONTEXT(bSet);
        
      
    }

    /**
     * Test of getMQOO_SAVE_ALL_CONTEXT method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_SAVE_ALL_CONTEXT() {
        System.out.println("getMQOO_SAVE_ALL_CONTEXT");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_SAVE_ALL_CONTEXT();
        assertEquals(expResult, result);
        
    }

    /**
     * Test of setMQOO_ALTERNATE_USER_AUTHORITY method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_ALTERNATE_USER_AUTHORITY() {
        System.out.println("setMQOO_ALTERNATE_USER_AUTHORITY");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_ALTERNATE_USER_AUTHORITY(bSet);
        
       
    }

    /**
     * Test of getMQOO_ALTERNATE_USER_AUTHORITY method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_ALTERNATE_USER_AUTHORITY() {
        System.out.println("getMQOO_ALTERNATE_USER_AUTHORITY");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_ALTERNATE_USER_AUTHORITY();
        assertEquals(expResult, result);
        
       
    }

    /**
     * Test of setMQOO_FAIL_IF_QUIESCING method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_FAIL_IF_QUIESCING() {
        System.out.println("setMQOO_FAIL_IF_QUIESCING");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_FAIL_IF_QUIESCING(bSet);
        
      
    }

    /**
     * Test of getMQOO_FAIL_IF_QUIESCING method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_FAIL_IF_QUIESCING() {
        System.out.println("getMQOO_FAIL_IF_QUIESCING");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_FAIL_IF_QUIESCING();
        assertEquals(expResult, result);
        
       
    }

    /**
     * Test of setMQOO_PASS_IDENTITY_CONTEXT method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_PASS_IDENTITY_CONTEXT() {
        System.out.println("setMQOO_PASS_IDENTITY_CONTEXT");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_PASS_IDENTITY_CONTEXT(bSet);
        
      
    }

    /**
     * Test of getMQOO_PASS_IDENTITY_CONTEXT method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_PASS_IDENTITY_CONTEXT() {
        System.out.println("getMQOO_PASS_IDENTITY_CONTEXT");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_PASS_IDENTITY_CONTEXT();
        assertEquals(expResult, result);
        
      
    }

    /**
     * Test of setMQOO_PASS_ALL_CONTEXT method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_PASS_ALL_CONTEXT() {
        System.out.println("setMQOO_PASS_ALL_CONTEXT");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_PASS_ALL_CONTEXT(bSet);
        
       
    }

    /**
     * Test of getMQOO_PASS_ALL_CONTEXT method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_PASS_ALL_CONTEXT() {
        System.out.println("getMQOO_PASS_ALL_CONTEXT");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_PASS_ALL_CONTEXT();
        assertEquals(expResult, result);
        
      
    }

    /**
     * Test of setMQOO_SET_IDENTITY_CONTEXT method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_SET_IDENTITY_CONTEXT() {
        System.out.println("setMQOO_SET_IDENTITY_CONTEXT");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_SET_IDENTITY_CONTEXT(bSet);
        
    
    }

    /**
     * Test of getMQOO_SET_IDENTITY_CONTEXT method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_SET_IDENTITY_CONTEXT() {
        System.out.println("getMQOO_SET_IDENTITY_CONTEXT");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_SET_IDENTITY_CONTEXT();
        assertEquals(expResult, result);
        
       
    }

    /**
     * Test of setMQOO_SET_ALL_CONTEXT method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_SET_ALL_CONTEXT() {
        System.out.println("setMQOO_SET_ALL_CONTEXT");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_SET_ALL_CONTEXT(bSet);
        
      
    }

    /**
     * Test of getMQOO_SET_ALL_CONTEXT method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_SET_ALL_CONTEXT() {
        System.out.println("getMQOO_SET_ALL_CONTEXT");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_SET_ALL_CONTEXT();
        assertEquals(expResult, result);
        
        
    }

    /**
     * Test of setMQOO_INQUIRE method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_INQUIRE() {
        System.out.println("setMQOO_INQUIRE");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_INQUIRE(bSet);
        
     
    }

    /**
     * Test of getMQOO_INQUIRE method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_INQUIRE() {
        System.out.println("getMQOO_INQUIRE");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_INQUIRE();
        assertEquals(expResult, result);
     
    }

    /**
     * Test of setMQOO_SET method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_SET() {
        System.out.println("setMQOO_SET");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_SET(bSet);
        
      
    }

    /**
     * Test of getMQOO_SET method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_SET() {
        System.out.println("getMQOO_SET");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_SET();
        assertEquals(expResult, result);
        
       
    }

    /**
     * Test of setMQOO_BIND_ON_OPEN method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_BIND_ON_OPEN() {
        System.out.println("setMQOO_BIND_ON_OPEN");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_BIND_ON_OPEN(bSet);
        
       
    }

    /**
     * Test of getMQOO_BIND_ON_OPEN method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_BIND_ON_OPEN() {
        System.out.println("getMQOO_BIND_ON_OPEN");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_BIND_ON_OPEN();
        assertEquals(expResult, result);
        
      
    }

    /**
     * Test of setMQOO_BIND_NOT_FIXED method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_BIND_NOT_FIXED() {
        System.out.println("setMQOO_BIND_NOT_FIXED");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_BIND_NOT_FIXED(bSet);
        
      
    }

    /**
     * Test of getMQOO_BIND_NOT_FIXED method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_BIND_NOT_FIXED() {
        System.out.println("getMQOO_BIND_NOT_FIXED");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_BIND_NOT_FIXED();
        assertEquals(expResult, result);
        
      
    }

    /**
     * Test of setMQOO_BIND_AS_Q_DEF method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_BIND_AS_Q_DEF() {
        System.out.println("setMQOO_BIND_AS_Q_DEF");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_BIND_AS_Q_DEF(bSet);
        
       
    }

    /**
     * Test of getMQOO_BIND_AS_Q_DEF method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_BIND_AS_Q_DEF() {
        System.out.println("getMQOO_BIND_AS_Q_DEF");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_BIND_AS_Q_DEF();
        assertEquals(expResult, result);
        
      
    }

    /**
     * Test of setMQOO_RESOLVE_NAMES method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testSetMQOO_RESOLVE_NAMES() {
        System.out.println("setMQOO_RESOLVE_NAMES");
        
        boolean bSet = true;
        
        
        mInstance.setMQOO_RESOLVE_NAMES(bSet);
        
      
    }

    /**
     * Test of getMQOO_RESOLVE_NAMES method, of class com.sun.jbi.mqbc.extservices.QueueAccessOptions.
     */
    public void testGetMQOO_RESOLVE_NAMES() {
        System.out.println("getMQOO_RESOLVE_NAMES");
        
        
        
        boolean expResult = false;
        boolean result = mInstance.getMQOO_RESOLVE_NAMES();
        assertEquals(expResult, result);
        
       
    }
    
}
