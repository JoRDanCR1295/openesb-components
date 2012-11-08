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
 * @(#)PMOTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;

import junit.framework.*;
import com.ibm.mq.MQPutMessageOptions;
import java.util.logging.Logger;
import java.util.logging.Level;
import com.sun.jbi.internationalization.Messages;

/**
 *
 * @author rchen
 */
public class PMOTest extends TestCase {
    PMO mInstance;
            
    public PMOTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        mInstance = new PMO();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(PMOTest.class);
        
        return suite;
    }

    /**
     * Test of optionsClearAll method, of class com.sun.jbi.mqbc.extservices.PMO.
     */
    public void testOptionsClearAll() {
        System.out.println("optionsClearAll");
        
        
        
        mInstance.optionsClearAll();
        
       
    }

    /**
     * Test of setMQPMO_SYNCPOINT method, of class com.sun.jbi.mqbc.extservices.PMO.
     */
    public void testSetMQPMO_SYNCPOINT() {
        System.out.println("setMQPMO_SYNCPOINT");
        
        boolean bSet = true;
        
        
        mInstance.setMQPMO_SYNCPOINT(bSet);
        
      
    }

   
    /**
     * Test of setMQPMO_NO_CONTEXT method, of class com.sun.jbi.mqbc.extservices.PMO.
     */
    public void testSetMQPMO_NO_CONTEXT() {
        System.out.println("setMQPMO_NO_CONTEXT");
        
        boolean bSet = true;
        
        
        mInstance.setMQPMO_NO_CONTEXT(bSet);
        
     
    }

    /**
     * Test of setMQPMO_DEFAULT_CONTEXT method, of class com.sun.jbi.mqbc.extservices.PMO.
     */
    public void testSetMQPMO_DEFAULT_CONTEXT() {
        System.out.println("setMQPMO_DEFAULT_CONTEXT");
        
        boolean bSet = true;
        
        
        mInstance.setMQPMO_DEFAULT_CONTEXT(bSet);
        
     
    }

    /**
     * Test of setMQPMO_SET_IDENTITY_CONTEXT method, of class com.sun.jbi.mqbc.extservices.PMO.
     */
    public void testSetMQPMO_SET_IDENTITY_CONTEXT() {
        System.out.println("setMQPMO_SET_IDENTITY_CONTEXT");
        
        boolean bSet = true;
        
        
        mInstance.setMQPMO_SET_IDENTITY_CONTEXT(bSet);
     
    }

    /**
     * Test of setMQPMO_SET_ALL_CONTEXT method, of class com.sun.jbi.mqbc.extservices.PMO.
     */
    public void testSetMQPMO_SET_ALL_CONTEXT() {
        System.out.println("setMQPMO_SET_ALL_CONTEXT");
        
        boolean bSet = true;
        
        
        mInstance.setMQPMO_SET_ALL_CONTEXT(bSet);
        
     
    }

    /**
     * Test of setMQPMO_FAIL_IF_QUIESCING method, of class com.sun.jbi.mqbc.extservices.PMO.
     */
    public void testSetMQPMO_FAIL_IF_QUIESCING() {
        System.out.println("setMQPMO_FAIL_IF_QUIESCING");
        
        boolean bSet = true;
        
        
        mInstance.setMQPMO_FAIL_IF_QUIESCING(bSet);
        
       
    }

    /**
     * Test of setMQPMO_NEW_MSG_ID method, of class com.sun.jbi.mqbc.extservices.PMO.
     */
    public void testSetMQPMO_NEW_MSG_ID() {
        System.out.println("setMQPMO_NEW_MSG_ID");
        
        boolean bSet = true;
        
        
        mInstance.setMQPMO_NEW_MSG_ID(bSet);
        
        
    }

    /**
     * Test of setMQPMO_NEW_CORREL_ID method, of class com.sun.jbi.mqbc.extservices.PMO.
     */
    public void testSetMQPMO_NEW_CORREL_ID() {
        System.out.println("setMQPMO_NEW_CORREL_ID");
        
        boolean bSet = true;
        
        
        mInstance.setMQPMO_NEW_CORREL_ID(bSet);
        
       
    }

    /**
     * Test of setMQPMO_LOGICAL_ORDER method, of class com.sun.jbi.mqbc.extservices.PMO.
     */
    public void testSetMQPMO_LOGICAL_ORDER() {
        System.out.println("setMQPMO_LOGICAL_ORDER");
        
        boolean bSet = true;
        
        
        mInstance.setMQPMO_LOGICAL_ORDER(bSet);
        
        
    }

    /**
     * Test of setMQPMO_NONE method, of class com.sun.jbi.mqbc.extservices.PMO.
     */
    public void testSetMQPMO_NONE() {
        System.out.println("setMQPMO_NONE");
        
        boolean bSet = true;
        
        
        mInstance.setMQPMO_NONE(bSet);
        
        
    }

    /**
     * Test of setMQPMO_PASS_IDENTITY_CONTEXT method, of class com.sun.jbi.mqbc.extservices.PMO.
     */
    public void testSetMQPMO_PASS_IDENTITY_CONTEXT() {
        System.out.println("setMQPMO_PASS_IDENTITY_CONTEXT");
        
        boolean bSet = true;
        
        
        mInstance.setMQPMO_PASS_IDENTITY_CONTEXT(bSet);
        
      
    }

    /**
     * Test of setMQPMO_PASS_ALL_CONTEXT method, of class com.sun.jbi.mqbc.extservices.PMO.
     */
    public void testSetMQPMO_PASS_ALL_CONTEXT() {
        System.out.println("setMQPMO_PASS_ALL_CONTEXT");
        
        boolean bSet = true;
        
        
        mInstance.setMQPMO_PASS_ALL_CONTEXT(bSet);
        
       
    }
    
}
