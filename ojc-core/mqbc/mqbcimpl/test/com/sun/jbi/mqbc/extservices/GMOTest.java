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
 * @(#)GMOTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;

import junit.framework.*;
import com.ibm.mq.MQGetMessageOptions;
import java.util.logging.Logger;
import java.util.logging.Level;
import com.sun.jbi.internationalization.Messages;


/**
 *
 * @author rchen
 */
public class GMOTest extends TestCase {
    
    public GMOTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
    }
    
    protected void tearDown() throws Exception {
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(GMOTest.class);
        
        return suite;
    }
    
    /**
     * Test of setWaitValue method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetWaitValue() {
        System.out.println("setWaitValue");
        
        int ms = 0;
        GMO instance = new GMO();
        
        instance.setWaitValue(ms);
        assertEquals(ms,instance.getWaitValue());
        
    }
    
    /**
     * Test of getWaitValue method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testGetWaitValue() {
        System.out.println("getWaitValue");
        
        GMO instance = new GMO();
        
        int expResult = 0;
        int result = instance.getWaitValue();
        assertEquals(expResult, result);
        
        
    }
    
    /**
     * Test of setUnlimitedWait method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetUnlimitedWait() {
        System.out.println("setUnlimitedWait");
        
        GMO instance = new GMO();
        
        instance.setUnlimitedWait();
        
    }
    
    /**
     * Test of optionsClearAll method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testOptionsClearAll() {
        System.out.println("optionsClearAll");
        
        GMO instance = new GMO();
        
        instance.optionsClearAll();
        
        
    }
    
    /**
     * Test of matchOptionsClearAll method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testMatchOptionsClearAll() {
        System.out.println("matchOptionsClearAll");
        
        GMO instance = new GMO();
        
        instance.matchOptionsClearAll();
        
        
    }
    
    /**
     * Test of setMQGMO_NONE method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_NONE() {
        System.out.println("setMQGMO_NONE");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_NONE(bSet);
        
        
    }
    
    /**
     * Test of setMQGMO_WAIT method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_WAIT() {
        System.out.println("setMQGMO_WAIT");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_WAIT(bSet);
        
        
    }
    
    /**
     * Test of setMQGMO_NO_WAIT method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_NO_WAIT() {
        System.out.println("setMQGMO_NO_WAIT");
        
        GMO instance = new GMO();
        instance.setMQGMO_NO_WAIT(true);
        instance.setMQGMO_NO_WAIT(false);
        
        
        
        
    }
    
    /**
     * Test of setMQGMO_SYNCPOINT method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_SYNCPOINT() {
        System.out.println("setMQGMO_SYNCPOINT");
        
        GMO instance = new GMO();
        instance.setMQGMO_SYNCPOINT(true);
        instance.setMQGMO_SYNCPOINT(false);
        
        
        
    }
    
    
    
    /**
     * Test of setMQGMO_BROWSE_FIRST method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_BROWSE_FIRST() {
        System.out.println("setMQGMO_BROWSE_FIRST");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_BROWSE_FIRST(bSet);
        
        
    }
    
    /**
     * Test of setMQGMO_BROWSE_NEXT method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_BROWSE_NEXT() {
        System.out.println("setMQGMO_BROWSE_NEXT");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_BROWSE_NEXT(bSet);
        
    }
    
    /**
     * Test of setMQGMO_BROWSE_MSG_UNDER_CURSOR method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_BROWSE_MSG_UNDER_CURSOR() {
        System.out.println("setMQGMO_BROWSE_MSG_UNDER_CURSOR");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_BROWSE_MSG_UNDER_CURSOR(bSet);
        
        
    }
    
    /**
     * Test of setMQGMO_LOCK method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_LOCK() {
        System.out.println("setMQGMO_LOCK");
        
        boolean bSet = true;
        GMO instance = new GMO();
        instance.setMQGMO_LOCK(bSet);
        
    }
    
    /**
     * Test of setMQGMO_UNLOCK method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_UNLOCK() {
        System.out.println("setMQGMO_UNLOCK");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_UNLOCK(bSet);
        assertEquals(com.ibm.mq.MQC.MQGMO_UNLOCK,instance.options & com.ibm.mq.MQC.MQGMO_UNLOCK);
        
    }
    
    /**
     * Test of setMQGMO_ACCEPT_TRUNCATED_MSG method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_ACCEPT_TRUNCATED_MSG() {
        System.out.println("setMQGMO_ACCEPT_TRUNCATED_MSG");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_ACCEPT_TRUNCATED_MSG(true);
        assertEquals(com.ibm.mq.MQC.MQGMO_ACCEPT_TRUNCATED_MSG,instance.options & com.ibm.mq.MQC.MQGMO_ACCEPT_TRUNCATED_MSG);
        
    }
    
    /**
     * Test of setMQGMO_FAIL_IF_QUIESCING method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_FAIL_IF_QUIESCING() {
        System.out.println("setMQGMO_FAIL_IF_QUIESCING");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_FAIL_IF_QUIESCING(bSet);
        
        assertEquals(com.ibm.mq.MQC.MQGMO_FAIL_IF_QUIESCING,instance.options & com.ibm.mq.MQC.MQGMO_FAIL_IF_QUIESCING);
    }
    
    /**
     * Test of setMQGMO_CONVERT method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_CONVERT() {
        System.out.println("setMQGMO_CONVERT");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_CONVERT(bSet);
        
        assertEquals(com.ibm.mq.MQC.MQGMO_CONVERT,instance.options & com.ibm.mq.MQC.MQGMO_CONVERT);
    }
    
    /**
     * Test of setMQGMO_SYNCPOINT_IF_PERSISTENT method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_SYNCPOINT_IF_PERSISTENT() {
        System.out.println("setMQGMO_SYNCPOINT_IF_PERSISTENT");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_SYNCPOINT_IF_PERSISTENT(bSet);
        assertEquals(com.ibm.mq.MQC.MQGMO_SYNCPOINT_IF_PERSISTENT,instance.options & com.ibm.mq.MQC.MQGMO_SYNCPOINT_IF_PERSISTENT);
    }
    
    /**
     * Test of setMQGMO_MARK_SKIP_BACKOUT method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_MARK_SKIP_BACKOUT() {
        System.out.println("setMQGMO_MARK_SKIP_BACKOUT");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_MARK_SKIP_BACKOUT(bSet);
        assertEquals(com.ibm.mq.MQC.MQGMO_MARK_SKIP_BACKOUT,instance.options & com.ibm.mq.MQC.MQGMO_MARK_SKIP_BACKOUT);
        
        
    }
    
    /**
     * Test of setMQGMO_MSG_UNDER_CURSOR method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_MSG_UNDER_CURSOR() {
        System.out.println("setMQGMO_MSG_UNDER_CURSOR");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_MSG_UNDER_CURSOR(bSet);
        
        assertEquals(com.ibm.mq.MQC.MQGMO_MSG_UNDER_CURSOR,instance.options & com.ibm.mq.MQC.MQGMO_MSG_UNDER_CURSOR);
        
    }
    
    /**
     * Test of setMQGMO_LOGICAL_ORDER method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_LOGICAL_ORDER() {
        System.out.println("setMQGMO_LOGICAL_ORDER");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_LOGICAL_ORDER(bSet);
        assertEquals(com.ibm.mq.MQC.MQGMO_LOGICAL_ORDER,
                instance.options & com.ibm.mq.MQC.MQGMO_LOGICAL_ORDER);
        
    }
    
    /**
     * Test of setMQGMO_COMPLETE_MSG method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_COMPLETE_MSG() {
        System.out.println("setMQGMO_COMPLETE_MSG");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_COMPLETE_MSG(bSet);
        assertEquals(com.ibm.mq.MQC.MQGMO_COMPLETE_MSG,
                instance.options & com.ibm.mq.MQC.MQGMO_COMPLETE_MSG);
        
    }
    
    /**
     * Test of setMQGMO_ALL_MSGS_AVAILABLE method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_ALL_MSGS_AVAILABLE() {
        System.out.println("setMQGMO_ALL_MSGS_AVAILABLE");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_ALL_MSGS_AVAILABLE(bSet);
        assertEquals(com.ibm.mq.MQC.MQGMO_ALL_MSGS_AVAILABLE,
                instance.options & com.ibm.mq.MQC.MQGMO_ALL_MSGS_AVAILABLE);
        
    }
    
    /**
     * Test of setMQGMO_ALL_SEGMENTS_AVAILABLE method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQGMO_ALL_SEGMENTS_AVAILABLE() {
        System.out.println("setMQGMO_ALL_SEGMENTS_AVAILABLE");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQGMO_ALL_SEGMENTS_AVAILABLE(bSet);
        
        assertEquals(com.ibm.mq.MQC.MQGMO_ALL_SEGMENTS_AVAILABLE,
                instance.options & com.ibm.mq.MQC.MQGMO_ALL_SEGMENTS_AVAILABLE);
    }
    
    /**
     * Test of setMQMO_MATCH_MSG_ID method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQMO_MATCH_MSG_ID() {
        System.out.println("setMQMO_MATCH_MSG_ID");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQMO_MATCH_MSG_ID(bSet);
        
        assertEquals(com.ibm.mq.MQC.MQMO_MATCH_MSG_ID,
                instance.matchOptions & com.ibm.mq.MQC.MQMO_MATCH_MSG_ID);
    }
    
    /**
     * Test of setMQMO_MATCH_CORREL_ID method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQMO_MATCH_CORREL_ID() {
        System.out.println("setMQMO_MATCH_CORREL_ID");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQMO_MATCH_CORREL_ID(bSet);
        
        assertEquals(com.ibm.mq.MQC.MQMO_MATCH_CORREL_ID,
                instance.matchOptions & com.ibm.mq.MQC.MQMO_MATCH_CORREL_ID);
    }
    
    /**
     * Test of setMQMO_MATCH_GROUP_ID method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQMO_MATCH_GROUP_ID() {
        System.out.println("setMQMO_MATCH_GROUP_ID");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQMO_MATCH_GROUP_ID(bSet);
        
        assertEquals(com.ibm.mq.MQC.MQMO_MATCH_GROUP_ID,
                instance.matchOptions & com.ibm.mq.MQC.MQMO_MATCH_GROUP_ID);
    }
    
    /**
     * Test of setMQMO_MATCH_MSG_SEQ_NUMBER method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQMO_MATCH_MSG_SEQ_NUMBER() {
        System.out.println("setMQMO_MATCH_MSG_SEQ_NUMBER");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQMO_MATCH_MSG_SEQ_NUMBER(bSet);
        
        assertEquals(com.ibm.mq.MQC.MQMO_MATCH_MSG_SEQ_NUMBER,
                instance.matchOptions & com.ibm.mq.MQC.MQMO_MATCH_MSG_SEQ_NUMBER);
    }
    
    /**
     * Test of setMQMO_NONE method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testSetMQMO_NONE() {
        System.out.println("setMQMO_NONE");
        
        boolean bSet = true;
        GMO instance = new GMO();
        
        instance.setMQMO_NONE(bSet);
        
        assertEquals(com.ibm.mq.MQC.MQMO_NONE,
                instance.options & com.ibm.mq.MQC.MQMO_NONE);
    }
    
    /**
     * Test of getResolvedQueueName method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testGetResolvedQueueName() {
        System.out.println("getResolvedQueueName");
        
        GMO instance = new GMO();
        
        String expResult = "";
        String result = instance.getResolvedQueueName();
        assertEquals(expResult, result);
        
        
    }
    
    /**
     * Test of getSegmentation method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testGetSegmentation() {
        System.out.println("getSegmentation");
        
        GMO instance = new GMO();
        
        char expResult = ' ';
        char result = instance.getSegmentation();
        assertEquals(expResult, result);
        
        
    }
    
    /**
     * Test of getSegmentationStatus method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testGetSegmentationStatus() {
        System.out.println("getSegmentationStatus");
        
        GMO instance = new GMO();
        
        char expResult = ' ';
        char result = instance.getSegmentationStatus();
        assertEquals(expResult, result);
        
        
    }
    
    /**
     * Test of isMessageInGroup method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testIsMessageInGroup() {
        System.out.println("isMessageInGroup");
        
        GMO instance = new GMO();
        
        boolean expResult = false;
        boolean result = instance.isMessageInGroup();
          assertEquals(expResult, result);
        
        
    }
    
    /**
     * Test of isMessageLastInGroup method, of class com.sun.jbi.mqbc.extservices.GMO.
     */
    public void testIsMessageLastInGroup() {
        System.out.println("isMessageLastInGroup");
        
        GMO instance = new GMO();
        
        boolean expResult = false;
        boolean result = instance.isMessageLastInGroup();
        assertEquals(expResult, result);
        
        
    }
    
}
