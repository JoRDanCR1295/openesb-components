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
 * @(#)MQBCAddressTest.java 
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

/** @author rchen */
public class MQBCAddressTest extends TestCase {
    
    public MQBCAddressTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
    }
    
    protected void tearDown() throws Exception {
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(MQBCAddressTest.class);
        
        return suite;
    }
    
    /**
     * Test of getElementType method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testGetElementType() {
        System.out.println("getElementType");
        
        MQBCAddress instance = new MQBCAddress();
        
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/mq/","address");
        QName result = instance.getElementType();
        assertEquals(expResult, result);
        
    }
    
    
    /**
     * Test of getRequired method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testGetRequired() {
        System.out.println("getRequired");
        
        MQBCAddress instance = new MQBCAddress();
        
        Boolean expResult = false;
        Boolean result = instance.getRequired();
        assertEquals(expResult, result);
        
        
    }
    
    /**
     * Test of setRequired method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testSetRequired() {
        System.out.println("setRequired");
        
        Boolean required = true;
        MQBCAddress instance = new MQBCAddress();
        
        instance.setRequired(required);
        
        assertEquals(required,instance.getRequired());
        assertSame(required,instance.getRequired());
    }
    
    /**
     * Test of getHostName method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testGetHostName() {
        System.out.println("getHostName");
        
        MQBCAddress instance = new MQBCAddress();
        
        String expResult = "";
        String result = instance.getHostName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setHostName method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testSetHostName() {
        System.out.println("setHostName");
        
        String val = "rchen-2k";
        MQBCAddress instance = new MQBCAddress();
        
        instance.setHostName(val);
        assertEquals(val, instance.getHostName());
    }
    
    /**
     * Test of getPortNumber method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testGetPortNumber() {
        System.out.println("getPortNumber");
        
        MQBCAddress instance = new MQBCAddress();
        
        int expResult = 1414;
        int result = instance.getPortNumber();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }
    
    /**
     * Test of setPortNumber method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testSetPortNumber() {
        System.out.println("setPortNumber");
        
        int val = 2828;
        MQBCAddress instance = new MQBCAddress();
        
        instance.setPortNumber(val);
        
        assertEquals(val, instance.getPortNumber());
    }
    
    /**
     * Test of getQueueManagerName method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testGetQueueManagerName() {
        System.out.println("getQueueManagerName");
        
        MQBCAddress instance = new MQBCAddress();
        
        String expResult = "";
        String result = instance.getQueueManagerName();
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }
    
    /**
     * Test of setQueueManagerName method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testSetQueueManagerName() {
        System.out.println("setQueueManagerName");
        
        String val = "QueueManagerName";
        MQBCAddress instance = new MQBCAddress();
        
        instance.setQueueManagerName(val);
        
         assertEquals(val, instance.getQueueManagerName());
    }
    
    /**
     * Test of getChannelName method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testGetChannelName() {
        System.out.println("getChannelName");
        
        MQBCAddress instance = new MQBCAddress();
        
        String expResult = "";
        String result = instance.getChannelName();
        assertEquals(expResult, result);
        
       
    }
    
    /**
     * Test of setChannelName method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testSetChannelName() {
        System.out.println("setChannelName");
        
        String val = "setChannelName";
        MQBCAddress instance = new MQBCAddress();
        
        instance.setChannelName(val);
        assertEquals(val,  instance.getChannelName());
     
    }
    
    /**
     * Test of getCodedCharacterSetID method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testGetCodedCharacterSetID() {
        System.out.println("getCodedCharacterSetID");
        
        MQBCAddress instance = new MQBCAddress();
        
        String expResult = "";
        String result = instance.getCodedCharacterSetID();
        assertEquals(expResult, result);
        
       
    }
    
    /**
     * Test of setCodedCharacterSetID method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testSetCodedCharacterSetID() {
        System.out.println("setCodedCharacterSetID");
        
        String val = "CodedCharacterSetID";
        MQBCAddress instance = new MQBCAddress();
        
        instance.setCodedCharacterSetID(val);
        
         assertEquals(val, instance.getCodedCharacterSetID());
    }
    
    /**
     * Test of getPassword method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testGetPassword() {
        System.out.println("getPassword");
        
        MQBCAddress instance = new MQBCAddress();
        
        String expResult = "";
        String result = instance.getPassword();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setPassword method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testSetPassword() {
        System.out.println("setPassword");
        
        String val = "Password";
        MQBCAddress instance = new MQBCAddress();
        
        instance.setPassword(val);
        
         assertEquals(val,   instance.getPassword());
    }
    
    /**
     * Test of getUserName method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testGetUserName() {
        System.out.println("getUserName");
        
        MQBCAddress instance = new MQBCAddress();
        
        String expResult = "";
        String result = instance.getUserName();
        assertEquals(expResult, result);
        
      
    }
    
    /**
     * Test of setUserName method, of class com.sun.jbi.mqbc.extensions.MQBCAddress.
     */
    public void testSetUserName() {
        System.out.println("setUserName");
        
        String val = "UserID";
        MQBCAddress instance = new MQBCAddress();
        
        instance.setUserName(val);
        
        assertEquals(val, instance.getUserName());
    }
    
    public void testGetCipherSuite() {
        MQBCAddress instance = new MQBCAddress();
        
        String input;
        String expected;
        String result;
        
        expected = "";
        result = instance.getCipherSuite();
        assertEquals("getCipherSuite: " + result + ", expected: " + expected,
                expected,
                result);
        
        input = "ImaginaryCipherSuite";
        expected = "ImaginaryCipherSuite";
        instance.setCipherSuite(input);
        result = instance.getCipherSuite();
        assertEquals("getCipherSuite: " + result + ", expected: " + expected,
                expected,
                result);
        
        input = "";
        expected = "";
        instance.setCipherSuite(input);
        result = instance.getCipherSuite();
        assertEquals("getCipherSuite: " + result + ", expected: " + expected,
                expected,
                result);
        
        input = null;
        expected = "";
        instance.setCipherSuite(input);
        result = instance.getCipherSuite();
        assertEquals("getCipherSuite: " + result + ", expected: " + expected,
                expected,
                result);
    }

    public void testSetCipherSuite() {
        MQBCAddress instance = new MQBCAddress();
        
        String input;
        String expected;
        String result;
        
        expected = "";
        result = instance.getCipherSuite();
        assertEquals("getCipherSuite: " + result + ", expected: " + expected,
                expected,
                result);
        
        input = "ImaginaryCipherSuite";
        expected = "ImaginaryCipherSuite";
        instance.setCipherSuite(input);
        result = instance.getCipherSuite();
        assertEquals("getCipherSuite: " + result + ", expected: " + expected,
                expected,
                result);
        
        input = "";
        expected = "";
        instance.setCipherSuite(input);
        result = instance.getCipherSuite();
        assertEquals("getCipherSuite: " + result + ", expected: " + expected,
                expected,
                result);
        
        input = null;
        expected = "";
        instance.setCipherSuite(input);
        result = instance.getCipherSuite();
        assertEquals("getCipherSuite: " + result + ", expected: " + expected,
                expected,
                result);
    }

    public void testGetSslPeerName() throws Exception {
        MQBCAddress instance = new MQBCAddress();
        
        String input;
        String expected;
        String result;
        
        expected = "";
        result = instance.getSslPeerName();
        assertEquals("getSslPeerName: " + result + ", expected: " + expected,
                expected,
                result);
        
        input = "ImaginaryPeerName";
        expected = "ImaginaryPeerName";
        instance.setSslPeerName(input);
        result = instance.getSslPeerName();
        assertEquals("getSslPeerName: " + result + ", expected: " + expected,
                expected,
                result);
        
        input = "";
        expected = "";
        instance.setSslPeerName(input);
        result = instance.getSslPeerName();
        assertEquals("getSslPeerName: " + result + ", expected: " + expected,
                expected,
                result);
        
        input = null;
        expected = "";
        instance.setSslPeerName(input);
        result = instance.getSslPeerName();
        assertEquals("getSslPeerName: " + result + ", expected: " + expected,
                expected,
                result);
    }

    public void testSetSslPeerName() throws Exception {
        MQBCAddress instance = new MQBCAddress();
        
        String input;
        String expected;
        String result;
        
        expected = "";
        result = instance.getSslPeerName();
        assertEquals("getSslPeerName: " + result + ", expected: " + expected,
                expected,
                result);
        
        input = "ImaginaryPeerName";
        expected = "ImaginaryPeerName";
        instance.setSslPeerName(input);
        result = instance.getSslPeerName();
        assertEquals("getSslPeerName: " + result + ", expected: " + expected,
                expected,
                result);
        
        input = "";
        expected = "";
        instance.setSslPeerName(input);
        result = instance.getSslPeerName();
        assertEquals("getSslPeerName: " + result + ", expected: " + expected,
                expected,
                result);
        
        input = null;
        expected = "";
        instance.setSslPeerName(input);
        result = instance.getSslPeerName();
        assertEquals("getSslPeerName: " + result + ", expected: " + expected,
                expected,
                result);
    }
}
