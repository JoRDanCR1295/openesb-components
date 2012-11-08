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
 * @(#)MQClientConfigurationTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;

import java.util.Properties;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 *
 * @author rchen
 */
public class MQClientConfigurationTest extends TestCase {
    MQClientConfiguration mInstance;
    
    public MQClientConfigurationTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
         mInstance = new MQClientConfiguration();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(MQClientConfigurationTest.class);
        
        return suite;
    }

    /**
     * Test of getQueueName method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testGetQueueName() {
        System.out.println("getQueueName");
        
       
        
        String expResult = "";
        String result = mInstance.getQueueName();
        assertEquals(expResult, result);
        
       
    }

    /**
     * Test of setQueueName method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testSetQueueName() {
        System.out.println("setQueueName");
        
        String val = "QueueName";
        
        mInstance.setQueueName(val);
        String result = mInstance.getQueueName();
       assertEquals(val, result);
       
    }

    /**
     * Test of getSecurityExit method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testGetSecurityExit() {
        System.out.println("getSecurityExit");
        
      
        String result = mInstance.getSecurityExit();
        assertTrue("".equals(result));
        
    }

    /**
     * Test of getSecurityExitPath method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testGetSecurityExitPath() {
        System.out.println("getSecurityExitPath");
        
      
        String result = mInstance.getSecurityExitPath();
        assertTrue("".equals(result));
        
    
    }

    /**
     * Test of getQueueManagerName method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testGetQueueManagerName() {
        System.out.println("getQueueManagerName");
        
     
        
       
        String result = mInstance.getQueueManagerName();
        assertTrue("".equals(result));
        
    
    }

    /**
     * Test of setQueueManagerName method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testSetQueueManagerName() {
        System.out.println("setQueueManagerName");
        
        String name = "";
        MQClientConfiguration instance = new MQClientConfiguration();
        
        instance.setQueueManagerName(name);
        
     
    }

    /**
     * Test of getUser method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testGetUser() {
        System.out.println("getUser");
        
      
        String result = mInstance.getUser();
        assertTrue("".equals(result));
        
     
    }

    /**
     * Test of setUser method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testSetUser() {
        System.out.println("setUser");
        
        String name = "";
       
        
        mInstance.setUser(name);
        
       
    }

    /**
     * Test of getPassword method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testGetPassword() {
        System.out.println("getPassword");
        
      
        
        String expResult ="";
        String result = mInstance.getPassword();
        assertEquals(expResult, result);
        
       
    }

    /**
     * Test of setPassword method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testSetPassword() {
        System.out.println("setPassword");
        
        String name = "password";
       
        mInstance.setPassword(name);
        assertEquals(name,  mInstance.getPassword());
      
    }

    /**
     * Test of getHost method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testGetHost() {
        System.out.println("getHost");
        
      
        
        String expResult = "";
        String result = mInstance.getHost();
        assertEquals(expResult, result);
        
      
    }

    /**
     * Test of setHost method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testSetHost() {
        System.out.println("setHost");
        
        String name = "Host";
        
        mInstance.setHost(name);
        assertEquals(name, mInstance.getHost());
      
    }

    /**
     * Test of setPort method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testSetPort() {
        System.out.println("setPort");
        
        int port = 0; 
        mInstance.setPort(port);
        Number result = mInstance.getPort();
        assertEquals(port, result.intValue());
      
    }

    /**
     * Test of getPort method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testGetPort() {
        System.out.println("getPort");
      
        Number result = mInstance.getPort();
        assertNotNull(result);
        
       
    }

    /**
     * Test of getXAMode method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testGetXAMode() {
        System.out.println("getXAMode");
        
       
        Boolean result = mInstance.getXAMode();
        assertFalse(result.booleanValue());
        
      
    }

    /**
     * Test of getChannelName method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testGetChannelName() {
        System.out.println("getChannelName");
        
     
        String result = mInstance.getChannelName();
        assertTrue("".equals(result));
        
      
    }

    /**
     * Test of setChannelName method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testSetChannelName() {
        System.out.println("setChannelName");
        
      String expected = "channelName";
        
       mInstance.setChannelName(expected);
       assertEquals(expected,mInstance.getChannelName());
    }

    /**
     * Test of logConfiguration method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testLogConfiguration() {
        System.out.println("logConfiguration");
        
      
        
        mInstance.logConfiguration();
        
       
    }

    /**
     * Test of getQueueAccessOptions method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testGetQueueAccessOptions() {
        System.out.println("getQueueAccessOptions");
        
      
        
        QueueAccessOptions expResult = null;
        QueueAccessOptions result = mInstance.getQueueAccessOptions();
        assertNotNull(result);
        
       
    }

    /**
     * Test of toProperties method, of class com.sun.jbi.mqbc.extservices.MQClientConfiguration.
     */
    public void testToProperties() {
        System.out.println("toProperties");
        
      
        
        Properties expResult = null;
        Properties result = mInstance.toProperties();
        assertNotNull(result);
        
      
    }

    public void testGetCipherSpec() {
        String input;
        String output;
        String expected;
   
        output = mInstance.getCipherSuite();
        expected = "";
        assertTrue("getCipherSuite: got: " + output + ", expected: " + expected,
                expected.equals(output));
    
        input = "ImaginaryCipherSuite";
        expected = input;
        mInstance.setCipherSuite(input);
        output = mInstance.getCipherSuite();
        assertTrue("getCipherSuite: got: " + output + ", expected: " + expected,
                expected.equals(output));
        
        input = "";
        expected = input;
        mInstance.setCipherSuite(input);
        output = mInstance.getCipherSuite();
        assertTrue("getCipherSuite: got: " + output + ", expected: " + expected,
                expected.equals(output));
        
        input = null;
        expected = "";
        mInstance.setCipherSuite(input);
        output = mInstance.getCipherSuite();
        assertTrue("getCipherSuite: got: " + output + ", expected: " + expected,
                expected.equals(output));
    }
    
    public void testSetCipherSpec() {
        String input;
        String output;
        String expected;
        
        input = "ImaginaryCipherSuite";
        expected = input;
        mInstance.setCipherSuite(input);
        output = mInstance.getCipherSuite();
        assertTrue("getCipherSuite: got: " + output + ", expected: " + expected,
                expected.equals(output));
        
        input = "";
        expected = input;
        mInstance.setCipherSuite(input);
        output = mInstance.getCipherSuite();
        assertTrue("getCipherSuite: got: " + output + ", expected: " + expected,
                expected.equals(output));
        
        input = null;
        expected = "";
        mInstance.setCipherSuite(input);
        output = mInstance.getCipherSuite();
        assertTrue("getCipherSuite: got: " + output + ", expected: " + expected,
                expected.equals(output));
    }
    
    public void testGetSslPeerName() {
        String input;
        String output;
        String expected;
        
        output = mInstance.getSslPeerName();
        expected = "";
        assertTrue("getSslPeerName: got: " + output + ", expected: " + expected,
                expected.equals(output));

        input = "ImaginaryPeer";
        expected = input;
        mInstance.setSslPeerName(input);
        output = mInstance.getSslPeerName();
        assertTrue("getSslPeerName: got: " + output + ", expected: " + expected,
                expected.equals(output));
        
        input = "";
        expected = input;
        mInstance.setSslPeerName(input);
        output = mInstance.getSslPeerName();
        assertTrue("getSslPeerName: got: " + output + ", expected: " + expected,
                expected.equals(output));
        
        input = null;
        expected = "";
        mInstance.setSslPeerName(input);
        output = mInstance.getSslPeerName();
        assertTrue("getSslPeerName: got: " + output + ", expected: " + expected,
                expected.equals(output));
    }
    
    public void testSetSslPeerName() {
        String input;
        String output;
        String expected;
        
        input = "ImaginaryPeer";
        expected = input;
        mInstance.setSslPeerName(input);
        output = mInstance.getSslPeerName();
        assertTrue("getSslPeerName: got: " + output + ", expected: " + expected,
                expected.equals(output));
        
        input = "";
        expected = input;
        mInstance.setSslPeerName(input);
        output = mInstance.getSslPeerName();
        assertTrue("getSslPeerName: got: " + output + ", expected: " + expected,
                expected.equals(output));
        
        input = null;
        expected = "";
        mInstance.setSslPeerName(input);
        output = mInstance.getSslPeerName();
        assertTrue("getSslPeerName: got: " + output + ", expected: " + expected,
                expected.equals(output));
    }
    
}
