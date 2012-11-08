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
 * @(#)ChannelManagerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jms;

import java.util.HashMap;

import javax.jbi.component.ComponentContext;
import javax.wsdl.BindingOperation;
import javax.xml.namespace.QName;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.jmock.Mock;

import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.jmsbc.Endpoint;
import com.sun.jbi.jmsbc.extensions.JMSAddress;
import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.jmsbc.extensions.JMSInput;
import com.sun.jbi.jmsbc.extensions.JMSMessage;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.extensions.JMSOutput;
import com.sun.jbi.jmsbc.mbeans.JMSBCRuntimeConfiguration;
import com.sun.jbi.jmsbc.mbeans.RuntimeConfiguration;

/**
 * Unit tests for ChannelManager
 */
public class ChannelManagerTest extends org.jmock.cglib.MockObjectTestCase {
    private ComponentContext componentContext = null;
    private Mock componentContextMock = null;
    private HashMap inMessageExchanges = null;
    private HashMap outMessageExchanges = null;
    private HashMap jmsOperations = null;
    
    private ChannelManagerImpl mgr;
        
    private Endpoint endpt1;
    private Mock endpt1Mock;
    private Endpoint endpt2;
    private Mock endpt2Mock;
    private Endpoint endpt3;
    private Mock endpt3Mock;
    private Endpoint endpt4;
    private Mock endpt4Mock;
    
    private QName op1;
    private QName op2;
    private QName op2a;
    private QName op3;
    private QName op4;
    
    private JMSOperation jmsOp1;
    private JMSOperation jmsOp2;
    private JMSOperation jmsOp2a;
    private JMSOperation jmsOp3;
    private JMSOperation jmsOp4;
    private BindingOperation jmsBindOp1;
    private Mock jmsBindOp1Mock;
    private BindingOperation jmsBindOp2;
    private Mock jmsBindOp2Mock;
    private BindingOperation jmsBindOp2a;
    private Mock jmsBindOp2aMock;
    private BindingOperation jmsBindOp3;
    private Mock jmsBindOp3Mock;
    private BindingOperation jmsBindOp4;
    private Mock jmsBindOp4Mock;
    
    private JMSAddress jmsAdd;
    
    private JMSInput jmsInput1;
    private JMSInput jmsInput2;
    private JMSInput jmsInput3;
    private JMSInput jmsInput4;
    private JMSOutput jmsOutput1;
    private JMSOutput jmsOutput2;

    private JMSMessage jmsMsg1;
    private JMSMessage jmsMsg2;
    private JMSMessage jmsMsg3;
    private JMSMessage jmsMsg4;
    private JMSMessage jmsMsg5;
    private JMSMessage jmsMsg6;
    
    private Mock runtimeConfigMock;
    private RuntimeConfiguration runtimeConfig;
    
    public ChannelManagerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        
        componentContextMock = mock(ComponentContext.class);
        componentContext = (ComponentContext)componentContextMock.proxy();

        String baseDir = getClass().getClassLoader().getResource("com/sun/jbi/jmsbc/testdir/config.properties").getPath();
        baseDir = baseDir.substring(0, baseDir.lastIndexOf('/'));
        runtimeConfigMock = mock(JMSBCRuntimeConfiguration.class,
                                    new Class [] {String.class, KeyStoreUtilClient.class},
                                    new Object[] {baseDir, null});
        runtimeConfig = (RuntimeConfiguration)runtimeConfigMock.proxy();
        
        inMessageExchanges = new HashMap();
        outMessageExchanges = new HashMap();
                
        mgr = new ChannelManagerImpl(componentContext,
                                     inMessageExchanges);
                        
        op1 = new QName("qname1");
        op2 = new QName("qname2");
        op2a = new QName("qname2a");
        op3 = new QName("qname3");
        op4 = new QName("qname4");
        
        jmsBindOp1Mock = mock(BindingOperation.class);
        jmsBindOp1 = (BindingOperation)jmsBindOp1Mock.proxy();
        jmsOp1 = new JMSOperation();
        jmsOp1.setBindingOperation(jmsBindOp1);
        jmsBindOp2Mock = mock(BindingOperation.class);
        jmsBindOp2 = (BindingOperation)jmsBindOp2Mock.proxy();
        jmsOp2 = new JMSOperation();
        jmsOp2.setBindingOperation(jmsBindOp2);
        jmsBindOp2aMock = mock(BindingOperation.class);
        jmsBindOp2a = (BindingOperation)jmsBindOp2aMock.proxy();
        jmsOp2a = new JMSOperation();
        jmsOp2a.setBindingOperation(jmsBindOp2a);
        jmsBindOp3Mock = mock(BindingOperation.class);
        jmsBindOp3 = (BindingOperation)jmsBindOp3Mock.proxy();
        jmsOp3 = new JMSOperation();
        jmsOp3.setBindingOperation(jmsBindOp3);
        jmsBindOp4Mock = mock(BindingOperation.class);
        jmsBindOp4 = (BindingOperation)jmsBindOp4Mock.proxy();
        jmsOp4 = new JMSOperation();
        jmsOp4.setBindingOperation(jmsBindOp4);
        
        jmsOperations = new HashMap();
        jmsOperations.put(op1, jmsOp1);
        jmsOperations.put(op2, jmsOp2);
        jmsOperations.put(op2a, jmsOp2a);
        jmsOperations.put(op3, jmsOp3);
        jmsOperations.put(op4, jmsOp4);
        
        jmsAdd = new JMSAddress();
        jmsAdd.setConnectionURL("mq://localhost:7676");

        // A outbound send channel
        endpt1Mock = mock (Endpoint.class);
        endpt1 = (Endpoint)endpt1Mock.proxy();
        endpt1Mock.stubs().method("getServiceUnitID").will(returnValue("A-SU-ID"));
        endpt1Mock.stubs().method("getEndpointType").will(returnValue(Endpoint.EndpointType.OUTBOUND));
        endpt1Mock.stubs().method("getServiceName").will(returnValue(new QName("service1")));
        endpt1Mock.stubs().method("getEndpointName").will((returnValue("endpt1")));
        endpt1Mock.stubs().method("getJMSOperations").will(returnValue(jmsOperations));
        endpt1Mock.stubs().method("getServiceQualities").will(returnValue(null));
        jmsOp1.setDestinationType(JMSConstants.TOPIC);
        jmsOp2.setTransaction(JMSConstants.TRANSACTION_NONE);
        jmsInput1 = new JMSInput();
        jmsMsg1 = new JMSMessage();
        jmsInput1.setJMSMessage(jmsMsg1);
        endpt1Mock.stubs().method("getJMSOperationInput").with(eq(jmsOp1)).will(returnValue(jmsInput1));
        endpt1Mock.stubs().method("getJMSOperationOutput").with(eq(jmsOp1)).will(returnValue(null));
        endpt1Mock.stubs().method("getJMSAddress").will(returnValue(jmsAdd));
        
        // A inbound receive channel
        endpt2Mock = mock (Endpoint.class);
        endpt2 = (Endpoint)endpt2Mock.proxy();
        endpt2Mock.stubs().method("getServiceUnitID").will(returnValue("A-SU-ID"));
        endpt2Mock.stubs().method("getEndpointType").will(returnValue(Endpoint.EndpointType.INBOUND));
        endpt2Mock.stubs().method("getServiceName").will(returnValue(new QName("service2")));
        endpt2Mock.stubs().method("getEndpointName").will((returnValue("endpt2")));
        endpt2Mock.stubs().method("getJMSOperations").will(returnValue(jmsOperations));
        endpt2Mock.stubs().method("getServiceQualities").will(returnValue(null));
        jmsOp2.setTransaction(JMSConstants.TRANSACTION_NONE);
        jmsInput2 = new JMSInput();
        jmsMsg2 = new JMSMessage();
        jmsInput2.setJMSMessage(jmsMsg2);
        endpt2Mock.stubs().method("getJMSOperationInput").with(eq(jmsOp2)).will(returnValue(jmsInput2));
        endpt2Mock.stubs().method("getJMSOperationOutput").with(eq(jmsOp2)).will(returnValue(null));
        endpt2Mock.stubs().method("getJMSAddress").will(returnValue(jmsAdd));
        
        // A outbound request/reply  channel
        endpt3Mock = mock (Endpoint.class);
        endpt3 = (Endpoint)endpt3Mock.proxy();
        endpt3Mock.stubs().method("getServiceUnitID").will(returnValue("A-SU-ID"));
        endpt3Mock.stubs().method("getEndpointType").will(returnValue(Endpoint.EndpointType.OUTBOUND));
        endpt3Mock.stubs().method("getServiceName").will(returnValue(new QName("service3")));
        endpt3Mock.stubs().method("getEndpointName").will((returnValue("endpt3")));
        endpt3Mock.stubs().method("getJMSOperations").will(returnValue(jmsOperations));
        endpt3Mock.stubs().method("getServiceQualities").will(returnValue(null));
        jmsOp3.setDestinationType(JMSConstants.QUEUE);
        jmsOp3.setTransaction(JMSConstants.TRANSACTION_NONE);
        jmsInput3 = new JMSInput();
        jmsMsg3 = new JMSMessage();
        jmsInput3.setJMSMessage(jmsMsg3);
        jmsOutput1 = new JMSOutput();
        jmsMsg4 = new JMSMessage();
        jmsOutput1.setJMSMessage(jmsMsg4);
        endpt3Mock.stubs().method("getJMSOperationInput").with(eq(jmsOp3)).will(returnValue(jmsInput3));
        endpt3Mock.stubs().method("getJMSOperationOutput").with(eq(jmsOp3)).will(returnValue(jmsOutput1));
        endpt3Mock.stubs().method("getJMSAddress").will(returnValue(jmsAdd));

        // A inbound request/reply  channel
        endpt4Mock = mock (Endpoint.class);
        endpt4 = (Endpoint)endpt4Mock.proxy();
        endpt4Mock.stubs().method("getServiceUnitID").will(returnValue("A-SU-ID"));
        endpt4Mock.stubs().method("getEndpointType").will(returnValue(Endpoint.EndpointType.INBOUND));
        endpt4Mock.stubs().method("getServiceName").will(returnValue(new QName("service4")));
        endpt4Mock.stubs().method("getEndpointName").will((returnValue("endpt4")));
        endpt4Mock.stubs().method("getJMSOperations").will(returnValue(jmsOperations));
        endpt4Mock.stubs().method("getServiceQualities").will(returnValue(null));
        jmsOp4.setDestinationType(JMSConstants.TOPIC);
        jmsOp4.setTransaction(JMSConstants.TRANSACTION_NONE);
        jmsInput4 = new JMSInput();
        jmsMsg5 = new JMSMessage();
        jmsInput4.setJMSMessage(jmsMsg5);
        jmsOutput2 = new JMSOutput();
        jmsMsg6 = new JMSMessage();
        jmsOutput2.setJMSMessage(jmsMsg6);
        endpt4Mock.stubs().method("getJMSOperationInput").with(eq(jmsOp4)).will(returnValue(jmsInput4));
        endpt4Mock.stubs().method("getJMSOperationOutput").with(eq(jmsOp4)).will(returnValue(jmsOutput2));
        endpt4Mock.stubs().method("getJMSAddress").will(returnValue(jmsAdd));        
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ChannelManagerTest.class);
        
        return suite;
    }

    /**
     * Test of lookup method, of class com.sun.jbi.jmsbc.jms.ChannelManager.
     */
    public void testAddChannelAndLookup1() {
        Channel ch = null;
        try {
            mgr.addChannel(endpt1, op1);
            mgr.addChannel(endpt2, op2);            
            ch = mgr.lookup(endpt1, op1);            
        } catch (Exception ex) {
            fail("The test case failed due to exception: " +
                 ex.getMessage());
        }
        
        assertTrue (ch instanceof SendChannel); 
    }

    /**
     * Test of lookup method, of class com.sun.jbi.jmsbc.jms.ChannelManager.
     */
    public void testAddChannelAndLookup2() {
        Channel ch = null;
        try {
            mgr.addChannel(endpt1, op1);
            mgr.addChannel(endpt2, op2);            
            ch = mgr.lookup(endpt2, op2);            
        } catch (Exception ex) {
            fail("The test case failed due to exception: " +
                 ex.getMessage());
        }
        
        assertTrue(ch instanceof ReceiveChannel);                
    }
    
    /**
     * Test of addChannel method, of class com.sun.jbi.jmsbc.jms.ChannelManager.
     */
    public void testAddChannelAndLookup3() {
        Channel ch = null;
        try {
            mgr.addChannel(endpt3, op3);
            ch = mgr.lookup(endpt3, op3);
        } catch (Exception ex) {
            fail("The test case failed due to exception: " +
                 ex.getMessage());
        }
        
        assertTrue(ch instanceof SendChannel);
    }

    /**
     * Test of removeChannel method, of class com.sun.jbi.jmsbc.jms.ChannelManager.
     */
    public void testAddChannelAndLookup4() {
        Channel ch = null;
        try {
            mgr.addChannel(endpt4, op4);
            ch = mgr.lookup(endpt4, op4);
        } catch (Exception ex) {
            fail("The test case failed due to exception: " +
                 ex.getMessage());
        }
        
        assertTrue(ch instanceof ReceiveChannel);
    }

    /**
     * Test of removeChannel method, of class com.sun.jbi.jmsbc.jms.ChannelManager.
     */
    public void testRemoveChannel() {
        Channel ch = null;
        try {
            mgr.addChannel(endpt4, op4);
            mgr.removeChannel(endpt4, op4);
            ch = mgr.lookup(endpt4, op4);
        } catch (Exception ex) {
            assertTrue (ch == null);
        }
    
        assertTrue(ch==null);
    }
    
}
