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
 * @(#)InboundReceiverTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import com.sun.jbi.sapbc.Endpoint.EndpointType;
import junit.framework.*;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.extensions.impl.ExtensibilityElementImpl;
import java.util.HashMap;
import java.util.Map;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.xml.namespace.QName;
import org.jmock.*;

/**
/**
 *  TODO: Develop tests when InboundReceiverTest is up and running with
 *  the sapbc
 * @author sweng
 */
public class InboundReceiverTest extends org.jmock.cglib.MockObjectTestCase {
    InboundReceiver instance = null;
    ExtensibilityElement e = new ExtensibilityElementImpl();
    Mock runtimeConfig = null;
    Mock deliveryChannel = null;
    Mock componentContext = null;
    
    Map operations = new HashMap();
    Endpoint endpoint = null;
    
    public InboundReceiverTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        String name = new String();
        QName serviceName = new QName("serviceName");
        WSDLDefinitions wsdlDef = null;
        EndpointType direction = null;
        
        endpoint = new EndpointImpl(name, serviceName, wsdlDef, direction);
        endpoint.setServiceName(new QName("SAPServiceName"));
        endpoint.setEndpointName("SAPEndpointName");
        
        runtimeConfig = mock(RuntimeConfiguration.class,
                             new Class[] {String.class, String.class, String.class},
                             new Object[] {"test/com/sun/jbi/sapbc/testDir", "", ""});
        componentContext = mock(ComponentContext.class);
        deliveryChannel = mock(DeliveryChannel.class);
        runtimeConfig.expects(once()).method("getThreads").will(returnValue(new Integer(10)));
        runtimeConfig.expects(once()).method("addNotificationListener").withAnyArguments();
        instance = new InboundReceiver((ComponentContext) componentContext.proxy(),
                                        (DeliveryChannel) deliveryChannel.proxy(),
                                       (RuntimeConfiguration) runtimeConfig.proxy());
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(InboundReceiverTest.class);
        
        return suite;
    }

    /**
     * Test of addInboundMessageProcessor method, of class com.sun.jbi.sapbc.InboundReceiver.
     */
    public void testAddInboundMessageProcessor() throws Exception {
        /*
        System.out.println("Testing addInboundMessageProcessor");
        
        // 1. testing for inbound endpoint
        endpoint.setEndpointType(Endpoint.EndpointType.INBOUND);
        endpoint.setServiceName(new QName("someServiceName"));
        endpoint.setEndpointName("someEndpointName");
        try {
            operations.put(new QName("somenamespace", "someOp1"), new SAPFmOperation(e));
            operations.put(new QName("somenamespace", "someOp2"), new SAPFmOperation(e));
            operations.put(new QName("somenamespace", "someOp3"), new SAPFmOperation(e));
            operations.put(new QName("somenamespace", "someOp4"), new SAPFmOperation(e));
            operations.put(new QName("somenamespace", "someOp5"), new SAPFmOperation(e));
            operations.put(new QName("somenamespace", "someOp6"), new SAPFmOperation(e));
            operations.put(new QName("somenamespace", "someOp7"), new SAPFmOperation(e));
            operations.put(new QName("somenamespace", "someOp8"), new SAPFmOperation(e));
        } catch (Exception ex) {
            ex.printStackTrace();
            String errMsg = "testAddInboundMessageProcessor: Exception Thrown ["+ex.getClass().getName()+"] Message ["+ex.getMessage()+"]";
            fail(errMsg);
        }
        endpoint.setSAPOperations(operations);
        try {
            instance.addInboundMessageProcessor(endpoint);
        } catch (Exception e) {
            fail("Failed to test addInboundMessageProcessor due to: " + e.getMessage());
        }
        
        Map procs = instance.getActivatedInboundMsgProcs();
        assertEquals(8, procs.size());
        for (Iterator it = procs.keySet().iterator(); it.hasNext(); ) {
            String key = (String)it.next();
            assertTrue(key.startsWith(endpoint.getServiceName() + endpoint.getEndpointName()));
        }
        
        // 2. testing for outbound endpoint
        endpoint.setEndpointType(Endpoint.EndpointType.OUTBOUND);
        try {
            instance.addInboundMessageProcessor(endpoint);
        } catch (Exception e) {
            fail("Failed to test addInboundMessageProcessor due to: " + e.getMessage());
        }
        
        procs = instance.getActivatedInboundMsgProcs();
        assertEquals(8, procs.size());
         **/
    }

    /**
     * Test of removeInboundMessageProcessor method, of class com.sun.jbi.sapbc.InboundReceiver.
     */
    /*
    public void testRemoveInboundMessageProcessor() {
        System.out.println("Testing removeInboundMessageProcessor");
        Map dummyProcs = new HashMap();
        QName serviceName = new QName("someServiceName");
        String endpointName = "someEndpointName";
        try {
            dummyProcs.put(serviceName + endpointName + "{somenamespace}someOp1", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                                                                                                             (DeliveryChannel) deliveryChannel.proxy(),
                                                                                                             endpoint,
                                                                                                             new QName("somenamespace", "someOp1")));
            dummyProcs.put(serviceName + endpointName + "{somenamespace}someOp2", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                                                                                                             (DeliveryChannel) deliveryChannel.proxy(),
                                                                                                             endpoint,
                                                                                                             new QName("somenamespace", "someOp2")));
            dummyProcs.put(serviceName + endpointName + "{somenamespace}someOp3", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                                                                                                             (DeliveryChannel) deliveryChannel.proxy(),
                                                                                                             endpoint,
                                                                                                             new QName("somenamespace", "someOp3")));
            dummyProcs.put(serviceName + endpointName + "{somenamespace}someOp4", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                                                                                                             (DeliveryChannel) deliveryChannel.proxy(),
                                                                                                             endpoint,
                                                                                                             new QName("somenamespace", "someOp4")));
            dummyProcs.put(serviceName + endpointName + "{somenamespace}someOp5", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                                                                                                             (DeliveryChannel) deliveryChannel.proxy(),
                                                                                                             endpoint,
                                                                                                             new QName("somenamespace", "someOp5")));
            dummyProcs.put(serviceName + endpointName + "{somenamespace}someOp6", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                                                                                                             (DeliveryChannel) deliveryChannel.proxy(),
                                                                                                             endpoint,
                                                                                                             new QName("somenamespace", "someOp6")));
            dummyProcs.put(serviceName + endpointName + "{somenamespace}someOp7", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                                                                                                             (DeliveryChannel) deliveryChannel.proxy(),
                                                                                                             endpoint,
                                                                                                             new QName("somenamespace", "someOp7")));
            dummyProcs.put(serviceName + endpointName + "{somenamespace}someOp8", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                                                                                                             (DeliveryChannel) deliveryChannel.proxy(),
                                                                                                             endpoint,
                                                                                                             new QName("somenamespace", "someOp8")));
       } catch (Exception e) {
           fail("Failed to set up successfully to test removeInboundMessageProcessor");
       }
        
        // 1. testing removing some of the activated processors
        endpoint.setEndpointType(Endpoint.EndpointType.INBOUND);
        endpoint.setServiceName(new QName("someServiceName"));
        endpoint.setEndpointName("someEndpointName");
        operations.clear();
        try {
            operations.put(new QName("somenamespace", "someOp4"), new SAPFmOperation(e));
            operations.put(new QName("somenamespace", "someOp5"), new SAPFmOperation(e));
        } catch (Exception ex) {
            ex.printStackTrace();
            String errMsg = "testRemoveInboundMessageProcessor: Exception Thrown ["+ex.getClass().getName()+"] Message ["+ex.getMessage()+"]";
            fail(errMsg);
        }
        
        endpoint.setSAPOperations(operations);
        instance.setActivatedInboundMsgProcs(dummyProcs);
        try {
            instance.removeInboundMessageProcessor(endpoint);
        } catch (Exception e) {
            fail("Failed to test removeInboundMessageProcessor due to: " + e.getMessage());
        }
        instance.removeInboundMessageProcessor(endpoint);
        Map procs = instance.getActivatedInboundMsgProcs();
        assertEquals(6, procs.size());
        
        // 2. testing for outbound endpoint
        try {
            operations.put(new QName("somenamespace", "someOp1"), new SAPFmOperation(e));
            operations.put(new QName("somenamespace", "someOp2"), new SAPFmOperation(e));
            operations.put(new QName("somenamespace", "someOp3"), new SAPFmOperation(e));
            operations.put(new QName("somenamespace", "someOp4"), new SAPFmOperation(e));
            operations.put(new QName("somenamespace", "someOp5"), new SAPFmOperation(e));
            operations.put(new QName("somenamespace", "someOp6"), new SAPFmOperation(e));
            operations.put(new QName("somenamespace", "someOp7"), new SAPFmOperation(e));
            operations.put(new QName("somenamespace", "someOp8"), new SAPFmOperation(e));
        } catch (Exception ex) {
            ex.printStackTrace();
            String errMsg = "testRemoveInboundMessageProcessor: Exception Thrown ["+ex.getClass().getName()+"] Message ["+ex.getMessage()+"]";
            fail(errMsg);
        }
        endpoint.setSAPOperations(operations);
        
        endpoint.setEndpointType(Endpoint.EndpointType.INBOUND);
        procs = instance.getActivatedInboundMsgProcs();
        assertEquals(6, procs.size());
        
        // 3. testing removing all activated processors
        endpoint.setEndpointType(Endpoint.EndpointType.OUTBOUND);
        try {
            instance.removeInboundMessageProcessor(endpoint);
        } catch (Exception e) {
            fail("Failed to test removeInboundMessageProcessor due to: " + e.getMessage());
        }
        instance.removeInboundMessageProcessor(endpoint);
        procs = instance.getActivatedInboundMsgProcs();
        assertEquals(0, procs.size());
    }
     */
}
