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

package com.sun.jbi.execbc;

import junit.framework.*;

import com.sun.jbi.execbc.Endpoint;
import com.sun.jbi.execbc.EndpointImpl;
import com.sun.jbi.execbc.InboundMessageProcessor;
import com.sun.jbi.execbc.InboundReceiver;
import com.sun.jbi.execbc.RuntimeConfiguration;
import com.sun.jbi.execbc.extensions.ExecOperation;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.NotificationListener;
import javax.management.Notification;
import javax.management.AttributeChangeNotification;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;
import org.jmock.*;

/**
 *
 * @author sweng
 */
public class InboundReceiverTest extends org.jmock.cglib.MockObjectTestCase {
    InboundReceiver instance = null;
    Mock runtimeConfig = null;
    Mock deliveryChannel = null;
    Mock componentContext = null;
    
    Map operations = new HashMap();
    Endpoint endpoint = new EndpointImpl();
    
    public InboundReceiverTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        runtimeConfig = mock(RuntimeConfiguration.class,
                             new Class[] {String.class, String.class, String.class},
                             new Object[] {"test/com/sun/jbi/execbc/testDir", "", ""});
        componentContext = mock(ComponentContext.class);
        deliveryChannel = mock(DeliveryChannel.class);
        runtimeConfig.expects(once()).method("getMaximumSessions").will(returnValue(new Integer(10)));
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
     * Test of addInboundMessageProcessor method, of class com.sun.jbi.execbc.InboundReceiver.
     */
    public void testAddInboundMessageProcessor() throws Exception {
        System.out.println("Testing addInboundMessageProcessor");
        QName serviceName = new QName("somenamespace", "someServiceName");
        String endpointName = "someEndpointName";
        
        // 1. testing for inbound endpoint
        endpoint.setEndpointType(0);
        endpoint.setServiceName(serviceName);
        endpoint.setEndpointName(endpointName);
        operations.put(new QName("somenamespace", "someOp1"), new ExecOperation());
        operations.put(new QName("somenamespace", "someOp2"), new ExecOperation());
        operations.put(new QName("somenamespace", "someOp3"), new ExecOperation());
        operations.put(new QName("somenamespace", "someOp4"), new ExecOperation());
        operations.put(new QName("somenamespace", "someOp5"), new ExecOperation());
        operations.put(new QName("somenamespace", "someOp6"), new ExecOperation());
        operations.put(new QName("somenamespace", "someOp7"), new ExecOperation());
        operations.put(new QName("somenamespace", "someOp8"), new ExecOperation());
        endpoint.setExecOperations(operations);
        {
            com.ibm.wsdl.DefinitionImpl definition = new com.ibm.wsdl.DefinitionImpl();

            javax.wsdl.PortType portType = definition.createPortType();
            portType.setQName(new QName("somenamespace", "somePortTypeName"));

            javax.wsdl.Binding binding = definition.createBinding();
            binding.setPortType(portType);

            javax.wsdl.Port port = definition.createPort();
            port.setBinding(binding);
            port.setName(endpointName);

            javax.wsdl.Service service = definition.createService();
            service.addPort(port);
            service.setQName(serviceName);

            definition.addService(service);
            endpoint.setDefinition(definition);
        }
        
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
        endpoint.setEndpointType(1);
        try {
            instance.addInboundMessageProcessor(endpoint);
        } catch (Exception e) {
            fail("Failed to test addInboundMessageProcessor due to: " + e.getMessage());
        }
        
        procs = instance.getActivatedInboundMsgProcs();
        assertEquals(8, procs.size());
    }

    /**
     * Test of removeInboundMessageProcessor method, of class com.sun.jbi.execbc.InboundReceiver.
     */
    public void testRemoveInboundMessageProcessor() {
        System.out.println("Testing removeInboundMessageProcessor");
        Map dummyProcs = new HashMap();
        QName serviceName = new QName("somenamespace", "someServiceName");
        String endpointName = "someEndpointName";
        
        endpoint.setEndpointName(endpointName);
        endpoint.setServiceName(new QName("somenamespace", "someServiceName"));
        {
            com.ibm.wsdl.DefinitionImpl definition = new com.ibm.wsdl.DefinitionImpl();

            javax.wsdl.PortType portType = definition.createPortType();
            portType.setQName(new QName("somenamespace", "somePortTypeName"));

            javax.wsdl.Binding binding = definition.createBinding();
            binding.setPortType(portType);

            javax.wsdl.Port port = definition.createPort();
            port.setBinding(binding);
            port.setName(endpointName);

            javax.wsdl.Service service = definition.createService();
            service.addPort(port);
            service.setQName(serviceName);

            definition.addService(service);
            endpoint.setDefinition(definition);
        }
        
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
           e.printStackTrace();
           fail("Failed to set up successfully to test removeInboundMessageProcessor");
       }
        
        // 1. testing removing some of the activated processors
        endpoint.setEndpointType(0);
        endpoint.setEndpointName("someEndpointName");
        operations.clear();
        operations.put(new QName("somenamespace", "someOp4"), new ExecOperation());
        operations.put(new QName("somenamespace", "someOp5"), new ExecOperation());
        
        endpoint.setExecOperations(operations);
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
        operations.put(new QName("somenamespace", "someOp1"), new ExecOperation());
        operations.put(new QName("somenamespace", "someOp2"), new ExecOperation());
        operations.put(new QName("somenamespace", "someOp3"), new ExecOperation());
        operations.put(new QName("somenamespace", "someOp4"), new ExecOperation());
        operations.put(new QName("somenamespace", "someOp5"), new ExecOperation());
        operations.put(new QName("somenamespace", "someOp6"), new ExecOperation());
        operations.put(new QName("somenamespace", "someOp7"), new ExecOperation());
        operations.put(new QName("somenamespace", "someOp8"), new ExecOperation());
        endpoint.setExecOperations(operations);
        
        endpoint.setEndpointType(1);
        procs = instance.getActivatedInboundMsgProcs();
        assertEquals(6, procs.size());
        
        // 3. testing removing all activated processors
        endpoint.setEndpointType(0);
        try {
            instance.removeInboundMessageProcessor(endpoint);
        } catch (Exception e) {
            fail("Failed to test removeInboundMessageProcessor due to: " + e.getMessage());
        }
        instance.removeInboundMessageProcessor(endpoint);
        procs = instance.getActivatedInboundMsgProcs();
        assertEquals(0, procs.size());
    }
}
