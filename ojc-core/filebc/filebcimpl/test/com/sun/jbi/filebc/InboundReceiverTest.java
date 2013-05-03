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
package com.sun.jbi.filebc;

import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import junit.framework.*;
import com.sun.jbi.filebc.extensions.FileOperation;
import com.sun.jbi.filebc.util.EPUtil;

import java.io.File;
import java.io.FileOutputStream;
import java.util.Iterator;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
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
        endpoint.setServiceUnitID("Test-SU");
    }

    protected void setUp() throws Exception {
        componentContext = mock(ComponentContext.class);
        KeyStoreUtilClient keystoreUtil = new KeyStoreUtilClient((ComponentContext) componentContext.proxy());
        runtimeConfig = mock(RuntimeConfiguration.class,
                new Class[]{String.class, KeyStoreUtilClient.class},
                new Object[]{"test/com/sun/jbi/filebc/testDir", keystoreUtil});
        deliveryChannel = mock(DeliveryChannel.class);
        runtimeConfig.expects(once()).method("getThreads").will(returnValue(new Integer(10)));
        runtimeConfig.expects(once()).method("addNotificationListener").withAnyArguments();
        
        deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory");
        
        // getIBWorkerThreads is called while initializing InboundMessageProcessor
        runtimeConfig.expects(atLeastOnce()).method("getIBWorkerThreads").will(returnValue(5));

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
     * Test of addInboundMessageProcessor method, of class com.sun.jbi.filebc.InboundReceiver.
     */
    public void testAddInboundMessageProcessor() throws Exception {
        System.out.println("Testing addInboundMessageProcessor");
        QName serviceName = new QName("somenamespace", "someServiceName");
        String endpointName = "someEndpointName";

        // 1. testing for inbound endpoint
        endpoint.setEndpointType(0);
        endpoint.setServiceName(serviceName);
        endpoint.setEndpointName(endpointName);
        operations.put(new QName("somenamespace", "someOp1"), new FileOperation());
        operations.put(new QName("somenamespace", "someOp2"), new FileOperation());
        operations.put(new QName("somenamespace", "someOp3"), new FileOperation());
        operations.put(new QName("somenamespace", "someOp4"), new FileOperation());
        operations.put(new QName("somenamespace", "someOp5"), new FileOperation());
        operations.put(new QName("somenamespace", "someOp6"), new FileOperation());
        operations.put(new QName("somenamespace", "someOp7"), new FileOperation());
        operations.put(new QName("somenamespace", "someOp8"), new FileOperation());
        endpoint.setFileOperations(operations);
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
            registerLock(endpoint);
        } catch (Exception e) {
            e.printStackTrace();
            fail("Failed to register lock for the endpoint, due to: " + e.getMessage());
        }

        try {
            // getIBWorkerThreads is called while initializing InboundMessageProcessor
            runtimeConfig.expects(once()).method("getIBWorkerThreads").will(returnValue(new Integer(5)));
            
            instance.addInboundMessageProcessor(endpoint);
            runtimeConfig.verify();
        } catch (Exception e) {
            e.printStackTrace();
            fail("Failed to test addInboundMessageProcessor due to: " + e.getMessage());
        }

        Map procs = instance.getActivatedInboundMsgProcs();
        assertEquals(8, procs.size());
        for (Iterator it = procs.keySet().iterator(); it.hasNext();) {
            String key = (String) it.next();
            assertTrue(key.startsWith(endpoint.getUniqueName()));
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
     * Test of removeInboundMessageProcessor method, of class com.sun.jbi.filebc.InboundReceiver.
     */
    public void atestRemoveInboundMessageProcessor() {
        System.out.println("Testing removeInboundMessageProcessor");
        Map dummyProcs = new HashMap();
        QName serviceName = new QName("somenamespace", "someServiceName");
        String endpointName = "someEndpointName";
        int numIBWorkers = 5;

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
            registerLock(endpoint);
        } catch (Exception e) {
            e.printStackTrace();
            fail("Failed to register lock for the endpoint, due to: " + e.getMessage());
        }

        try {
            String endpointUniqueName = endpoint.getUniqueName();
            dummyProcs.put(endpointUniqueName + ":" + "{somenamespace}someOp1", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                    (DeliveryChannel) deliveryChannel.proxy(),
                    endpoint,
                    new QName("somenamespace", "someOp1"),numIBWorkers));
            dummyProcs.put(endpointUniqueName + ":" + "{somenamespace}someOp2", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                    (DeliveryChannel) deliveryChannel.proxy(),
                    endpoint,
                    new QName("somenamespace", "someOp2"),numIBWorkers));
            dummyProcs.put(endpointUniqueName + ":" + "{somenamespace}someOp3", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                    (DeliveryChannel) deliveryChannel.proxy(),
                    endpoint,
                    new QName("somenamespace", "someOp3"),numIBWorkers));
            dummyProcs.put(endpointUniqueName + ":" + "{somenamespace}someOp4", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                    (DeliveryChannel) deliveryChannel.proxy(),
                    endpoint,
                    new QName("somenamespace", "someOp4"),numIBWorkers));
            dummyProcs.put(endpointUniqueName + ":" + "{somenamespace}someOp5", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                    (DeliveryChannel) deliveryChannel.proxy(),
                    endpoint,
                    new QName("somenamespace", "someOp5"),numIBWorkers));
            dummyProcs.put(endpointUniqueName + ":" + "{somenamespace}someOp6", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                    (DeliveryChannel) deliveryChannel.proxy(),
                    endpoint,
                    new QName("somenamespace", "someOp6"),numIBWorkers));
            dummyProcs.put(endpointUniqueName + ":" + "{somenamespace}someOp7", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                    (DeliveryChannel) deliveryChannel.proxy(),
                    endpoint,
                    new QName("somenamespace", "someOp7"),numIBWorkers));
            dummyProcs.put(endpointUniqueName + ":" + "{somenamespace}someOp8", new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                    (DeliveryChannel) deliveryChannel.proxy(),
                    endpoint,
                    new QName("somenamespace", "someOp8"),numIBWorkers));
        } catch (Exception e) {
            e.printStackTrace();
            fail("Failed to set up successfully to test removeInboundMessageProcessor");
        }

        // 1. testing removing some of the activated processors
        endpoint.setEndpointType(0);
        endpoint.setEndpointName("someEndpointName");
        operations.clear();
        operations.put(new QName("somenamespace", "someOp4"), new FileOperation());
        operations.put(new QName("somenamespace", "someOp5"), new FileOperation());

        endpoint.setFileOperations(operations);
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
        operations.put(new QName("somenamespace", "someOp1"), new FileOperation());
        operations.put(new QName("somenamespace", "someOp2"), new FileOperation());
        operations.put(new QName("somenamespace", "someOp3"), new FileOperation());
        operations.put(new QName("somenamespace", "someOp4"), new FileOperation());
        operations.put(new QName("somenamespace", "someOp5"), new FileOperation());
        operations.put(new QName("somenamespace", "someOp6"), new FileOperation());
        operations.put(new QName("somenamespace", "someOp7"), new FileOperation());
        operations.put(new QName("somenamespace", "someOp8"), new FileOperation());
        endpoint.setFileOperations(operations);

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

    private void registerLock(Endpoint endpoint) throws Exception {
        //String key = endpoint.getServiceName().toString() + endpoint.getEndpointName();
        String key = endpoint.getEPUUID();
        String baseWorkDir = "test/com/sun/jbi/filebc/input/" + EPUtil.getWorkAreaBaseDir(endpoint);
        File fWorkAreaBaseDir = new File(baseWorkDir);
        fWorkAreaBaseDir.mkdirs();
        String lockFilePath = baseWorkDir + File.separator + "filebc.lck";

        File lockFile = new File(lockFilePath);
        lockFile.createNewFile();
        FileOutputStream fos = new FileOutputStream(lockFile);
        LockRegistry.register(key, new Lock((fos != null ? fos.getChannel() : null), new ReentrantLock(), lockFilePath));

    }
}
