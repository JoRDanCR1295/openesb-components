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
 * @(#)InboundMessageProcessorTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc;

import junit.framework.*;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.execbc.Endpoint;
import com.sun.jbi.execbc.ExecNormalizer;
import com.sun.jbi.execbc.IBExecWorker;
import com.sun.jbi.execbc.InboundMessageProcessor;
import com.sun.jbi.execbc.extensions.ExecAddress;
import com.sun.jbi.execbc.extensions.ExecInput;
import com.sun.jbi.execbc.extensions.ExecMessage;
import com.sun.jbi.execbc.extensions.ExecOperation;
import com.sun.jbi.execbc.extensions.ExecOutput;

import java.util.HashMap;
import java.util.Map;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import org.jmock.*;

/**
 *
 * @author sweng
 * @author Jun Xu
 */
public class InboundMessageProcessorTest extends org.jmock.cglib.MockObjectTestCase {
    static final QName THE_OPERATION = new QName("myoperation");
    static final QName THE_SERVICE = new QName("myServiceName");
    static final String THE_ENDPOINT = "myEndpointName";
    InboundMessageProcessor instance = null;
    IBExecWorker worker = null;
    Map<QName, Service> services = null;
    
    Mock deliveryChannel = null;
    Mock componentContext = null;
    Mock endpoint = null;
    Mock endpointStatus = null;
    Mock serviceEndpoint = null;
    Mock msgExchange = null;
    Mock msgExchangeFactory = null;
    Mock normalizer = null;
    Mock normalizedMsg = null;
    Mock wsdlDefinition = null;
    Mock service = null;
    Mock port = null;
    Mock portType = null;
    Mock binding = null;
    
    Map operations = new HashMap();
    Map operationMeps = new HashMap();
    
    public InboundMessageProcessorTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        componentContext = mock(ComponentContext.class);
        deliveryChannel = mock(DeliveryChannel.class);
        endpoint = mock(Endpoint.class);
        endpointStatus = mock(EndpointStatus.class);
        serviceEndpoint = mock(ServiceEndpoint.class);
        msgExchangeFactory = mock(MessageExchangeFactory.class);
        msgExchange = mock(MessageExchange.class);
        normalizer = mock(ExecNormalizer.class);
        normalizedMsg = mock(NormalizedMessage.class);
        wsdlDefinition = mock(Definition.class);
        service = mock(Service.class);
        port = mock(Port.class);
        portType = mock(PortType.class);
        binding = mock(Binding.class);

        services = new HashMap<QName, Service>();
        services.put(THE_SERVICE, (Service) service.proxy());

        endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(atLeastOnce()).method("getDefinition").will(returnValue(wsdlDefinition.proxy()));
        wsdlDefinition.expects(atLeastOnce()).method("getServices").will(returnValue((Map<QName, Service>)services));
        service.expects(atLeastOnce()).method("getPort").will(returnValue((Port) port.proxy()));
        port.expects(atLeastOnce()).method("getBinding").will(returnValue((Binding) binding.proxy()));
        binding.expects(atLeastOnce()).method("getPortType").will(returnValue((PortType) portType.proxy()));
        portType.expects(atLeastOnce()).method("getQName").will(returnValue(QName.valueOf("portType1")));
        
        instance = new InboundMessageProcessor((ComponentContext)componentContext.proxy(),
                                               (DeliveryChannel)deliveryChannel.proxy(),
                                               (Endpoint)endpoint.proxy(),
                                               THE_OPERATION);
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(InboundMessageProcessorTest.class);
        
        return suite;
    }
    
    /**
     * Test of run method, of class com.sun.jbi.execbc.InboundMessageProcessor 
     * for the scenario where the message exchange pattern is not valid
     */
    public void testRunInvalidMEP() {
        System.out.println("Testing run() for the scenario where the message exchange pattern is not valid");
        
        ExecOperation execoperation = new ExecOperation();
        operations.put(THE_OPERATION, execoperation);
        
        ExecInput execInput = new ExecInput();
        execInput.setExecMessage(new ExecMessage());
        execoperation.setExecOperationInput(execInput);
        
        ExecAddress addr = new ExecAddress();
        addr.setHostName("localhost");
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(atLeastOnce()).method("getExecOperations").will(returnValue(operations));
        endpoint.expects(atLeastOnce()).method("getOperationMsgExchangePattern").will(returnValue(operationMeps));
        
        // 1. testing the case where message exchange pattern is not found (null)
        operationMeps.remove(THE_OPERATION);
        operationMeps.put("dummyOperation", "inonly");

        instance.stopReceiving();
        // for junit testing - avoid multiple inbound worker threads
        instance.run();
        
        // 2. testing the case where message exchange status is invalid (not inonly or inout)
        operationMeps.put(THE_OPERATION, "outin");
        instance.run();
    }    
    
    /**
     * Test of execute method, of class com.sun.jbi.execbc.InboundMessageProcessor
     * for the scenario where basic file extensibility element attribute validation fails.
     */
    public void testValidateInboundMessageProperties() {
        System.out.println("Testing basic runtime wsdl validation");
        
        String mep = "inout";
        ExecOperation operation = new ExecOperation();
        ExecInput execInput = new ExecInput();
        ExecOutput execOutput = new ExecOutput();
        ExecMessage execMessage = new ExecMessage();
        
        // testing an invalid mep
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to invalid message exchange pattern: " + mep);
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties with an invalid message exchange pattern: " + mep);
        }
        
        // testing another invalid mep
        mep = "unsupported";
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to invalid message exchange pattern: " + mep);
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties with an invalid message exchange pattern: " + mep);
        }
        
        mep = "inonly";
        // testing null input properties
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to null File Input properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties with null File Input properties.");
        }
        
        // testing null Exec Receive properties
        operation.setExecOperationInput(execInput);
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to null Exec Receive properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties with null Exec Receive properties.");
        }
        
        // testing missing required Exec Receive attributes
        operation.setExecOperationInput(execInput);
        execInput.setExecMessage(execMessage);
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to missing required Exec Receive attribute: command.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties: missing required Exec Receive attribute: command.");
        }
        
        operation.setCommand("vmstat");
        execMessage.setExecUseType(ExecMessage.EXEC_USE_TYPE_ENCODED);
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to missing required Exec Receive attribute: encodingStyle.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties: missing required Exec Receive attribute: encodingStyle.");
        }
        
        execMessage.setExecEncodingStyle("customencoder-1.0");
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
        } catch (Exception e) {
            fail("Failed to test validateInboundMessageProperties - no exception is expected.");
        }
        
        System.out.println("Successfully tested basic runtime wsdl validation");
    }
    
    /**
     * Test of execute method, of class com.sun.jbi.execbc.InboundMessageProcessor
     * for the scenario where the message exchange type is inonly.
     */
    public void testExecuteInOnlyExchangeCase1() {
        System.out.println("Testing in-only message exchange case 1...");
        
        ExecInput execInput = new ExecInput();
        ExecMessage execMessage = new ExecMessage();
        ExecAddress execAddress = new ExecAddress();
        msgExchange = mock(InOnly.class);

        execAddress.setHostName("localhost");
        
        deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory").will(returnValue(msgExchangeFactory.proxy()));
        msgExchangeFactory.expects(atLeastOnce()).method("createInOnlyExchange").will(returnValue((InOnly)msgExchange.proxy()));
        componentContext.expects(atLeastOnce()).method("getEndpoint").with(eq(THE_SERVICE), eq(THE_ENDPOINT)).will(returnValue((ServiceEndpoint)serviceEndpoint.proxy()));
        msgExchange.expects(atLeastOnce()).method("getExchangeId");
        normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(returnValue((NormalizedMessage)normalizedMsg.proxy()));
        msgExchange.expects(once()).method("setEndpoint").with(isA(ServiceEndpoint.class));
        msgExchange.expects(once()).method("setOperation").with(eq(THE_OPERATION));
        msgExchange.expects(once()).method("setMessage").with(eq(normalizedMsg.proxy()), eq("in"));
        
        deliveryChannel.expects(once()).method("send").with(eq(msgExchange.proxy()));
        endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(once()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpointStatus.expects(once()).method("incrementSentRequests");
        
        try {
            instance.addWorker(worker = new IBExecWorker(instance, "inonly", execMessage));
            worker.setNormalizer((ExecNormalizer)normalizer.proxy());
            instance.setStopped(true); // let the worker loop once and quit
            String command;
            String os = System.getProperty("os.name");
            if (os != null) {
                os = os.toLowerCase();
            }
            if (os != null && os.startsWith("windows")) {
                command = "hostname";
            } else if (os != null && os.startsWith("mac os")) {
                command = "vm_stat";
            } else {
                //Assume UNIX
                command = "vmstat";
            }
            execInput.setExecMessage(execMessage);
            instance.ensureExchangeFactoryAndEndPoint();
            instance.execute("inonly", command, execInput, execAddress);
            instance.startWorkers("inonly", execInput);
            Thread.sleep(1000);
        } catch (Exception e) {
            e.printStackTrace();
            fail("Failed to test InOnly exchanges due to: " + e.getMessage());
        }
        msgExchange.verify();
        msgExchangeFactory.verify();
        componentContext.verify();
        endpointStatus.verify();
        deliveryChannel.verify();

        System.out.println("Successfully tested in-only message exchange case 1");
    }
}
