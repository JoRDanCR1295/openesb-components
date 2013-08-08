#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(${symbol_pound})OutboundMessageProcessorTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc;

import junit.framework.*;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.filebc.extensions.FileAddress;
import com.sun.jbi.filebc.extensions.FileInput;
import com.sun.jbi.filebc.extensions.FileOperation;
import com.sun.jbi.filebc.extensions.FileOutput;
import com.sun.jbi.filebc.extensions.FileMessage;
import com.sun.jbi.common.qos.messaging.MessagingChannel;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;
import javax.jbi.messaging.*;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.jmock.*;

/**
 *
 * @author sweng
 * @author jfu
 */
public class OutboundMessageProcessorTest extends org.jmock.cglib.MockObjectTestCase {

    static final QName THE_PORTTYPE = new QName("http://myfile-test", "portType1");
    static final QName THE_OPERATION = new QName("http://myfile-test", "operation1");
    static final QName THE_SERVICE = new QName("http://myfile-test", "MyFileService");
    static final String THE_ENDPOINT = "MyFilePort";
    static final int THE_TYPE = 0;
    byte[] data = null;
    private static final String REDELIVERY_LAST_RETRY_TIME = "com.sun.jbi.common.qos.redelivery.lastRetryTime";
    private static final String REDELIVERY_TOTAL_RETRIES = "com.sun.jbi.common.qos.redelivery.totalRetries";
    private static final String REDELIVERY_REMAINING_RETRIES = "com.sun.jbi.common.qos.redelivery.remainingRetries";
    private static final String REDELIVERY_ERROR_DETAILS = "com.sun.jbi.common.qos.redelivery.errorDetails";
    private static final String REDELIVERY_FAILURE = "com.sun.jbi.common.qos.redelivery.failure";
    OutboundMessageProcessor instance = null;
    Endpoint endpoint = null;
    QName operation = null;
    Map<QName, Service> services = null;
    Mock deliveryChannel = null;
    Mock endpointProxy = null;
    Mock endpointStatus = null;
    Mock serviceUnit = null;
    Mock service = null;
    Mock port = null;
    Mock portType = null;
    Mock binding = null;
    Mock wsdlDefinition = null;
    List endpoints = new ArrayList();
    Map serviceUnits = new HashMap();
    Map operations = new HashMap();
    Document doc = null;

    public OutboundMessageProcessorTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        data = "Hello World!!!".getBytes();
        wsdlDefinition = mock(Definition.class);
        operation = THE_OPERATION;
        //deliveryChannel = mock(DeliveryChannel.class);
        deliveryChannel = mock(MessagingChannel.class);
        endpointProxy = mock(Endpoint.class);
        endpointStatus = mock(EndpointStatus.class);
        endpoint = (Endpoint) endpointProxy.proxy();
        service = mock(Service.class);
        port = mock(Port.class);
        portType = mock(PortType.class);
        binding = mock(Binding.class);
        serviceUnit = mock(ServiceUnit.class);
        endpoints.add(endpointProxy.proxy());
        serviceUnits.put("1", serviceUnit.proxy());

        services = new HashMap<QName, Service>();
        services.put(THE_SERVICE, (Service) service.proxy());
        File lockFile = new File("test/com/sun/jbi/filebc/input/filebc.lck");
        RandomAccessFile raf = new RandomAccessFile(lockFile, "rw");
        LockRegistry.register("fakedUUID", new Lock(raf.getChannel(), new ReentrantLock(), lockFile.getCanonicalPath()));

        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            synchronized (dbf) {
                dbf.setNamespaceAware(true);
                doc = dbf.newDocumentBuilder().newDocument();
            }
        } catch (Exception e) {
            System.out.println("Cannot create a document");
        }

        if (doc != null) {
            org.w3c.dom.Element root = doc.createElement("Simple-file-write-test");
            doc.appendChild(root);
        }

        instance = new OutboundMessageProcessor((MessagingChannel) deliveryChannel.proxy(),
                serviceUnits,
                new HashMap());
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(OutboundMessageProcessorTest.class);

        return suite;
    }

    /**
     * Test of processRequestReplyOutbound method, of class com.sun.jbi.filebc.OutboundMessageProcessor.
     */
    public void testProcessRequestReplyOutbound() {
        System.out.println("Testing processRequestReplyOutbound");

        Mock inout = mock(InOut.class);
        Mock inMessage = mock(NormalizedMessage.class);
        Mock outMessage = mock(NormalizedMessage.class);
        Mock denormalizer = mock(FileDenormalizer.class);

        FileOperation fileoperation = new FileOperation();
        FileInput fileInput = new FileInput();
        FileMessage fileMessage = new FileMessage();
        FileAddress addr = new FileAddress();

        fileInput.setFileMessage(fileMessage);
        fileoperation.setFileOperationInput(fileInput);
        fileMessage.setFileName("TestRequestResponseOutbound.txt");
        operations.put(THE_OPERATION, fileoperation);
        addr.setFileDirectory("test/com/sun/jbi/filebc/output");

        endpointProxy.expects(atLeastOnce()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
//        endpointProxy.expects(atLeastOnce()).method("getFileOperations").will(returnValue(operations));
//        endpointProxy.expects(atLeastOnce()).method("getFileAddress").will(returnValue(addr));
//        endpointProxy.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
//        endpointProxy.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
//        endpointProxy.expects(atLeastOnce()).method("getEndpointType").will(returnValue(THE_TYPE));
//        deliveryChannel.expects(atLeastOnce()).method("send").with(isA(InOut.class));
//        endpointProxy.expects(atLeastOnce()).method("getDefinition").will(returnValue(wsdlDefinition.proxy()));
//        wsdlDefinition.expects(atLeastOnce()).method("getServices").will(returnValue((Map<QName, Service>)services));
//        service.expects(atLeastOnce()).method("getPort").will(returnValue((Port) port.proxy()));
//        port.expects(atLeastOnce()).method("getBinding").will(returnValue((Binding) binding.proxy()));
//        binding.expects(atLeastOnce()).method("getPortType").will(returnValue((PortType) portType.proxy()));
//        portType.expects(atLeastOnce()).method("getQName").will(returnValue(THE_PORTTYPE));

        // 1. testing the case when exchange status is done
        inout.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.DONE));
        endpointStatus.expects(atLeastOnce()).method("incrementReceivedDones");
        instance.processRequestReplyOutbound((InOut) inout.proxy(), endpoint, operation);
        endpointStatus.verify();

        // 2. testing the case when exchange status is error
        inout.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.ERROR));
        endpointStatus.expects(once()).method("incrementReceivedErrors");
        instance.processRequestReplyOutbound((InOut) inout.proxy(), endpoint, operation);
        endpointStatus.verify();

        // Outbound Req Reply is used for Sync (Solicited) Read now.
//        // 3. testing the case when status is active and file write is successful
//        endpointStatus.expects(atLeastOnce()).method("incrementReceivedRequests");
//        endpointStatus.expects(atLeastOnce()).method("incrementSentReplies");
//        denormalizer.expects(atLeastOnce()).method("denormalize").withAnyArguments().will(returnValue(data));
//        instance.setFileDenormalizer((FileDenormalizer)denormalizer.proxy());
//        inout.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.ACTIVE));
//        inout.expects(atLeastOnce()).method("getInMessage").will(returnValue(inMessage.proxy()));
//        inout.expects(atLeastOnce()).method("createMessage").will(returnValue(outMessage.proxy()));
//        inout.expects(atLeastOnce()).method("setOutMessage").withAnyArguments();
//        outMessage.expects(atLeastOnce()).method("setContent").withAnyArguments();
//        outMessage.expects(atLeastOnce()).method("setProperty").withAnyArguments();
//        instance.processRequestReplyOutbound((InOut)inout.proxy(), endpoint, operation);
//        inMessage.verify();
//        outMessage.verify();
//        
//        // 4. testing the case when status is active but file write fails
//        denormalizer.expects(once()).method("denormalize").will(throwException(new Exception("Some error occurred.")));
//        inout.expects(once()).method("setError").with(isA(Exception.class));
//        instance.processRequestReplyOutbound((InOut)inout.proxy(), endpoint, operation);
//        inMessage.verify();
//        outMessage.verify();
    }

    /**
     * Test of processRequestReplyInbound method, of class com.sun.jbi.filebc.OutboundMessageProcessor.
     */
    public void testProcessRequestReplyInbound() {
        System.out.println("Testing processRequestReplyInbound");

        Mock inout = mock(InOut.class);
        Mock inMessage = mock(NormalizedMessage.class);
        Mock denormalizer = mock(FileDenormalizer.class);
        Mock listener = mock(MessageExchangeReplyListener.class);

        FileOperation fileoperation = new FileOperation();
        FileOutput fileOutput = new FileOutput();
        FileMessage fileMessage = new FileMessage();
        FileAddress addr = new FileAddress();

        fileOutput.setFileMessage(fileMessage);
        fileMessage.setFileName("TestRequestResponseInbound.txt");
        fileoperation.setFileOperationOutput(fileOutput);
        operations.put(THE_OPERATION, fileoperation);
        addr.setFileDirectory("test/com/sun/jbi/filebc/output");

        String epUUID = "fakedUUID";
        endpointProxy.expects(atLeastOnce()).method("getEPUUID").will(returnValue(epUUID));
        endpointProxy.expects(atLeastOnce()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpointProxy.expects(atLeastOnce()).method("getFileOperations").will(returnValue(operations));
        endpointProxy.expects(atLeastOnce()).method("getFileAddress").will(returnValue(addr));
        endpointProxy.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpointProxy.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpointProxy.expects(atLeastOnce()).method("getEndpointType").will(returnValue(THE_TYPE));
        endpointProxy.expects(atLeastOnce()).method("getWorkAreaDir");
        deliveryChannel.expects(atLeastOnce()).method("send").with(isA(InOut.class));
        endpointProxy.expects(atLeastOnce()).method("getDefinition").will(returnValue(wsdlDefinition.proxy()));
        wsdlDefinition.expects(atLeastOnce()).method("getServices").will(returnValue((Map<QName, Service>) services));
        service.expects(atLeastOnce()).method("getPort").will(returnValue((Port) port.proxy()));
        port.expects(atLeastOnce()).method("getBinding").will(returnValue((Binding) binding.proxy()));
        binding.expects(atLeastOnce()).method("getPortType").will(returnValue((PortType) portType.proxy()));
        portType.expects(atLeastOnce()).method("getQName").will(returnValue(THE_PORTTYPE));

        // QOS: Re-delivery
        String groupid = java.util.UUID.randomUUID().toString();
        String messageid = "1";
        inout.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_GROUP_ID)).will(returnValue(groupid));
        inout.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_MESSAGE_ID)).will(returnValue(messageid));
        String lastRetryTime = "50000";
        String totalRetries = "3";
        String remainingRetries = "1";
        Exception errorDetails = new Exception();
        String redeliveryFailure = "false";
        inout.expects(atLeastOnce()).method("getProperty").with(eq(REDELIVERY_LAST_RETRY_TIME)).will(returnValue(lastRetryTime));
        inout.expects(atLeastOnce()).method("getProperty").with(eq(REDELIVERY_TOTAL_RETRIES)).will(returnValue(totalRetries));
        inout.expects(atLeastOnce()).method("getProperty").with(eq(REDELIVERY_REMAINING_RETRIES)).will(returnValue(remainingRetries));
        //inout.expects(once()).method("getProperty").with(eq(REDELIVERY_ERROR_DETAILS)).will(returnValue(errorDetails));
        inout.expects(atLeastOnce()).method("getProperty").with(eq(REDELIVERY_FAILURE)).will(returnValue(redeliveryFailure));
        inout.expects(atLeastOnce()).method("getExchangeId");

        listener.expects(atLeastOnce()).method("processReplyMessage");

        // 1. testing the case when exchange status is done
        inout.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.DONE));
		inout.expects(atLeastOnce()).method("getError");
        endpointStatus.expects(once()).method("incrementReceivedDones");
        instance.processRequestReplyInbound((InOut) inout.proxy(), endpoint, operation, (MessageExchangeReplyListener) listener.proxy());
        endpointStatus.verify();

        // 2. testing the case when exchange status is error
        Mock serviceEndpoint = mock(ServiceEndpoint.class);
        inout.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.ERROR));
        inout.expects(atLeastOnce()).method("getEndpoint").will(returnValue(serviceEndpoint.proxy()));
        inout.expects(atLeastOnce()).method("getOperation");
        serviceEndpoint.expects(atLeastOnce()).method("getServiceName");
        serviceEndpoint.expects(atLeastOnce()).method("getEndpointName");
        endpointStatus.expects(once()).method("incrementReceivedErrors");
        inout.expects(atLeastOnce()).method("getError");
        endpointProxy.expects(atLeastOnce()).method("getServiceUnitID");
        endpointProxy.expects(atLeastOnce()).method("getRedeliveryConfiguration");
        instance.processRequestReplyInbound((InOut) inout.proxy(), endpoint, operation, (MessageExchangeReplyListener) listener.proxy());
        endpointStatus.verify();

        // 3. testing the case when status is active and file write is successful
        endpointStatus.expects(atLeastOnce()).method("incrementReceivedReplies");
        endpointStatus.expects(atLeastOnce()).method("incrementSentDones");
        //denormalizer.expects(atLeastOnce()).method("denormalize").withAnyArguments().will(returnValue(data));
        denormalizer.expects(atLeastOnce()).method("denormalize").withAnyArguments().will(returnValue(new ByteArrayInputStream(data)));
        instance.setFileDenormalizer((FileDenormalizer) denormalizer.proxy());
        inout.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.ACTIVE));
        inout.expects(atLeastOnce()).method("getOutMessage").will(returnValue(inMessage.proxy()));
        inMessage.expects(atLeastOnce()).method("getProperty");
        inMessage.expects(atLeastOnce()).method("setProperty").withAnyArguments();
        inout.expects(atLeastOnce()).method("setStatus").with(eq(ExchangeStatus.DONE));
        listener.expects(once()).method("processReplyMessage").with(isA(InOut.class));
        inout.expects(atLeastOnce()).method("getExchangeId");
        instance.processRequestReplyInbound((InOut) inout.proxy(), endpoint, operation, (MessageExchangeReplyListener) listener.proxy());
        inMessage.verify();

        // 4. testing the case when status is active but file write fails
        denormalizer.expects(once()).method("denormalize").will(throwException(new Exception("Some error occurred.")));
        endpointStatus.expects(atLeastOnce()).method("incrementSentErrors");
        inout.expects(once()).method("setError").with(isA(Exception.class));
        inout.expects(atLeastOnce()).method("setStatus").with(eq(ExchangeStatus.ERROR));
        inout.expects(atLeastOnce()).method("setProperty").withAnyArguments();
        listener.expects(once()).method("processReplyMessage").with(isA(InOut.class));
        instance.processRequestReplyInbound((InOut) inout.proxy(), endpoint, operation, (MessageExchangeReplyListener) listener.proxy());
        inMessage.verify();
    }

    /**
     * Test of processOneWayOutbound method, of class com.sun.jbi.filebc.OutboundMessageProcessor.
     */
    public void testProcessOneWayOutbound() {
        System.out.println("Testing processOneWayOutbound");

        Mock inonly = mock(InOnly.class);
        Mock inMessage = mock(NormalizedMessage.class);
        Mock denormalizer = mock(FileDenormalizer.class);

        FileOperation fileoperation = new FileOperation();
        FileInput fileInput = new FileInput();
        FileMessage fileMessage = new FileMessage();
        FileAddress addr = new FileAddress();

        fileoperation.setFileOperationInput(fileInput);
        fileInput.setFileMessage(fileMessage);
        fileMessage.setFileName("TestOneWayOutbound.txt");
        fileMessage.setMultipleRecordsPerFile(Boolean.FALSE);
        operations.put(THE_OPERATION, fileoperation);

        addr.setFileDirectory("test/com/sun/jbi/filebc/output");
        String epUUID = "fakedUUID";
        endpointProxy.expects(atLeastOnce()).method("getEPUUID").will(returnValue(epUUID));
        endpointProxy.expects(atLeastOnce()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpointProxy.expects(atLeastOnce()).method("getFileOperations").will(returnValue(operations));
        endpointProxy.expects(atLeastOnce()).method("getFileAddress").will(returnValue(addr));
        endpointProxy.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpointProxy.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpointProxy.expects(atLeastOnce()).method("getEndpointType").will(returnValue(THE_TYPE));
        endpointProxy.expects(atLeastOnce()).method("getServiceUnitID").will(returnValue("theID"));
        endpointProxy.expects(atLeastOnce()).method("getWorkAreaDir");
        endpointStatus.expects(atLeastOnce()).method("incrementReceivedRequests");
        deliveryChannel.expects(atLeastOnce()).method("send").with(isA(InOnly.class));
        endpointProxy.expects(atLeastOnce()).method("getDefinition").will(returnValue(wsdlDefinition.proxy()));
        wsdlDefinition.expects(atLeastOnce()).method("getServices").will(returnValue((Map<QName, Service>) services));
        service.expects(atLeastOnce()).method("getPort").will(returnValue((Port) port.proxy()));
        port.expects(atLeastOnce()).method("getBinding").will(returnValue((Binding) binding.proxy()));
        binding.expects(atLeastOnce()).method("getPortType").will(returnValue((PortType) portType.proxy()));
        portType.expects(atLeastOnce()).method("getQName").will(returnValue(THE_PORTTYPE));

        // 1. testing success scenario
        inonly.expects(atLeastOnce()).method("getInMessage").will(returnValue(inMessage.proxy()));
        inonly.expects(atLeastOnce()).method("setStatus").with(eq(ExchangeStatus.DONE));
        endpointStatus.expects(once()).method("incrementSentDones");
        denormalizer.expects(atLeastOnce()).method("denormalize").withAnyArguments().will(returnValue(new ByteArrayInputStream(data)));
        inMessage.expects(atLeastOnce()).method("getProperty");
        inMessage.expects(atLeastOnce()).method("setProperty").withAnyArguments();
        instance.setFileDenormalizer((FileDenormalizer) denormalizer.proxy());
        instance.processOneWayOutbound((InOnly) inonly.proxy(), endpoint, operation);
        inMessage.verify();
        inonly.verify();

        // 2. testing failure scenario 
        inonly.expects(atLeastOnce()).method("setStatus").with(eq(ExchangeStatus.ERROR));
        inonly.expects(once()).method("setError").withAnyArguments();
        inonly.expects(atLeastOnce()).method("setProperty");
        endpointStatus.expects(once()).method("incrementSentErrors");
        denormalizer.expects(once()).method("denormalize").will(throwException(new Exception("Some error occurred.")));
        instance.processOneWayOutbound((InOnly) inonly.proxy(), endpoint, operation);
        inMessage.verify();
        inonly.verify();
    }

    /**
     * Test of processOneWayInbound method, of class com.sun.jbi.filebc.OutboundMessageProcessor.
     */
    public void testProcessOneWayInbound() {
        System.out.println("Testing processOneWayInbound");

        Mock inonly = mock(InOnly.class);
        Mock listener = mock(MessageExchangeReplyListener.class);
        listener.expects(atLeastOnce()).method("processReplyMessage").with(isA(InOnly.class));
        Mock normalizedMessage = mock(NormalizedMessage.class);

        // 1. testing the case when exchange status is done
        endpointProxy.expects(atLeastOnce()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        inonly.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.DONE));

        String groupid = java.util.UUID.randomUUID().toString();
        String messageid = "1";
        inonly.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_GROUP_ID)).will(returnValue(groupid));
        inonly.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_MESSAGE_ID)).will(returnValue(messageid));
        String lastRetryTime = "50000";
        String totalRetries = "3";
        String remainingRetries = "1";
        Exception errorDetails = new Exception();
        String redeliveryFailure = "false";
        inonly.expects(atLeastOnce()).method("getProperty").with(eq(REDELIVERY_LAST_RETRY_TIME)).will(returnValue(lastRetryTime));
        inonly.expects(atLeastOnce()).method("getProperty").with(eq(REDELIVERY_TOTAL_RETRIES)).will(returnValue(totalRetries));
        inonly.expects(atLeastOnce()).method("getProperty").with(eq(REDELIVERY_REMAINING_RETRIES)).will(returnValue(remainingRetries));
        //inonly.expects(once()).method("getProperty").with(eq(REDELIVERY_ERROR_DETAILS)).will(returnValue(errorDetails));
        inonly.expects(atLeastOnce()).method("getProperty").with(eq(REDELIVERY_FAILURE)).will(returnValue(redeliveryFailure));
        inonly.expects(atLeastOnce()).method("getExchangeId");
		inonly.expects(atLeastOnce()).method("getError");
        endpointStatus.expects(once()).method("incrementReceivedDones");
        instance.processOneWayInbound((InOnly) inonly.proxy(), endpoint, (MessageExchangeReplyListener) listener.proxy());
        inonly.verify();
        endpointStatus.verify();

        // 2. testing the case when exchange status is error
        Mock serviceEndpoint = mock(ServiceEndpoint.class);
//        inonly.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.ERROR));
        inonly.expects(atLeastOnce()).method("getEndpoint").will(returnValue(serviceEndpoint.proxy()));
        inonly.expects(atLeastOnce()).method("getOperation");
        serviceEndpoint.expects(atLeastOnce()).method("getServiceName");
        serviceEndpoint.expects(atLeastOnce()).method("getEndpointName");
        inonly.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.ERROR));
        endpointStatus.expects(once()).method("incrementReceivedErrors");
        inonly.expects(atLeastOnce()).method("getError");
        endpointProxy.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpointProxy.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpointProxy.expects(atLeastOnce()).method("getServiceUnitID");
        endpointProxy.expects(atLeastOnce()).method("getRedeliveryConfiguration");
        instance.processOneWayInbound((InOnly) inonly.proxy(), endpoint, (MessageExchangeReplyListener) listener.proxy());
        inonly.verify();
        endpointStatus.verify();
    }

    /**
     * Test of findEndpoint method, of class com.sun.jbi.filebc.OutboundMessageProcessor
     * for the scenario where a match is found
     */
    public void testFindEndpointMatchFound() {
        System.out.println("Testing findEndpoint");

        endpointProxy.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpointProxy.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        serviceUnit.expects(atLeastOnce()).method("getEndpoints").will(returnValue(endpoints));
        String suId = "abc";
        serviceUnit.expects(atLeastOnce()).method("getServiceUnitId").will(returnValue(suId));
        endpointProxy.expects(atLeastOnce()).method("setServiceUnitID").with(eq(suId));

        Mock msgExchange = mock(MessageExchange.class);
        Mock serviceEndpoint = mock(ServiceEndpoint.class);
        msgExchange.expects(atLeastOnce()).method("getEndpoint").will(returnValue(serviceEndpoint.proxy()));
        serviceEndpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        serviceEndpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));


        Endpoint result = instance.findEndpoint((MessageExchange) msgExchange.proxy());
        assertNotNull(result);
        assertEquals(THE_SERVICE, result.getServiceName());
        assertEquals(THE_ENDPOINT, result.getEndpointName());
    }

    /**
     * Test of findEndpoint method, of class com.sun.jbi.filebc.OutboundMessageProcessor
     * for the scenario where no match is found
     */
    public void testFindEndpoinNoMatchFound() {
        System.out.println("Testing findEndpoint");

        endpointProxy.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        serviceUnit.expects(atLeastOnce()).method("getEndpoints").will(returnValue(endpoints));

        Mock msgExchange = mock(MessageExchange.class);
        Mock serviceEndpoint = mock(ServiceEndpoint.class);
        msgExchange.expects(atLeastOnce()).method("getEndpoint").will(returnValue(serviceEndpoint.proxy()));
        serviceEndpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(new QName("notMatchingService")));
        serviceEndpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue("notMatchingEndpoint"));
        Endpoint result = instance.findEndpoint((MessageExchange) msgExchange.proxy());
        assertNull(result);
    }

    /**
     * Test of writeMessage method, of class com.sun.jbi.filebc.OutboundMessageProcessor
     */
    public void testWriteMessage() {
        Mock normalizedMsg = mock(NormalizedMessage.class);
        Mock denormalizer = mock(FileDenormalizer.class);

        FileAddress addr = new FileAddress();
        FileMessage fileMessage = new FileMessage();
        QName opname = QName.valueOf("operation1");

        addr.setFileDirectory("test/com/sun/jbi/filebc/output");
        String epUUID = "fakedUUID";
        endpointProxy.expects(atLeastOnce()).method("getEPUUID").will(returnValue(epUUID));
        endpointProxy.expects(atLeastOnce()).method("getFileAddress").will(returnValue(addr));
        endpointProxy.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpointProxy.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpointProxy.expects(atLeastOnce()).method("getEndpointType").will(returnValue(THE_TYPE));
        endpointProxy.expects(atLeastOnce()).method("getServiceUnitID");
        endpointProxy.expects(atLeastOnce()).method("getWorkAreaDir");
        instance.setFileDenormalizer((FileDenormalizer) denormalizer.proxy());
        normalizedMsg.expects(atLeastOnce()).method("getProperty");
        normalizedMsg.expects(atLeastOnce()).method("setProperty");

        // testing simple file write with real file name
        fileMessage.setFileName("TestFileWrite.txt");
        denormalizer.expects(atLeastOnce()).method("denormalize").withAnyArguments().will(returnValue(new ByteArrayInputStream(data)));
        try {
            instance.writeMessage((NormalizedMessage) normalizedMsg.proxy(), (Endpoint) endpointProxy.proxy(), opname, fileMessage);
            assertEquals("Hello World!!!", getFileContents(new File("test/com/sun/jbi/filebc/output/TestFileWrite.txt").getAbsolutePath()));
        } catch (Exception e) {
            fail("Failed to test writeMessage due to: " + e.getMessage());
        }

        // testing writing EOL with real file name
        fileMessage.setAddEOL(Boolean.TRUE);
        denormalizer.expects(atLeastOnce()).method("denormalize").withAnyArguments().will(returnValue(new ByteArrayInputStream(data)));
        try {
            instance.writeMessage((NormalizedMessage) normalizedMsg.proxy(), (Endpoint) endpointProxy.proxy(), opname, fileMessage);
            assertEquals("Hello World!!!", getFileContents(new File("test/com/sun/jbi/filebc/output/TestFileWrite.txt").getAbsolutePath()).trim());
        } catch (Exception e) {
            fail("Failed to test writeMessage due to: " + e.getMessage());
        }

        // testing writing multiple records with real file name
        File[] files = new File("test/com/sun/jbi/filebc/output").listFiles();
        for (int ii = 0; ii < files.length; ii++) {
            File afile = (File) files[ii];
            afile.delete();
        }
        fileMessage.setAddEOL(Boolean.FALSE);
        fileMessage.setMultipleRecordsPerFile(Boolean.TRUE);
        fileMessage.setRecordDelimiter("${symbol_escape}r${symbol_escape}n");
        try {
            denormalizer.expects(atLeastOnce()).method("denormalize").withAnyArguments().will(returnValue(new ByteArrayInputStream(data)));
            instance.writeMessage((NormalizedMessage) normalizedMsg.proxy(), (Endpoint) endpointProxy.proxy(), opname, fileMessage);
            denormalizer.expects(atLeastOnce()).method("denormalize").withAnyArguments().will(returnValue(new ByteArrayInputStream(data)));
            instance.writeMessage((NormalizedMessage) normalizedMsg.proxy(), (Endpoint) endpointProxy.proxy(), opname, fileMessage);
            denormalizer.expects(atLeastOnce()).method("denormalize").withAnyArguments().will(returnValue(new ByteArrayInputStream(data)));
            instance.writeMessage((NormalizedMessage) normalizedMsg.proxy(), (Endpoint) endpointProxy.proxy(), opname, fileMessage);
            assertEquals("Hello World!!!${symbol_escape}r${symbol_escape}nHello World!!!${symbol_escape}r${symbol_escape}nHello World!!!", getFileContents(new File("test/com/sun/jbi/filebc/output/TestFileWrite.txt").getAbsolutePath()).trim());
        } catch (Exception e) {
            fail("Failed to test writeMessage due to: " + e.getMessage());
        }

        // testing writing single record with file name pattern
        /**
        fileMessage.setFileNameIsPattern(Boolean.TRUE);
        try {
        instance.writeMessage((NormalizedMessage) normalizedMsg.proxy(), (Endpoint)endpointProxy.proxy(), opname, fileMessage);
        assertTrue(new File("test/com/sun/jbi/filebc/output/blah0.dat").exists());
        assertEquals("Hello World!!!", getFileContents(new File("test/com/sun/jbi/filebc/output/blah0.dat").getAbsolutePath()).trim());
        } catch (Exception e) {
        fail("Failed to test writeMessage due to: " + e.getMessage());
        }
         **/
    }

    /**
     * Test of validateRequestReplyInboundMessageExchangeProperties method, 
     * of class com.sun.jbi.filebc.OutboundMessageProcessor
     */
    public void testValidateRequestReplyInboundMessageExchangeProperties() {
        FileOperation operation = new FileOperation();
        FileOutput fileOutput = new FileOutput();
        FileMessage fileMessage = new FileMessage();
        QName opname = QName.valueOf("operation1");

        // testing null input properties
        try {
            instance.validateRequestReplyInboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateRequestReplyInboundMessageExchangeProperties - an exception should be raised due to null File Input properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateRequestReplyInboundMessageExchangeProperties with null File Input properties.");
        }

        // testing null File write properties
        operation.setFileOperationOutput(fileOutput);
        try {
            instance.validateRequestReplyInboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateRequestReplyInboundMessageExchangeProperties - an exception should be raised due to null File Write properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateRequestReplyInboundMessageExchangeProperties with null File Write properties.");
        }

        // testing missing required File write attributes
        fileOutput.setFileMessage(fileMessage);
        try {
            instance.validateRequestReplyInboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateRequestReplyInboundMessageExchangeProperties - an exception should be raised due to undefined fileName attribute when fileNameIsPattern is false.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateRequestReplyInboundMessageExchangeProperties when fileName is not defined and fileNameIsPattern is false.");
        }

        fileMessage.setFileNameIsPattern(Boolean.TRUE);
        try {
            instance.validateRequestReplyInboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateRequestReplyInboundMessageExchangeProperties - an exception should be raised due to undefined filePrefix and fileExtension attributes when fileNameIsPattern is true.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateRequestReplyInboundMessageExchangeProperties when both filePrefix and fileExtension are not defined and fileNameIsPattern is true.");
        }

        // testing invalid File Write attribute value
        fileMessage.setFileType("undefined");
        try {
            instance.validateRequestReplyInboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateRequestReplyInboundMessageExchangeProperties - an exception should be raised due to invalid File Write attribute value: fileType is undefined.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateRequestReplyInboundMessageExchangeProperties: invalid File Write attribute value: fileType is undefined.");
        }
    }

    /**
     * Test of validateOutboundMessageExchangeProperties method, 
     * of class com.sun.jbi.filebc.OutboundMessageProcessor
     */
    public void testValidateOutboundMessageExchangeProperties() {
        FileOperation operation = new FileOperation();
        FileInput fileInput = new FileInput();
        FileMessage fileMessage = new FileMessage();
        QName opname = QName.valueOf("operation1");

        // testing null input properties
        try {
            instance.validateOutboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateOutboundMessageExchangeProperties - an exception should be raised due to null File Input properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateOutboundMessageExchangeProperties with null File Input properties.");
        }

        // testing null File write properties
        operation.setFileOperationInput(fileInput);
        try {
            instance.validateOutboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateOutboundMessageExchangeProperties - an exception should be raised due to null File Write properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateOutboundMessageExchangeProperties with null File Write properties.");
        }

        // testing missing required File write attributes
        fileInput.setFileMessage(fileMessage);
        try {
            instance.validateOutboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateOutboundMessageExchangeProperties - an exception should be raised due to undefined fileName attribute when fileNameIsPattern is false.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateOutboundMessageExchangeProperties when fileName is not defined and fileNameIsPattern is false.");
        }

        fileMessage.setFileNameIsPattern(Boolean.TRUE);
        try {
            instance.validateOutboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateOutboundMessageExchangeProperties - an exception should be raised due to undefined filePrefix and fileExtension attributes when fileNameIsPattern is true.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateOutboundMessageExchangeProperties when both filePrefix and fileExtension are not defined and fileNameIsPattern is true.");
        }

        // testing invalid File Write attribute value
        fileMessage.setFileType("undefined");
        try {
            instance.validateOutboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateOutboundMessageExchangeProperties - an exception should be raised due to invalid File Write attribute value: fileType is undefined.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateOutboundMessageExchangeProperties: invalid File Write attribute value: fileType is undefined.");
        }
    }

    private String getFileContents(String fileName) {
        StringBuffer output = new StringBuffer();
        java.io.FileReader reader = null;

        try {
            reader = new java.io.FileReader(fileName);
            char[] buff = new char[512];
            int len = reader.read(buff);
            while (len > 0) {
                output.append(buff, 0, len);
                len = reader.read(buff);
            }
            reader.close();
        } catch (Exception e) {
            fail("Failed to retrieve content from " + fileName + ".");
        }

        return output.toString();
    }
}
