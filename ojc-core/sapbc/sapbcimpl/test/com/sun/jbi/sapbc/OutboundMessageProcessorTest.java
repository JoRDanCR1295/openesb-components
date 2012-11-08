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
 * @(#)OutboundMessageProcessorTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import junit.framework.*;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.sapbc.Endpoint.EndpointType;
import com.sun.jbi.sapbc.extensions.SAPAddress;
import com.sun.jbi.sapbc.extensions.SAPEnvironmentalVars;
import com.sun.jbi.sapbc.extensions.SAPFmOperation;
import com.sun.jbi.sapbc.extensions.WSDLInput;
import com.sun.jbi.sapbc.extensions.WSDLOutput;
import com.sun.jbi.sapbc.extensions.SAPMessage;
import com.sun.jbi.sapbc.util.SAPWSDLUtilities;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.jmock.*;


/**
 *
 * @author sweng
 */
public class OutboundMessageProcessorTest extends org.jmock.cglib.MockObjectTestCase {
    String targetNamespace;
    QName THE_OPERATION = null;
    QName THE_SERVICE = null;
    String THE_ENDPOINT = null;
    //byte[] data = null;
    
    OutboundMessageProcessor instance = null;
    Endpoint endpoint = null;
    
    Mock deliveryChannel = null;
    Mock endpointProxy = null;
    Mock endpointStatus = null;
    Mock serviceUnit = null;
    
    List endpoints = new ArrayList();
    Map serviceUnits = new HashMap();
    Map operations = new HashMap();
    
    SAPFmOperation sapOperation = null;
    WSDLInput wsdlInput = null;
    WSDLOutput wsdlOutput = null;
    SAPMessage sapMessage = null;
    SAPAddress addr = null;
        
    Document doc = null;
    
    public OutboundMessageProcessorTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        targetNamespace = "urn:sap-com:document:sap:soap:functions:mc-style";
        THE_OPERATION  = new QName("", "FlightGetDetail");
        THE_SERVICE = new QName(targetNamespace, "Z_FlightWSDService");
        THE_ENDPOINT = "Z_FlightWSDSAPBindingPort";
        
        File wsdlFile = new File( "test/com/sun/jbi/sapbc/wsdls/Z_FlightWSD_EUC_SAP.wsdl" );
        QName interfaceQName = new QName(targetNamespace, "Z_FlightWSD");
        EndpointType direction = EndpointType.INBOUND;
        SAPEnvironmentalVars envVars = new SAPEnvironmentalVars(new HashMap());
        endpoint = (EndpointImpl) SAPWSDLUtilities.getWSDLEndpointByName(wsdlFile, 
                interfaceQName, 
                THE_SERVICE, 
                THE_ENDPOINT, 
                direction,
                envVars); 
        Object op = endpoint.getSAPOperations().get(THE_OPERATION);
        if (op instanceof SAPFmOperation) {
            sapOperation = (SAPFmOperation) op;
        } else {
            fail("Operation ["+THE_OPERATION.toString()+"] is not an instance of SAPFmOperation");
        }
        wsdlInput = sapOperation.getSAPOperationInput();
        wsdlOutput = sapOperation.getSAPOperationOutput();
        sapMessage = wsdlInput.getSAPMessage();
        addr = endpoint.getSAPAddress();
        
        
        deliveryChannel = mock(DeliveryChannel.class);
        endpointProxy = mock(Endpoint.class);
        endpointStatus = mock(EndpointStatus.class);
        //endpoint = (Endpoint) endpointProxy.proxy();
        serviceUnit = mock(ServiceUnit.class);
        endpoints.add(endpointProxy.proxy());
        serviceUnits.put("1", serviceUnit.proxy());
        
        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            synchronized(dbf) {
                dbf.setNamespaceAware(true);
                doc = dbf.newDocumentBuilder().newDocument();
            }
        } catch(Exception e) {
            System.out.println("Cannot create a document");
        }
        
        if (doc != null) {
            org.w3c.dom.Element root = doc.createElement("Simple-file-write-test");
            doc.appendChild(root);
        }
        
        instance = new OutboundMessageProcessor((DeliveryChannel)deliveryChannel.proxy(), serviceUnits);
        
    }
    
    protected void tearDown() throws Exception {
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(OutboundMessageProcessorTest.class);
        
        return suite;
    }
    
    /**
     * Test of processRequestReplyOutbound method, of class com.sun.jbi.sapbc.OutboundMessageProcessor.
     */
    public void testProcessRequestReplyOutbound() {
        System.out.println("Testing processRequestReplyOutbound");
        
        Mock inout = mock(InOut.class);
        Mock inMessage = mock(NormalizedMessage.class);
        Mock outMessage =  mock(NormalizedMessage.class);
        //Mock denormalizer = mock(SAPDenormalizer.class);
        
        wsdlInput.setSAPMessage(sapMessage);
        sapOperation.setSAPOperationInput(wsdlInput);
        //sapMessage.setSAPName("TestRequestResponseOutbound.txt");
        operations.put(THE_OPERATION, sapOperation);
        //addr.setSAPDirectory("test/com/sun/jbi/sapbc/output");
        
        //endpointProxy.expects(atLeastOnce()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        //endpointProxy.expects(atLeastOnce()).method("getSAPOperations").will(returnValue(operations));
        //endpointProxy.expects(atLeastOnce()).method("getSAPAddress").will(returnValue(addr));
        //deliveryChannel.expects(atLeastOnce()).method("send").with(isA(InOut.class));
        
        /* processRequestReplyOutbound not implemented
        // 1. testing the case when exchange status is done
        inout.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.DONE));
        endpointStatus.expects(atLeastOnce()).method("incrementReceivedDones");
        instance.processRequestReplyOutbound((InOut)inout.proxy(), endpoint, operation);
        endpointStatus.verify();
         
        // 2. testing the case when exchange status is error
        inout.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.ERROR));
        endpointStatus.expects(once()).method("incrementReceivedErrors");
        instance.processRequestReplyOutbound((InOut)inout.proxy(), endpoint, operation);
        endpointStatus.verify();
         
        // 3. testing the case when status is active and file write is successful
        endpointStatus.expects(atLeastOnce()).method("incrementReceivedRequests");
        endpointStatus.expects(atLeastOnce()).method("incrementSentReplies");
        //denormalizer.expects(atLeastOnce()).method("denormalize").withAnyArguments().will(returnValue(data));
        //instance.setSAPDenormalizer((SAPDenormalizer)denormalizer.proxy());
        inout.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.ACTIVE));
        inout.expects(atLeastOnce()).method("getInMessage").will(returnValue(inMessage.proxy()));
        inout.expects(atLeastOnce()).method("createMessage").will(returnValue(outMessage.proxy()));
        inout.expects(atLeastOnce()).method("setOutMessage").withAnyArguments();
        outMessage.expects(atLeastOnce()).method("setContent").withAnyArguments();
        outMessage.expects(atLeastOnce()).method("setProperty").withAnyArguments();
        instance.processRequestReplyOutbound((InOut)inout.proxy(), endpoint, operation);
        inMessage.verify();
        outMessage.verify();
         
        // 4. testing the case when status is active but file write fails
        denormalizer.expects(once()).method("denormalize").will(throwException(new Exception("Some error occurred.")));
        inout.expects(once()).method("setError").with(isA(Exception.class));
        instance.processRequestReplyOutbound((InOut)inout.proxy(), endpoint, operation);
        inMessage.verify();
        outMessage.verify();
         */
    }
    
    /**
     * Test of processRequestReplyInbound method, of class com.sun.jbi.sapbc.OutboundMessageProcessor.
     */
    public void testProcessRequestReplyInbound() {
        System.out.println("Testing processRequestReplyInbound");
        
        Mock inout = mock(InOut.class);
        Mock inMessage = mock(NormalizedMessage.class);
        // Mock denormalizer = mock(SAPDenormalizer.class);
        Mock listener = mock(MessageExchangeReplyListener.class);
                
        wsdlOutput.setSAPMessage(sapMessage);
        //sapMessage.setSAPName("TestRequestResponseInbound.txt");
        sapOperation.setSAPOperationOutput(wsdlOutput);
        operations.put(THE_OPERATION, sapOperation);
        //addr.setSAPDirectory("test/com/sun/jbi/sapbc/output");
        
        //endpointProxy.expects(atLeastOnce()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        //endpointProxy.expects(atLeastOnce()).method("getSAPOperations").will(returnValue(operations));
        //endpointProxy.expects(atLeastOnce()).method("getSAPAddress").will(returnValue(addr));
        //deliveryChannel.expects(atLeastOnce()).method("send").with(isA(InOut.class));
        
        /*processRequestReplyInbound not implemented
        // 1. testing the case when exchange status is done
        inout.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.DONE));
        endpointStatus.expects(once()).method("incrementReceivedDones");
        instance.processRequestReplyInbound((InOut)inout.proxy(), endpoint, operation, (MessageExchangeReplyListener)listener.proxy());
        endpointStatus.verify();
         
        // 2. testing the case when exchange status is error
        inout.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.ERROR));
        endpointStatus.expects(once()).method("incrementReceivedErrors");
        instance.processRequestReplyInbound((InOut)inout.proxy(), endpoint, operation, (MessageExchangeReplyListener)listener.proxy());
        endpointStatus.verify();
         
        // 3. testing the case when status is active and file write is successful
        endpointStatus.expects(atLeastOnce()).method("incrementReceivedReplies");
        endpointStatus.expects(atLeastOnce()).method("incrementSentDones");
        denormalizer.expects(atLeastOnce()).method("denormalize").withAnyArguments().will(returnValue(data));
        instance.setSAPDenormalizer((SAPDenormalizer)denormalizer.proxy());
        inout.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.ACTIVE));
        inout.expects(atLeastOnce()).method("getOutMessage").will(returnValue(inMessage.proxy()));
        inout.expects(atLeastOnce()).method("setStatus").with(eq(ExchangeStatus.DONE));
        listener.expects(once()).method("processReplyMessage").with(isA(InOut.class));
        instance.processRequestReplyInbound((InOut)inout.proxy(), endpoint, operation, (MessageExchangeReplyListener)listener.proxy());
        inMessage.verify();
         
        // 4. testing the case when status is active but file write fails
        denormalizer.expects(once()).method("denormalize").will(throwException(new Exception("Some error occurred.")));
        endpointStatus.expects(atLeastOnce()).method("incrementSentErrors");
        inout.expects(once()).method("setError").with(isA(Exception.class));
        inout.expects(atLeastOnce()).method("setStatus").with(eq(ExchangeStatus.ERROR));
        listener.expects(once()).method("processReplyMessage").with(isA(InOut.class));
        instance.processRequestReplyInbound((InOut)inout.proxy(), endpoint, operation, (MessageExchangeReplyListener)listener.proxy());
        inMessage.verify();
         */
    }
    
    /**
     * Test of processOneWayOutbound method, of class com.sun.jbi.sapbc.OutboundMessageProcessor.
     */
    public void testProcessOneWayOutbound() {
        System.out.println("Testing processOneWayOutbound");
        
        /* processOneWayOutbound not implemented
        Mock inonly = mock(InOnly.class);
        Mock inMessage = mock(NormalizedMessage.class);
        //Mock denormalizer = mock(SAPDenormalizer.class);
        
        sapOperation.setSAPOperationInput(wsdlInput);
        wsdlInput.setSAPMessage(sapMessage);
        //sapMessage.setSAPName("TestOneWayOutbound.txt");
        //sapMessage.setMultipleRecordsPerSAP(Boolean.FALSE);
        operations.put(THE_OPERATION, sapOperation);
        
        //addr.setSAPDirectory("test/com/sun/jbi/sapbc/output");
        endpointProxy.expects(atLeastOnce()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpointProxy.expects(atLeastOnce()).method("getSAPOperations").will(returnValue(operations));
        endpointProxy.expects(atLeastOnce()).method("getSAPAddress").will(returnValue(addr));
        endpointStatus.expects(atLeastOnce()).method("incrementReceivedRequests");
        deliveryChannel.expects(atLeastOnce()).method("send").with(isA(InOnly.class));
        
        // 1. testing success scenario
        inonly.expects(atLeastOnce()).method("getInMessage").will(returnValue(inMessage.proxy()));
        inonly.expects(atLeastOnce()).method("setStatus").with(eq(ExchangeStatus.DONE));
        endpointStatus.expects(once()).method("incrementSentDones");
        denormalizer.expects(atLeastOnce()).method("denormalize").withAnyArguments().will(returnValue(data));
        instance.setSAPDenormalizer((SAPDenormalizer)denormalizer.proxy());
        instance.processOneWayOutbound((InOnly)inonly.proxy(), endpoint, operation);
        inMessage.verify();
        inonly.verify();
         
        // 2. testing failure scenario
        inonly.expects(atLeastOnce()).method("setStatus").with(eq(ExchangeStatus.ERROR));
        endpointStatus.expects(once()).method("incrementSentErrors");
        denormalizer.expects(once()).method("denormalize").will(throwException(new Exception("Some error occurred.")));
        instance.processOneWayOutbound((InOnly)inonly.proxy(), endpoint, operation);
        inMessage.verify();
        inonly.verify();
         */
    }
    
    /**
     * Test of processOneWayInbound method, of class com.sun.jbi.sapbc.OutboundMessageProcessor.
     */
    public void testProcessOneWayInbound() {
        System.out.println("Testing processOneWayInbound");
        
       /* processOneWayInbound not implemented
        Mock inonly = mock(InOnly.class);
        Mock listener = mock(MessageExchangeReplyListener.class);
        listener.expects(atLeastOnce()).method("processReplyMessage").with(isA(InOnly.class));
        
         // 1. testing the case when exchange status is done
        endpointProxy.expects(atLeastOnce()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        inonly.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.DONE));
        endpointStatus.expects(once()).method("incrementReceivedDones");
        instance.processOneWayInbound((InOnly)inonly.proxy(), endpoint, (MessageExchangeReplyListener)listener.proxy());
        inonly.verify();
        endpointStatus.verify();
         
        // 2. testing the case when exchange status is error
        inonly.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.ERROR));
        endpointStatus.expects(once()).method("incrementReceivedErrors");
        instance.processOneWayInbound((InOnly)inonly.proxy(), endpoint, (MessageExchangeReplyListener)listener.proxy());
        inonly.verify();
        endpointStatus.verify();
         */
    }
    
    /**
     * Test of findEndpoint method, of class com.sun.jbi.sapbc.OutboundMessageProcessor
     * for the scenario where a match is found
     */
    public void testFindEndpointMatchFound() {
        System.out.println("Testing findEndpoint");
        
        endpointProxy.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpointProxy.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        serviceUnit.expects(atLeastOnce()).method("getEndpoints").will(returnValue(endpoints));
        
        Mock msgExchange = mock(MessageExchange.class);
        Mock serviceEndpoint = mock(ServiceEndpoint.class);
        msgExchange.expects(atLeastOnce()).method("getEndpoint").will(returnValue(serviceEndpoint.proxy()));
        serviceEndpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        serviceEndpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        Endpoint result = instance.findEndpoint((MessageExchange)msgExchange.proxy());
        assertNotNull(result);
        assertEquals(THE_SERVICE, result.getServiceName());
        assertEquals(THE_ENDPOINT, result.getEndpointName());
    }
    
    /**
     * Test of findEndpoint method, of class com.sun.jbi.sapbc.OutboundMessageProcessor
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
        Endpoint result = instance.findEndpoint((MessageExchange)msgExchange.proxy());
        assertNull(result);
    }
    
    
    /**
     * Test of validateRequestReplyInboundMessageExchangeProperties method,
     * of class com.sun.jbi.sapbc.OutboundMessageProcessor
     */
    public void testValidateRequestReplyInboundMessageExchangeProperties() {
        /* validateRequestReplyInboundMessageExchangeProperties not implemented
        // testing null input properties
        try {
            instance.validateRequestReplyInboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateRequestReplyInboundMessageExchangeProperties - an exception should be raised due to null SAP Request properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateRequestReplyInboundMessageExchangeProperties with null SAP Request properties.");
        }
         
        // testing null File write properties
        operation.setSAPOperationOutput(wsdlOutput);
        try {
            instance.validateRequestReplyInboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateRequestReplyInboundMessageExchangeProperties - an exception should be raised due to null File Write properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateRequestReplyInboundMessageExchangeProperties with null File Write properties.");
        }
         
        // testing missing required File write attributes
        wsdlOutput.setSAPMessage(sapMessage);
        try {
            instance.validateRequestReplyInboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateRequestReplyInboundMessageExchangeProperties - an exception should be raised due to undefined fileName attribute when fileNameIsPattern is false.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateRequestReplyInboundMessageExchangeProperties when fileName is not defined and fileNameIsPattern is false.");
        }
         
        sapMessage.setFileNameIsPattern(Boolean.TRUE);
        try {
            instance.validateRequestReplyInboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateRequestReplyInboundMessageExchangeProperties - an exception should be raised due to undefined filePrefix and fileExtension attributes when fileNameIsPattern is true.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateRequestReplyInboundMessageExchangeProperties when both filePrefix and fileExtension are not defined and fileNameIsPattern is true.");
        }
         
        // testing invalid File Write attribute value
        sapMessage.setFileType("undefined");
        try {
            instance.validateRequestReplyInboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateRequestReplyInboundMessageExchangeProperties - an exception should be raised due to invalid File Write attribute value: fileType is undefined.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateRequestReplyInboundMessageExchangeProperties: invalid File Write attribute value: fileType is undefined.");
        }
         */
    }
    
    /**
     * Test of validateOutboundMessageExchangeProperties method,
     * of class com.sun.jbi.sapbc.OutboundMessageProcessor
     */
    public void testValidateOutboundMessageExchangeProperties() {
        /* validateOutboundMessageExchangeProperties not implemented
        // testing null input properties
        try {
            instance.validateOutboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateOutboundMessageExchangeProperties - an exception should be raised due to null SAP Request properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateOutboundMessageExchangeProperties with null SAP Request properties.");
        }
         
        // testing null File write properties
        operation.setSAPOperationInput(wsdlInput);
        try {
            instance.validateOutboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateOutboundMessageExchangeProperties - an exception should be raised due to null SAP Write properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateOutboundMessageExchangeProperties with null SAP Write properties.");
        }
         
        // testing missing required SAP write attributes
        wsdlInput.setSAPMessage(sapMessage);
        try {
            instance.validateOutboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateOutboundMessageExchangeProperties - an exception should be raised due to undefined fileName attribute when fileNameIsPattern is false.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateOutboundMessageExchangeProperties when fileName is not defined and fileNameIsPattern is false.");
        }
         
        sapMessage.setFileNameIsPattern(Boolean.TRUE);
        try {
            instance.validateOutboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateOutboundMessageExchangeProperties - an exception should be raised due to undefined filePrefix and fileExtension attributes when fileNameIsPattern is true.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateOutboundMessageExchangeProperties when both filePrefix and fileExtension are not defined and fileNameIsPattern is true.");
        }
         
        // testing invalid File Write attribute value
        sapMessage.setFileType("undefined");
        try {
            instance.validateOutboundMessageExchangeProperties(operation, opname);
            fail("Failed to test validateOutboundMessageExchangeProperties - an exception should be raised due to invalid File Write attribute value: fileType is undefined.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateOutboundMessageExchangeProperties: invalid File Write attribute value: fileType is undefined.");
        }
         **/
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
            fail("Failed to retrieve content from " + fileName  + ".");
        }
        
        return output.toString();
    }
}
