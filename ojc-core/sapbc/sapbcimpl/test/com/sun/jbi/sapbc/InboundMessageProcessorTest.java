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

package com.sun.jbi.sapbc;

import junit.framework.*;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.sapbc.extensions.SAPFmOperation;
import com.sun.jbi.sapbc.extensions.SAPAddress;
import com.sun.jbi.sapbc.extensions.WSDLInput;
import com.sun.jbi.sapbc.extensions.WSDLOutput;
import com.sun.jbi.sapbc.extensions.SAPMessage;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.extensions.impl.ExtensibilityElementImpl;
import java.util.HashMap;
import java.util.Map;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import org.jmock.*;

/**
 *  TODO: Develop tests when InboundMessageProcessor is up and running with
 *  the sapbc
 * @author sweng
 */
public class InboundMessageProcessorTest extends org.jmock.cglib.MockObjectTestCase {
    static final QName THE_OPERATION = new QName("myoperation");
    static final QName THE_SERVICE = new QName("myServiceName");
    static final String THE_ENDPOINT = "myEndpointName";
    InboundMessageProcessor instance = null;
    ExtensibilityElement e = new ExtensibilityElementImpl();
    
    Mock deliveryChannel = null;
    Mock componentContext = null;
    Mock endpoint = null;
    Mock endpointStatus = null;
    Mock serviceEndpoint = null;
    Mock msgExchange = null;
    Mock msgExchangeFactory = null;
    //Mock normalizer = null;
    Mock normalizedMsg = null;
    
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
        //normalizer = mock(SAPNormalizer.class);
        normalizedMsg = mock(NormalizedMessage.class);
        
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
     * Test of run method, of class com.sun.jbi.sapbc.InboundMessageProcessor 
     * for the scenario where the message exchange pattern is not valid
     */
    public void testRunInvalidMEP() {
    /*
        System.out.println("Testing run() for the scenario where the message exchange pattern is not valid");
        SAPFmOperation sapOperation;
        SAPAddress addr;
        
        try {
            sapOperation = new SAPFmOperation(e);
            operations.put(THE_OPERATION, sapOperation);
            
            WSDLInput wsdlInput = new WSDLInput();
            wsdlInput.setSAPMessage(new SAPMessage(e));
            sapOperation.setSAPOperationInput(wsdlInput);
            
            addr = new SAPAddress(e);
        } catch (Exception ex) {
            ex.printStackTrace();
            String errMsg = "testRunInvalidMEP: Exception Thrown ["+ex.getClass().getName()+"] Message ["+ex.getMessage()+"]";
            fail(errMsg);
        }
        
        //addr.setFileDirectory("test/com/sun/jbi/sapbc/input");
        endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(atLeastOnce()).method("getSAPOperations").will(returnValue(operations));
        endpoint.expects(atLeastOnce()).method("getOperationMsgExchangePattern").will(returnValue(operationMeps));
        
        // 1. testing the case where message exchange pattern is not found (null)
        operationMeps.remove(THE_OPERATION);
        operationMeps.put("dummyOperation", "inonly");
        instance.run();
        
        // 2. testing the case where message exchange status is invalid (not inonly or inout)
        operationMeps.put(THE_OPERATION, "outin");
        instance.run();
     **/    
    }
    
    /**
     * Test of execute method, of class com.sun.jbi.sapbc.InboundMessageProcessor
     * for the scenario where basic sap extensibility element attribute validation fails.
     */
    /*
    public void testValidateInboundMessageProperties() {
        System.out.println("Testing basic runtime wsdl validation");
        
        SAPFmOperation operation;
        SAPMessage sapMessage;
        
        try {
            String mep = "outin";
            operation = new SAPFmOperation(e);
            WSDLInput wsdlInput = new WSDLInput();
            WSDLOutput wsdlOutput = new WSDLOutput();
            sapMessage = new SAPMessage(e);
        } catch (Exception ex) {
            ex.printStackTrace();
            String errMsg = "testValidateInboundMessageProperties: Exception Thrown ["+ex.getClass().getName()+"] Message ["+ex.getMessage()+"]";
            fail(errMsg);
        }
        
        // todo: Fill in correct properties
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
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to null SAP Request properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties with null SAP Request properties.");
        }
        
        // testing null File read properties
        operation.setSAPOperationInput(wsdlInput);
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to null File Read properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties with null File Read properties.");
        }
        
        // testing missing required File Read attributes
        operation.setSAPOperationInput(wsdlInput);
        wsdlInput.setSAPMessage(sapMessage);
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to missing required File Read attribute: pollingMillis.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties: missing required File Read attribute: pollingMillis.");
        }
        
        operation.setSAPOperationInput(wsdlInput);
        wsdlInput.setSAPMessage(sapMessage);
        //sapMessage.setPollingInterval(new Long(5000));
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to missing required File Read attribute: fileName.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties: missing required File Read attribute: fileName.");
        }
        
        // testing invalid File Read attribute value
        //sapMessage.setFileType("stream");
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to invalid File Read attribute value: fileType is stream.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties: invalid File Read attribute value: fileType is stream.");
        }
        
        // testing with in-out mep 
        // testing with null File output properties
        mep = "inout";
        operation.setSAPOperationOutput(wsdlOutput);
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to null SAP Response properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties with null SAP Response properties.");
        }
        
        // testing missing required File write attributes
         try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to undefined fileName attribute when fileNameIsPattern is false.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties when fileName is not defined and fileNameIsPattern is false.");
        }
        
        //sapMessage.setFileNameIsPattern(Boolean.TRUE);
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to undefined filePrefix and fileExtension attributes when fileNameIsPattern is true.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties when both filePrefix and fileExtension are not defined and fileNameIsPattern is true.");
        }
        
        // testing invalid File Write attribute value
        //sapMessage.setFileType("undefined");
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to invalid File Write attribute value: fileType is undefined.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties: invalid File Write attribute value: fileType is undefined.");
        }
        
        System.out.println("Successfully tested basic runtime wsdl validation");
    }
         */
    
    /**
     * Test of execute method, of class com.sun.jbi.sapbc.InboundMessageProcessor
     * for the scenario where the message exchange type is inonly.
     */
    /* TODO: Modify to work with SAP
    public void testExecuteInOnlyExchangeCase1() {
        System.out.println("Testing in-only message exchange case 1...");
        
        WSDLInput wsdlInput = new WSDLInput();
        SAPMessage sapMessage = new SAPMessage();
        msgExchange = mock(InOnly.class);
        File processedFile = new File("test/com/sun/jbi/sapbc/input/TestInputInOnly.txt_processed");
        String inputFile = "test/com/sun/jbi/sapbc/input/InputInOnly.txt";
        String testFile = "test/com/sun/jbi/sapbc/input/TestInputInOnly.txt";
        String errorFile = "test/com/sun/jbi/sapbc/input/TestInputInOnly.txt_error";
        
        try {
            if (processedFile.exists()) {
                processedFile.delete();
            }
            if (!new File(testFile).exists()) {
                copyFile(inputFile, testFile);
            }
        } catch (Exception e) {
            fail("Failed to create input file to test execute for InOnly message exchanges: " + e.getMessage());
        }
        
        // Testing the following scenario
        // 1. no file name match pattern
        // 2. successful "normalization"
        // 3. successful msgexchange.send
        
        //sapMessage.setFileName("TestInputInOnly.txt");
        //sapMessage.setPollingInterval(new Long(5000));
        wsdlInput.setSAPMessage(sapMessage);
        
        deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory").will(returnValue(msgExchangeFactory.proxy()));
        msgExchangeFactory.expects(atLeastOnce()).method("createInOnlyExchange").will(returnValue((InOnly)msgExchange.proxy()));
        componentContext.expects(atLeastOnce()).method("getEndpoint").with(eq(THE_SERVICE), eq(THE_ENDPOINT)).will(returnValue((ServiceEndpoint)serviceEndpoint.proxy()));
        msgExchange.expects(atLeastOnce()).method("getExchangeId");
        //normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(returnValue((NormalizedMessage)normalizedMsg.proxy()));
        //instance.setSAPNormalizer((SAPNormalizer) normalizer.proxy());
        msgExchange.expects(once()).method("setEndpoint").with(isA(ServiceEndpoint.class));
        msgExchange.expects(once()).method("setOperation").with(eq(THE_OPERATION));
        msgExchange.expects(once()).method("setMessage").with(eq(normalizedMsg.proxy()), eq("in"));
        
        deliveryChannel.expects(once()).method("send").with(eq(msgExchange.proxy()));
        endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(once()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpointStatus.expects(once()).method("incrementSentRequests");

        try {
            instance.execute("inonly", "test/com/sun/jbi/sapbc/input", "TestInputInOnly.txt", Boolean.FALSE, wsdlInput);
            assertTrue(processedFile.exists());
        } catch (Exception e) {
            fail("Failed to test InOnly exchanges due to: " + e.getMessage());
        }
        msgExchange.verify();
        msgExchangeFactory.verify();
        componentContext.verify();
        endpointStatus.verify();
        
        // Testing the following scenario
        // 1. no file name match pattern
        // 2. Unsuccessful "normalization"
        try {
            if (processedFile.exists()) {
                processedFile.delete();
            }
            if (!new File(testFile).exists()) {
                copyFile(inputFile, testFile);
            }
        } catch (Exception e) {
            fail("Failed to create input file to test execute for InOnly message exchanges: " + e.getMessage());
        }
        //normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(throwException(new Exception("Failed to normalize")));
        //instance.setSAPNormalizer((SAPNormalizer) normalizer.proxy());
        
        try {
            instance.execute("inonly", "test/com/sun/jbi/sapbc/input", "TestInputInOnly.txt", Boolean.FALSE, wsdlInput);
            fail("Failed to test in-only message exchange when an expected exception should be caught");
        } catch (Exception e) {
            System.out.println("Caught the expected exception!!");
        }
        msgExchange.verify();
        msgExchangeFactory.verify();
        componentContext.verify();
        endpointStatus.verify();
        
        // clean up 
        if (new File(testFile).exists()) {
            new File(testFile).delete();
        }
        
        if (new File(errorFile).exists()) {
            new File(errorFile).delete();
        }
        
        if (processedFile.exists()) {
            processedFile.delete();
        }
        
        System.out.println("Successfully tested in-only message exchange case 1");
    }
*/    
    /**
     * Test of execute method, of class com.sun.jbi.sapbc.InboundMessageProcessor
     * for the scenario where the message exchange type is inonly.
     */
    /* TODO: Modify to work with SAP
    public void testExecuteInOnlyExchangeCase2() {
        System.out.println("Testing in-only message exchange case 2...");
        
        WSDLInput wsdlInput = new WSDLInput();
        SAPMessage sapMessage = new SAPMessage();
        msgExchange = mock(InOnly.class);
        File processedFile = new File("test/com/sun/jbi/sapbc/input/I8n.dat_processed");
        String inputFile = "test/com/sun/jbi/sapbc/input/InputInOnly.txt";
        String testFile = "test/com/sun/jbi/sapbc/input/I8n.dat";
        String errorFile = "test/com/sun/jbi/sapbc/input/I8n.dat_error";
        
        try {
            if (processedFile.exists()) {
                processedFile.delete();
            }
            if (!new File(testFile).exists()) {
                copyFile(inputFile, testFile);
            }
        } catch (Exception e) {
            fail("Failed to create input file to test execute for InOnly message exchanges: " + e.getMessage());
        }
        
        // Testing the following scenario
        // 1. file name is pattern
        // 2. successful "normalization"
        // 3. successful msgexchange.send
        //sapMessage.setFileName("Input%u.txt");
        //sapMessage.setPollingInterval(new Long(5000));
        wsdlInput.setSAPMessage(sapMessage);
        
        deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory").will(returnValue(msgExchangeFactory.proxy()));
        msgExchangeFactory.expects(atLeastOnce()).method("createInOnlyExchange").will(returnValue((InOnly)msgExchange.proxy()));
        componentContext.expects(atLeastOnce()).method("getEndpoint").with(eq(THE_SERVICE), eq(THE_ENDPOINT)).will(returnValue((ServiceEndpoint)serviceEndpoint.proxy()));
        msgExchange.expects(atLeastOnce()).method("getExchangeId");
        //normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(returnValue((NormalizedMessage)normalizedMsg.proxy()));
        //instance.setSAPNormalizer((SAPNormalizer) normalizer.proxy());
        msgExchange.expects(once()).method("setEndpoint").with(isA(ServiceEndpoint.class));
        msgExchange.expects(once()).method("setOperation").with(eq(THE_OPERATION));
        msgExchange.expects(once()).method("setMessage").with(eq(normalizedMsg.proxy()), eq("in"));
        
        deliveryChannel.expects(once()).method("send").with(eq(msgExchange.proxy()));
        endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(once()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpointStatus.expects(once()).method("incrementSentRequests");
        
        try {
            instance.execute("inonly", "test/com/sun/jbi/sapbc/input", "I%dn.dat", Boolean.TRUE, wsdlInput);
            assertTrue(processedFile.exists());
        } catch (Exception e) {
            fail("Failed to test InOnly exchanges due to: " + e.getMessage());
        }
        msgExchange.verify();
        msgExchangeFactory.verify();
        componentContext.verify();
        endpointStatus.verify();
        
        // Testing the following scenario
        // 1. file name is pattern
        // 2. Unsuccessful "normalization"
        try {
            if (processedFile.exists()) {
                processedFile.delete();
            }
            if (!new File(testFile).exists()) {
                copyFile(inputFile, testFile);
            }
        } catch (Exception e) {
            fail("Failed to create input file to test execute for InOnly message exchanges: " + e.getMessage());
        }
        //normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(throwException(new Exception("Failed to normalize")));
        //instance.setSAPNormalizer((SAPNormalizer) normalizer.proxy());
        
        try {
            instance.execute("inonly", "test/com/sun/jbi/sapbc/input", "I%dn.dat", Boolean.TRUE, wsdlInput);
            fail("Failed to test in-only message exchange when an expected exception should be caught");
        } catch (Exception e) {
            System.out.println("Caught the expected exception!!");
        }
        msgExchange.verify();
        msgExchangeFactory.verify();
        componentContext.verify();
        endpointStatus.verify();
        
        // clean up 
        if (new File(testFile).exists()) {
            new File(testFile).delete();
        }
        
        if (new File(errorFile).exists()) {
            new File(errorFile).delete();
        }
        
        if (processedFile.exists()) {
            processedFile.delete();
        }
        
        System.out.println("Successfully tested in-only message exchange case 2");
    }
*/    
    
    /**
     * Test of execute method, of class com.sun.jbi.sapbc.InboundMessageProcessor
     * for the scenario where the message exchange type is inout.
     */
    /* TODO: Modify to work with SAP
    public void testExecuteInOutExchangeCase1 () {
        System.out.println("Testing in-out message exchange case 1...");
        
        WSDLInput wsdlInput = new WSDLInput();
        SAPMessage sapMessage = new SAPMessage();
        msgExchange = mock(InOut.class);
        File processedFile = new File("test/com/sun/jbi/sapbc/input/TestInputInOut.txt_processed");
        String inputFile = "test/com/sun/jbi/sapbc/input/InputInOut.txt";
        String testFile = "test/com/sun/jbi/sapbc/input/TestInputInOut.txt";
        String errorFile = "test/com/sun/jbi/sapbc/input/TestInputInOut.txt_error";
        
        try {
            if (processedFile.exists()) {
                processedFile.delete();
            }
            if (!new File(testFile).exists()) {
                copyFile(inputFile, testFile);
            }
        } catch (Exception e) {
            fail("Failed to create input file to test execute for InOut message exchanges: " + e.getMessage());
        }
        
        // Testing the following scenario
        // 1. no file name match pattern
        // 2. successful "normalization"
        // 3. successful msgexchange.send
        
        //sapMessage.setFileName("TestInputInOut.txt");
        //sapMessage.setPollingInterval(new Long(5000));
        wsdlInput.setSAPMessage(sapMessage);
        
        deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory").will(returnValue(msgExchangeFactory.proxy()));
        msgExchangeFactory.expects(atLeastOnce()).method("createInOutExchange").will(returnValue((InOut)msgExchange.proxy()));
        componentContext.expects(atLeastOnce()).method("getEndpoint").with(eq(THE_SERVICE), eq(THE_ENDPOINT)).will(returnValue((ServiceEndpoint)serviceEndpoint.proxy()));
        msgExchange.expects(atLeastOnce()).method("getExchangeId");
        //normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(returnValue((NormalizedMessage)normalizedMsg.proxy()));
        //instance.setSAPNormalizer((SAPNormalizer) normalizer.proxy());
        msgExchange.expects(once()).method("setEndpoint").with(isA(ServiceEndpoint.class));
        msgExchange.expects(once()).method("setOperation").with(eq(THE_OPERATION));
        msgExchange.expects(once()).method("setMessage").with(eq(normalizedMsg.proxy()), eq("in"));
        
        deliveryChannel.expects(once()).method("send").with(eq(msgExchange.proxy()));
        endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(once()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpointStatus.expects(once()).method("incrementSentRequests");

        try {
            instance.execute("inout", "test/com/sun/jbi/sapbc/input", "TestInputInOut.txt", Boolean.FALSE, wsdlInput);
            assertTrue(processedFile.exists());
        } catch (Exception e) {
            fail("Failed to test InOnly exchanges due to: " + e.getMessage());
        }
        msgExchange.verify();
        msgExchangeFactory.verify();
        componentContext.verify();
        endpointStatus.verify();
        
        // Testing the following scenario
        // 1. no file name match pattern
        // 2. Unsuccessful "normalization"
        try {
            if (processedFile.exists()) {
                processedFile.delete();
            }
            if (!new File(testFile).exists()) {
                copyFile(inputFile, testFile);
            }
        } catch (Exception e) {
            fail("Failed to create input file to test execute for InOnly message exchanges: " + e.getMessage());
        }
        //normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(throwException(new Exception("Failed to normalize")));
        //instance.setSAPNormalizer((SAPNormalizer) normalizer.proxy());
        
        try {
            instance.execute("inout", "test/com/sun/jbi/sapbc/input", "TestInputInOut.txt", Boolean.FALSE, wsdlInput);
            fail("Failed to test in-out message exchange when an expected exception should be caught");
        } catch (Exception e) {
            System.out.println("Caught the expected exception!!");
        }
        msgExchange.verify();
        msgExchangeFactory.verify();
        componentContext.verify();
        endpointStatus.verify();
        
        // clean up 
       if (new File(testFile).exists()) {
            new File(testFile).delete();
        }
        
        if (new File(errorFile).exists()) {
            new File(errorFile).delete();
        }
        
        if (processedFile.exists()) {
            processedFile.delete();
        }
        
        System.out.println("Successfully tested in-out message exchange case 1");
    }
*/
    /**
     * Test of execute method, of class com.sun.jbi.sapbc.InboundMessageProcessor
     * for the scenario where the message exchange type is inout.
     */
    /* TODO: Modify to work with SAP
    public void testExecuteInOutExchangeCase2() {
        System.out.println("Testing in-out message exchange case 2...");
        
        WSDLInput wsdlInput = new WSDLInput();
        SAPMessage sapMessage = new SAPMessage();
        msgExchange = mock(InOut.class);
        File processedFile = new File("test/com/sun/jbi/sapbc/input/I8n.dat_processed");
        String inputFile = "test/com/sun/jbi/sapbc/input/InputInOut.txt";
        String testFile = "test/com/sun/jbi/sapbc/input/I8n.dat";
        String errorFile = "test/com/sun/jbi/sapbc/input/I8n.dat_error";
        
        try {
            if (processedFile.exists()) {
                processedFile.delete();
            }
            if (!new File(testFile).exists()) {
                copyFile(inputFile, testFile);
            }
        } catch (Exception e) {
            fail("Failed to create input file to test execute for InOut message exchanges: " + e.getMessage());
        }
        
        // Testing the following scenario
        // 1. no file name match pattern
        // 2. successful "normalization"
        // 3. successful msgexchange.send
        
        //sapMessage.setFileName("[A-Z][0-9]n.[a-e]a[t-z]");
        //sapMessage.setPollingInterval(new Long(5000));
        wsdlInput.setSAPMessage(sapMessage);
        
        deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory").will(returnValue(msgExchangeFactory.proxy()));
        msgExchangeFactory.expects(atLeastOnce()).method("createInOutExchange").will(returnValue((InOut)msgExchange.proxy()));
        componentContext.expects(atLeastOnce()).method("getEndpoint").with(eq(THE_SERVICE), eq(THE_ENDPOINT)).will(returnValue((ServiceEndpoint)serviceEndpoint.proxy()));
        msgExchange.expects(atLeastOnce()).method("getExchangeId");
        //normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(returnValue((NormalizedMessage)normalizedMsg.proxy()));
        //instance.setSAPNormalizer((SAPNormalizer) normalizer.proxy());
        msgExchange.expects(once()).method("setEndpoint").with(isA(ServiceEndpoint.class));
        msgExchange.expects(once()).method("setOperation").with(eq(THE_OPERATION));
        msgExchange.expects(once()).method("setMessage").with(eq(normalizedMsg.proxy()), eq("in"));
        
        deliveryChannel.expects(once()).method("send").with(eq(msgExchange.proxy()));
        endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(once()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpointStatus.expects(once()).method("incrementSentRequests");

        try {
            instance.execute("inout", "test/com/sun/jbi/sapbc/input", "I%dn.dat", Boolean.TRUE, wsdlInput);
            assertTrue(processedFile.exists());
        } catch (Exception e) {
            fail("Failed to test InOnly exchanges due to: " + e.getMessage());
        }
        msgExchange.verify();
        msgExchangeFactory.verify();
        componentContext.verify();
        endpointStatus.verify();
        
        // Testing the following scenario
        // 1. no file name match pattern
        // 2. Unsuccessful "normalization"
        try {
            if (processedFile.exists()) {
                processedFile.delete();
            }
            if (!new File(testFile).exists()) {
                copyFile(inputFile, testFile);
            }
        } catch (Exception e) {
            fail("Failed to create input file to test execute for InOnly message exchanges: " + e.getMessage());
        }
        //normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(throwException(new Exception("Failed to normalize")));
        //instance.setSAPNormalizer((SAPNormalizer) normalizer.proxy());
        
        try {
            instance.execute("inout", "test/com/sun/jbi/sapbc/input", "I%dn.dat", Boolean.TRUE, wsdlInput);
            fail("Failed to test in-out message exchange when an expected exception should be caught");
        } catch (Exception e) {
            System.out.println("Caught the expected exception!!");
        }
        msgExchange.verify();
        msgExchangeFactory.verify();
        componentContext.verify();
        endpointStatus.verify();
        
        // clean up 
        if (new File(testFile).exists()) {
            new File(testFile).delete();
        }
        
        if (new File(errorFile).exists()) {
            new File(errorFile).delete();
        }
        
        if (processedFile.exists()) {
            processedFile.delete();
        }

        System.out.println("Successfully tested in-out message exchange case 2");
    }
*/
    
    /**
     * Test of processReplyMessage method, of class com.sun.jbi.sapbc.InboundMessageProcessor.
     */
    /*
    public void testProcessReplyMessage() {
        System.out.println("Testing processReplyMessage");
        
        Map inboundReplys = new HashMap();
        Map Ids = new HashMap();
        
        msgExchange = mock(MessageExchange.class);
        msgExchange.stubs().method("getPattern");
    
        // testing invalid message exchange
        try {
            instance.processReplyMessage((MessageExchange)msgExchange.proxy());
            fail("Failed to test processReplyMessage when an validation exception should be caught.");
        } catch (Exception e) {
            System.out.println("Successfully tested processReply when the MessageExchange is neither in-out nor in-only");
        }
        
        // testing no exchangeId found
        msgExchange = mock(InOut.class);
        msgExchange.expects(once()).method("getExchangeId").will(returnValue("789"));
        Ids.put("123", "");
        instance.setInboundExchangeIds(Ids);
        try {
            instance.processReplyMessage((InOut)msgExchange.proxy());
            System.out.println("Successfully tested processReply when message exchange ID is not found");
        } catch (Exception e) {
            fail("Failed to test processReplyMessage when message exchange ID is not found.");
        }
        
        // todo fill this in when methods developed
        // testing exchange Id is found and status is DONE
        msgExchange.expects(once()).method("getExchangeId").will(returnValue("123"));
        msgExchange.expects(once()).method("getStatus").will(returnValue(ExchangeStatus.DONE));
        try {
            instance.processReplyMessage((InOut)msgExchange.proxy());
            Map inboundIds = instance.getInboundExchanges();
            assertTrue(!inboundIds.containsKey("123"));
        } catch (Exception e) {
            fail("Failed to test processReplyMessage when message exchange status is DONE.");
        }
        
        // testing exchang Id is found but status is not DONE
        String origFile = "test/com/sun/jbi/sapbc/input/InputInOnly.txt";
        String inputFile = "test/com/sun/jbi/sapbc/input/TestInputInOnly.txt";
        String processedFile = "test/com/sun/jbi/sapbc/input/TestInputInOnly.txt_processed";
        String errorFile = "test/com/sun/jbi/sapbc/input/TestInputInOnly.txt_error";
        try {
            if (new File(errorFile).exists()) {
                new File(errorFile).delete();
            }
            if (!new File(processedFile).exists()) {
                copyFile(origFile, inputFile);
                copyFile(origFile, processedFile);
            }
        } catch (Exception e) {
            fail("Failed to create a processed file to properly test processReplyMessage method: " + e.getMessage());
        }
        
        inboundReplys.put("123", new File(inputFile));
        instance.setInboundReplyIds(inboundReplys);
        Ids.put("123", "");
        instance.setInboundExchangeIds(Ids);
        msgExchange.expects(once()).method("getExchangeId").will(returnValue("123"));
        msgExchange.expects(once()).method("getStatus").will(returnValue(ExchangeStatus.ERROR));
        try {
            instance.processReplyMessage((InOut)msgExchange.proxy());
            Map inboundIds = instance.getInboundExchanges();
            assertTrue(!inboundIds.containsKey("123"));
            assertTrue(new File(errorFile).exists());
            System.out.println("Successfully tested processReply when message exchange status is not DONE.");
        } catch (Exception e) {
            fail("Failed to test processReplyMessage when message exchange status is not DONE.");
        }
        
        // clean up 
        if (new File(processedFile).exists()) {
            new File(processedFile).delete();
        }
        
        if (new File(inputFile).exists()) {
            new File(inputFile).delete();
        }
        
        if (new File(errorFile).exists()) {
            new File(errorFile).delete();
        }
        
        System.out.println("Successfully tested processReplyMessage");
    }
*/
    
    private String copyFile(String fromFile, String toFile) {
        StringBuffer output = new StringBuffer();
        try {
            java.io.FileReader reader = new java.io.FileReader(fromFile);
            char[] buff = new char[512];
            int len = reader.read(buff);
            int length = len;
            while (len > 0) {
                output.append(buff, 0, len);
                len = reader.read(buff);
            }
            java.io.FileWriter writer = new java.io.FileWriter(toFile);
            writer.write(buff, 0, length);
            writer.flush();
            writer.close();            
        } catch (Exception e) {
            System.err.println("Failed to copy " + fromFile +  " to " + toFile);
        }
         
        return output.toString();
    }
}
