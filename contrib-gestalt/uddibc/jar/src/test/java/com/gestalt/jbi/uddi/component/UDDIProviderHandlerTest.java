/**
 *   uddi-binding-component - UDDI Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com/
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.uddi.component;

import com.gestalt.jbi.component.handler.AbstractMessageExchangeHandler;
import com.gestalt.jbi.component.manager.ServiceUnit;
import com.gestalt.jbi.mock.javax.jbi.component.MockComponentContext;
import com.gestalt.jbi.mock.javax.jbi.component.MockComponentContextBasic;
import com.gestalt.jbi.mock.javax.jbi.messaging.MockDeliveryChannel;
import com.gestalt.jbi.mock.javax.jbi.messaging.MockDeliveryChannelBasic;
import com.gestalt.jbi.mock.javax.jbi.messaging.MockInOutBasic;
import com.gestalt.jbi.mock.javax.jbi.messaging.MockMessageExchangeFactoryBasic;
import com.gestalt.jbi.mock.javax.jbi.messaging.MockNormalizedMessageBasic;
import com.gestalt.jbi.uddi.component.uddi.UDDIUtils;
import com.gestalt.jbi.uddi.extensions.UDDIAddress;
import com.gestalt.jbi.uddi.extensions.UDDIBinding;
import com.gestalt.jbi.uddi.extensions.UDDIExtensionRegistry;
import com.gestalt.jbi.uddi.extensions.UDDIOperation;
import com.gestalt.jbi.uddi.extensions.UDDIOperationInput;
import com.gestalt.jbi.uddi.extensions.UDDIOperationOutput;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.uddi4j.UDDIException;

import org.uddi4j.client.UDDIProxy;

import org.uddi4j.transport.TransportException;

import org.w3c.dom.Document;

import org.xml.sax.EntityResolver;
import org.xml.sax.SAXException;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;


/**
 * Currently suspending until we can figure out how to write a unit test against
 * a Handler. Author: cgallemore Date: Apr 16, 2007
 */
public class UDDIProviderHandlerTest extends TestCase {
    UDDIProviderHandler handler;
    MockUddiEndpoint uddiEndpoint;
    MockUddiUtils uddiUtils;

    public UDDIProviderHandlerTest(String whichTest) {
        super(whichTest);
    }

    /**
     * Used by JUnit to determine which tests to run.
     *
     * @return Test suite to be run by JUnit
     */
    public static Test suite() {
        TestSuite suite = new TestSuite();

        // suite.addTest(new UDDIProviderHandlerTest("testProcessMessage"));
        return suite;
    }

    public void setUp() throws Exception {
        // ServiceUnit mockServiceUnit = new ServiceUnit("mock-uddi", null, null);
        // CatalogManager cm = new CatalogManager();
        // File wsdlFile = new File("src/test/resources/uddiTestWsdl.wsdl");
        // JBIDescriptorParser parser = new JBIDescriptorParser("src/test/resources");
        // Definition def = readWSDL(wsdlFile);
        uddiUtils = new MockUddiUtils();
        handler = new UDDIProviderHandler(uddiEndpoint);
        handler.setUddiUtils(uddiUtils);
    }

    protected final Document createServiceDescription(File wsdl)
        throws ParserConfigurationException, SAXException, IOException {
        final DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
        final DocumentBuilder documentBuilder = docBuilderFactory.newDocumentBuilder();

        return documentBuilder.parse(wsdl);
    }

    public void testProcessMessage() throws FileNotFoundException {
        // UDDIOperation uddiOperation = new UDDIOperation();
        // UDDIOperationInput uddiOperationInput = new UDDIOperationInput();
        // uddiOperationInput.setBusinessName("Microsoft DRMS Dev");
        // uddiOperationInput.setServiceName("Certification");
        // uddiOperation.setUDDIOperationInput(uddiOperationInput);
        // Map<QName, UDDIOperation> uddiOperations = new HashMap<QName,
        // UDDIOperation>();
        // uddiOperations.put(operationName, uddiOperation);
        // uddiEndpoint.setUDDIOperations(uddiOperations);
        // uddiEndpoint.setUDDIOperationInput(uddiEndpoint.getUDDIOperations().get(operationName),
        // uddiOperationInput);
        try {
            MockMessageExchangeFactoryBasic mockExchangeFactory = new MockMessageExchangeFactoryBasic();
            MockInOutBasic mockInOut = new MockInOutBasic();

            mockInOut.setNormalizedMessage(new MockNormalizedMessageBasic());
            mockExchangeFactory.setInOut(mockInOut);

            MockDeliveryChannel mockDC = new MockDeliveryChannelBasic();
            mockDC.setMessageExchangeFactory(mockExchangeFactory);

            MockComponentContext mockContext = new MockComponentContextBasic();
            mockContext.setDeliveryChannel(mockDC);

            InOut exchange1 = mockExchangeFactory.createInOutExchange();

            NormalizedMessage input = new MockNormalizedMessageBasic();
            NormalizedMessage output;

            File file = new File("src/test/resources/soapRequest.xml");
            FileInputStream fileStream = new FileInputStream(file.getAbsolutePath());

            Source testMessageSource = new StreamSource(fileStream);

            input.setContent(testMessageSource);

            exchange1.setInMessage(input);
            handler.setDeliveryChannel(mockDC);
            handler.setMessageExchange(exchange1);
            handler.processMessageExchange();
            output = exchange1.getOutMessage();

            Source testResponse = output.getContent();
            assertTrue("SOAPMaker implementation has changed" +
                "--this test needs to be rewritten\n" + "expected " +
                className(DOMSource.class) + ", got " +
                className(testResponse), testResponse instanceof DOMSource);
        } catch (MessagingException e) {
            e.printStackTrace();
        }
    }

    protected final Definition readWSDL(File file, EntityResolver resolver)
        throws WSDLException {
        final WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        final WSDLReader reader = wsdlFactory.newWSDLReader();
        reader.setExtensionRegistry(new UDDIExtensionRegistry());

        return reader.readWSDL(null, file.getAbsolutePath());
    }

    static String className(Object obj) {
        if (obj == null) {
            return null;
        } else if (obj instanceof Class) {
            return ((Class<?>) obj).getName();
        } else {
            return obj.getClass().getName();
        }
    }

    private class MockUddiUtils extends UDDIUtils {
        public List<String> getBusinessKeys(String businessName, UDDIProxy uddi)
            throws UDDIException, TransportException {
            return null;
        }

        public List<String> lookupService(String serviceName,
            String businessName, UDDIProxy uddi) throws Exception {
            List<String> list = new ArrayList<String>();

            return list;
        }
    }

    private class MockUddiEndpoint extends UDDIEndpoint {
        private UDDIProxy inquiryProxy;
        private UDDIBinding uddiBinding;
        private UDDIAddress uddiAddress;
        private UDDIUtils uddiUtils = new UDDIUtils();
        private Map<QName, UDDIOperation> uddiOperations;
        private Map<UDDIOperation, UDDIOperationInput> uddiOperationInputs = new HashMap<UDDIOperation, UDDIOperationInput>();
        private Map<UDDIOperation, UDDIOperationOutput> uddiOperationOutputs = new HashMap<UDDIOperation, UDDIOperationOutput>();

        public MockUddiEndpoint(QName serviceName, QName interfaceName,
            String endpointName, MessageExchange.Role role,
            Document serviceDescription, Definition def, ServiceUnit serviceUnit) {
            super(serviceName, interfaceName, endpointName, role,
                serviceDescription, def, serviceUnit);
        }

        public void setUDDIBinding(UDDIBinding uddiBinding) {
            this.uddiBinding = uddiBinding;
        }

        public UDDIBinding getUDDIBinding() {
            return uddiBinding;
        }

        public UDDIProxy getInquiryProxy() {
            return inquiryProxy;
        }

        public void setUDDIAddress(UDDIAddress uddiAddress) {
            this.uddiAddress = uddiAddress;
        }

        public UDDIAddress getUDDIAddress() {
            return uddiAddress;
        }

        public void setUDDIOperations(Map<QName, UDDIOperation> uddiOperations) {
            this.uddiOperations = uddiOperations;
        }

        public Map<QName, UDDIOperation> getUDDIOperations() {
            return uddiOperations;
        }

        public void setUDDIOperationOutput(UDDIOperation uddiOperation,
            UDDIOperationOutput uddiOperationOutput) {
            uddiOperationOutputs.put(uddiOperation, uddiOperationOutput);
        }

        public UDDIOperationOutput getUDDIOperationOutput(
            UDDIOperation uddiOperation) {
            return uddiOperationOutputs.get(uddiOperation);
        }

        public void setUDDIOperationInput(UDDIOperation uddiOperation,
            UDDIOperationInput uddiOperationInput) {
            uddiOperationInputs.put(uddiOperation, uddiOperationInput);
        }

        public UDDIOperationInput getUDDIOperationInput(
            UDDIOperation uddiOperation) {
            return uddiOperationInputs.get(uddiOperation);
        }

        public Map<QName, UDDIOperation> getOperations() {
            return uddiOperations;
        }

        public void activate() throws Exception {
            super.activate();
            inquiryProxy = new UDDIProxy();
        }

        public void deactivate() throws Exception {
        }

        public AbstractMessageExchangeHandler createConsumerExchangeHandler() {
            return null;
        }

        public AbstractMessageExchangeHandler createProviderExchangeHandler() {
            UDDIProviderHandler uddiProviderHandler = new UDDIProviderHandler(this);
            uddiProviderHandler.setUddiUtils(uddiUtils);

            return uddiProviderHandler;
        }
    }
}
