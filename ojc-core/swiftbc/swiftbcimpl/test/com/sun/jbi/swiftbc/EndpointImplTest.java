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
 * @(#)EndpointImplTest.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.swiftbc;

import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.swiftbc.extensions.SwiftAddress;
import com.sun.jbi.swiftbc.extensions.SwiftBinding;
import com.sun.jbi.swiftbc.extensions.SwiftInput;
import com.sun.jbi.swiftbc.extensions.SwiftOperation;
import com.sun.jbi.swiftbc.extensions.SwiftOutput;
import com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties;

import junit.framework.*;

import org.jmock.*;

import org.w3c.dom.Document;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jbi.servicedesc.ServiceEndpoint;

import javax.wsdl.Definition;

import javax.xml.namespace.QName;


/**
 * @author Sun Microsystems, Inc.
 */
public class EndpointImplTest extends MockObjectTestCase {
    EndpointImpl instance = null;

    public EndpointImplTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new EndpointImpl();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(EndpointImplTest.class);

        return suite;
    }

    /**
     * Test of setServiceName and getServiceName method, of class com.sun.jbi.swiftbc.EndpointImpl.
     */
    public void testSetGetServiceName() {
        System.out.println("Testing setServiceName and getServiceName");

        QName expResult = new QName("http://my-swiftbc-test/mynamespace",
                "mySwiftService");
        instance.setServiceName(new QName(
                "http://my-swiftbc-test/mynamespace", "mySwiftService"));

        QName result = instance.getServiceName();
        assertEquals(expResult, result);

        System.out.println(
            "Successfully tested setServiceName and getServiceName");
    }

    /**
     * Test of setEndpointName and getEndpointName method, of class com.sun.jbi.swiftbc.EndpointImpl.
     */
    public void testSetGetEndpointName() {
        System.out.println("Testing setEndpointName and getEndpointName");

        String expResult = "mySwiftTestPort";
        instance.setEndpointName(expResult);

        String result = instance.getEndpointName();
        assertEquals(expResult, result);

        System.out.println(
            "Successfully tested setEndpointName and getEndpointName");
    }

    /**
     * Test of setDefinition and getDefinition method, of class com.sun.jbi.swiftbc.EndpointImpl.
     */
    public void testSetGetDefinition() {
        System.out.println("Testing setDefinition and getDefinition");

        QName definitionQName = new QName("serviceDefinition");
        Mock definitionMock = mock(Definition.class);
        definitionMock.stubs().method("getQName")
                      .will(returnValue(definitionQName));

        Definition expResult = (Definition) definitionMock.proxy();
        instance.setDefinition(expResult);

        Definition result = instance.getDefinition();
        assertEquals(expResult.getQName(), result.getQName());

        System.out.println(
            "Successfully tested setDefinition and getDefinition");
    }

    /**
     * Test of setState and getState method, of class com.sun.jbi.swiftbc.EndpointImpl.
     */
    public void testSetGetState() {
        System.out.println("Testing setState and getState");

        int expResult = Endpoint.EndpointState.SHUTDOWN;
        instance.setState(expResult);

        int result = instance.getState();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setState and getState");
    }

    /**
     * Test of setEndpointStatus and getEndpointStatus method, of class
     * com.sun.jbi.swiftbc.EndpointImpl.
     */
    public void testSetGetEndpointStatus() {
        System.out.println("Testing setEndpointStatus and getEndpointStatus");

        Mock endpointStatusMock = mock(EndpointStatus.class);
        EndpointStatus expResult = (EndpointStatus) endpointStatusMock.proxy();
        instance.setEndpointStatus(expResult);

        EndpointStatus result = instance.getEndpointStatus();
        assertEquals(expResult, result);

        System.out.println(
            "Successfully tested setEndpointStatus and getEndpointStatus");
    }

    /**
     * Test of setEndpointType and getEndpointType method, of class com.sun.jbi.swiftbc.EndpointImpl.
     */
    public void testSetGetEndpointType() {
        System.out.println("Testing setEndpointType and getEndpointType");

        int expResult = Endpoint.EndpointType.OUTBOUND;
        instance.setEndpointType(expResult);

        int result = instance.getEndpointType();
        assertEquals(expResult, result);

        System.out.println(
            "Successfully tested setEndpointType and getEndpointType");
    }

    /**
     * Test of setServiceEndpoint and getServiceEndpoint method, of class
     * com.sun.jbi.swiftbc.EndpointImpl.
     */
    public void testSetGetServiceEndpoint() {
        System.out.println("Testing setServiceEndpoint and getServiceEndpoint");

        Mock serviceEndpointMock = mock(ServiceEndpoint.class);
        ServiceEndpoint expResult = (ServiceEndpoint) serviceEndpointMock.proxy();
        instance.setServiceEndpoint(expResult);

        ServiceEndpoint result = instance.getServiceEndpoint();
        assertEquals(expResult, result);

        System.out.println(
            "Successfully tested setServiceEndpoint and getServiceEndpoint");
    }

    /**
     * Test of setServiceDescription and getServiceDescription method, of class
     * com.sun.jbi.swiftbc.EndpointImpl.
     */
    public void testSetServiceDescription() {
        System.out.println(
            "Testing setServiceDescription and getServiceDescription");

        Mock domMock = mock(Document.class);
        Document expResult = (Document) domMock.proxy();
        instance.setServiceDescription(expResult);

        Document result = instance.getServiceDescription();
        assertEquals(expResult, result);

        System.out.println(
            "Successfully tested setServiceDescription and getServiceDescription");
    }

    /**
     * Test of setSwiftAddress and getSwiftAddress method, of class com.sun.jbi.swiftbc.EndpointImpl.
     */
    public void testSetGetSwiftAddress() {
        System.out.println("Testing setSwiftAddress and getSwiftAddress");

        String locationURL = "Swift://localhost:4040";
        SwiftAddress expResult = new SwiftAddress();
        expResult.setSwiftServerLocationURL(locationURL);
        instance.setSwiftAddress(expResult);

        SwiftAddress result = instance.getSwiftAddress();
        assertEquals(expResult.getSwiftServerLocationURL(),
            result.getSwiftServerLocationURL());

        System.out.println(
            "Successfully tested setSwiftAddress and getSwiftAddress");
    }

    /**
     * Test of setSwiftProtocolProperties and getSwiftProtocolProperties method, of class
     * com.sun.jbi.swiftbc.EndpointImpl.
     */
    public void testSetGetSwiftProtocolProperties() {
        System.out.println(
            "Testing setSwiftProtocolProperties and getSwiftProtocolProperties");

        SwiftProtocolProperties expResult = new SwiftProtocolProperties();
        instance.setSwiftProtocolProperties(expResult);

        SwiftProtocolProperties result = instance.getSwiftProtocolProperties();
        assertTrue(result instanceof SwiftProtocolProperties);
     
        System.out.println(
            "Successfully tested SwiftProtocolProperties and getSwiftProtocolProperties");
    }

    /**
     * Test of setSwiftBinding and getSwiftBinding method, of class com.sun.jbi.swiftbc.EndpointImpl.
     */
    public void testSetGetSwiftBinding() {
        System.out.println("Testing setSwiftBinding and getSwiftBinding");

        instance.setSwiftBinding(new SwiftBinding());

        SwiftBinding result = instance.getSwiftBinding();
        assertTrue(result instanceof SwiftBinding);

        System.out.println(
            "Successfully tested setSwiftBinding and getSwiftBinding");
    }

    /**
     * Test of setSwiftOperations and getSwiftOperations method, of class
     * com.sun.jbi.swiftbc.EndpointImpl.
     */
    public void testSetGetSwiftOperations() {
        System.out.println("Testing setSwiftOperations and getSwiftOperations");

        Map val = new HashMap();
        val.put(new QName("http://some-url", "operation1"), new SwiftOperation());
        val.put(new QName("http://some-url", "operation2"), new SwiftOperation());
        instance.setSwiftOperations(val);

        Map result = instance.getSwiftOperations();
        assertTrue(result instanceof Map);
        assertEquals(val, result);

        System.out.println(
            "Successfully tested setSwiftOperations and getSwiftOperations");
    }

    /**
     * Test of setSwiftOperationInput and getSwiftOperationInput method, of class
     * com.sun.jbi.swiftbc.EndpointImpl.
     */
    public void testSetGetSwiftOperationInput() {
        System.out.println(
            "Testing setSwiftOperationInput and getSwiftOperationInput");

        SwiftOperation swiftOP = new SwiftOperation();
        SwiftInput swiftIn = new SwiftInput();
        instance.setSwiftOperationInput(swiftOP, swiftIn);

        SwiftInput result = (SwiftInput) instance.getSwiftOperationInput(swiftOP);

        assertEquals(swiftIn, result);

        System.out.println(
            "Successfully tested setSwiftOperationInput and getSwiftOperationInput");
    }

    /**
    * Test of setSwiftOperationOutput and getSwiftOperationOutput method, of class
    * com.sun.jbi.swiftbc.EndpointImpl.
    */
    public void testSetGetSwiftOperationOutput() {
        System.out.println(
            "Testing setSwiftOperationOutput and getSwiftOperationOutput");

        SwiftOperation swiftOP = new SwiftOperation();
        SwiftOutput swiftOut = new SwiftOutput();
        instance.setSwiftOperationOutput(swiftOP, swiftOut);

        SwiftOutput result = (SwiftOutput) instance.getSwiftOperationOutput(swiftOP);

        assertEquals(swiftOut, result);

        System.out.println(
            "Successfully tested setSwiftOperationOutput and getSwiftOperationOutput");
    }

    /**
     * Test of setOperationMsgExchangePattern and getOperationMsgExchangePattern method, of class
     * com.sun.jbi.swiftbc.EndpointImpl.
     */
    public void testSetGetOperationMsgExchangePattern() {
        System.out.println(
            "Testing setOperationMsgExchangePattern and getOperationMsgExchangePattern");

        Map val = new HashMap();
        val.put(QName.valueOf("operation1"), "inonly");
        val.put(QName.valueOf("operation2"), "inout");

        instance.setOperationMsgExchangePattern(val);

        Map result = instance.getOperationMsgExchangePattern();
        assertTrue(result instanceof Map);
        assertEquals(val, result);

        System.out.println(
            "Successfully tested setOperationMsgExchangePattern and getOperationMsgExchangePattern");
    }

    /**
     * Test of setXsdsList and getXsdsList method, of class com.sun.jbi.swiftbc.EndpointImpl.
     */
    public void testSetGetXsdsList() {
        System.out.println("Testing setXsdsList and getXsdsList");

        List xsds = new ArrayList();
        xsds.add("ACK.xsd");
        xsds.add("ADT_A49.xsd");
        instance.setXsdsList(xsds);

        List result = instance.getXsdsList();

        assertEquals(xsds, result);

        System.out.println("Successfully tested setXsdsList and getXsdsList");
    }
}
