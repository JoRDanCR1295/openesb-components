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

package com.sun.jbi.hl7bc;

import junit.framework.*;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.hl7bc.Endpoint.EndpointType;
import com.sun.jbi.hl7bc.Endpoint.EndpointState;
import com.sun.jbi.hl7bc.extensions.HL7Address;
import com.sun.jbi.hl7bc.extensions.HL7Binding;
import com.sun.jbi.hl7bc.extensions.HL7Operation;
import com.sun.jbi.hl7bc.extensions.HL7Input;
import com.sun.jbi.hl7bc.extensions.HL7Output;
import com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;
import com.ibm.wsdl.DefinitionImpl;
import org.w3c.dom.Document;
import org.jmock.*;

/**
 * @author Raghunadh
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
     * Test of setServiceName and getServiceName method, of class com.sun.jbi.hl7bc.EndpointImpl.
     */
    public void testSetGetServiceName() {
        System.out.println("Testing setServiceName and getServiceName");

        QName expResult = new QName("http://my-hl7bc-test/mynamespace", "myHL7Service");
        instance.setServiceName(new QName("http://my-hl7bc-test/mynamespace", "myHL7Service"));
        QName result = instance.getServiceName();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setServiceName and getServiceName");
    }

    /**
     * Test of setEndpointName and getEndpointName method, of class com.sun.jbi.hl7bc.EndpointImpl.
     */
    public void testSetGetEndpointName() {
        System.out.println("Testing setEndpointName and getEndpointName");

        String expResult = "myHL7TestPort";
        instance.setEndpointName(expResult);
        String result = instance.getEndpointName();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setEndpointName and getEndpointName");
    }

    /**
     * Test of setDefinition and getDefinition method, of class com.sun.jbi.hl7bc.EndpointImpl.
     */
    public void testSetGetDefinition() {
        System.out.println("Testing setDefinition and getDefinition");

		 
        QName definitionQName = new QName("serviceDefinition");
        Mock definitionMock = mock (Definition.class);
        definitionMock.stubs().method("getQName").will(returnValue(definitionQName));
        Definition expResult = (Definition)definitionMock.proxy();
        instance.setDefinition(expResult);
        Definition result = instance.getDefinition();
        assertEquals(expResult.getQName(), result.getQName());        
        
		System.out.println("Successfully tested setDefinition and getDefinition");
    }

    /**
     * Test of setState and getState method, of class com.sun.jbi.hl7bc.EndpointImpl.
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
     * com.sun.jbi.hl7bc.EndpointImpl.
     */
    public void testSetGetEndpointStatus() {
        System.out.println("Testing setEndpointStatus and getEndpointStatus");

		Mock endpointStatusMock = mock (EndpointStatus.class);        
        EndpointStatus expResult = (EndpointStatus)endpointStatusMock.proxy();
        instance.setEndpointStatus(expResult);
        EndpointStatus result = instance.getEndpointStatus();
        assertEquals(expResult, result);       

		System.out.println("Successfully tested setEndpointStatus and getEndpointStatus");
    }

    /**
     * Test of setEndpointType and getEndpointType method, of class com.sun.jbi.hl7bc.EndpointImpl.
     */
    public void testSetGetEndpointType() {
        System.out.println("Testing setEndpointType and getEndpointType");

        int expResult = Endpoint.EndpointType.OUTBOUND;
        instance.setEndpointType(expResult);
        int result = instance.getEndpointType();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setEndpointType and getEndpointType");
    }

    /**
     * Test of setServiceEndpoint and getServiceEndpoint method, of class
     * com.sun.jbi.hl7bc.EndpointImpl.
     */
    public void testSetGetServiceEndpoint() {
        System.out.println("Testing setServiceEndpoint and getServiceEndpoint");

        Mock serviceEndpointMock = mock(ServiceEndpoint.class);
		ServiceEndpoint expResult = (ServiceEndpoint)serviceEndpointMock.proxy();
        instance.setServiceEndpoint(expResult);
        ServiceEndpoint result = instance.getServiceEndpoint();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setServiceEndpoint and getServiceEndpoint");
    }

    /**
     * Test of setServiceDescription and getServiceDescription method, of class
     * com.sun.jbi.hl7bc.EndpointImpl.
     */
    public void testSetServiceDescription() {
        System.out.println("Testing setServiceDescription and getServiceDescription");

        Mock domMock = mock(Document.class);
		Document expResult = (Document)domMock.proxy();
        instance.setServiceDescription(expResult);
        Document result = instance.getServiceDescription();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setServiceDescription and getServiceDescription");
    }

    /**
     * Test of setHL7Address and getHL7Address method, of class com.sun.jbi.hl7bc.EndpointImpl.
     */
    public void testSetGetHL7Address() {
        System.out.println("Testing setHL7Address and getHL7Address");

		String locationURL = "hl7://localhost:4040";
		HL7Address expResult = new HL7Address();
		expResult.setHL7ServerLocationURL(locationURL);
        instance.setHL7Address(expResult);
        HL7Address result = instance.getHL7Address();
		assertEquals(expResult.getHL7ServerLocationURL(), result.getHL7ServerLocationURL());
	

        System.out.println("Successfully tested setHL7Address and getHL7Address");
    }

    /**
     * Test of setHL7ProtocolProperties and getHL7ProtocolProperties method, of class
     * com.sun.jbi.hl7bc.EndpointImpl.
     */
    public void testSetGetHL7ProtocolProperties() {
        System.out.println("Testing setHL7ProtocolProperties and getHL7ProtocolProperties");

		HL7ProtocolProperties expResult = new HL7ProtocolProperties();
		expResult.setStartBlockChar(new Byte("24"));
        instance.setHL7ProtocolProperties(expResult);
        HL7ProtocolProperties result = instance.getHL7ProtocolProperties();
        assertTrue(result instanceof HL7ProtocolProperties);
		assertEquals(expResult.getStartBlockChar().byteValue(), result.getStartBlockChar().byteValue());

        System.out.println("Successfully tested HL7ProtocolProperties and getHL7ProtocolProperties");
    }

    /**
     * Test of setHL7Binding and getHL7Binding method, of class com.sun.jbi.hl7bc.EndpointImpl.
     */
    public void testSetGetHL7Binding() {
        System.out.println("Testing setHL7Binding and getHL7Binding");

        instance.setHL7Binding(new HL7Binding());
        HL7Binding result = instance.getHL7Binding();
        assertTrue(result instanceof HL7Binding);

        System.out.println("Successfully tested setHL7Binding and getHL7Binding");
    }

    /**
     * Test of setHL7Operations and getHL7Operations method, of class
     * com.sun.jbi.hl7bc.EndpointImpl.
     */
    public void testSetGetHL7Operations() {
        System.out.println("Testing setHL7Operations and getHL7Operations");

        Map val = new HashMap();
        val.put(new QName("http://some-url", "operation1"), new HL7Operation());
        val.put(new QName("http://some-url", "operation2"), new HL7Operation());
        instance.setHL7Operations(val);
        Map result = instance.getHL7Operations();
        assertTrue(result instanceof Map);
        assertEquals(val, result);

        System.out.println("Successfully tested setHL7Operations and getHL7Operations");
    }

    /**
     * Test of setHL7OperationInput and getHL7OperationInput method, of class
     * com.sun.jbi.hl7bc.EndpointImpl.
     */
    public void testSetGetHL7OperationInput() {
        System.out.println("Testing setHL7OperationInput and getHL7OperationInput");

        HL7Operation hl7OP = new HL7Operation();
        HL7Input hl7In = new HL7Input();
        instance.setHL7OperationInput(hl7OP, hl7In);
        HL7Input result = (HL7Input) instance.getHL7OperationInput(hl7OP);

        assertEquals(hl7In, result);

        System.out.println("Successfully tested setHL7OperationInput and getHL7OperationInput");
    }

	  /**
     * Test of setHL7OperationOutput and getHL7OperationOutput method, of class
     * com.sun.jbi.hl7bc.EndpointImpl.
     */
    public void testSetGetHL7OperationOutput() {
        System.out.println("Testing setHL7OperationOutput and getHL7OperationOutput");

        HL7Operation hl7OP = new HL7Operation();
        HL7Output hl7Out = new HL7Output();
        instance.setHL7OperationOutput(hl7OP, hl7Out);
        HL7Output result = (HL7Output) instance.getHL7OperationOutput(hl7OP);

        assertEquals(hl7Out, result);

        System.out.println("Successfully tested setHL7OperationOutput and getHL7OperationOutput");
    }

    /**
     * Test of setOperationMsgExchangePattern and getOperationMsgExchangePattern method, of class
     * com.sun.jbi.hl7bc.EndpointImpl.
     */
    public void testSetGetOperationMsgExchangePattern() {
        System.out.println("Testing setOperationMsgExchangePattern and getOperationMsgExchangePattern");

        Map val = new HashMap();
        val.put(QName.valueOf("operation1"), "inonly");
        val.put(QName.valueOf("operation2"), "inout");

        instance.setOperationMsgExchangePattern(val);
        Map result = instance.getOperationMsgExchangePattern();
        assertTrue(result instanceof Map);
        assertEquals(val, result);

        System.out.println("Successfully tested setOperationMsgExchangePattern and getOperationMsgExchangePattern");
    }

    /**
     * Test of setXsdsList and getXsdsList method, of class com.sun.jbi.hl7bc.EndpointImpl.
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
