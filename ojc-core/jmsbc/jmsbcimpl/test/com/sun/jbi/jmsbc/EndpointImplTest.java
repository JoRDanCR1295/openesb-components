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

package com.sun.jbi.jmsbc;

import junit.framework.*;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;
import org.w3c.dom.Document;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.jmsbc.Endpoint.EndpointType;
import com.sun.jbi.jmsbc.Endpoint.EndpointState;
import com.sun.jbi.jmsbc.extensions.JMSAddress;
import com.sun.jbi.jmsbc.extensions.JMSBinding;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.jmsbc.extensions.JMSInput;
import com.sun.jbi.jmsbc.extensions.JMSOutput;
import com.sun.jbi.jmsbc.extensions.JMSMessage;

import org.jmock.core.*;
import org.jmock.*;

/**
 *
 * JUnit tests for EndpointImpl class
 */
public class EndpointImplTest extends org.jmock.MockObjectTestCase {
    
    public EndpointImplTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(EndpointImplTest.class);
        
        return suite;
    }

    /**
     * Test of setServiceName and getServiceName methods, of class com.sun.jbi.jmsbc.EndpointImpl.
     */
    public void testSetAndGetServiceName() {
        System.out.println("Test setServiceName and getServiceName");
        
        EndpointImpl instance = new EndpointImpl();
        
        QName expResult = new QName("serviceName");
        instance.setServiceName(expResult);
        QName result = instance.getServiceName();
        assertEquals(expResult, result);        
    }

    /**
     * Test of setEndpointName and getEndpointName methods, of class com.sun.jbi.jmsbc.EndpointImpl.
     */
    public void testSetAndGetEndpointName() {
        System.out.println("Test setEndpointName and getEndpointName");
        
        EndpointImpl instance = new EndpointImpl();
        
        String expResult = "endpointName";
        instance.setEndpointName(expResult);
        String result = instance.getEndpointName();
        assertEquals(expResult, result);        
    }

    /**
     * Test of setDefinition and getDefinition methods, of class com.sun.jbi.jmsbc.EndpointImpl.
     */
    public void testSetAndGetDefinition() {
        System.out.println("Test setDefinition and getDefinition");
        
        EndpointImpl instance = new EndpointImpl();
        
        QName definitionQName = new QName("serviceDefinition");
        Mock definitionMock = mock (Definition.class);
        definitionMock.stubs().method("getQName").will(returnValue(definitionQName));
        Definition expResult = (Definition)definitionMock.proxy();
        instance.setDefinition(expResult);
        Definition result = instance.getDefinition();
        assertEquals(expResult.getQName(), result.getQName());        
    }

    /**
     * Test of setState and getState methods, of class com.sun.jbi.jmsbc.EndpointImpl.
     */
    public void testSetAndGetState() {
        System.out.println("TeSt setState and getState");
        
        EndpointImpl instance = new EndpointImpl();
        
        int expResult = Endpoint.EndpointState.RUNNING;
        instance.setState(expResult);
        int result = instance.getState();
        assertEquals(expResult, result);        
    }
    
    /**
     * Test of setEndpointStatus and getEndpointStatus methods, of class com.sun.jbi.jmsbc.EndpointImpl.
     */
    public void testSetAndGetEndpointStatus() {
        System.out.println("Test setEndpointStatus and getEndpointStatus");
        
        EndpointImpl instance = new EndpointImpl();

        Mock endpointStatusMock = mock (EndpointStatus.class);        
        EndpointStatus expResult = (EndpointStatus)endpointStatusMock.proxy();
        instance.setEndpointStatus(expResult);
        EndpointStatus result = instance.getEndpointStatus();
        assertEquals(expResult, result);        
    }

    /**
     * Test of setEndpointType and getEndpointType methods, of class com.sun.jbi.jmsbc.EndpointImpl.
     */
    public void testSetAndGetEndpointType() {
        System.out.println("Test setEndpointType and getEndpointType");
        
        EndpointImpl instance = new EndpointImpl();
        
        int expResult = Endpoint.EndpointType.INBOUND;
        instance.setEndpointType(expResult);
        int result = instance.getEndpointType();
        assertEquals(expResult, result);        
    }


    /**
     * Test of etServiceEndpoint and getServiceEndpoint methods, of class com.sun.jbi.jmsbc.EndpointImpl.
     */
    public void testSetAndGetServiceEndpoint() {
        System.out.println("Test setServiceEndpoint and getServiceEndpoint");
        
        EndpointImpl instance = new EndpointImpl();
        
        Mock serviceEndpointMock = mock(ServiceEndpoint.class);
        ServiceEndpoint expResult = (ServiceEndpoint)serviceEndpointMock.proxy();
        instance.setServiceEndpoint(expResult);
        ServiceEndpoint result = instance.getServiceEndpoint();                
        assertEquals(expResult, result);
    }

    /**
     * Test of setServiceDescription and getServiceDescription methods, of class com.sun.jbi.jmsbc.EndpointImpl.
     */
    public void testSetAndGetServiceDescription() {
        System.out.println("Test setServiceDescription and getServiceDescription");
        
        EndpointImpl instance = new EndpointImpl();
        
        Mock domMock = mock(Document.class);
        Document expResult = (Document)domMock.proxy();
        instance.setServiceDescription(expResult);
        Document result = instance.getServiceDescription();
        assertEquals(expResult, result);        
    }

    /**
     * Test of setJMSAddress and getJMSAddress methods, of class com.sun.jbi.jmsbc.EndpointImpl.
     */
    public void testSetAndGetJMSAddress() {
        System.out.println("Test setJMSAddress and getJMSAddress");
        
        EndpointImpl instance = new EndpointImpl();
        
        String connURL = "mq://localhost:7676";
        JMSAddress expResult = new JMSAddress();
        expResult.setConnectionURL(connURL);
        instance.setJMSAddress(expResult);
        JMSAddress result = instance.getJMSAddress();
        assertEquals(expResult.getConnectionURL(), result.getConnectionURL());        
    }

    /**
     * Test of setJMSBinding and getJMSBinding methods, of class com.sun.jbi.jmsbc.EndpointImpl.
     */
    public void testSetAndGetJMSBinding() {
        System.out.println("Test setJMSBinding and getJMSBinding");
        
        EndpointImpl instance = new EndpointImpl();
        
        JMSBinding expResult = new JMSBinding();
        expResult.setRequired(new Boolean(true));
        instance.setJMSBinding(expResult);
        JMSBinding result = instance.getJMSBinding();
        assertEquals(expResult.getRequired().booleanValue(), 
                     result.getRequired().booleanValue());        
    }

    /**
     * Test of setJMSOperations and getJMSOperations methods, of class com.sun.jbi.jmsbc.EndpointImpl.
     */
    public void testSetAndGetJMSOperations() {
        System.out.println("Test setJMSOperations and getJMSOperations");
        
        EndpointImpl instance = new EndpointImpl();
        
        QName opQName = new QName("jmsSendOneWayOperation");
        JMSOperation expjmsOp = new JMSOperation();
        expjmsOp.setMEP(Endpoint.EndpointMessageType.IN_OUT);
        Map expResult = new HashMap();
        expResult.put(opQName, expjmsOp);
        instance.setJMSOperations(expResult);
        Map result = instance.getJMSOperations();
        JMSOperation resOp = (JMSOperation) result.get(opQName);
        assertEquals(expjmsOp.getMEP(), resOp.getMEP());        
    }

    /**
     * Test of Test setJMSOperationInput and getJMSOperationInput methods, of class com.sun.jbi.jmsbc.EndpointImpl.
     */
    public void testSetAndGetJMSOperationInput() {
        System.out.println("Test setJMSOperationInput and getJMSOperationInput");
        
        JMSOperation operation = new JMSOperation();
        
        EndpointImpl instance = new EndpointImpl();
        
        JMSInput expResult = new JMSInput();
        JMSMessage jmsMsg = new JMSMessage();
        jmsMsg.setMessageType(JMSConstants.MAP_MESSAGE);
        operation.setDestination("DummyTopic");
        expResult.setJMSMessage(jmsMsg);
        instance.setJMSOperationInput(operation, expResult);
        
        JMSInput result = instance.getJMSOperationInput(operation);
        assertEquals(result.getJMSMessage().getMessageType(),
                     expResult.getJMSMessage().getMessageType());
    }


    /**
     * Test of setJMSOperationOutput and getJMSOperationOutput methodS, of class com.sun.jbi.jmsbc.EndpointImpl.
     */
    public void testGetJMSOperationOutput() {
        System.out.println("Test setJMSOperationOutput and getJMSOperationOutput");
        
        JMSOperation operation = new JMSOperation();
        EndpointImpl instance = new EndpointImpl();
        
        JMSOutput expResult = new JMSOutput();
        JMSMessage jmsMsg = new JMSMessage();
        jmsMsg.setMessageType(JMSConstants.TEXT_MESSAGE);
        operation.setDestination("DummyTopic");
        expResult.setJMSMessage(jmsMsg);

        instance.setJMSOperationOutput(operation, expResult);
        JMSOutput result = instance.getJMSOperationOutput(operation);
        assertEquals(result.getJMSMessage().getMessageType(),
                     expResult.getJMSMessage().getMessageType());
    }

}
