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

package com.sun.jbi.smtpbc;

import junit.framework.*;

import java.util.HashMap;
import java.util.Map;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;
import org.w3c.dom.Document;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.smtpbc.Endpoint.EndpointType;
import com.sun.jbi.smtpbc.Endpoint.EndpointState;
import com.sun.jbi.smtpbc.extensions.SMTPAddress;
import com.sun.jbi.smtpbc.extensions.SMTPBinding;
import com.sun.jbi.smtpbc.extensions.SMTPOperation;
import com.sun.jbi.smtpbc.extensions.SMTPOperationInput;
import com.sun.jbi.smtpbc.extensions.SMTPOperationOutput;

import org.jmock.*;

/**
 *
 * @author rchen
 */
public class EndpointImplTest extends org.jmock.cglib.MockObjectTestCase {
    EndpointImpl instance =null;
    public EndpointImplTest(final String testName) {
        super(testName);
    }

    @Override
	protected void setUp() throws Exception {
        instance = new EndpointImpl();
    }

    @Override
	protected void tearDown() throws Exception {
    }

    /**
     * Test of getServiceName method, of class com.sun.jbi.smtpbc.EndpointImpl.
     */
    public void testSetGetServiceName() {
        System.out.println("setServiceName getServiceName");
        final QName serviceName = new QName("Somenamespace","someop");     
        instance.setServiceName(serviceName);    
        final QName expResult = new QName("Somenamespace","someop");
        final QName result = instance.getServiceName();
        Assert.assertEquals(expResult, result);
        
       
    }

   

    /**
     * Test of getEndpointName method, of class com.sun.jbi.smtpbc.EndpointImpl.
     */
    public void testSetGetEndpointName() {
        System.out.println("setEndpointName getEndpointName");
        
        final String endpointName = "this is endpointname"; 
        instance.setEndpointName(endpointName);
        
        final String expResult = "this is endpointname";
        final String result = instance.getEndpointName();
        Assert.assertEquals(expResult, result);
        
    }

    /**
     * Test of setDefinition and getDefinition methods, of class com.sun.jbi.jmsbc.EndpointImpl.
     */
    public void testSetAndGetDefinition() {
        System.out.println("Test setDefinition and getDefinition");
              
        final QName definitionQName = new QName("serviceDefinition");
        final Mock definitionMock = mock (Definition.class);
        definitionMock.stubs().method("getQName").will(returnValue(definitionQName));
        final Definition expResult = (Definition)definitionMock.proxy();
        instance.setDefinition(expResult);
        final Definition result = instance.getDefinition();
        Assert.assertEquals(expResult.getQName(), result.getQName());        
    }

    /**
     * Test of getState method, of class com.sun.jbi.smtpbc.EndpointImpl.
     */
    public void testSetGetState() {
        System.out.println("setState getState");
               
        instance.setState(EndpointState.SHUTDOWN);
          
        final EndpointState expResult = EndpointState.SHUTDOWN;
        final EndpointState result = instance.getState();
        Assert.assertEquals(expResult, result);
        
      
    }


    /**
     * Test of setEndpointStatus method, of class com.sun.jbi.smtpbc.EndpointImpl.
     */
    public void testSetGetEndpointStatus() {
        System.out.println("setEndpointStatus");
        
        final Mock endpointStatusMock = mock (EndpointStatus.class);        
        final EndpointStatus expResult = (EndpointStatus)endpointStatusMock.proxy();
        
        instance.setEndpointStatus(expResult);
        final EndpointStatus result = instance.getEndpointStatus();
        Assert.assertEquals(expResult, result);       
    }

    

    /**
     * Test of getEndpointType method, of class com.sun.jbi.smtpbc.EndpointImpl.
     */
    public void testSetGetEndpointType() {
        System.out.println("setEndpointType getEndpointType");
        
       instance.setEndpointType(EndpointType.OUTBOUND);   
        final EndpointType expResult = EndpointType.OUTBOUND;
        final EndpointType result = instance.getEndpointType();
        Assert.assertEquals(expResult, result);
        
       
    }


    /**
     * Test of getServiceEndpoint method, of class com.sun.jbi.smtpbc.EndpointImpl.
     */
    public void testSetGetServiceEndpoint() {
        System.out.println("getServiceEndpoint");
        
        final Mock serviceEndpointMock = mock(ServiceEndpoint.class);
        final ServiceEndpoint expResult = (ServiceEndpoint)serviceEndpointMock.proxy();
        instance.setServiceEndpoint(expResult);
       
        final ServiceEndpoint result = instance.getServiceEndpoint();
        Assert.assertEquals(expResult, result);
        
    }


    /**
     * Test of getServiceDescription method, of class com.sun.jbi.smtpbc.EndpointImpl.
     */
    public void testSetGetServiceDescription() {
        System.out.println("setServiceDescription getServiceDescription");
        
        final Mock domMock = mock(Document.class);
        final Document expResult = (Document)domMock.proxy();
        instance.setServiceDescription(expResult);
        final Document result = instance.getServiceDescription();
        Assert.assertEquals(expResult, result);
        
    }

   

    /**
     * Test of getSMTPAddress method, of class com.sun.jbi.smtpbc.EndpointImpl.
     */
    public void testGetSMTPAddress() {
        System.out.println("getSMTPAddress");
          
        final SMTPAddress expResult = new  SMTPAddress();
        instance.setSMTPAddress(expResult);
        final SMTPAddress result = instance.getSMTPAddress();
        Assert.assertEquals(expResult, result);
      
    }

   

    /**
     * Test of getSMTPBinding method, of class com.sun.jbi.smtpbc.EndpointImpl.
     */
    public void testSetGetSMTPBinding() {
        System.out.println("getSMTPBinding");
        
        final SMTPBinding expResult = new SMTPBinding();
        instance.setSMTPBinding(expResult);
        final SMTPBinding result = instance.getSMTPBinding();
        Assert.assertEquals(expResult, result);
        
    }


    /**
     * Test of getSMTPOperations method, of class com.sun.jbi.smtpbc.EndpointImpl.
     */
    public void testSetGetSMTPOperations() {
        System.out.println("setSMTPOperations getSMTPOperations");
   
        
        final Map expResult = new HashMap();
        instance.setSMTPOperations(expResult);
        final Map result = instance.getSMTPOperations();
        Assert.assertEquals(expResult, result);
        
    }


    /**
     * Test of getSMTPOperationInput method, of class com.sun.jbi.smtpbc.EndpointImpl.
     */
    public void testSetGetSMTPOperationInput() {
        System.out.println("setSMTPOperationInput getSMTPOperationInput");
        
       
         final SMTPOperation operation = new SMTPOperation();
        final SMTPOperationInput expResult = new SMTPOperationInput();
        instance.setSMTPOperationInput(operation, expResult);
          
        final SMTPOperationInput result = instance.getSMTPOperationInput(operation);
        Assert.assertEquals(expResult, result);
        
       
    }

   

    /**
     * Test of getSMTPOperationOutput method, of class com.sun.jbi.smtpbc.EndpointImpl.
     */
    public void testSetGetSMTPOperationOutput() {
        System.out.println("setSMTPOperationOutput getSMTPOperationOutput");
        
        
        final SMTPOperation operation = new SMTPOperation();
        final SMTPOperationOutput expResult = new SMTPOperationOutput();
        instance.setSMTPOperationOutput(operation, expResult);
          
        final SMTPOperationOutput result = instance.getSMTPOperationOutput(operation);
        Assert.assertEquals(expResult, result);
    }

   
    
}
