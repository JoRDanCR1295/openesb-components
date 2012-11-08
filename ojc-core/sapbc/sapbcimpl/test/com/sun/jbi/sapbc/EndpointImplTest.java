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

package com.sun.jbi.sapbc;

import junit.framework.*;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.sapbc.extensions.SAPAddress;
import com.sun.jbi.sapbc.extensions.SAPBinding;
import com.sun.jbi.sapbc.extensions.SAPFmOperation;
import java.util.HashMap;
import java.util.Map;
import javax.jbi.servicedesc.ServiceEndpoint;
import com.sun.jbi.sapbc.Endpoint.EndpointState;
import com.sun.jbi.sapbc.Endpoint.EndpointType;
import com.sun.jbi.sapbc.extensions.SAPEnvironmentalVars;
import com.sun.jbi.sapbc.util.SAPWSDLUtilities;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.impl.WSDLDefinitionsImpl;
import java.io.File;
import java.util.Iterator;
import javax.xml.namespace.QName;
import org.w3c.dom.Document;
import org.jmock.*;

/**
 *
 * @author sweng
 */
public class EndpointImplTest extends MockObjectTestCase {
    EndpointImpl instance = null;
    
    public EndpointImplTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        File wsdlFile = new File( "test/com/sun/jbi/sapbc/wsdls/Z_FlightWSD_EUC_SAP.wsdl" );
        String targetNamespace = "urn:sap-com:document:sap:soap:functions:mc-style";
        QName interfaceQName = new QName(targetNamespace, "Z_FlightWSD");
        QName serviceQName = new QName(targetNamespace, "Z_FlightWSDService");
        String endpointName = "Z_FlightWSDSAPBindingPort";
        EndpointType direction = EndpointType.INBOUND;
        SAPEnvironmentalVars envVars = new SAPEnvironmentalVars(new HashMap());
        instance = (EndpointImpl) SAPWSDLUtilities.getWSDLEndpointByName(wsdlFile, 
                interfaceQName, 
                serviceQName, 
                endpointName, 
                direction,
                envVars); 

    }
    
    protected void tearDown() throws Exception {
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(EndpointImplTest.class);
        
        return suite;
    }
    
    /**
     * Test of setServiceName and getServiceName method, of class com.sun.jbi.sapbc.EndpointImpl.
     */
    public void testSetGetServiceName() {
        System.out.println("Testing setServiceName and getServiceName");
        
        QName expResult = new QName("http://my-sapbc-test/mynamespace", "mySAPService");
        instance.setServiceName(new QName("http://my-sapbc-test/mynamespace", "mySAPService"));
        QName result = instance.getServiceName();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setServiceName and getServiceName");
    }
    
    /**
     * Test of setEndpointName and getEndpointName method, of class com.sun.jbi.sapbc.EndpointImpl.
     */
    public void testSetGetEndpointName() {
        System.out.println("Testing setEndpointName and getEndpointName");
        
        String expResult = "mySAPTestPort";
        instance.setEndpointName("mySAPTestPort");
        String result = instance.getEndpointName();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setEndpointName and getEndpointName");
    }
    
    
    /**
     * Test of setDefinition and getDefinition method, of class com.sun.jbi.sapbc.EndpointImpl.
     */
    public void testSetGetDefinition() {
        System.out.println("Testing setDefinition and getDefinition");
        
        WSDLDefinitions val = new WSDLDefinitionsImpl();
        instance.setDefinition(val);
        WSDLDefinitions result = instance.getDefinition();
        assertTrue(result instanceof WSDLDefinitions);
        
        System.out.println("Successfully tested setDefinition and getDefinition");
    }
    
    /**
     * Test of setState and getState method, of class com.sun.jbi.sapbc.EndpointImpl.
     */
    public void testSetGetState() {
        System.out.println("Testing setState and getState");
        
        EndpointState expResult = Endpoint.EndpointState.RUNNING;
        instance.setState(expResult);
        EndpointState result = instance.getState();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setState and getState");
    }
    
    /**
     * Test of setEndpointStatus and getEndpointStatus method, of class com.sun.jbi.sapbc.EndpointImpl.
     */
    public void testSetGetEndpointStatus() {
        System.out.println("Testing setEndpointStatus and getEndpointStatus");
        
        Mock endpointStatus = mock(EndpointStatus.class);
        instance.setEndpointStatus((EndpointStatus)endpointStatus.proxy());
        EndpointStatus result = instance.getEndpointStatus();
        assertTrue(result instanceof EndpointStatus);
        
        System.out.println("Successfully tested setEndpointStatus and getEndpointStatus");
    }
    
    /**
     * Test of setEndpointType and getEndpointType method, of class com.sun.jbi.sapbc.EndpointImpl.
     */
    public void testSetGetEndpointType() {
        System.out.println("Testing setEndpointType and getEndpointType");
        
        instance.setEndpointType(EndpointType.OUTBOUND);
        EndpointType expResult = EndpointType.OUTBOUND;
        EndpointType result = instance.getEndpointType();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setEndpointType and getEndpointType");
    }
    
    /**
     * Test of setServiceEndpoint and getServiceEndpoint method, of class com.sun.jbi.sapbc.EndpointImpl.
     */
    public void testSetGetServiceEndpoint() {
        System.out.println("Testing setServiceEndpoint and getServiceEndpoint");
        
        Mock serviceEndpoint = mock(ServiceEndpoint.class);
        instance.setServiceEndpoint((ServiceEndpoint) serviceEndpoint.proxy());
        ServiceEndpoint result = instance.getServiceEndpoint();
        assertTrue(result instanceof ServiceEndpoint);
        
        System.out.println("Successfully tested setServiceEndpoint and getServiceEndpoint");
    }
    
    /**
     * Test of setServiceDescription and getServiceDescription method, of class com.sun.jbi.sapbc.EndpointImpl.
     */
    public void testSetServiceDescription() {
        System.out.println("Testing setServiceDescription and getServiceDescription");
        
        Mock document = mock(Document.class);
        instance.setServiceDescription((Document)document.proxy());
        Document result = instance.getServiceDescription();
        assertTrue(result instanceof Document);
        
        System.out.println("Successfully tested setServiceDescription and getServiceDescription");
    }
    
    /**
     * Test of setSAPAddress and getSAPAddress method, of class com.sun.jbi.sapbc.EndpointImpl.
     */
    public void testSetGetSAPAddress() {
        System.out.println("Testing setSAPAddress and getSAPAddress");
        
        try {
            instance.setSAPAddress(instance.getSAPAddress());
        } catch (Exception ex) {
            ex.printStackTrace();
            String errMsg = "testSetGetSAPAddress: Exception Thrown ["+ex.getClass().getName()+"] Message ["+ex.getMessage()+"]";
            fail(errMsg);
        }
        SAPAddress result = instance.getSAPAddress();
        assertTrue(result instanceof SAPAddress);
        
        System.out.println("Successfully tested setSAPAddress and getSAPAddress");
    }
    
    /**
     * Test of setSAPBinding and getSAPBinding method, of class com.sun.jbi.sapbc.EndpointImpl.
     */
    public void testSetGetSAPBinding() {
        System.out.println("Testing setSAPBinding and getSAPBinding");
        
        try {
            instance.setSAPBinding(instance.getSAPBinding());
        } catch (Exception ex) {
            ex.printStackTrace();
            String errMsg = "testSetGetSAPBinding: Exception Thrown ["+ex.getClass().getName()+"] Message ["+ex.getMessage()+"]";
            fail(errMsg);
        }
        SAPBinding result = instance.getSAPBinding();
        assertTrue(result instanceof SAPBinding);
        
        System.out.println("Successfully tested setSAPBinding and getSAPBinding");
    }
    
    /**
     * Test of setSAPOperations and getSAPOperations method, of class com.sun.jbi.sapbc.EndpointImpl.
     */
    public void testSetGetSAPOperations() {
        System.out.println("Testing setSAPOperations and getSAPOperations");
        
        SAPFmOperation sapfmoper = null; 
        QName opQName = new QName("", "FlightGetDetail");
        Object op = instance.getSAPOperations().get(opQName);
        if (op instanceof SAPFmOperation) {
            sapfmoper = (SAPFmOperation) op;
        } else {
            fail("Operation ["+opQName.toString()+"] is not an instance of SAPFmOperation");
        }
        
        Map val = new HashMap();
        try {
            val.put(new QName("http://some-url", "operation1"), sapfmoper);
            val.put(new QName("http://some-url", "operation2"), sapfmoper);
        } catch (Exception ex) {
            ex.printStackTrace();
            String errMsg = "testSetGetSAPOperations: Exception Thrown ["+ex.getClass().getName()+"] Message ["+ex.getMessage()+"]";
            fail(errMsg);
        }
        instance.setSAPOperations(val);
        Map result = instance.getSAPOperations();
        assertTrue(result instanceof Map);
        assertEquals(val, result);
        
        System.out.println("Successfully tested setSAPOperations and getSAPOperations");
    }
    
    /**
     * Test of setOperationMsgExchangePattern and getOperationMsgExchangePattern method, of class com.sun.jbi.sapbc.EndpointImpl.
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
    
}
