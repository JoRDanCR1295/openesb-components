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

package com.sun.jbi.execbc;

import junit.framework.*;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.execbc.EndpointImpl;
import com.sun.jbi.execbc.Endpoint.EndpointState;
import com.sun.jbi.execbc.Endpoint.EndpointType;
import com.sun.jbi.execbc.extensions.ExecAddress;
import com.sun.jbi.execbc.extensions.ExecBinding;
import com.sun.jbi.execbc.extensions.ExecOperation;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;
import com.ibm.wsdl.DefinitionImpl;
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
        instance = new EndpointImpl();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(EndpointImplTest.class);
        
        return suite;
    }

    /**
     * Test of setServiceName and getServiceName method, of class com.sun.jbi.execbc.EndpointImpl.
     */
    public void testSetGetServiceName() {
        System.out.println("Testing setServiceName and getServiceName");
        
        QName expResult = new QName("http://my-execbc-test/mynamespace", "myExecService");
        instance.setServiceName(new QName("http://my-execbc-test/mynamespace", "myExecService"));
        QName result = instance.getServiceName();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setServiceName and getServiceName");
    }
    
    /**
     * Test of setEndpointName and getEndpointName method, of class com.sun.jbi.execbc.EndpointImpl.
     */
    public void testSetGetEndpointName() {
        System.out.println("Testing setEndpointName and getEndpointName");
        
        String expResult = "myFileTestPort";
        instance.setEndpointName("myFileTestPort");
        String result = instance.getEndpointName();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setEndpointName and getEndpointName");
    }


    /**
     * Test of setDefinition and getDefinition method, of class com.sun.jbi.execbc.EndpointImpl.
     */
    public void testSetGetDefinition() {
        System.out.println("Testing setDefinition and getDefinition");
        
        Definition val = new DefinitionImpl();
        instance.setDefinition(val);
        Definition result = instance.getDefinition();
        assertTrue(result instanceof Definition);
        
        System.out.println("Successfully tested setDefinition and getDefinition");
    }

    /**
     * Test of setState and getState method, of class com.sun.jbi.execbc.EndpointImpl.
     */
    public void testSetGetState() {
        System.out.println("Testing setState and getState");
        
        int expResult = 0;
        instance.setState(0);
        int result = instance.getState();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setState and getState");
    }

    /**
     * Test of setEndpointStatus and getEndpointStatus method, of class com.sun.jbi.execbc.EndpointImpl.
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
     * Test of setEndpointType and getEndpointType method, of class com.sun.jbi.execbc.EndpointImpl.
     */
    public void testSetGetEndpointType() {
        System.out.println("Testing setEndpointType and getEndpointType");
        
        int expResult = 1;
        instance.setEndpointType(1);
        int result = instance.getEndpointType();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setEndpointType and getEndpointType");
    }

    /**
     * Test of setServiceEndpoint and getServiceEndpoint method, of class com.sun.jbi.execbc.EndpointImpl.
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
     * Test of setServiceDescription and getServiceDescription method, of class com.sun.jbi.execbc.EndpointImpl.
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
     * Test of setFileAddress and getFileAddress method, of class com.sun.jbi.execbc.EndpointImpl.
     */
    public void testSetGetFileAddress() {
        System.out.println("Testing setFileAddress and getFileAddress");
        
        instance.setExecAddress(new ExecAddress());
        ExecAddress result = instance.getExecAddress();
        assertTrue(result instanceof ExecAddress);
        
        System.out.println("Successfully tested setFileAddress and getFileAddress");
    }

    /**
     * Test of setFileBinding and getFileBinding method, of class com.sun.jbi.execbc.EndpointImpl.
     */
    public void testSetGetFileBinding() {
        System.out.println("Testing setFileBinding and getFileBinding");
        
        instance.setExecBinding(new ExecBinding());
        ExecBinding result = instance.getExecBinding();
        assertTrue(result instanceof ExecBinding);
        
        System.out.println("Successfully tested setFileBinding and getFileBinding");
    }

    /**
     * Test of setFileOperations and getFileOperations method, of class com.sun.jbi.execbc.EndpointImpl.
     */
    public void testSetGetFileOperations() {
        System.out.println("Testing setFileOperations and getFileOperations");
        
        Map val = new HashMap();
        val.put(new QName("http://some-url", "operation1"), new ExecOperation());
        val.put(new QName("http://some-url", "operation2"), new ExecOperation());
        instance.setExecOperations(val);
        Map result = instance.getExecOperations();
        assertTrue(result instanceof Map);
        assertEquals(val, result);
        
        System.out.println("Successfully tested setFileOperations and getFileOperations");
    }

    /**
     * Test of setOperationMsgExchangePattern and getOperationMsgExchangePattern method, of class com.sun.jbi.execbc.EndpointImpl.
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
