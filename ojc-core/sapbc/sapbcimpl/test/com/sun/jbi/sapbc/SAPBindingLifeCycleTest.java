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
 * @(#)SAPBindingLifeCycleTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import com.sun.jbi.sapbc.Endpoint.EndpointType;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.common.model.impl.XMLElementImpl;
import junit.framework.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.MBeanServer;
import javax.xml.namespace.QName;
import org.w3c.dom.Document;
import org.jmock.*;

/**
 *
 * @author sweng
 */
public class SAPBindingLifeCycleTest extends org.jmock.cglib.MockObjectTestCase {
    SAPBindingLifeCycle instance = null;
    
    Mock jbiContext = mock(ComponentContext.class);
    Mock deliveryChannel = mock(DeliveryChannel.class);
    
    public SAPBindingLifeCycleTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new SAPBindingLifeCycle();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(SAPBindingLifeCycleTest.class);
        
        return suite;
    }

    /**
     * Test of getLifeCycle method, of class com.sun.jbi.sapbc.SAPBindingLifeCycle.
     */
    public void testGetLifeCycle() {
        System.out.println("Testing getLifeCycle");
        
        ComponentLifeCycle result = instance.getLifeCycle();
        assertEquals(instance, result);
    }
    
    /**
     * Test of init method, of class com.sun.jbi.sapbc.SAPBindingLifeCycle.
     */
    public void testInit() throws Exception {
        System.out.println("Testing init");

        // 1. testing the success scenario

        Mock mbServer = mock(MBeanServer.class);
        jbiContext.expects(atLeastOnce()).method("getLogger").with(eq(SAPBindingLifeCycle.class.getName()),eq("com.sun.jbi.sapbc.messages.Bundle")).will(returnValue(Logger.getLogger(SAPBindingLifeCycle.class.getName(), "com.sun.jbi.sapbc.messages.Bundle")));
        jbiContext.expects(atLeastOnce()).method("getLogger").with(eq("com.sun.jbi.sapbc.SAPBindingDeployer"), eq("com.sun.jbi.sapbc.messages.Bundle")).will(returnValue(Logger.getLogger("com.sun.jbi.sapbc.SAPBindingDeployer", "com.sun.jbi.sapbc.messages.Bundle")));
        //jbiContext.expects(atLeastOnce()).method("getLogger").with(eq("com.sun.jbi.sapbc.InboundMessageProcessor"), eq("com.sun.jbi.sapbc.messages.Bundle")).will(returnValue(Logger.getLogger("com.sun.jbi.sapbc.InboundMessageProcessor", "com.sun.jbi.sapbc.messages.Bundle")));
        jbiContext.expects(atLeastOnce()).method("getLogger").with(eq("com.sun.jbi.sapbc.OutboundMessageProcessor"), eq("com.sun.jbi.sapbc.messages.Bundle")).will(returnValue(Logger.getLogger("com.sun.jbi.sapbc.OutboundMessageProcessor", "com.sun.jbi.sapbc.messages.Bundle")));
        //jbiContext.expects(atLeastOnce()).method("getLogger").with(eq("com.sun.jbi.sapbc.RuntimeConfiguration"), eq("com.sun.jbi.sapbc.messages.Bundle")).will(returnValue(Logger.getLogger("com.sun.jbi.sapbc.RuntimeConfiguration", "com.sun.jbi.sapbc.messages.Bundle")));
        jbiContext.expects(atLeastOnce()).method("getMBeanServer").will(returnValue(mbServer.proxy()));
        mbServer.expects(atLeastOnce()).method("isRegistered").will(returnValue(true));
        jbiContext.expects(atLeastOnce()).method("getComponentName").will(returnValue("someComponentName"));
        jbiContext.expects(atLeastOnce()).method("getWorkspaceRoot").will(returnValue("test/com/sun/jbi/sapbc/testDir"));
        jbiContext.expects(atLeastOnce()).method("getDeliveryChannel").will(returnValue(deliveryChannel.proxy()));
        deliveryChannel.expects(atLeastOnce()).method("accept").will(returnValue((MessageExchange) mock(MessageExchange.class).proxy()));
        jbiContext.expects(atLeastOnce()).method("getInstallRoot").will(returnValue("test/com/sun/jbi/filebc/testDir"));
        
        try {
            instance.init((ComponentContext)jbiContext.proxy());
            System.out.println("Successfully tested init for the scenario where no exception is expected.");
        } catch (Exception e) {
            fail("Failed to test init due to: " + e.getMessage());
        }
        jbiContext.verify();
        
        // 2. testing the failure scenario
        jbiContext.expects(once()).method("getDeliveryChannel").will(throwException(new MessagingException("someException")));
        
        try {
            instance.init((ComponentContext)jbiContext.proxy());
            fail("Failed to test init when an exception should be caught - a MessagingException is raised for failure to get Delivery Channel.");
        } catch (Exception e) {
            System.out.println("Successfully tested init when an exception is raised.");
        }
        jbiContext.verify();
    }

    /**
     * Test of shutDown method, of class com.sun.jbi.sapbc.SAPBindingLifeCycle.
     */
//    public void testShutDown() throws Exception {
//        System.out.println("Testing shutDown");
//        
//        // 1. testing the success scenario
//        deliveryChannel.expects(once()).method("close");
//        try {
//            instance.shutDown();
//            System.out.println("Successfully tested shutDown");
//        } catch (Exception e) {
//            fail("Failed to test shutDown due to: " + e.getMessage());
//        }
//        
//        // 2. testing the failure scenario
//        deliveryChannel.expects(once()).method("close").will(throwException (new MessagingException("someException")));
//        try {
//            instance.shutDown();
//            fail("Failed to test shutDown when an exception should be caught - a MessagingException should be raised for failure to close Delivery Channel.");
//        } catch (Exception e) {
//            System.out.println("Successfully tested shutDown when an exception is raised.");
//        }
//    }
    
    /** 
     * Test of getServiceDescription method, of class com.sun.jbi.sapbc.SAPBindingLifeCycle.
     */
    public void testGetServiceDescription() throws Exception {
        System.out.println("Testing getServiceDescription");
        
        QName serviceName = new QName("testServiceDescription");
        String endpointName = "testGetServiceDescriptionName";
        Mock wsdlDefProxy = mock(WSDLDefinitions.class);
        WSDLDefinitions wsdlDef = (WSDLDefinitions) wsdlDefProxy.proxy();
        EndpointType direction = null;       
        Endpoint endpoint = new EndpointImpl(endpointName, serviceName, wsdlDef, direction);
        
        Mock serviceUnit = mock(ServiceUnit.class);
        Mock serviceEndpoint = mock(ServiceEndpoint.class);
        Mock document = mock(Document.class);
        Mock deployer = mock(SAPBindingDeployer.class, 
                             new Class[] {ComponentContext.class, SAPBindingLifeCycle.class}, 
                             new Object[] {(ComponentContext) jbiContext.proxy(), instance});
        
        Document theDoc = (Document) document.proxy();
        serviceEndpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(serviceName));
        serviceEndpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(endpointName));
        
        // 1. testing the scenario where there is a match
        endpoint.setServiceDescription(theDoc);
        List endpoints = new ArrayList();        
        endpoints.add(endpoint);
        serviceUnit.expects(atLeastOnce()).method("getEndpoints").will(returnValue(endpoints));
        HashMap serviceUnits = new HashMap();
        serviceUnits.put("someKey", (ServiceUnit)serviceUnit.proxy());
        
        deployer.expects(atLeastOnce()).method("getServiceUnits").will(returnValue(serviceUnits));
        instance.setServiceUnitManager((SAPBindingDeployer)deployer.proxy());
        
        Document result = instance.getServiceDescription((ServiceEndpoint) serviceEndpoint.proxy());
        assertEquals(theDoc, result);
        
        // 2. testing the scenario where there is no match
        serviceEndpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(serviceName));
        serviceEndpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue("noMatchingEndpoint"));
        result = instance.getServiceDescription((ServiceEndpoint) serviceEndpoint.proxy());
        assertNull(result);
        
    }
}
