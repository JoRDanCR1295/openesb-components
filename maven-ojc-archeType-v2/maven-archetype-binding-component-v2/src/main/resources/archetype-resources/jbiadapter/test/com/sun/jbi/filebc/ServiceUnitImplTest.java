#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(${symbol_pound})ServiceUnitImplTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc;

import junit.framework.*;

import com.sun.jbi.eManager.provider.StatusProviderHelper;
import javax.jbi.messaging.DeliveryChannel;
import java.util.ArrayList;
import java.util.List;
import javax.jbi.component.ComponentContext;
import javax.management.MBeanServer;
import javax.xml.namespace.QName;
import org.jmock.*;

/**
 *
 * @author sweng
 */
public class ServiceUnitImplTest extends MockObjectTestCase {

    ServiceUnitImpl instance = null;
    Mock componentContext = mock(ComponentContext.class);
    Mock deliveryChannel = mock(DeliveryChannel.class);
    Mock mbServer = mock(MBeanServer.class);
    StatusProviderHelper statusHelper = null;
    InboundReceiver inboundReceiver = null;
    Endpoint endpoint = null;
    List endpoints = new ArrayList();

    public ServiceUnitImplTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        statusHelper = new StatusProviderHelper("shortName", "componentType", "componentName", (MBeanServer) mock(MBeanServer.class).proxy());
        endpoint = new EndpointImpl();
        endpoint.setServiceName(new QName("MySerivceName"));
        endpoint.setEndpointName("MyEndpointName");
        endpoints.add(endpoint);

        inboundReceiver = new InboundReceiver((ComponentContext) componentContext.proxy(),
                (DeliveryChannel) deliveryChannel.proxy(),
                new RuntimeConfiguration("test/com/sun/jbi/filebc/testDir", null));
        instance = new ServiceUnitImpl("TestId",
                "test/com/sun/jbi/filebc/packaging/descriptors",
                (ComponentContext) componentContext.proxy(),
                new RuntimeConfiguration("test/com/sun/jbi/filebc/testDir", null),
                statusHelper,
                inboundReceiver);
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ServiceUnitImplTest.class);

        return suite;
    }

    /**
     * Test of getServiceUnitId method, of class com.sun.jbi.filebc.ServiceUnitImpl.
     */
    public void testGetServiceUnitId() {
        System.out.println("Testing getServiceUnitId");

        String result = instance.getServiceUnitId();
        assertEquals("TestId", result);
    }

    /**
     * Test of init method, of class com.sun.jbi.filebc.ServiceUnitImpl.
     */
    public void testInit() throws Exception {
        System.out.println("Testing init");

        instance.setEndpoints(endpoints);
        //componentContext.expects(atLeastOnce()).method("getComponentName").will(returnValue("someComponentName"));
        //componentContext.expects(atLeastOnce()).method("getLogger").with(eq(DeploymentLookup.class.getName()), eq(null)).will(returnValue(Logger.getLogger(DeploymentLookup.class.getName(), null)));
        //componentContext.expects(atLeastOnce()).method("getMBeanServer").will(returnValue(mbServer.proxy()));
        //mbServer.expects(atLeastOnce()).method("queryNames").with(isA(ObjectName.class), NULL);

        try {
            instance.init();
            System.out.println("Successfully tested init");
        } catch (Exception e) {
            fail("Failed to test init method due to: " + e.getMessage());
        }
    }

    /**
     * Test of start method, of class com.sun.jbi.filebc.ServiceUnitImpl.
     */
    public void testStart() throws Exception {
        System.out.println("Testing start");
        try {
            instance.start();
            System.out.println("Successfully tested start");
        } catch (Exception e) {
            fail("Failed to test start method due to: " + e.getMessage());
        }
    }

    /**
     * Test of stop method, of class com.sun.jbi.filebc.ServiceUnitImpl.
     */
    public void testStop() throws Exception {
        System.out.println("Testing stop");

        try {
            instance.stop();
            System.out.println("Successfully tested stop");
        } catch (Exception e) {
            fail("Failed to test stop method due to: " + e.getMessage());
        }
    }

    /**
     * Test of shutdown method, of class com.sun.jbi.filebc.ServiceUnitImpl.
     */
    public void testShutdown() throws Exception {
        System.out.println("Testing shutdown");

        try {
            instance.shutdown();
            System.out.println("Successfully tested shutdown");
        } catch (Exception e) {
            fail("Failed to test shutdown method due to: " + e.getMessage());
        }
    }
    /**
     * Test of activateEndpoint method, of class com.sun.jbi.filebc.ServiceUnitImpl.
     */
//    public void testActivateEndpoint() throws Exception {
//        System.out.println("Testing activateEndpoint");
//        
//        // 1. testing the scenario where endpoint type is outbound
//        endpoint.setEndpointType(1);
//        Mock serviceEndpoint = mock(ServiceEndpoint.class);
//        componentContext.expects(atLeastOnce()).method("activateEndpoint").with(eq(new QName("MySerivceName")), eq("MyEndpointName")).
//            will(returnValue((ServiceEndpoint)serviceEndpoint.proxy()));
//        
//        instance.activateEndpoint(endpoint);
//        componentContext.verify();
//        System.out.println("Successfully tested activateEndpoint for an outbound endpoint");
//        
//        // 2. testing the scenario where endpoint type is inbound
//        endpoint.setEndpointType(0);
//        try {
//            instance.activateEndpoint(endpoint);
//            System.out.println("Successfully tested activateEndpoint for an inbound endpoint");
//        } catch (Exception e) {
//            fail("Failed to test activateEndpoint for an inbound endpoint");
//        }
//    }
    /**
     * Test of deactivateEndpoint method, of class com.sun.jbi.filebc.ServiceUnitImpl.
     */
//        public void testDeactivateEndpoint() throws Exception {
//            System.out.println("Testing deactivateEndpoint");
//
//            // 1. testing the scenario where endpoint type is outbound
//            endpoint.setEndpointType(1);
//            Mock serviceEndpoint = mock(ServiceEndpoint.class);
//            endpoint.setServiceEndpoint((ServiceEndpoint) serviceEndpoint.proxy());
//            componentContext.expects(atLeastOnce()).method("deactivateEndpoint").with(eq(serviceEndpoint.proxy()));
//            instance.deactivateEndpoint(endpoint);
//            componentContext.verify();
//            System.out.println("Successfully tested deactivateEndpoint for an outbound endpoint");
//
//            // 2. testing the scenario where endpoint type is inbound
//            endpoint.setEndpointType(0);
//            try {
//                instance.deactivateEndpoint(endpoint);
//                System.out.println("Successfully tested deactivateEndpoint for an inbound endpoint");
//            } catch (Exception e) {
//                fail("Failed to test deactivateEndpoint for an inbound endpoint");
//            }
//        }
}
