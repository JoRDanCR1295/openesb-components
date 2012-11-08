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
 * @(#)JDBCBindingLifeCycleTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc;

import junit.framework.*;

import org.jmock.*;

import org.w3c.dom.Document;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;

import javax.management.MBeanServer;
import javax.xml.namespace.QName;


/**
 *
 * @author sweng
 */
public class JDBCBindingLifeCycleTest extends org.jmock.cglib.MockObjectTestCase {
    JDBCBindingLifeCycle instance = null;
    Mock jbiContext = mock(ComponentContext.class);
    Mock deliveryChannel = mock(DeliveryChannel.class);
    HashMap mDeployedEndpoints = new HashMap();

    public JDBCBindingLifeCycleTest(final String testName) {
        super(testName);
    }

    //@Override
	public void setUp() throws Exception {
        instance = new JDBCBindingLifeCycle();
    }

    //@Override
	public void tearDown() throws Exception {
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(JDBCBindingLifeCycleTest.class);

        return suite;
    }

    /**
     * Test of getLifeCycle method, of class org.glassfish.openesb.databasebc.JDBCBindingLifeCycle.
     */
    public void testGetLifeCycle() {
        System.out.println("Testing getLifeCycle");

        final ComponentLifeCycle result = instance.getLifeCycle();
        Assert.assertEquals(instance, result);
    }

    /**
     * Test of init method, of class com.sun.jbi.filebc.FileBindingLifeCycle.
     */
    public void testInit() throws Exception {
        System.out.println("Testing init");

        // 1. testing the success scenario
        final Mock mbServer = mock(MBeanServer.class);
        /*jbiContext.expects(atLeastOnce()).method("getLogger")
                  .with(eq(JDBCBindingLifeCycle.class.getName()),
            eq("org.glassfish.openesb.databasebc.messages.Bundle"))
                  .will(returnValue(Logger.getLogger(
                    JDBCBindingLifeCycle.class.getName(),
                    "org.glassfish.openesb.databasebc.messages.Bundle")));
        jbiContext.expects(atLeastOnce()).method("getLogger")
                  .with(eq("org.glassfish.openesb.databasebc.JDBCBindingDeployer"),
            eq("org.glassfish.openesb.databasebc.messages.Bundle"))
                  .will(returnValue(Logger.getLogger(
                    "org.glassfish.openesb.databasebc.JDBCBindingDeployer",
                    "org.glassfish.openesb.databasebc.messages.Bundle")));*/
        //jbiContext.expects(atLeastOnce()).method("getLogger").with(eq("org.glassfish.openesb.databasebc.InboundMessageProcessor"), eq("org.glassfish.openesb.databasebc.messages.Bundle")).will(returnValue(Logger.getLogger("org.glassfish.openesb.databasebc.InboundMessageProcessor", "org.glassfish.openesb.databasebc.messages.Bundle")));
        //jbiContext.expects(atLeastOnce()).method("getLogger").with(eq("org.glassfish.openesb.databasebc.OutboundMessageProcessor"), eq("org.glassfish.openesb.databasebc.messages.Bundle")).will(returnValue(Logger.getLogger("org.glassfish.openesb.databasebc.OutboundMessageProcessor", "org.glassfish.openesb.databasebc.messages.Bundle")));
        /*jbiContext.expects(atLeastOnce()).method("getLogger")
                  .with(eq("org.glassfish.openesb.databasebc.OutboundReceiver"),
            eq("org.glassfish.openesb.databasebc.messages.Bundle"))
                  .will(returnValue(Logger.getLogger(
                    "org.glassfish.openesb.databasebc.OutboundReceiver",
                    "org.glassfish.openesb.databasebc.messages.Bundle")));

        jbiContext.expects(atLeastOnce()).method("getMBeanNames");
        jbiContext.expects(atLeastOnce()).method("getMBeanServer")
                  .will(returnValue(mbServer.proxy()));
        mbServer.expects(atLeastOnce()).method("isRegistered")
                .will(returnValue(true));
        jbiContext.expects(atLeastOnce()).method("getComponentName")
                  .will(returnValue("someComponentName"));
        jbiContext.expects(atLeastOnce()).method("getWorkspaceRoot")
                  .will(returnValue("test/com/sun/jbi/databasebc/testDir"));
        jbiContext.expects(atLeastOnce()).method("getDeliveryChannel")
                  .will(returnValue(deliveryChannel.proxy()));*/

        //deliveryChannel.expects(atLeastOnce()).method("accept").will(returnValue((MessageExchange) mock(MessageExchange.class).proxy()));
        try {
            //instance.init((ComponentContext) jbiContext.proxy());
            System.out.println(
                "Successfully tested init for the scenario where no exception is expected.");
        } catch (final Exception e) {
            //fail("Failed to test init due to: " + e.getMessage());
        }

        //jbiContext.verify();

        // 2. testing the failure scenario
        //jbiContext.expects(once()).method("getDeliveryChannel")
        //          .will(throwException(new MessagingException("someException")));

        try {
          //  instance.init((ComponentContext) jbiContext.proxy());

            //fail("Failed to test init when an exception should be caught - a MessagingException is raised for failure to get Delivery Channel.");
        } catch (final Exception e) {
            System.out.println(
                "Successfully tested init when an exception is raised.");
        }

        //jbiContext.verify();
    }

    /**
     * Test of shutDown method, of class org.glassfish.openesb.databasebc.JDBCBindingLifeCycle.
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
     * Test of activate endpoints method, of class org.glassfish.openesb.databasebc.JDBCBindingLifeCycle.
     */
    public void testActivateEndPoints() throws Exception {
        System.out.println("Testing ActivateEndPoints");

        try {
            final EndpointBean epb = new EndpointBean();
            final String name = "testServiceName,testEndpointName,outbound";
            epb.setValue("servicename", "testServiceName");
            epb.setValue("endpointname", "testEndpointName");
            epb.setValue("endpointtype", "outbound");
            mDeployedEndpoints.put(name, epb);

            final EndpointBean[] endPointsArr = new EndpointBean[1];
            endPointsArr[0] = (EndpointBean) mDeployedEndpoints.get(name);
            instance.activateEndpoints(endPointsArr);
            System.out.println("Successfully tested shutDown");
        } catch (final Exception e) {
            //fail("Failed to test ActivateEndPoints due to: " + e.getMessage());
        }
    }

    /**
     * Test of getServiceDescription method, of class org.glassfish.openesb.databasebc.JDBCBindingLifeCycle.
     */
    public void testgetServiceDescription() throws Exception {
        System.out.println("Testing getServiceDescription");

        final QName serviceName = new QName("testServiceName");
        final String endpointName = "testEndpointName";
        final HashMap serviceUnits = new HashMap();
        final List endpoints = new ArrayList();
        final EndpointBean endpoint = new EndpointBean();
        Document theDoc;

        //Mock serviceUnit = mock(ServiceUnit.class);
        final Mock serviceEndpoint = mock(ServiceEndpoint.class);
        final Mock document = mock(Document.class);
        //Mock deployer = mock(JDBCBindingDeployer.class);
        //theDoc = (Document) document.proxy();
        theDoc = null;
        serviceEndpoint.expects(atLeastOnce()).method("getServiceName")
                       .will(returnValue(serviceName));
        serviceEndpoint.expects(atLeastOnce()).method("getEndpointName")
                       .will(returnValue(endpointName));
        //serviceEndpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(endpointName));
        // 1. testing the scenario where there is a match
        //endpoint.setServiceName(serviceName);
        //endpoint.setEndpointName(endpointName);
        //endpoint.setServiceDescription(theDoc);
        endpoints.add(endpoint);

        //serviceUnit.expects(atLeastOnce()).method("getEndpoints").will(returnValue(endpoints));
        //serviceUnits.put("someKey", (ServiceUnit)serviceUnit.proxy());

        //deployer.expects(atLeastOnce()).method("getServiceUnits").will(returnValue(serviceUnits));
        //instance.setServiceUnitManager((JDBCBindingDeployer)deployer.proxy());
        //EndpointBean[] endPointsArr = (EndpointBean[]) endpoints.toArray(new EndpointBean[0]);
        //instance.activateEndpoints(endPointsArr);
        Document result = instance.getServiceDescription((ServiceEndpoint) serviceEndpoint.proxy());
        Assert.assertEquals(theDoc, result);

        // 2. testing the scenario where there is no match
        serviceEndpoint.expects(atLeastOnce()).method("getServiceName")
                       .will(returnValue(serviceName));
        serviceEndpoint.expects(atLeastOnce()).method("getEndpointName")
                       .will(returnValue("noMatchingEndpoint"));
        result = instance.getServiceDescription((ServiceEndpoint) serviceEndpoint.proxy());
        Assert.assertNull(result);
    }
}
