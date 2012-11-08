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
 * @(#)InboundReceiverTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import java.util.HashMap;
import java.util.Map;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.jmock.Mock;
import org.jmock.MockObjectTestCase;

import com.sun.jbi.jdbcbc.extensions.JDBCOperation;

/**
 *
 *
 */
public class InboundReceiverTest extends MockObjectTestCase {
    InboundReceiver instance = null;
    Mock runtimeConfig = null;
    Mock deliveryChannel = null;
    EndpointBean epb = null;
    Mock componentContext = null;
    Map operations = new HashMap();
    Map endpoints = new HashMap();
    EndpointBean endpoint = new EndpointBean();
    Definition defwsdl;

    public InboundReceiverTest(final String testName) {
        super(testName);
    }

    //@Override
	public void setUp() throws Exception {
        runtimeConfig = mock(RuntimeConfigurationMBean.class);
        componentContext = mock(ComponentContext.class);
        deliveryChannel = mock(MessagingChannel.class);

        //runtimeConfig.expects(once()).method("getThreads").will(returnValue(new Integer(10)));
        //runtimeConfig.expects(once()).method("addNotificationListener").withAnyArguments();
        //instance = new InboundReceiver((DeliveryChannel) deliveryChannel.proxy(),
        //		endpoints,(RuntimeConfiguration) runtimeConfig.proxy(),(ComponentContext) componentContext.proxy());
    }

    @Override
    public void tearDown() throws Exception {
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(InboundReceiverTest.class);

        return suite;
    }

    /**
     * Test of addInboundMessageProcessor method, of class com.sun.jbi.filebc.InboundReceiver.
     */
    public void testAddInboundMessageProcessor() throws Exception {
        System.out.println("Testing addInboundMessageProcessor");
        epb = new EndpointBean();
        // 1. testing for inbound endpoint
        endpoint.setValue("descriptor", "descriptor");
        endpoint.setValue("servicename", "servicename");
        endpoint.setValue("endpointname", "endpointname");
        endpoint.setValue("endpointtype", "endpointtype");
        operations.put(new QName("somenamespace", "someOp1"),
            new JDBCOperation());
        operations.put(new QName("somenamespace", "someOp2"),
            new JDBCOperation());
        operations.put(new QName("somenamespace", "someOp3"),
            new JDBCOperation());
        operations.put(new QName("somenamespace", "someOp4"),
            new JDBCOperation());
        operations.put(new QName("somenamespace", "someOp5"),
            new JDBCOperation());
        operations.put(new QName("somenamespace", "someOp6"),
            new JDBCOperation());
        operations.put(new QName("somenamespace", "someOp7"),
            new JDBCOperation());
        operations.put(new QName("somenamespace", "someOp8"),
            new JDBCOperation());

        //endpoint.setFileOperations(operations);
        try {
            //instance.addInboundMessageProcessor(endpoint);
            //epb.expects(atLeastOnce()).method("getValueObj").withAnyArguments().will(returnValue(defwsdl));
            //epb.expects(atLeastOnce()).method("getValue").withAnyArguments().will(returnValue("endpointName"));
        } catch (final Exception e) {
            //fail("Failed to test addInboundMessageProcessor due to: " + e.getMessage());
        }
    }

    /**
     * Test of removeInboundMessageProcessor method, of class com.sun.jbi.filebc.InboundReceiver.
     */
    public void testRemoveInboundMessageProcessor() {
        System.out.println("Testing removeInboundMessageProcessor");

        final Map dummyProcs = new HashMap();
        final QName serviceName = new QName("someServiceName");
        final String endpointName = "someEndpointName";

        try {
            dummyProcs.put(serviceName + endpointName +
                "{somenamespace}someOp1",
                new InboundMessageProcessor(
                    (MessagingChannel) deliveryChannel.proxy(), endpoint,
                    (ComponentContext) componentContext.proxy(),
                    new QName("somenamespace", "someOp1")));
            dummyProcs.put(serviceName + endpointName +
                "{somenamespace}someOp2",
                new InboundMessageProcessor(
                    (MessagingChannel) deliveryChannel.proxy(), endpoint,
                    (ComponentContext) componentContext.proxy(),
                    new QName("somenamespace", "someOp2")));
            dummyProcs.put(serviceName + endpointName +
                "{somenamespace}someOp3",
                new InboundMessageProcessor(
                    (MessagingChannel) deliveryChannel.proxy(), endpoint,
                    (ComponentContext) componentContext.proxy(),
                    new QName("somenamespace", "someOp3")));
            dummyProcs.put(serviceName + endpointName +
                "{somenamespace}someOp4",
                new InboundMessageProcessor(
                    (MessagingChannel) deliveryChannel.proxy(), endpoint,
                    (ComponentContext) componentContext.proxy(),
                    new QName("somenamespace", "someOp4")));
            dummyProcs.put(serviceName + endpointName +
                "{somenamespace}someOp5",
                new InboundMessageProcessor(
                    (MessagingChannel) deliveryChannel.proxy(), endpoint,
                    (ComponentContext) componentContext.proxy(),
                    new QName("somenamespace", "someOp5")));
            dummyProcs.put(serviceName + endpointName +
                "{somenamespace}someOp6",
                new InboundMessageProcessor(
                    (MessagingChannel) deliveryChannel.proxy(), endpoint,
                    (ComponentContext) componentContext.proxy(),
                    new QName("somenamespace", "someOp6")));
            dummyProcs.put(serviceName + endpointName +
                "{somenamespace}someOp7",
                new InboundMessageProcessor(
                    (MessagingChannel) deliveryChannel.proxy(), endpoint,
                    (ComponentContext) componentContext.proxy(),
                    new QName("somenamespace", "someOp7")));
            dummyProcs.put(serviceName + endpointName +
                "{somenamespace}someOp8",
                new InboundMessageProcessor(
                    (MessagingChannel) deliveryChannel.proxy(), endpoint,
                    (ComponentContext) componentContext.proxy(),
                    new QName("somenamespace", "someOp8")));
        } catch (final Exception e) {
            //fail("Failed to set up successfully to test removeInboundMessageProcessor");
        }

        // 1. testing removing some of the activated processors
        endpoint.setValue("descriptor", "descriptor");
        endpoint.setValue("servicename", "servicename");
        endpoint.setValue("endpointname", "endpointname");
        endpoint.setValue("endpointtype", "endpointtype");
        operations.clear();
        operations.put(new QName("somenamespace", "someOp4"),
            new JDBCOperation());
        operations.put(new QName("somenamespace", "someOp5"),
            new JDBCOperation());

        //instance.setActivatedInboundMsgProcs(dummyProcs);
        try {
            //instance.removeInboundMessageProcessor(endpoint);
        } catch (final Exception e) {
            //fail("Failed to test removeInboundMessageProcessor due to: " + e.getMessage());
        }

        //instance.removeInboundMessageProcessor(endpoint);
        //Map procs = instance.getActivatedInboundMsgProcs();
        //assertEquals(6, procs.size());

        // 2. testing for outbound endpoint
        operations.put(new QName("somenamespace", "someOp1"),
            new JDBCOperation());
        operations.put(new QName("somenamespace", "someOp2"),
            new JDBCOperation());
        operations.put(new QName("somenamespace", "someOp3"),
            new JDBCOperation());
        operations.put(new QName("somenamespace", "someOp4"),
            new JDBCOperation());
        operations.put(new QName("somenamespace", "someOp5"),
            new JDBCOperation());
        operations.put(new QName("somenamespace", "someOp6"),
            new JDBCOperation());
        operations.put(new QName("somenamespace", "someOp7"),
            new JDBCOperation());
        operations.put(new QName("somenamespace", "someOp8"),
            new JDBCOperation());

        //endpoint.setFileOperations(operations);

        //endpoint.setEndpointType(1);
        //procs = instance.getActivatedInboundMsgProcs();
        //assertEquals(6, procs.size());

        // 3. testing removing all activated processors
        //endpoint.setEndpointType(0);
        try {
            //instance.removeInboundMessageProcessor(endpoint);
        } catch (final Exception e) {
            //fail("Failed to test removeInboundMessageProcessor due to: " + e.getMessage());
        }

        //instance.removeInboundMessageProcessor(endpoint);
        //procs = instance.getActivatedInboundMsgProcs();
        //assertEquals(0, procs.size());
    }
}
