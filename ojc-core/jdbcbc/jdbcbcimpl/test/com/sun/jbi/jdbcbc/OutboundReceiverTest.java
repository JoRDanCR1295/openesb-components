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
 * @(#)OutboundReceiverTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import junit.framework.*;

import org.jmock.*;

import java.util.HashMap;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.*;


/**
 *
 * @author
 */
public class OutboundReceiverTest extends org.jmock.cglib.MockObjectTestCase {
    private Mock componentContext = null;
    private Mock deliveryChannel = null;
    private Mock runtimeConfiguration = null;
    HashMap endpoints;
    OutboundReceiver instance = null;

    public OutboundReceiverTest(final String testName) {
        super(testName);
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(OutboundReceiverTest.class);

        return suite;
    }

    //@Override
	public void setUp() throws Exception {
        componentContext = mock(ComponentContext.class);
        endpoints = new HashMap();
        deliveryChannel = mock(DeliveryChannel.class);
        runtimeConfiguration = mock(RuntimeConfiguration.class,
                new Class[] { String.class },
                new Object[] { "test/com/sun/jbi/jdbcbc/testDir" });
        runtimeConfiguration.stubs().method("getThreads")
                            .will(returnValue(new Integer(10)));
        runtimeConfiguration.stubs().method("addNotificationListener");

        //instance = new OutboundReceiver((DeliveryChannel) deliveryChannel.proxy(), endpoints,
        //		(RuntimeConfiguration) runtimeConfiguration.proxy(), (ComponentContext) componentContext.proxy());
    }

    //@Override
	public void tearDown() throws Exception {
    }

    /**
     * Test of setThreads method, of class com.sun.jbi.jdbcbc.OutboundReceiver.
     */
    public void testSetThreads() {
        System.out.println("Testing setThreads");
        //instance.setThreads(10);
        //assertEquals(10, instance.mOutboundCorePoolSize);
        System.out.println("Testing setThreads completed");
    }

    /**
     * Test of stopReceiving method, of class com.sun.jbi.jdbcbc.OutboundReceiver.
     */
    public void testStopReceiving() {
        //System.out.println("Testing stopReceiving");
        //instance.stopReceiving();
    }
}
