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
 * @(#)InboundMessageProcessorTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.eManager.provider.EndpointStatus;
import junit.framework.*;

import org.jmock.*;

import java.util.HashMap;
import java.util.Map;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;

import javax.xml.namespace.QName;


/**
 *
 *
 */
public class InboundMessageProcessorTest extends MockObjectTestCase {
    static final QName THE_OPERATION = new QName("myoperation");
    static final QName THE_SERVICE = new QName("myServiceName");
    static final String THE_ENDPOINT = "myEndpointName";
    InboundMessageProcessor instance = null;
    Mock deliveryChannel = null;
    Mock componentContext = null;
    EndpointBean endpoint = null;
    Mock endpointStatus = null;
    Mock serviceEndpoint = null;
    Mock msgExchange = null;
    Mock msgExchangeFactory = null;
    JDBCNormalizer normalizer = null;
    Mock normalizedMsg = null;
    Map operations = new HashMap();
    Map operationMeps = new HashMap();

    public InboundMessageProcessorTest(final String testName) {
        super(testName);
    }

    //@Override
	public void setUp() throws Exception {
        componentContext = mock(ComponentContext.class);
        deliveryChannel = mock(MessagingChannel.class);
        endpoint = new EndpointBean();
        endpointStatus = mock(EndpointStatus.class);
        serviceEndpoint = mock(ServiceEndpoint.class);
        msgExchangeFactory = mock(MessageExchangeFactory.class);
        msgExchange = mock(MessageExchange.class);
        normalizer = new JDBCNormalizer();
        normalizedMsg = mock(NormalizedMessage.class);

        instance = new InboundMessageProcessor((MessagingChannel) deliveryChannel.proxy(),
                 endpoint,
                (ComponentContext) componentContext.proxy(), InboundMessageProcessorTest.THE_OPERATION);
    }

    //@Override
	public void tearDown() throws Exception {
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(InboundMessageProcessorTest.class);

        return suite;
    }

    /**
     * Test of run method, of class com.sun.jbi.jdbcbc.InboundMessageProcessor
     * for the scenario where the message exchange pattern is not valid
     */
    public void testRunInvalidMEP() {
        System.out.println(
            "Testing run() for the scenario where the message exchange pattern is not valid");

        /*
        JDBCOperation jdbcoperation = new JDBCOperation();
        operations.put(THE_OPERATION, jdbcoperation);
        
        JDBCOperationInput jdbcopInput = new JDBCOperationInput();
        jdbcopInput.setJDBCSql(new JDBCSql());
        deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory");
        msgExchangeFactory.expects(atLeastOnce()).method("createInOnlyExchange").will(returnValue(msgExchange));
        endpoint.expects(atLeastOnce()).method("getValueObj").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getValue").will(returnValue(THE_ENDPOINT));
        msgExchange.expects(atLeastOnce()).method("setEndpoint");
        msgExchange.expects(atLeastOnce()).method("setOperation");
        
        // 1. testing the case where message exchange pattern is not found (null)
        operationMeps.remove(THE_OPERATION);
        operationMeps.put("dummyOperation", "inonly");
        instance.run();
        // 2. testing the case where message exchange status is invalid (not inonly or inout)
        operationMeps.put(THE_OPERATION, "outin");
        instance.run();
        */
    }

    /**
     * Test of processReplyMessage method, of class com.sun.jbi.filebc.InboundMessageProcessor.
     */
    public void testProcessInOnly() {
        System.out.println("Testing processInOnly");
    }

    /**
    * Test of processReplyMessage method, of class com.sun.jbi.filebc.InboundMessageProcessor.
    */
    public void testProcessReplyMessage() {
        System.out.println("Testing processReplyMessage");

        final Map inboundReplys = new HashMap();
        final Map Ids = new HashMap();

        msgExchange = mock(MessageExchange.class);
        
        msgExchange.stubs().method("isTransacted").will(returnValue(false));
        msgExchange.stubs().method("getPattern");
        
        // testing invalid message exchange
        try {
            instance.processReplyMessage((MessageExchange) msgExchange.proxy());
            Assert.fail(
                "Failed to test processReplyMessage when an validation exception should be caught.");
        } catch (final Exception e) {
            System.out.println(
                "Successfully tested processReply when the MessageExchange is neither in-out nor in-only");
        }

        // testing no exchangeId found
        msgExchange = mock(InOut.class);
        msgExchange.expects(atLeastOnce()).method("getExchangeId")
                   .will(returnValue("789"));
        Ids.put("123", "");
        
        msgExchange.stubs().method("isTransacted").will(returnValue(false));
        //instance.setInboundExchangeIds(Ids);
        try {
            instance.processReplyMessage((InOut) msgExchange.proxy());
            System.out.println(
                "Successfully tested processReply when message exchange ID is not found");
        } catch (final Exception e) {
            Assert.fail(
                "Failed to test processReplyMessage when message exchange ID is not found.");
        }

        // testing exchange Id is found and status is DONE
        msgExchange.expects(atLeastOnce()).method("getExchangeId")
                   .will(returnValue("123"));

        //msgExchange.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.DONE));
        try {
            instance.processReplyMessage((InOut) msgExchange.proxy());

            final Map inboundIds = InboundMessageProcessor.getInboundExchanges();
            Assert.assertTrue(!inboundIds.containsKey("123"));
        } catch (final Exception e) {
            Assert.fail(
                "Failed to test processReplyMessage when message exchange status is DONE.");
        }

        System.out.println("Successfully tested processReplyMessage");
    }
}
