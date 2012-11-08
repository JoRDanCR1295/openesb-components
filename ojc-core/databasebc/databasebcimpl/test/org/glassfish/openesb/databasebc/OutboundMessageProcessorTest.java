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
 * @(#)OutboundMessageProcessorTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import org.jmock.*;
import java.sql.Connection;
import java.util.HashMap;
import java.util.Map;
import javax.jbi.messaging.*;

import javax.xml.namespace.QName;


public class OutboundMessageProcessorTest extends org.jmock.cglib.MockObjectTestCase {
    static final QName THE_OPERATION = new QName("myoperation");
    static final QName THE_SERVICE = new QName("myServiceName");
    static final String THE_ENDPOINT = "myEndpointName";
    OutboundMessageProcessor instance = null;
    Mock deliveryChannel;
    Mock messageExchange;
    Mock endpoint;
    Map endpoints = new HashMap();
    Mock componentContext;
    Map inboundMessageExchanges = new HashMap();
    Map operations = new HashMap();
    Mock inonly;
    Mock inout;
    Mock listener;
    Mock normMsg;
    Mock opMetaData;
    Mock connection;

    //@Override
	public void setUp() throws Exception {
        deliveryChannel = mock(MessagingChannel.class);
        messageExchange = mock(MessageExchange.class);
        //componentContext = mock(JDBCComponentContext.class);
        endpoint = mock(EndpointBean.class);
        connection = mock(Connection.class);
        instance = new OutboundMessageProcessor((MessagingChannel) deliveryChannel.proxy(),
                (MessageExchange) messageExchange.proxy(), endpoints,
                JDBCComponentContext.getInstance(),
                inboundMessageExchanges);
    }

    //@Override
	public void tearDown() throws Exception {
        super.tearDown();
    }

    public void testOutboundMessageProcessor() {
    }

    public void testRun() {
        System.out.println(
            "Testing run() for the scenario where the message exchange pattern is not valid");

        try {
            /*messageExchange.expects(atLeastOnce()).method("getExchangeId");

            final JDBCOperation jdbcoperation = new JDBCOperation();
            inboundMessageExchanges.put(OutboundMessageProcessorTest.THE_OPERATION, jdbcoperation);

            final JDBCOperationInput jdbcopInput = new JDBCOperationInput();
            jdbcopInput.setJDBCSql(new JDBCOperationInput());

            messageExchange.expects(atLeastOnce()).method("getPattern");
            messageExchange.expects(atLeastOnce()).method("getEndpoint");

            // 1. testing the case where message exchange pattern is not found (null)
            inboundMessageExchanges.remove(OutboundMessageProcessorTest.THE_OPERATION);
            inboundMessageExchanges.put("dummyOperation", "inonly");
            instance.run();
            // 2. testing the case where message exchange status is invalid (not inonly or inout)
            inboundMessageExchanges.put(OutboundMessageProcessorTest.THE_OPERATION, "outin");
            //instance.run();*/
        } catch (final Exception e) {
            System.out.println("failed to execute");

            //System.out.println(e.toString());
        }
    }

    public void testExecute() {
        System.out.println(
            "Testing run() for the scenario where the message exchange pattern is not valid");

        try {
            /*messageExchange.expects(atLeastOnce()).method("getExchangeId");

            final JDBCOperation jdbcoperation = new JDBCOperation();
            inboundMessageExchanges.put(OutboundMessageProcessorTest.THE_OPERATION, jdbcoperation);

            final JDBCOperationInput jdbcopInput = new JDBCOperationInput();
            jdbcopInput.setJDBCSql(new JDBCOperationInput());

            messageExchange.expects(atLeastOnce()).method("getPattern");
            messageExchange.expects(atLeastOnce()).method("getEndpoint");

            // 1. testing the case where message exchange pattern is not found (null)
            inboundMessageExchanges.remove(OutboundMessageProcessorTest.THE_OPERATION);
            inboundMessageExchanges.put("dummyOperation", "inonly");
            instance.execute();
            // 2. testing the case where message exchange status is invalid (not inonly or inout)
            inboundMessageExchanges.put(OutboundMessageProcessorTest.THE_OPERATION, "outin");
            instance.execute();*/
        } catch (final Exception e) {
            System.out.println("failed to execute");

            //System.out.println(e.toString());
        }
    }

    public void testProcessInOnlyInbound() {
        inonly = mock(InOnly.class);
        listener = mock(MessageExchangeReplyListener.class);

        try {
            /*inonly.expects(atLeastOnce()).method("getStatus");
            inonly.expects(atLeastOnce()).method("getEndpoint");
            listener.expects(atLeastOnce()).method("processReplyMessage");
            instance.processInOnlyInbound((InOnly) inonly.proxy(),
                (EndpointBean) endpoint.proxy(),
                (MessageExchangeReplyListener) listener.proxy());*/
        } catch (final Exception e) {
            //e.printStackTrace();			
        }
    }

    public void testProcessInOut() {
        try {
            inout = mock(InOut.class);
            /*inout.expects(atLeastOnce()).method("getStatus");
            endpoint.expects(atLeastOnce()).method("getValueObj");
            inout.expects(atLeastOnce()).method("getOperation");
            //instance.processInOut((InOut) inout.proxy(),
            //    (EndpointBean) endpoint.proxy());*/
        } catch (final Exception e) {
            //e.printStackTrace();
        }
    }

    public void testProcessInOnly() {
        try {
            inonly = mock(InOnly.class);
            //endpoint.expects(atLeastOnce()).method("getEndpointStatus");
            //instance.processInOnly((InOnly) inonly.proxy(),
             //   (EndpointBean) endpoint.proxy());
        } catch (final Exception e) {
            //e.printStackTrace();
        }
    }

    public void testExecuteOutboundSQLSelect() {
        try {
            normMsg = mock(NormalizedMessage.class);
            opMetaData = mock(OperationMetaData.class);
            /*opMetaData.expects(atLeastOnce()).method("getJDBCSql");
            instance.executeOutboundSQLSelect((NormalizedMessage) normMsg.proxy(),
                (EndpointBean) endpoint.proxy(),
                (OperationMetaData) opMetaData.proxy(),
                (Connection) connection.proxy());*/
        } catch (final Exception e) {
            //e.printStackTrace();
        }
    }

    public void testExecuteOutboundSQL() {
        try {
            normMsg = mock(NormalizedMessage.class);
            opMetaData = mock(OperationMetaData.class);
            /*opMetaData.expects(atLeastOnce()).method("getJDBCSql");
            instance.executeOutboundSQL((NormalizedMessage) normMsg.proxy(),
                (EndpointBean) endpoint.proxy(),
                (OperationMetaData) opMetaData.proxy(),
                (Connection) connection.proxy());*/
        } catch (final Exception e) {
            //e.printStackTrace();
        }
    }

    public void testExecuteOutboundProc() {
        try {
            normMsg = mock(NormalizedMessage.class);
            opMetaData = mock(OperationMetaData.class);
           /* opMetaData.expects(atLeastOnce()).method("getJDBCSql");
            instance.executeOutboundProc((NormalizedMessage) normMsg.proxy(),
                (EndpointBean) endpoint.proxy(),
                (OperationMetaData) opMetaData.proxy(),
                (Connection) connection.proxy());*/
        } catch (final Exception e) {
            //e.printStackTrace();
        }
    }
}
