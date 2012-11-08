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
 * @(#)InboundReplyContextTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc;

import javax.jbi.messaging.NormalizedMessage;

import junit.framework.*;
import org.jmock.*;

import com.sun.jbi.swiftbc.extservice.server.SwiftCallback;

/**
 * @author Raghunadh
 */
public class InboundReplyContextTest extends MockObjectTestCase {
    InboundReplyContext instance = null;

    Mock SwiftRequest = mock(NormalizedMessage.class);

    NormalizedMessage norMsg = (NormalizedMessage) SwiftRequest.proxy();

    Mock listener = mock(MessageExchangeReplyListener.class);

    MessageExchangeReplyListener msgExeRepLis = (MessageExchangeReplyListener) listener.proxy();

    Mock SwiftCallback = mock(SwiftCallback.class);

    SwiftCallback SwiftCalBack = (SwiftCallback) SwiftCallback.proxy();

    public InboundReplyContextTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new InboundReplyContext((long) 1000, norMsg, msgExeRepLis, SwiftCalBack, "AL");
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(InboundReplyContextTest.class);

        return suite;
    }

    /**
     * Test of getRequestInvocationTime method, of class com.sun.jbi.swiftbc.InboundReplyContext.
     */
    public void testGetRequestInvocationTime() {
        System.out.println("Testing RequestInvocationTime");
        long exp = 1000;
        long result = instance.getRequestInvocationTime();
        assertEquals(exp, result);
    }

    /**
     * Test of getSwiftRequest method, of class com.sun.jbi.swiftbc.InboundReplyContext.
     */
    public void testGetSwiftRequest() {
        System.out.println("Testing getSwiftRequest");

        NormalizedMessage result = instance.getSwiftRequest();
        assertEquals(norMsg, result);
    }

    /**
     * Test of getMessageExchangeReplyListener method, of class
     * com.sun.jbi.swiftbc.InboundReplyContext.
     */
    public void testGetMessageExchangeReplyListener() {
        System.out.println("Testing getMessageExchangeReplyListener");

        MessageExchangeReplyListener result = instance.getMessageExchangeReplyListener();
        assertEquals(msgExeRepLis, result);
    }

    /**
     * Test of getSwiftCallback method, of class com.sun.jbi.swiftbc.InboundReplyContext.
     */
    public void testGetSwiftCallback() {
        System.out.println("Testing getSwiftCallback");

        SwiftCallback result = instance.getSwiftCallback();
        assertEquals(SwiftCalBack, result);
    }

    /**
     * Test of getAppAckCondition method, of class com.sun.jbi.swiftbc.InboundReplyContext.
     */
    public void testGetAppAckCondition() {
        System.out.println("Testing getAppAckCondition erase me..");

//        String result = instance.getAppAckCondition();
//        assertEquals("AL", result);
    }

}
