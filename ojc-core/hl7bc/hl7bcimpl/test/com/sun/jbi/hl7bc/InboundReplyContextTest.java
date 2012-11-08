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

package com.sun.jbi.hl7bc;

import javax.xml.transform.Source;

import junit.framework.*;
import org.jmock.*;

import com.sun.jbi.hl7bc.extservice.server.HL7Callback;

/**
 * @author Raghunadh
 */
public class InboundReplyContextTest extends MockObjectTestCase {
    InboundReplyContext instance = null;

    Mock hl7Request = mock(Source.class);

    Source src = (Source) hl7Request.proxy();

    Mock listener = mock(MessageExchangeReplyListener.class);

    MessageExchangeReplyListener msgExeRepLis = (MessageExchangeReplyListener) listener.proxy();

    Mock hl7Callback = mock(HL7Callback.class);

    HL7Callback hl7CalBack = (HL7Callback) hl7Callback.proxy();

    public InboundReplyContextTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new InboundReplyContext((long) 1000, src, msgExeRepLis, hl7CalBack, "AL");
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(InboundReplyContextTest.class);

        return suite;
    }

    /**
     * Test of getRequestInvocationTime method, of class com.sun.jbi.hl7bc.InboundReplyContext.
     */
    public void testGetRequestInvocationTime() {
        System.out.println("Testing RequestInvocationTime");
        long exp = 1000;
        long result = instance.getRequestInvocationTime();
        assertEquals(exp, result);
    }

    /**
     * Test of getHL7Request method, of class com.sun.jbi.hl7bc.InboundReplyContext.
     */
    public void testGetHL7Request() {
        System.out.println("Testing getHL7Request");

        Source result = instance.getHL7Request();
        assertEquals(src, result);
    }

    /**
     * Test of getMessageExchangeReplyListener method, of class
     * com.sun.jbi.hl7bc.InboundReplyContext.
     */
    public void testGetMessageExchangeReplyListener() {
        System.out.println("Testing getMessageExchangeReplyListener");

        MessageExchangeReplyListener result = instance.getMessageExchangeReplyListener();
        assertEquals(msgExeRepLis, result);
    }

    /**
     * Test of getHL7Callback method, of class com.sun.jbi.hl7bc.InboundReplyContext.
     */
    public void testGetHL7Callback() {
        System.out.println("Testing getHL7Callback");

        HL7Callback result = instance.getHL7Callback();
        assertEquals(hl7CalBack, result);
    }

    /**
     * Test of getAppAckCondition method, of class com.sun.jbi.hl7bc.InboundReplyContext.
     */
    public void testGetAppAckCondition() {
        System.out.println("Testing getAppAckCondition");

        String result = instance.getAppAckCondition();
        assertEquals("AL", result);
    }

}
