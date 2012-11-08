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
 * @(#)ListenerMetaTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import junit.framework.*;
import org.jmock.*;

/**
 *
 * @author sweng
 */
public class ListenerMetaTest extends MockObjectTestCase {
    Mock replyListener = mock(MessageExchangeReplyListener.class);
    ListenerMeta instance = new ListenerMeta(1000, (MessageExchangeReplyListener)replyListener.proxy());
    
    public ListenerMetaTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of getRequestInvocationTime method, of class com.sun.jbi.sapbc.ListenerMeta.
     */
    public void testGetRequestInvocationTime() {
        System.out.println("Testing getRequestInvocationTime");
        long expResult = 1000;
        long result = instance.getRequestInvocationTime();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested getRequestInvocationTime");
    }

    /**
     * Test of getMessageExchangeReplyListener method, of class com.sun.jbi.sapbc.ListenerMeta.
     */
    public void testGetMessageExchangeReplyListener() {
        System.out.println("Testing getMessageExchangeReplyListener");
        MessageExchangeReplyListener result = instance.getMessageExchangeReplyListener();
        assertTrue(result instanceof MessageExchangeReplyListener);
        
        System.out.println("Successfully tested getMessageExchangeReplyListener");
    }
    
}
