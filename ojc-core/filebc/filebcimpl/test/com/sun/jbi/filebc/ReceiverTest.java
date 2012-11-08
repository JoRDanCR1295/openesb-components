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
 * @(#)ReceiverTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc;

import junit.framework.*;
import java.util.List;
import java.util.HashMap;
import org.jmock.*;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import javax.jbi.component.ComponentContext;

/**
 *
 * @author sweng
 */
public class ReceiverTest extends org.jmock.cglib.MockObjectTestCase {

    Receiver instance = null;
    Mock runtimeConfig = null;
    Mock deliveryChannel = null;
    private Mock componentContext = null;

    public ReceiverTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        componentContext = mock(ComponentContext.class);
        KeyStoreUtilClient keystoreUtil = new KeyStoreUtilClient((ComponentContext) componentContext.proxy());
        runtimeConfig = mock(RuntimeConfiguration.class,
                new Class[]{String.class, KeyStoreUtilClient.class},
                new Object[]{"test/com/sun/jbi/filebc/testDir", keystoreUtil});
        deliveryChannel = mock(MessagingChannel.class);
        runtimeConfig.expects(once()).method("getThreads").will(returnValue(new Integer(10)));
        runtimeConfig.expects(once()).method("addNotificationListener").withAnyArguments();
        instance = new Receiver((MessagingChannel) deliveryChannel.proxy(),
                new HashMap(),
                (RuntimeConfiguration) runtimeConfig.proxy());
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ReceiverTest.class);

        return suite;
    }

    /**
     * Test of setThreads method, of class com.sun.jbi.filebc.Receiver.
     */
    public void testSetThreads() {
        System.out.println("Testing setThreads");

        instance.setThreads(8);
        List workers = instance.getWorkers();
        assertEquals(8, workers.size());
    }

    /**
     * Test of stopReceiving method, of class com.sun.jbi.filebc.Receiver.
     */
    public void testStopReceiving() {
        System.out.println("Testing stopReceiving");
        instance.stopReceiving();

        List workers = instance.getWorkers();
        assertEquals(0, workers.size());
    }
}
