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
 * @(#)MQClientAgentTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;

import com.ibm.mq.MQQueue;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/** @author rchen */
public class MQClientAgentTest extends TestCase {
    MQClientAgent mInstance;
    MQClientConfiguration cliConfig;
    boolean connectSetUpToMQServer = false;

    public MQClientAgentTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        cliConfig = new MQClientConfiguration();
        cliConfig.setQueueManagerName("QM_test");
        cliConfig.setHost("localhost");
        cliConfig.setChannelName("S_test_nosec");
        cliConfig.setPort(1415);
        cliConfig.setQueueName("default");
        cliConfig.setReceiveMode(true);
        mInstance = new MQClientAgent(cliConfig);
        try {
            mInstance.connect();
            connectSetUpToMQServer = true;
        } catch (Exception ex) {
            System.out.println("Cannot connect to a MQ Series Server will pass all connection test");
        }
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        return new TestSuite(MQClientAgentTest.class);
    }

    /** Test of connect method, of class com.sun.jbi.mqbc.extservices.MQClientAgent. */
    public void testConnect() {
        try {
            mInstance.connect();
        } catch (Exception ex) {
            String exStr = ex.getMessage();
            if (!connectSetUpToMQServer) {
                return;
            } else {
                assertTrue(exStr, exStr.contains("1933: Client already connected"));
            }
        }
    }

    /** Test of accessQueue method, of class com.sun.jbi.mqbc.extservices.MQClientAgent. */
    public void testAccessQueue() throws Exception {
        if (!this.connectSetUpToMQServer) {
            return;
        }
        String queueName = "default";
        int options = 0;
        String qmgrName = "QM_test";
        mInstance.accessQueue(queueName, options, qmgrName);
    }

    /** Test of selectQueue method, of class com.sun.jbi.mqbc.extservices.MQClientAgent. */
    public void testSelectQueue() throws Exception {
        if (!this.connectSetUpToMQServer) {
            return;
        }
        String queueName = "default";
        MQQueue result = mInstance.selectQueue(queueName);
        assertNotNull(result);
    }


    /** Test of isConnected method, of class com.sun.jbi.mqbc.extservices.MQClientAgent. */
    public void testIsConnected() {
        if (!this.connectSetUpToMQServer) {
            return;
        }
        boolean result = mInstance.isConnected();
        assertTrue(result);
    }


    /** Test of getXAResource method, of class com.sun.jbi.mqbc.extservices.MQClientAgent. */
    public void testGetXAResource() {
        if (!this.connectSetUpToMQServer) {
            return;
        }
        try {
            mInstance.getXAResource();
        } catch (Exception ex) {
            String exStr = "******" + ex.getMessage();
            assertTrue(exStr, exStr.contains(
                    "1930: Cannot provide XA resource - not in transactional mode"));
        }
    }

    /** Test of newMessage method, of class com.sun.jbi.mqbc.extservices.MQClientAgent. */
    public void testNewMessage() throws Exception {
        if (!this.connectSetUpToMQServer) {
            return;
        }
        ExtServiceMQMessage result = mInstance.newMessage();
        assertNotNull(result);
    }
}
