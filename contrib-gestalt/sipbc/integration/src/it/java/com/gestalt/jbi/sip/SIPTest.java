/**
 *   sip-binding-component - SIP Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com/
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.sip;

import junit.framework.TestCase;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.xml.XmlBeanFactory;

import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.sip.message.Request;

import com.sun.jbi.internationalization.Messages;


/**
 * Integration tests for a single-domain and inter-domain environments.
 *
 * Inter-domain tests can be skipped if the Spring loaded value for
 * proxyHost2 is null, empty, or equal to that of proxyHost1
 */
public class SIPTest extends TestCase {
    private static final Logger log = Logger.getLogger(SIPTest.class.getName());
    private static final Messages messages = Messages.getMessages(SIPTest.class);

    private static final long TIMEOUT = 10000;
    private static final String SPRING_FILE = "sip.xml";
    private static final String PROXY_HOST_1_BEAN_NAME = "proxyHost1";
    private static final String PROXY_HOST_2_BEAN_NAME = "proxyHost2";
    private static final int PROXYPORT = 5060;
    private static final int PROXYTIMEOUT = 10000;
    private static final String USERNAME1 = "alice";
    private static final String PASSWORD1 = "alice";
    private static final String USERNAME2 = "bob";
    private static final String PASSWORD2 = "bob";
    private static final SIPConnectionManager manager = SIPConnectionManager.getInstance();
    static String proxyHost1;
    static String proxyHost2 = null;
    private static boolean skipInterDomain = false;

    static {
        Resource resource = new ClassPathResource(SPRING_FILE);
        BeanFactory factory = new XmlBeanFactory(resource);
        proxyHost1 = (String) factory.getBean(PROXY_HOST_1_BEAN_NAME);

        try {
            proxyHost2 = (String) factory.getBean(PROXY_HOST_2_BEAN_NAME);
        } catch (Exception e) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00600.exceptionTryingToGetSpringBean",
                PROXY_HOST_2_BEAN_NAME));
        }

        if ((proxyHost1 == null) || proxyHost1.equals("")) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00601.usableValueForProxyHost1NotFoundInTheSpringXMLFile"));
            fail();
        }

        if ((proxyHost2 == null) || proxyHost2.equals("") ||
                proxyHost2.equals(proxyHost1)) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00602.usableValueForProxyHost2NotFoundInTheSpringXMLFileInterdomainTestsWillBeSkippedAutomaticallyPassed"));
            skipInterDomain = true;
        }

        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Using ProxyHost1: " + proxyHost1);
        }
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Using ProxyHost2: " + proxyHost2);
        }
    }

    SIPUser user1;
    SIPUser user2;
    SIPConnection user1Connection;
    SIPConnection user2Connection;
    String user1uri;
    String user2uri;
    private String sdp = "v=0\n" + "o=- 7 2 IN IP4 10.9.5.120\n" +
        "s=CounterPath X-Lite 3.0\n" + "c=IN IP4 10.9.5.120\n" + "t=0 0\n" +
        "m=audio 4400 RTP/AVP 107 119 100 106 0 105 98 8 101\n" +
        "a=alt:1 4 : qat29g4X 6hUfuSb7 10.9.5.180 4400\n" +
        "a=alt:2 3 : gm+MmRu2 0eKKRkNv 10.9.5.120 4400\n" +
        "a=alt:3 2 : wrkskVSe OW8bSlyq 192.168.106.1 4400\n" +
        "a=alt:4 1 : 3Vy+YDMr VQd7V0hq 192.168.95.1 4400\n" +
        "a=fmtp:101 0-15\n" + "a=rtpmap:107 BV32/16000\n" +
        "a=rtpmap:119 BV32-FEC/16000\n" + "a=rtpmap:100 SPEEX/16000\n" +
        "a=rtpmap:106 SPEEX-FEC/16000\n" + "a=rtpmap:105 SPEEX-FEC/8000\n" +
        "a=rtpmap:98 iLBC/8000\n" + "a=rtpmap:101 telephone-event/8000\n" +
        "a=sendrecv\n\n";

    public void setUp() {
        // nothing to do here
    }

    public void tearDown() {
        if (user1Connection != null) {
            manager.removeConnection(user1, user1Connection.getSipStack());
        }

        if (user2Connection != null) {
            manager.removeConnection(user2, user2Connection.getSipStack());
        }

        // clear out instance variables
        user1 = null;
        user2 = null;
        user1Connection = null;
        user2Connection = null;
        user1uri = null;
        user2uri = null;
    }

    public void testUser2ReceiveMessageFromUser1SingleDomain()
        throws Exception {
        initSingleDomainSetup();
        runUser2ReceivesMessageFromUser1(user1uri, user2uri, user1Connection,
            user2Connection);
    }

    public void testUser1CallsUser2SingleDomain() throws Exception {
        initSingleDomainSetup();
        runUser1CallsUser2(user1uri, user2uri, user1Connection, user2Connection);
    }

    public void testUser2ReceiveMessageFromUser1InterDomain()
        throws Exception {
        if (skipInterDomain) {
            return;
        }

        initInterDomainSetup();
        runUser2ReceivesMessageFromUser1(user1uri, user2uri, user1Connection,
            user2Connection);
    }

    public void testUser1CallsUser2InterDomain() throws Exception {
        if (skipInterDomain) {
            return;
        }

        initInterDomainSetup();
        runUser1CallsUser2(user1uri, user2uri, user1Connection, user2Connection);
    }

    /**
     * User1 sends an instant message to User2.  Test that
     * the message reaches User2.
     *
     * @param user1uri
     * @param user2uri
     * @param user1Connection
     * @param user2Connection
     */
    private void runUser2ReceivesMessageFromUser1(String user1uri,
        String user2uri, SIPConnection user1Connection,
        SIPConnection user2Connection) {
        final String msg1 = "Message from user1 to user2";

        RequestTestObserver messageObserver = new RequestTestObserver(Request.MESSAGE);
        SIPObservable.addObserver(user2uri, messageObserver);

        boolean b = user1Connection.sendRequest(Request.MESSAGE, user2uri, msg1);
        assertTrue("Sending the message returned false, indicating failure", b);

        try {
            // wait for observer to be notified of MESSAGE request
            synchronized (messageObserver) {
                messageObserver.wait(TIMEOUT);
            }
        } catch (InterruptedException e) {
            log.log(Level.SEVERE,
                messages.getString("SIPBC-E00603.receivedInterruptedExceptionWhileWaitingForObserverToBeNotified"),
                e);
        }

        assertTrue("Observer was not notified with the correct message",
            messageObserver.wasNotified(msg1));
    }

    /**
     * User1 sends INVITE to user2.  User2 responds with 200. User1 sends ACK.
     * Test user2 got the INVITE, user1 got the 200, and user2 got the ACK.
     *
     * @param user1uri
     * @param user2uri
     * @param user1Connection
     * @param user2Connection
     */
    private void runUser1CallsUser2(String user1uri, String user2uri,
        SIPConnection user1Connection, SIPConnection user2Connection) {
        final int ok = 200;

        // Create an observer to be notified when user2 receives the INVITE and ACK
        final RequestTestObserver inviteObserver = new RequestTestObserver(Request.INVITE);
        final RequestTestObserver ackObserver = new RequestTestObserver(Request.ACK);

        // Create an observer to be notified when user1 receives 200 to INVITE
        final ResponseTestObserver okObserver = new ResponseTestObserver(Request.INVITE);

        // Add observers
        SIPObservable.addObserver(user2uri, inviteObserver);
        SIPObservable.addObserver(user2uri, ackObserver);
        SIPObservable.addObserver(user1uri, okObserver);

        boolean b = user1Connection.sendRequest(Request.INVITE, user2uri, sdp);
        assertTrue("Sending the INVITE returned false, indicating failure", b);

        try {
            // wait for observer to be notified of INVITE
            synchronized (inviteObserver) {
                inviteObserver.wait(TIMEOUT);
            }
        } catch (InterruptedException e) {
            log.log(Level.SEVERE,
                messages.getString("SIPBC-E00604.receivedInterruptedExceptionWhileWaitingForObserverToBeNotified"),
                e);
        }

        assertTrue("Observer was not notified with the correct INVITE",
            inviteObserver.wasNotified(sdp));

        // send 200 to invite
        boolean b2 = user2Connection.sendResponse(Request.INVITE, user1uri, ok,
                sdp);
        assertTrue("Sending the 200 returned false, indicating failure", b2);

        try {
            // wiat for observer to be notified of 200
            synchronized (okObserver) {
                okObserver.wait(TIMEOUT);
            }

            // wait for observer to be notified of ACK
            synchronized (ackObserver) {
                ackObserver.wait(TIMEOUT);
            }
        } catch (InterruptedException e) {
            log.log(Level.SEVERE,
                messages.getString("SIPBC-E00605.receivedInterruptedExceptionWhileWaitingForObserverToBeNotified"),
                e);
        }

        assertTrue("Observer was not notified with the correct 200 response to INVITE",
            okObserver.wasNotified(ok));
        assertTrue("Observer was not notified witht he correct ACK request",
            ackObserver.wasNotified(null));
    }

    /**
     * Setup instance variables for single-domain tests
     */
    private void initSingleDomainSetup() throws Exception {
        initUser1();
        initUser2(proxyHost1);
    }

    /**
     * Setup instance variables for inter-domain tests
     */
    private void initInterDomainSetup() throws Exception {
        initUser1();
        initUser2(proxyHost2);
    }

    /**
     * Init user1:
     * create SIPUser, SIPConnection, and URI (String)
     *
     * Note: user1 will always be registered with proxyHost1
     *
     * @throws Exception
     */
    private void initUser1() throws Exception {
        user1 = SIPUser.getSIPUser(USERNAME1, PASSWORD1, proxyHost1, PROXYPORT,
                PROXYTIMEOUT);
        user1Connection = manager.createConnection(user1);
        user1uri = USERNAME1 + "@" + proxyHost1;

        if (user1Connection == null) {
            fail("Unable to create SIPConnection for User1");
        }

        if (!user1Connection.isRegistered()) {
            fail("Unable to register User1");
        }
    }

    /**
     * Inits user1 based on provided proxy:
     * create SIPUser, SIPConnection, and URI (String)
     *
     * @param proxy - proxy to register user2 with. It will be the
     * same as proxyHost1 for single-domain, or will be set to
     * proxyHost2 for inter-domain tests.
     *
     * @throws Exception
     */
    private void initUser2(String proxy) throws Exception {
        user2 = SIPUser.getSIPUser(USERNAME2, PASSWORD2, proxy, PROXYPORT,
                PROXYTIMEOUT);
        user2Connection = manager.createConnection(user2);
        user2uri = USERNAME2 + "@" + proxy;

        if (user2Connection == null) {
            fail("Unable to create SIPConnection for User1");
        }

        if (!user2Connection.isRegistered()) {
            fail("Unable to register User1");
        }
    }
}
