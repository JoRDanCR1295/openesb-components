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

import org.jmock.Mockery;

import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;

import org.junit.Test;

import org.junit.runner.RunWith;

import java.util.Properties;

import javax.sip.ListeningPoint;
import javax.sip.PeerUnavailableException;
import javax.sip.SipFactory;
import javax.sip.SipStack;


/**
 * Created by IntelliJ IDEA.
 * User: csturtz
 * Date: May 23, 2007
 * Time: 4:16:52 PM
 * To change this template use File | Settings | File Templates.
 */
@RunWith(JMock.class)
public class SIPConnectionManagerTest extends TestCase {
    static final Integer PROXY_TIMEOUT = 10;
    static final int PROXY_PORT = 12345;
    Mockery context = new JUnit4Mockery();

    @Test
    public void testGetInstance() {
        SIPConnectionManager manager1 = SIPConnectionManager.getInstance();
        assertTrue("Instance is null", manager1 != null);

        SIPConnectionManager manager2 = SIPConnectionManager.getInstance();
        assertTrue("Instances are not the same", manager1 == manager2);
    }

    @Test
    public void testCreateListeningPoint() throws PeerUnavailableException {
        Properties p = new Properties();
        p.setProperty("javax.sip.STACK_NAME", "testCreateListeningPointStack");

        SipStack sipStack = SipFactory.getInstance().createSipStack(p);

        SIPConnectionManager manager = SIPConnectionManager.getInstance();
        ListeningPoint lp = manager.createListeningPoint(sipStack);

        assertTrue("ListeningPoint is null", lp != null);
        assertTrue("Port is out of range",
            ((lp.getPort() > 0) &&
            (lp.getPort() < SIPConnectionManager.MAX_PORT)));
    }

    @Test
    public void testDetermineLocalIP() throws Exception {
        SIPConnectionManager manager = SIPConnectionManager.getInstance();
        String localIP = manager.determineLocalIP("localhost", PROXY_PORT);
        assertTrue("LocalIP is null", localIP != null);
    }

    @Test
    public void testCreateConnection() throws Exception {
        String username = "username";
        String username2 = "username2";
        String password = "pass";
        String proxyhost = "localhost";

        SIPUser user = SIPUser.getSIPUser(username, password, proxyhost,
                PROXY_PORT, PROXY_TIMEOUT);
        SIPUser user2 = SIPUser.getSIPUser(username2, password, proxyhost,
                PROXY_PORT, PROXY_TIMEOUT);
        SIPConnectionManager manger = SIPConnectionManager.getInstance();

        SIPConnection connection = manger.createConnection(user);
        assertTrue("SIPConnection 1 is null", connection != null);

        SIPConnection connection2 = manger.createConnection(user2);
        assertTrue("SIPConnection 2 is null", connection2 != null);

        manger.removeConnection(user, connection.getSipStack());
        manger.removeConnection(user2, connection2.getSipStack());
    }

    @Test
    public void testRemoveConnection() throws Exception {
        String username = "username";
        String password = "pass";
        String proxyhost = "localhost";

        SIPUser user = SIPUser.getSIPUser(username, password, proxyhost,
                PROXY_PORT, PROXY_TIMEOUT);
        SIPConnectionManager manger = SIPConnectionManager.getInstance();
        SIPConnection connection = manger.createConnection(user);

        manger.removeConnection(user, connection.getSipStack());

        SIPConnection connection2 = manger.createConnection(user);
        assertTrue("Connections should not be the same",
            connection != connection2);
    }
}
