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


/**
 * Unit tests for the SIPUser class
 */
public class SIPUserTest extends TestCase {
    public static final int PROXY_TIMEOUT = 9;
    public static final int PROXY_PORT_1 = 9998;
    public static final int PROXY_PORT_2 = 9999;

    public void testGenerateUniqueID() {
        String username1 = "username1";
        String username2 = "username2";
        String proxyhost = "host";

        String id1 = SIPUser.generateUniqueID(username1, proxyhost);
        String id2 = SIPUser.generateUniqueID(username2, proxyhost);
        String id3 = SIPUser.generateUniqueID(username2, proxyhost);

        assertFalse("Test1: Unique IDs Should Not Match",
            id1.equalsIgnoreCase(id2));
        assertTrue("Test2: Unique IDs Should Match", id2.equalsIgnoreCase(id3));
    }

    public void testConstructor() throws Exception {
        String username = "user";
        String password = "pass";
        String proxyhost = "host";

        // test creating a new SIPUser for a non-existant username/proxyhost
        // combination
        SIPUser user1 = SIPUser.getSIPUser(username, password, proxyhost,
                PROXY_PORT_1, PROXY_TIMEOUT);
        assertTrue("User is Null", user1 != null);

        // test creating a new SIPUser when a user with the same username/proxy
        // host alreday exists
        SIPUser user2 = SIPUser.getSIPUser(username, password, proxyhost,
                PROXY_PORT_2, PROXY_TIMEOUT);
        assertTrue("Users are not equal", user1 == user2);
    }

    public void testSessions() throws Exception {
        String username = "user";
        String password = "pass";
        String proxyhost = "host";

        SIPUser user1 = SIPUser.getSIPUser(username, password, proxyhost,
                PROXY_PORT_1, PROXY_TIMEOUT);

        String remoteUri1 = "uri1";

        assertTrue("Should not have found a session",
            !user1.sessionExists(remoteUri1));

        SIPSession session1 = new SIPSession(remoteUri1, null);
        user1.addSession(session1);

        assertTrue("Should have found a session",
            user1.sessionExists(remoteUri1));
        assertTrue("Coudld not retreive the session",
            user1.getSession(remoteUri1) != null);

        SIPSession session2 = new SIPSession(remoteUri1, null);

        try {
            user1.addSession(session2);
            fail("Should not have been able to add the session");
        } catch (Exception e) {
            assertTrue("Could not successfully remove the session",
                user1.removeSession(remoteUri1) != null);
            assertTrue("Session should have already been removed",
                user1.getSession(remoteUri1) == null);
        }
    }
}
