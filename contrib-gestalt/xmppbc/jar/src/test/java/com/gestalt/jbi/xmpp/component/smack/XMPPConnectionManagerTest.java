/**
 *   xmpp-binding-component - XMPP Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com
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
package com.gestalt.jbi.xmpp.component.smack;

import com.gestalt.jbi.xmpp.component.XMPPEndpoint;

import junit.framework.TestCase;

import org.jivesoftware.smack.ConnectionConfiguration;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;

import org.jmock.Expectations;
import org.jmock.Mockery;

import org.jmock.integration.junit4.JUnit4Mockery;

import org.jmock.lib.legacy.ClassImposteriser;

import org.junit.Test;


public class XMPPConnectionManagerTest extends TestCase {
    private static final int CLIENT_PORT = 5555;
    Mockery context = new JUnit4Mockery() {

            {
                setImposteriser(ClassImposteriser.INSTANCE);
            }
        };

    /**
     * Create a successful XMPP connnection.
     *
     * @throws Exception
     */
    @Test
    public void testCreatesConnection() throws Exception {
        final XMPPEndpoint endpoint = context.mock(XMPPEndpoint.class);
        final XMPPConnection mockConnection = context.mock(XMPPConnection.class);

        context.checking(new Expectations() {

                {
                    one(mockConnection)
                        .login("testuser", "testpass", "testresource");
                }
            });

        XMPPConnectionManager manager = new XMPPConnectionManager() {
                protected XMPPConnection createXMPPConnection(String username,
                    String password, String resource,
                    ConnectionConfiguration connectionConfiguration)
                    throws XMPPException {
                    return mockConnection;
                }
            };

        XMPPConnection testConnection = manager.createConnection("nosuchserver",
                CLIENT_PORT, "testuser", "testpass", "testresource", false,
                false, endpoint);

        assertNotNull("XMPP connection not established", testConnection);
    }

    /**
     * Test the successful destroy of a previously registered connection. It
     * will be removed from the map of known connections.
     *
     * @throws Exception
     */
    @Test
    public void testDestroysExistingConnection() throws Exception {
        final XMPPEndpoint endpoint = context.mock(XMPPEndpoint.class);
        final XMPPConnection mockConnection = context.mock(XMPPConnection.class);

        context.checking(new Expectations() {

                {
                    one(mockConnection)
                        .login("testuser", "testpass", "testresource");
                    one(mockConnection).disconnect();
                }
            });

        XMPPConnectionManager manager = new XMPPConnectionManager() {
                protected XMPPConnection createXMPPConnection(String username,
                    String password, String resource,
                    ConnectionConfiguration connectionConfiguration)
                    throws XMPPException {
                    return mockConnection;
                }
            };

        XMPPConnection testConnection = manager.createConnection("nosuchserver",
                CLIENT_PORT, "testuser", "testpass", "testresource", false,
                false, endpoint);

        assertNotNull("XMPP connection not established", testConnection);

        // this stroy will remove the connection
        manager.destroyConnection("nosuchserver", "testuser", "testresource",
            endpoint);

        // this time, the connection will not be found and will silently
        // continue along
        manager.destroyConnection("nosuchserver", "testuser", "testresource",
            endpoint);
    }

    /**
     * Try to destroy a connection that is not registered. The lookip in the map
     * will silently fail with no exceptions.
     *
     * @throws Exception
     */
    @Test
    public void testDoesNotDestroyNonexistingConnection()
        throws Exception {
        final XMPPEndpoint endpoint = context.mock(XMPPEndpoint.class);
        final XMPPConnection mockConnection = context.mock(XMPPConnection.class);

        context.checking(new Expectations() {

                {
                    never(mockConnection).disconnect();
                }
            });

        XMPPConnectionManager manager = new XMPPConnectionManager();

        manager.destroyConnection("nosuchserver2", "testuser2",
            "testresource2", endpoint);
    }
}
