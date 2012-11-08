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

import com.gestalt.jbi.xmpp.component.XMPPConsumerHandler;
import com.gestalt.jbi.xmpp.component.XMPPEndpoint;
import com.gestalt.jbi.xmpp.component.XMPPProviderHandler;

import junit.framework.TestCase;

import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smack.packet.Presence;

import org.jmock.Expectations;
import org.jmock.Mockery;

import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;

import org.jmock.lib.legacy.ClassImposteriser;

import org.junit.Test;

import org.junit.runner.RunWith;


@RunWith(JMock.class)
public class IncomingPacketListenerTest extends TestCase {
    Mockery context = new JUnit4Mockery() {

            {
                setImposteriser(ClassImposteriser.INSTANCE);
            }
        };

    /**
     * Test that a message packet will be handled properly by a ProviderHandler.
     *
     * @throws Exception
     */
    @Test
    public void testProcessesMessagePacket() throws Exception {
        final XMPPProviderHandler provider = context.mock(XMPPProviderHandler.class);
        final XMPPEndpoint endpoint = context.mock(XMPPEndpoint.class);
        final Message message = context.mock(Message.class);

        context.checking(new Expectations() {

                {
                    one(provider).getMessage();
                    will(returnValue(message));
                }
            });

        IncomingPacketListener listener = new IncomingPacketListener(endpoint) {
                protected XMPPProviderHandler getEndpointProviderHandler(
                    Message message) {
                    return new XMPPProviderHandler(null);
                }
            };

        listener.processPacket(message);
        assertNotNull("Message is NULL", provider.getMessage());
    }

    /**
     * Tests that a message not previously being tracked by a provider will be
     * scheduled to be sent by a consumer handler.
     *
     * @throws Exception
     */
    @Test
    public void testSetsMessageOnConsumerHandler() throws Exception {
        final XMPPEndpoint endpoint = context.mock(XMPPEndpoint.class);
        final Message message = context.mock(Message.class);
        final XMPPConsumerHandler handler = context.mock(XMPPConsumerHandler.class);

        context.checking(new Expectations() {

                {
                    allowing(message).getFrom();
                    will(returnValue("bob"));
                    allowing(message).getPacketID();
                    will(returnValue("pid1"));
                    one(endpoint).getProcessor();
                    will(returnValue(handler));
                    one(handler).setMessage(message);
                    allowing(handler).run();
                }
            });

        IncomingPacketListener listener = new IncomingPacketListener(endpoint);
        listener.processPacket(message);
    }

    /**
     * Test that a non Message packet (e.g. Presence) will be ignored.
     *
     * @throws Exception
     */
    @Test
    public void testDoesNotProcessNonMessagePacket() throws Exception {
        // This will be a Presence packet, not a Message Packet
        final Packet packet = new Presence(Presence.Type.available);
        final XMPPEndpoint endpoint = context.mock(XMPPEndpoint.class);

        IncomingPacketListener listener = new IncomingPacketListener(endpoint) {
                protected XMPPProviderHandler getEndpointProviderHandler(
                    Message message) {
                    return new MockXMPPProviderHandler(null);
                }
            };

        listener.processPacket(packet);

        assertFalse("A Message was set unexpectedly",
            (MockXMPPProviderHandler.isMessageSet));
    }
}
