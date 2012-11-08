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

import com.gestalt.jbi.xmpp.component.AbstractXMPPConsumerHandler;
import com.gestalt.jbi.xmpp.component.IProviderHandler;
import com.gestalt.jbi.xmpp.component.XMPPEndpoint;

import org.jivesoftware.smack.PacketListener;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Packet;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.Logger;


public class IncomingPacketListener implements PacketListener {
    private static final int THREAD_POOL_SIZE = 5;
    private static final Logger log = Logger.getLogger(IncomingPacketListener.class.getName());
    private ExecutorService handlerThreadPool = Executors.newFixedThreadPool(THREAD_POOL_SIZE);
    private XMPPEndpoint endpoint;

    public IncomingPacketListener(XMPPEndpoint endpoint) {
        this.endpoint = endpoint;
    }

    public void processPacket(Packet packet) {
        if (packet instanceof Message) {
            Message message = (Message) packet;
            IProviderHandler provider = getEndpointProviderHandler(message);

            if (provider != null) {
                provider.setMessage(message);

                return;
            }

            AbstractXMPPConsumerHandler handler = (AbstractXMPPConsumerHandler) endpoint.getProcessor();
            handler.setMessage(message);
            handlerThreadPool.execute(handler);

            return;
        }

        log.log(Level.INFO,
            "Unknown XMPP Packet class: " + packet.getClass().getName());
    }

    protected IProviderHandler getEndpointProviderHandler(Message message) {
        return XMPPEndpoint.getWaitingProcessor(message.getPacketID(),
            message.getFrom());
    }
}
