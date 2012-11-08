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
package com.gestalt.jbi.xmpp.component;

import com.gestalt.jbi.component.AbstractComponent;
import com.gestalt.jbi.component.lifecycle.AbstractComponentLifeCycle;
import com.gestalt.jbi.component.lifecycle.AbstractMessageExchangeProcessor;
import com.gestalt.jbi.xmpp.component.smack.XMPPConnectionManager;

import org.jivesoftware.smack.XMPPConnection;

import org.jivesoftware.smackx.muc.MultiUserChat;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.jbi.JBIException;

import javax.management.ObjectName;


public class XMPPComponentLifeCycle extends AbstractComponentLifeCycle {
    private XMPPConnectionManager xmppConnectionManager;
    private ConcurrentHashMap<String, MultiUserChat> chat = new ConcurrentHashMap<String, MultiUserChat>();
    private ConcurrentHashMap<XMPPEndpoint, AbstractXMPPConsumerHandler> handlers = new ConcurrentHashMap<XMPPEndpoint, AbstractXMPPConsumerHandler>();

    public XMPPComponentLifeCycle(AbstractComponent arg0) {
        super(arg0);
    }

    public XMPPConnectionManager getXmppConnectionManager() {
        return xmppConnectionManager;
    }

    public void setXmppConnectionManager(
        XMPPConnectionManager xmppConnectionManager) {
        this.xmppConnectionManager = xmppConnectionManager;
    }

    public Map<String, MultiUserChat> getGroupChats() {
        return chat;
    }

    public void setGroupChats(String key, String roomName, XMPPConnection conn) {
        chat.putIfAbsent(key, new MultiUserChat(conn, roomName));
    }

    public void removeGroupChat(String key) {
        chat.remove(key);
    }

    public Map<XMPPEndpoint, AbstractXMPPConsumerHandler> getHandlers() {
        return handlers;
    }

    public void setHandlers(XMPPEndpoint key, AbstractXMPPConsumerHandler value) {
        handlers.putIfAbsent(key, value);
    }

    public void removeHandler(XMPPEndpoint key) {
        handlers.remove(key);
    }

    public Object createMBean() {
        return null;
    }

    public ObjectName createMBeanName() {
        return null;
    }

    public AbstractMessageExchangeProcessor createMessageExchangeProcessor() {
        return new AbstractMessageExchangeProcessor(component);
    }

    public void doInit() throws JBIException {
        this.xmppConnectionManager = new XMPPConnectionManager();
    }

    public void doShutDown() throws JBIException {
    }

    public void doStart() throws JBIException {
    }

    public void doStop() throws JBIException {
    }
}
