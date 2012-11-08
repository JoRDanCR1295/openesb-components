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

import com.gestalt.jbi.component.handler.AbstractMessageExchangeHandler;
import com.gestalt.jbi.component.manager.PooledEndpoint;
import com.gestalt.jbi.component.manager.ServiceUnit;
import com.gestalt.jbi.xmpp.component.smack.GroupChatManager;
import com.gestalt.jbi.xmpp.component.smack.IncomingPacketListener;
import com.gestalt.jbi.xmpp.component.smack.InvitationManager;
import com.gestalt.jbi.xmpp.component.smack.XMPPConnectionManager;
import com.gestalt.jbi.xmpp.extensions.*;

import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.filter.PacketTypeFilter;
import org.jivesoftware.smack.packet.Message;

import org.jivesoftware.smackx.muc.MultiUserChat;

import org.w3c.dom.Document;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessageExchange.Role;

import javax.wsdl.Definition;

import javax.xml.namespace.QName;


public class XMPPEndpoint extends PooledEndpoint {
    private static final java.util.logging.Logger log = Logger.getLogger(XMPPEndpoint.class.toString());
    private static ConcurrentHashMap<String, IProviderHandler> waitingHandlers = new ConcurrentHashMap<String, IProviderHandler>();
    private XMPPConnection xmppConnection;
    private IncomingPacketListener listener = new IncomingPacketListener(this);
    private XMPPBinding binding;
    private XMPPAddress address;
    private Map<QName, XMPPOperation> xmppOperations;
    private Map<XMPPOperation, XMPPOperationInput> xmppOperationInputs = new HashMap<XMPPOperation, XMPPOperationInput>();
    private Map<XMPPOperation, XMPPOperationOutput> xmppOperationOutputs = new HashMap<XMPPOperation, XMPPOperationOutput>();
    private GroupChatManager groupChatManager;
    private boolean isXbean = false;

    public XMPPEndpoint() {
        this(null, null, null, null, null, null, null, false);
    }

    public XMPPEndpoint(QName arg0, QName arg1, String arg2, Role arg3,
        Document arg4, Definition def, ServiceUnit arg5, boolean isXbean) {
        super(arg0, arg1, arg2, arg3, arg4, def, arg5);
        this.isXbean = isXbean;
    }

    public Map<QName, XMPPOperation> getOperations() {
        return xmppOperations;
    }

    public XMPPConnection getXmppConnection() {
        return xmppConnection;
    }

    public AbstractMessageExchangeHandler createConsumerExchangeHandler() {
        AbstractXMPPConsumerHandler handler;

        if (isXbean) {
            handler = new XMPPXBeanConsumerHandler(this);
        } else {
            handler = new XMPPConsumerHandler(this);
        }

        ((XMPPComponentLifeCycle) serviceUnit.getComponent().getLifeCycle()).setHandlers(this,
            handler);

        return handler;
    }

    public AbstractMessageExchangeHandler createProviderExchangeHandler() {
        if (isXbean) {
            return new XMPPXBeanProviderHandler(this);
        } else {
            return new XMPPProviderHandler(this);
        }
    }

    @Override
    public void activate() throws Exception {
        super.activate();

        try {
            XMPPConnectionManager xmppConnectionManager = ((XMPPComponentLifeCycle) serviceUnit.getComponent()
                                                                                               .getLifeCycle()).getXmppConnectionManager();
            xmppConnection = xmppConnectionManager.createConnection(address.getDomain(),
                    address.getPort(), address.getUsername(),
                    address.getPassword(), address.getResource(),
                    binding.getTlsEnabled(), binding.getSaslEnabled(), this);

            GroupChatManager manager = new GroupChatManager(this);
            InvitationManager invitationManager = new InvitationManager(manager);
            manager.setInvitationManager(invitationManager);
            MultiUserChat.addInvitationListener(xmppConnection,
                manager.getInvitationManager());
            setGroupChatManager(manager);

            String group = address.getGroup();

            if ((group != null) && !group.equals("")) {
                String[] groupName = group.split(",");

                for (String name : groupName) {
                    groupChatManager.createGroups(name, address.getUsername(),
                        xmppConnection);
                }
            }

            if (Role.CONSUMER.equals(role)) {
                getProcessor();
                xmppConnection.addPacketListener(listener,
                    new PacketTypeFilter(Message.class));
            }
        } catch (XMPPException xmppe) {
            log.warning("Unable to connect to XMPP server: " + " domain=" +
                address.getDomain() + " port=" + address.getPort() +
                " username=" + address.getUsername() + " password=" +
                address.getPassword() + " resource=" + address.getResource());
            log.throwing(getClass().getName(), "activate", xmppe);
        }
    }

    @Override
    public void deactivate() throws Exception {
        if (Role.CONSUMER.equals(role)) {
            ((XMPPComponentLifeCycle) serviceUnit.getComponent().getLifeCycle()).removeHandler(this);
            xmppConnection.removePacketListener(listener);
        }

        MultiUserChat.removeInvitationListener(xmppConnection,
            groupChatManager.getInvitationManager());

        List<String> list = groupChatManager.getKeys();

        for (String key : list) {
            groupChatManager.removeMultiUserChats(key);
        }

        ((XMPPComponentLifeCycle) serviceUnit.getComponent().getLifeCycle()).getXmppConnectionManager()
         .destroyConnection(address.getDomain(), address.getUsername(),
            address.getResource(), this);
        super.deactivate();
    }

    public void setXMPPOperationInput(XMPPOperation xmppOperation,
        XMPPOperationInput input) {
        xmppOperationInputs.put(xmppOperation, input);
    }

    public void setXMPPOperationOutput(XMPPOperation xmppOperation,
        XMPPOperationOutput output) {
        xmppOperationOutputs.put(xmppOperation, output);
    }

    public XMPPBinding getXMPPBinding() {
        return binding;
    }

    public void setXMPPBinding(XMPPBinding binding) {
        this.binding = binding;
    }

    public void setXMPPAddress(XMPPAddress address) {
        this.address = address;
    }

    public XMPPAddress getXMPPAddress() {
        return address;
    }

    public void setXMPPOperations(Map<QName, XMPPOperation> operations) {
        this.xmppOperations = operations;
    }

    public Map<QName, XMPPOperation> getXMPPOperations() {
        return xmppOperations;
    }

    public XMPPOperationInput getXMPPOperationInput(XMPPOperation xmppOperation) {
        return xmppOperationInputs.get(xmppOperation);
    }

    public XMPPOperationOutput getXMPPOperationOutput(
        XMPPOperation xmppOperation) {
        return xmppOperationOutputs.get(xmppOperation);
    }

    private static String generateId(String id, String remoteAddress) {
        StringBuilder builder = new StringBuilder();
        builder.append(id);
        builder.append("-");
        builder.append(remoteAddress);

        return builder.toString();
    }

    public Map<XMPPOperation, XMPPOperationInput> getXMPPOperationInputs() {
        return xmppOperationInputs;
    }

    public void setXMPPOperationInputs(
        Map<XMPPOperation, XMPPOperationInput> xmppOperationInputs) {
        this.xmppOperationInputs = xmppOperationInputs;
    }

    public Map<XMPPOperation, XMPPOperationOutput> getXMPPOperationOutputs() {
        return xmppOperationOutputs;
    }

    public void setXMPPOperationOutputs(
        Map<XMPPOperation, XMPPOperationOutput> xmppOperationOutputs) {
        this.xmppOperationOutputs = xmppOperationOutputs;
    }

    public static IProviderHandler getWaitingProcessor(String id,
        String remoteAddress) {
        String key = generateId(id, remoteAddress);
        log.log(Level.INFO, "Attempting to remove handler for key " + key);

        return waitingHandlers.remove(key);
    }

    public static boolean registerWaitingHandler(String id,
        String remoteAddress, IProviderHandler handler) {
        String key = generateId(id, remoteAddress);
        log.log(Level.INFO, "Registering a handler for " + key);

        boolean tmp = waitingHandlers.putIfAbsent(key, handler) == null;

        if (!tmp) {
            log.log(Level.INFO,
                "We already have a handler registered for " + key);
        }

        return tmp;
    }

    public void setGroupChatManager(GroupChatManager groupChatManager) {
        this.groupChatManager = groupChatManager;
    }

    public GroupChatManager getGroupChatManager() {
        return groupChatManager;
    }

    public Role getRole(){
        return role;
    }
}
