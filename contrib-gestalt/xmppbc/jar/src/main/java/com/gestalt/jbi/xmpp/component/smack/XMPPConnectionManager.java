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

import com.gestalt.jbi.component.manager.Endpoint;

import org.jivesoftware.smack.ConnectionConfiguration;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;


public class XMPPConnectionManager {
    private static final Logger log = Logger.getLogger(XMPPConnectionManager.class.getName());
    private static final String AT = "@";
    private static final String FORWARD_SLASH = "/";
    private ConcurrentHashMap<String, XMPPConnectionWrapper> xmppConnections = new ConcurrentHashMap<String, XMPPConnectionWrapper>();
    private ReentrantLock mutex = new ReentrantLock();

    public XMPPConnection createConnection(String server, int port,
        String username, String password, String resource, boolean tlsEnabled,
        boolean saslEnabled, Endpoint endpoint) throws XMPPException {
        mutex.lock();

        try {
            String key = generateKey(server, username, resource);
            log.log(Level.INFO, "Creating xmpp connection for - " + key);
            log.info("tlsEnabled " + tlsEnabled +  " sasl Enabled " + saslEnabled );
            XMPPConnectionWrapper xmppProxyWrapper = new XMPPConnectionWrapper();
            XMPPConnectionWrapper tmp = xmppConnections.putIfAbsent(key,
                    xmppProxyWrapper);

            if (tmp == null) {
                try {
                    xmppProxyWrapper.setConnectionConfiguration(createConnectionConfiguration(
                            server, port, saslEnabled, tlsEnabled));
                    xmppProxyWrapper.setXmppConnection(createXMPPConnection(
                            username, password, resource,
                            xmppProxyWrapper.getConnectionConfiguration()));
                    xmppProxyWrapper.registerEndpoint(endpoint);
                    log.log(Level.INFO, "XMPP Connection created");

                    return xmppProxyWrapper.getXmppConnection();
                } catch (Throwable e) {
                    log.log(Level.SEVERE, "Exception Creating XMPP Connection! " + e.getMessage());
                    throw new XMPPException(
                        "Throwing Exception - Exception creating XMPP Connection", e);
                }
            }

            tmp.registerEndpoint(endpoint);
            log.log(Level.INFO, "Registered with existing XMPP Connection");

            return tmp.getXmppConnection();
        } finally {
            mutex.unlock();
        }
    }

    private String generateKey(String server, String username, String resource) {
        StringBuilder builder = new StringBuilder();
        builder.append(username);
        builder.append(AT);
        builder.append(server);
        builder.append(FORWARD_SLASH);
        builder.append(resource);

        return builder.toString();
    }

    private ConnectionConfiguration createConnectionConfiguration(String host,
        int port, boolean saslEnabled, boolean tlsEnabled) {
        ConnectionConfiguration configuration = null;
        String serviceName = null;
        if ( "talk.google.com".equals(host)) {
            //TODO: fix this hack.
            // temporary hack to configure the google talk to work for j1.
            // serviceName should be address extension attribute.
            serviceName = "gmail.com";
            configuration = new ConnectionConfiguration(host, port, "gmail.com");
            configuration.setSecurityMode(ConnectionConfiguration.SecurityMode.required);
            configuration.setSASLAuthenticationEnabled(true);
        } else {
            //TODO. create configuration using "host,port,serviceName"
            configuration = new ConnectionConfiguration(host, port);
            if (tlsEnabled) { // it should be required
                configuration.setSecurityMode(ConnectionConfiguration.SecurityMode.required);
            }
            configuration.setSASLAuthenticationEnabled(saslEnabled);
        }
        configuration.setDebuggerEnabled(false);
        //No Longer in 3.0.4 API
        //        configuration.setTLSEnabled(tlsEnabled);


        return configuration;
    }

    protected XMPPConnection createXMPPConnection(String username,
        String password, String resource,
        ConnectionConfiguration connectionConfiguration)
        throws XMPPException {
        XMPPConnection xmppConnection = new XMPPConnection(connectionConfiguration);
        xmppConnection.connect();
        xmppConnection.login(username, password, resource);

        return xmppConnection;
    }

    public void destroyConnection(String server, String username,
        String resource, Endpoint endpoint) {
        mutex.lock();

        try {
            String key = generateKey(server, username, resource);
            XMPPConnectionWrapper xmppProxyWrapper = xmppConnections.get(key);

            if (xmppProxyWrapper != null) {
                xmppProxyWrapper.unregisterEndpoint(endpoint);

                if (!xmppProxyWrapper.hasRegisteredEndpoints()) {
                    xmppConnections.remove(key);
                    log.log(Level.INFO,
                        "XMPP Connection no longer has endpoints and has been removed - " +
                        key);
                    xmppProxyWrapper.getXmppConnection().disconnect();
                }
            }
        } finally {
            mutex.unlock();
        }
    }

    private class XMPPConnectionWrapper {
        private XMPPConnection xmppConnection;
        private ConnectionConfiguration connectionConfiguration;
        private List<Endpoint> endpoints;

        public XMPPConnectionWrapper() {
            endpoints = new ArrayList<Endpoint>();
        }

        public ConnectionConfiguration getConnectionConfiguration() {
            return connectionConfiguration;
        }

        public void setConnectionConfiguration(
            ConnectionConfiguration connectionConfiguration) {
            this.connectionConfiguration = connectionConfiguration;
        }

        public XMPPConnection getXmppConnection() {
            return xmppConnection;
        }

        public void setXmppConnection(XMPPConnection xmppConnection) {
            this.xmppConnection = xmppConnection;
        }

        public void registerEndpoint(Endpoint endpoint) {
            endpoints.add(endpoint);
        }

        public void unregisterEndpoint(Endpoint endpoint) {
            endpoints.remove(endpoint);
        }

        public boolean hasRegisteredEndpoints() {
            return endpoints.size() > 0;
        }
    }
}
