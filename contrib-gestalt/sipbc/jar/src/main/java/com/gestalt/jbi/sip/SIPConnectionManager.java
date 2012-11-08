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

import com.gestalt.sip.utilities.message.RequestGenerator;
import com.gestalt.sip.utilities.message.ResponseGenerator;
import com.gestalt.sip.utilities.message.SipUtil;
import com.sun.jbi.internationalization.Messages;

import java.io.IOException;

import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.Socket;

import java.util.Map;
import java.util.Properties;
import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.sip.ListeningPoint;
import javax.sip.SipFactory;
import javax.sip.SipProvider;
import javax.sip.SipStack;
import javax.sip.address.AddressFactory;
import javax.sip.header.HeaderFactory;
import javax.sip.message.MessageFactory;


/**
 * Singleton
 *
 * @author : csturtz
 */
public final class SIPConnectionManager {
    private static final Logger log = Messages.getLogger(SIPConnectionManager.class);
    private static Messages messages = Messages.getMessages(SIPConnectionManager.class);
    private static SIPConnectionManager instance = null;
    private static final String HTTP_BUSY_MSG = "Sorry, This User is Busy";
    public static final String MAGIC_COOKIE = "z9hG4bK";
    public static final int MAX_PORT = 10000;
    public static final int MAX_SOCKET_TRIES = 1000;
    private int stackCounter = 0;
    private String myHost = "";
    private String transport = "udp";
    private Map<String, SIPConnection> connections = new ConcurrentHashMap<String, SIPConnection>();

    /**
     * Private Constructor
     */
    private SIPConnectionManager() {
    }

    /**
     * Manages the Singleton instance
     *
     * @return
     */
    public static SIPConnectionManager getInstance() {
        if (instance != null) {
            return instance;
        }

        instance = new SIPConnectionManager();

        return instance;
    }

    /**
     * Create a Properties Object with a unique property value for
     * javax.sip.STACK_NAME. This will be used to get a unique instance of
     * SipStack.
     *
     * @return
     */
    private Properties createProperties(String outboundProxy) {
        stackCounter++;

        Properties p = new Properties();
        p.setProperty("javax.sip.STACK_NAME", "sipbc" + stackCounter);
        p.setProperty("javax.sip.OUTBOUND_PROXY", outboundProxy);
//        p.setProperty("gov.nist.javax.sip.TRACE_LEVEL", "32");
//        p.setProperty("gov.nist.javax.sip.SERVER_LOG","jain_sip_server.log");
//        p.setProperty("gov.nist.javax.sip.LOG_MESSAGE_CONTENT","true");
//        p.setProperty("gov.nist.javax.sip.DEBUG_LOG",
//				"jain_sip_debug.log");

        return p;
    }

    /**
     * Create a SIPConnection instance. There is a one-to-one ratio of
     * SIPConnection to SIPUser.
     *
     * To create a SIPConnection, other objects must be created as well.
     * SipStack MessageFactory HeaderFactory AddressFactory SipProvider
     * SipServer
     *
     * After all required objects have been created, we create the SIPConnection
     * and attempt to register the user with the proxy.
     *
     * We return the SIPConnection whether registration was successful or not.
     * Eventhough, at this point there is no way to try to register again.
     *
     * @param user
     * @return
     */
    public SIPConnection createConnection(SIPUser user) {
        String key = generateKey(user);
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"The following Connection Key was generated: " + key);
        }

        if (connections.containsKey(key)) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00260.theSIPUserIsAlreadyRegisteredWithThisUsernameReturningTheExistingConnection"));
            user.incrementActiveEndpoints();
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"Added an active endpoint.  Currently " +
                    user.getActiveEndpoints() + " endpoint(s) are activated");
            }
            return connections.get(key);
        }

        try {
            myHost = determineLocalIP(user.getProxyhost(), user.getProxyport());
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"My host is: " + myHost);
            }

        } catch (Exception e) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00261.unableToObtainLocalHostAddressDefaultingToLocalhost"),e);
            myHost = "localhost";
        }

        SIPConnection sipConnection = null;

        try {
            SipFactory sipFactory = SipFactory.getInstance();
            SipStack sipStack = sipFactory.createSipStack(createProperties(user.getProxyhost() +
                        ":" + user.getProxyport() + "/" + transport));

            MessageFactory messageFactory = sipFactory.createMessageFactory();
            HeaderFactory headerFactory = sipFactory.createHeaderFactory();
            AddressFactory addressFactory = sipFactory.createAddressFactory();

            ListeningPoint listeningPoint = createListeningPoint(sipStack);
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"Created a Listening Point on port: " +
                    listeningPoint.getPort());
            }

            SipProvider sipProvider = sipStack.createSipProvider(listeningPoint);
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"Created new SipProvider: " + sipProvider);
            }

            RequestGenerator requestGenerator = new RequestGenerator(messageFactory,
                    headerFactory, addressFactory, sipProvider,
                    user.getUsername(),
                    SipUtil.buildUserAtHost(user.getUsername(),
                        user.getProxyhost()), this.myHost, this.transport,
                    user.getProxyhost(),
                    sipProvider.getListeningPoint(transport).getPort(),
                    user.getProxyport());
            ResponseGenerator responseGenerator = new ResponseGenerator(messageFactory,
                    headerFactory, addressFactory, user.getUsername(),
                    HTTP_BUSY_MSG, myHost,
                    sipProvider.getListeningPoint(transport).getPort());

            SIPServer sipServer = new SIPServer(sipProvider, requestGenerator,
                    responseGenerator);
            sipConnection = new SIPConnection(user, sipStack, requestGenerator,
                    responseGenerator, sipServer);

            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE, "Adding SipListener or SipProvider");
            }
            sipProvider.addSipListener(sipServer);
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"A SipListener was added");
            }
        } catch (Exception e) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00262.unableToRegisterTheUserAnExceptionOccuredWhileCreatingTheListeningPoint/SipProvider/SipServer"), e);

            return null;
        }

        if (sipConnection != null) {
            connections.put(key, sipConnection);
            sipConnection.register();
            user.incrementActiveEndpoints();
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"Added an active endpoint.  Currently " +
                    user.getActiveEndpoints() + " endpoint(s) are activated");
            }
        }

        return sipConnection;
    }

    /**
     * Removes a SIPConnection instance (if allowed) and cleans up.
     *
     * This method is called when an endpoint is deactivated. If there are
     * multiple endpoints activated for the same SIPConnection, we don't want to
     * actually remove the connection. So, we keep track of how many active
     * endpoints there are for the provided user. When that number is 1 and this
     * method is called, we will actually remove the connection.
     *
     * First, we attempt to unregister the user from the proxy. Independent of
     * whether or not that is successful, the SipStack associated with this
     * connection and SIPUser will be removed (SipStack.stop()).
     *
     * @param user
     * @param sipStack
     * @return
     */
    public boolean removeConnection(SIPUser user, SipStack sipStack) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Currently " + user.getActiveEndpoints() +
                " endpoint(s) are activated");
        }

        boolean success = false;

        user.decrementActiveEndpoints();

        if (user.getActiveEndpoints() > 0) {
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"Not removing the SIP Connection." +
                    " Other endpoints have been activated that also use this connection");
            }

            return true;
        }

        // remove user from SIPUsers if it's the last active endpoint
        SIPUser.removeSIPUser(user);

        SIPConnection sipConnection = connections.remove(generateKey(user));

        if (sipConnection != null) {
            sipConnection.unregister();
            sipConnection.close();

            sipStack.stop();
                if (log.isLoggable(Level.FINE)){
                    log.log(Level.FINE,"The SipStack was stopped");
                }
            success = true;

            if (sipConnection.isRegistered()) {
                if (log.isLoggable(Level.FINE)){
                    log.log(Level.FINE,"Unable to unregister with the proxy");
                }
                success = false;
            }
        }

        return success;
    }

    /**
     * Create a ListeningPoint for the provided SipStack. In Jain SIP 1.2, only
     * one ListeningPoint exists per SipStack.
     *
     * We start with a random integer and try the next 1000 ports.
     *
     * @param sipStack
     * @return
     */
    public ListeningPoint createListeningPoint(SipStack sipStack) {
        ListeningPoint listeningPoint = null;
        Random random = new Random();
        int port = random.nextInt(MAX_PORT);

        // create ListeningPort on an unused UDP port
        // if assigned port is in use, then locate another
        // that is not being used
        int i = 0;

        for (i = 0; i < MAX_SOCKET_TRIES; i++) {
            try {
                if (log.isLoggable(Level.FINEST)){
                    log.log(Level.FINEST, "Trying on port: " + port);
                }
                    listeningPoint = sipStack.createListeningPoint(myHost, port,
                        transport);
                break;
            } catch (Exception e) {
                if (log.isLoggable(Level.FINEST)){
                    log.log(Level.FINEST,"An exception occured while creating a listening point on port "
                            + port + ". Trying the next port",e);
                }
                port = random.nextInt(MAX_PORT);
            }
        }

        if (i == MAX_SOCKET_TRIES) {
            log.log(Level.SEVERE, messages.getString("SIPBC-E00263.unableToFindAnAvailablePort"));
        }

        return listeningPoint;
    }

    /**
     * Here we discover the local host address. We confirm this by opening a
     * socket connection to the proxy.
     *
     * @param serverHost
     * @param serverPort
     * @return
     */
    protected String determineLocalIP(String serverHost, Integer serverPort) {
        String localIP = null;
        Socket socket = null;

        try {
            socket = new Socket(serverHost, serverPort);
            localIP = socket.getLocalAddress().getHostAddress();
        } catch (Exception e) {
            DatagramSocket datagramSocket = null;

            try {
                datagramSocket = new DatagramSocket();
                datagramSocket.connect(new InetSocketAddress(serverHost,
                        serverPort));
                localIP = datagramSocket.getLocalAddress().getHostAddress();
            } catch (Exception e1) {
                throw new IllegalStateException("Unable to contact to the following server: [" +
                    serverHost + ":" + serverPort + "]");
            } finally {
                if (datagramSocket != null) {
                    try {
                        datagramSocket.disconnect();
                        datagramSocket.close();
                    } catch (Exception e1) {
                        if (log.isLoggable(Level.FINEST)){
                            log.log(Level.FINEST,"An exception occured while closing the socket",e);
                        }
                    }
                }
            }
        } finally {
            if (socket != null) {
                try {
                    socket.close();
                } catch (IOException e) {
                    if (log.isLoggable(Level.FINEST)){
                        log.log(Level.FINEST,"An exception occured while closing the socket",e);
                    }
                }
            }
        }

        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Local IP Determined as: " + localIP);
        }
        return localIP;
    }

    /**
     * Generates a unique key (String) to identify a SIPConnection. Since a
     * SIPConnection is one-to-one with SIPUser, we use a public static method
     * of the SIPUser class to generate the key.
     *
     * @param user
     * @return
     */
    private String generateKey(SIPUser user) {
        String key = SIPUser.generateUniqueID(user.getUsername(),
                user.getProxyhost());

        return key;
    }
}
