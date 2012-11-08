/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)SocksSocketFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/******************************************************************************
 * Copyright © 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * California 95054, U.S.A. All rights reserved. Sun Microsystems, Inc. has
 * intellectual property rights relating to technology embodied in the product
 * that is described in this document. In particular, and without limitation,
 * these intellectual property rights may include one or more of the U.S. patents
 * listed at http://www.sun.com/patents and one or more additional patents or
 * pending patent applications in the U.S. and in other countries. THIS PRODUCT
 * CONTAINS CONFIDENTIAL INFORMATION AND TRADE SECRETS OF SUN MICROSYSTEMS, INC.
 * USE, DISCLOSURE OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN
 * PERMISSION OF SUN MICROSYSTEMS, INC.U.S. Government Rights - Commercial
 * software.  Government users are subject to the Sun Microsystems, Inc. standard
 * license agreement and applicable provisions of the FAR and its supplements.
 * Use is subject to license terms.  This distribution may include materials
 * developed by third parties. Sun, Sun Microsystems, the Sun logo, Java
 * Composite Application Platform Suite,  SeeBeyond, eGate, eInsight, eVision, eTL,
 * eXchange, eView, eIndex, eBAM and  eWay are trademarks or registered trademarks of
 * Sun Microsystems, Inc. in the U.S. and other countries. All SPARC trademarks are
 * used under license and are trademarks or registered trademarks of SPARC
 * International, Inc. in the U.S. and other countries. Products bearing SPARC
 * trademarks are based upon architecture developed by Sun Microsystems, Inc.
 * UNIX is a registered trademark in the U.S. and other countries, exclusively
 * licensed through X/Open Company, Ltd. This product is covered and controlled by
 * U.S. Export Control laws and may be subject to the export or import laws in
 * other countries.  Nuclear, missile, chemical biological weapons or nuclear
 * maritime end uses or end users, whether direct or indirect, are strictly
 * prohibited.  Export or reexport to countries subject to U.S. embargo or to
 * entities identified on U.S. export exclusion lists, including, but not limited
 * to, the denied persons and specially designated nationals lists is strictly
 * prohibited.
 **/
package com.sun.jbi.ftpbc.ftp;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

import org.apache.commons.net.SocketFactory;

/**
 * This class implements the org.apache.commons.net.SocketFactory interface by
 * applying the SOCKS protocol to the socket creators. SOCKS versions 4, 4A, and 5
 * are supported.
 *
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */

// The java.net package does support basic SOCKS V4 after
// setting two system properties about SOCKS by using some
// code snippets. This feature is implemented in
// java.net.PlainSocketImpl, it is the default socket
// implementation that java.net package provided. This is a
// built-in feature of java.net package, it depends on system
// properties "socksProxyHost" and "socksProxyPort". Client
// codes will be like below.
//
// Specify SOCKS proxy host in system properties:
// System.getProperties().put("socksProxyHost", "your.socks.host");
//
// Specify SOCKS proxy port number in system properties:
// System.getProperties().put("socksProxyPort", "1080");
//
// Actually, most http clients use the similar approach to provide
// proxy support.
//
// There are some limitations after enabling java.net package's SOCKS
// support.
// 1. All stream sockets will always go through the same SOCKS server
//    inside a same Java Virtual Machine regardless what kind of socket
//    connections (intranet, internet, http or ftp) are used, this
//    may not be intended and may affect performance remarkably.
//    For example, we may want internal socket stream (inside internal
//    network) to be sent directly and not through the SOCKS server.
//    Another example, in one collaboratin, we may need several different
//    socket-related connections, such as ftp connection and http connection,
//    they are in the same JVM, but we may intend to route ftp socket stream
//    to SOCKS server A and route http socket stream to SOCKS server B.
// 2. Applications will always throw java.net.UnknownHostException
//    if the client can not find the destination host's IP address
//    but the SOCKS server does can resolve the destination host's
//    domain name. SOCKS 4A is used to deal with this problem.
// 3. Applications which are using java.net.DatagramSocket will
//    always fail in going through SOCKS server because datagram packet
//    can not be relayed by SOCKS4 protocol (of course, for BatchFtp,
//    we don't care about datagram packet).
//
// Alternatively, Sun Microsystems provides a mechanism to allow us to
// implement our own java.net.SocketImplFactory, such as
// SocksSocketImpleFactory (it implements interface java.net.SocketImplFactory).
// To implement SocketImplFactory, we have to provide our own java.net.SocketImpl.
// Thus, before "new Socket()", perform some codes like
//
//       SocketImplFactory factory = new SocksSocketImpleFactory();
//       Socket.setSocketImplFactory(factory);
//
// From now on, all "new Socket()" will invoke the method createSocketImpl()
// of class SocksSocketImpleFactory to return our own java.net.SocketImpl
// first, then this actual SocketImpl will be used in all consequent function
// calls.
// Note: java.net provides this extension mechanism, besides our
//       own java.net.SocketImplFactory implementation, we have to
//       provide our own java.net.SocketImpl (it is used for
//       SocketImplFactory implementation). This mechanism remains
//       the first limitation stated before: once the SocketImplFactory
//       is set, it exists and takes effect in the whole Java Virtual
//       Machine, and it is not allowed to be changed. If we implement
//       our SOCKS support in it, all socket streams in the same JVM will
//       always go through the same SOCKS server although some internal
//       connections don't need it actually or they need a different one.
//
// We try to use another mechanism to implement SOCKS support. That is,
// we can provide a separate set of SOCKS socket classes:
// 1. SocksSocket is similar to class Socket.
// 2. SocksServerSocket is similar to class ServerSocket.
// 3. SocksDatagramSocket is similar to class DatagramSocket.
//
// Then, through this factory class, we can provide SOCKS sockets independently.
// And most importantly, according to this architecture, we can expose the special
// SOCKS socket class only, all protocol-related stuffs are hidden from users, so
// it is possible to plug SOCKS feature to an existing system by minor changes.

class SocksSocketFactory implements SocketFactory {
    private static final Messages mMessages =
            Messages.getMessages(SocksSocketFactory.class);
    private static final Logger mLogger =
            Messages.getLogger(SocksSocketFactory.class);
    
    private Socks socks = null;
    private SocksChain socksList = null;
    
    
    /**
     * Creates a socket connected to the specified host and port.
     * <p>
     * @param host The host name to connect to.
     * @param port The port to connect to.
     * @return A socket connected to the given host and port.
     * @exception UnknownHostException  If the host name cannot be resolved.
     * @exception IOException If an input-output error occurs while creating the socket.
     */
    public Socket createSocket(String host, int port)
    throws UnknownHostException, IOException {
        try {
            if (this.socksList != null) {
                return new SocksSocket(host, port, this.socksList);
            } else {
                return new SocksSocket(this.socks, host, port);
            }
        } catch (SocksException e) {
            throw new IOException(e.toString());
        }
    }
    
    /**
     * Creates a Socket connected to the specified host and port.
     * <p>
     * @param address The address of the host to connect to.
     * @param port The port to connect to.
     * @return A socket connected to the given host and port.
     * @exception IOException If an input-output error occurs while creating the socket.
     */
    public Socket createSocket(InetAddress address, int port)
    throws IOException {
        try {
            if (this.socksList != null) {
                return new SocksSocket(address, port, this.socksList);
            } else {
                return new SocksSocket(this.socks, address, port);
            }
        } catch (SocksException e) {
            throw new IOException(e.toString());
        }
    }
    
    /**
     * Creates a socket connected to the given host and port. The created socket
     * originates from the specified local address and port.
     * <p>
     * @param host The host name to connect to.
     * @param port The port to connect to.
     * @param localAddr  The local address to use.
     * @param localPort  The local port to use.
     * @return A socket connected to the given host and port.
     * @exception UnknownHostException  If the host name cannot be resolved.
     * @exception IOException If an input-output error occurs while creating the socket.
     */
    public Socket createSocket(
            String host,
            int port,
            InetAddress localAddr,
            int localPort)
            throws UnknownHostException, IOException {
        try {
            if (this.socksList != null) {
                return new SocksSocket(host, port, localAddr, localPort, this.socksList);
            } else {
                return new SocksSocket(this.socks, host, port, localAddr, localPort);
            }
        } catch (SocksException e) {
            throw new IOException(e.toString());
        }
    }
    
    /**
     * Creates a socket connected to the given host and port. The created socket
     * originates from the specified local address and port.
     * <p>
     * @param address The address of the host to connect to.
     * @param port The port to connect to.
     * @param localAddr  The local address to use.
     * @param localPort  The local port to use.
     * @return A socket connected to the given host and port.
     * @exception IOException If an input-output error occurs while creating the socket.
     */
    public Socket createSocket(
            InetAddress address,
            int port,
            InetAddress localAddr,
            int localPort)
            throws IOException {
        try {
            if (this.socksList != null) {
                return new SocksSocket(address, port, localAddr, localPort, this.socksList);
            } else {
                return new SocksSocket(this.socks, address, port, localAddr, localPort);
            }
        } catch (SocksException e) {
            throw new IOException(e.toString());
        }
    }
    
    /**
     * Creates a ServerSocket bound to a specified port. A port
     * of 0 (zero) creates the ServerSocket on a system-determined free port.
     * <p>
     * @param port  The port on which to listen, or 0 to use any free port.
     * @return A ServerSocket that listens on a specified port.
     * @exception IOException If an input-output error occurs while creating
     *                        the ServerSocket.
     */
    public ServerSocket createServerSocket(int port)
    throws IOException {
        try {
            return new SocksServerSocket(this.socks, port);
        } catch (SocksException e) {
            throw new IOException(e.toString());
        }
        
    }
    
    /**
     * Creates a ServerSocket bound to a specified port with a given
     * maximum queue length for incoming connections. A port of 0 (zero)
     * creates the ServerSocket on a system-determined free port.
     * <p>
     * @param port  The port on which to listen, or 0 to use any free port.
     * @param backlog  The maximum length of the queue for incoming connections.
     * @return A ServerSocket that listens on a specified port.
     * @exception IOException If an input-output error occurs while creating
     *                        the ServerSocket.
     */
    public ServerSocket createServerSocket(int port, int backlog)
    throws IOException {
        try {
            return new SocksServerSocket(this.socks, port, backlog);
        } catch (SocksException e) {
            throw new IOException(e.toString());
        }
    }
    
    /**
     * Creates a ServerSocket bound to a specified port at a specified local
     * address with a given maximum queue length for incoming connections.
     * A port of 0 (zero)
     * creates the ServerSocket on a system-determined free port.
     * <p>
     * @param port  The port on which to listen, or 0 to use any free port.
     * @param backlog  The maximum length of the queue for incoming connections.
     * @param bindAddr  The local address to which the ServerSocket binds.
     * @return A ServerSocket that listens on a specified port.
     * @exception IOException If an input-output error occurs while creating
     *                        the ServerSocket.
     */
    public ServerSocket createServerSocket(
            int port,
            int backlog,
            InetAddress bindAddr)
            throws IOException {
        try {
            return new SocksServerSocket(this.socks, port, backlog, bindAddr);
        } catch (SocksException e) {
            throw new IOException(e.toString());
        }
    }
    
    /**
     * Constructor.
     *
     * @param socksList A SOCKS chain.
     */
    public SocksSocketFactory(SocksChain socksList) {
        String msg = null;
        this.socksList = socksList;
        if (this.socksList == null) {
            if (mLogger.isLoggable(Level.WARNING)) 
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006019.WRN_EXT_FTP_NO_SOCKS_SERVER_SPECIFIED", new Object[] {"SocksSocketFactory.SocksSocketFactory(SocksChain socksList)"}));
        }
    }
    
    /**
     * Constructor.
     *
     * @param socks A SOCKS server.
     */
    public SocksSocketFactory(Socks socks) {
        String msg = null;
        this.socks = socks;
        if (this.socks == null) {
            if (mLogger.isLoggable(Level.WARNING))
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006019.WRN_EXT_FTP_NO_SOCKS_SERVER_SPECIFIED", new Object[] {"SocksSocketFactory.SocksSocketFactory(Socks socks)"}));
        }
    }
    
    /**
     * Constructor.
     *
     */
    public SocksSocketFactory() {
    }
}
