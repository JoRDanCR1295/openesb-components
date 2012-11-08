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
 * @(#)SocksServerSocket.java 
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
import java.net.SocketException;
import java.net.SocketImplFactory;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

/**
 * This class allows you to accept connections from one particular
 * host through the SOCKS4 or SOCKS5 proxy server.
 *
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */
class SocksServerSocket extends ServerSocket {
    private static final Logger mLogger =
            Messages.getLogger(SocksServerSocket.class);
    
    private Socks socks = null;
    private int port = 0;
    private int backlog = 50;
    private InetAddress bindAddr = null;
    
    private ServerSocket socksServerSocket = null;
    
    /**
     * Creates a server with the specified port, listen backlog, and
     * local IP address to bind to. The <i>bindAddr</i> argument
     * can be used on a multi-home host for a ServerSocket that
     * only accepts connection requests to one of its addresses.
     * If <i>bindAddr</i> is a null, the host defaults to accepting
     * connections on any/all local addresses.
     * The port number must be between 0 and 65535, inclusive.
     * <P>
     * @param socks  Represent a SOCKS server.
     * @param port The local TCP port.
     * @param backlog The listen backlog.
     * @param bindAddr The local InetAddress the server binds to
     *
     * @throws SocksException If something is wrong with SOCKS.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksServerSocket(
            Socks socks,
            int port,
            int backlog,
            InetAddress bindAddr)
            throws SocksException, IOException {
        super(0);
        this.socksServerSocket = new ServerSocket(port, backlog, bindAddr);
        
        this.socks = socks;
        this.port = port;
        this.backlog = backlog;
        this.bindAddr = bindAddr;
    }
    
    /**
     * Creates a server socket on a specified port. A port of
     * <code>0</code> creates a socket on any free port.
     * <p>
     * The maximum queue length for incoming connection indications
     * (requests to connect) is set to <code>50</code>. If a connection
     * indication arrives when the queue is full, the connection is refused.
     * @param      port  The port number, or <code>0</code> meaning to use any
     *                   free port.
     * @throws SocksException If something is wrong with SOCKS.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksServerSocket(int port)
    throws SocksException, IOException {
        this(null, port, 50, null);
    }
    
    /**
     * Creates a server socket and binds it to the specified local port
     * number. A port number of <code>0</code> creates a socket on any
     * free port.
     * <p>
     * The maximum queue length for incoming connection indications
     * (requests to connect) is set to the <code>backlog</code> parameter. If
     * a connection indication arrives when the queue is full, the
     * connection is refused.
     * <p>
     * @param      port     The specified port, or <code>0</code> meaning to use
     *                      any free port.
     * @param      backlog  The maximum length of the queue.
     *
     * @throws SocksException If something is wrong with SOCKS.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksServerSocket(int port, int backlog)
    throws SocksException, IOException {
        this(null, port, backlog, null);
    }
    
    /**
     * Creates a server with the specified port, listen backlog, and
     * local IP address to bind to. The <i>bindAddr</i> argument
     * can be used on a multi-home host for a ServerSocket that
     * only accepts connection requests to one of its addresses.
     * If <i>bindAddr</i> is a null, the host defaults to accepting
     * connections on any/all local addresses.
     * The port number must be between 0 and 65535, inclusive.
     * <P>
     * @param port The local TCP port
     * @param backlog The listen backlog
     * @param bindAddr The local InetAddress the server binds to
     *
     * @throws SocksException If something is wrong with SOCKS.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksServerSocket(int port, int backlog, InetAddress bindAddr)
    throws SocksException, IOException {
        this(null, port, backlog, bindAddr);
    }
    
    /**
     * Creates a server socket on a specified port. A port of
     * <code>0</code> creates a socket on any free port.
     * <p>
     * The maximum queue length for incoming connection indications
     * (requests to connect) is set to <code>50</code>. If a connection
     * indication arrives when the queue is full, the connection is refused.
     * @param     socks  Represent a SOCKS server.
     * @param      port  The port number, or <code>0</code> meaning to use any
     *                   free port.
     * @throws SocksException If something is wrong with SOCKS.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksServerSocket(Socks socks, int port)
    throws SocksException, IOException {
        this(socks, port, 50, null);
    }
    
    /**
     * Creates a server socket and binds it to the specified local port
     * number. A port number of <code>0</code> creates a socket on any
     * free port.
     * <p>
     * The maximum queue length for incoming connection indications
     * (requests to connect) is set to the <code>backlog</code> parameter. If
     * a connection indication arrives when the queue is full, the
     * connection is refused.
     * <p>
     * @param     socks     Represent a SOCKS server.
     * @param      port     The specified port, or <code>0</code> meaning to use
     *                      any free port.
     * @param      backlog  The maximum length of the queue.
     *
     * @throws SocksException If something is wrong with SOCKS.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksServerSocket(Socks socks, int port, int backlog)
    throws SocksException, IOException {
        this(socks, port, backlog, null);
    }
    
    /**
     * Sets the server socket implementation factory for the
     * application.
     *
     * @param      fac   The desired factory.
     * @exception  IOException  If an input-output error occurs when setting the
     *             socket factory.
     */
    public static void setSocketFactory(SocketImplFactory factory)
    throws IOException {
        // this.socksServerSocket.setSocketFactory(factory);
        ServerSocket.setSocketFactory(factory);
    }
    
    /**
     * Accepts the incoming connection.
     * This method listens for a connection to be made to this socket and accepts
     * it. Additionally, the method blocks until a connection is made.
     * @return An instance of the socket.
     * @exception  IOException  If an input-output error occurs.
     */
    public Socket accept() throws IOException {
        // return this.socksServerSocket.accept();
        
        if (this.socks == null) {
            return this.socksServerSocket.accept();
        }
        
        Socket socket = null;
        try {
            socket = new SocksSocket(this.socks, this.bindAddr, this.port);
        } catch (SocksException e) {
            throw new IOException(e.toString());
        }
        
        return socket;
    }
    
    /**
     * Closes the connection to the proxy server if the socket has not been accepted.
     * If the direct connection is used, this method closes the direct ServerSocket. If the
     * client socket has already been accepted, the method does nothing.
     * @exception  IOException  If an input-output error occurs.
     */
    public void close() throws IOException {
        this.socksServerSocket.close();
    }
    
    
    
    /**
     * Gets the address, assigned by proxy, to listen for incoming
     * connections, or the local machine address, if the local system is doing a direct
     * connection.
     * @return  The address to which the current socket is connected.
     *          or <code>null</code> If the socket is not yet connected.
     */
    public InetAddress getInetAddress() {
        return this.socksServerSocket.getInetAddress();
    }
    
    /**
     * Gets the port assigned by proxy to listen for incoming connections, or
     * the port chosen by the local system, if the system is accepting data directly.
     * @return  The port number to which the current socket is listening.
     */
    public int getLocalPort() {
        return this.socksServerSocket.getLocalPort();
    }
    
    /**
     * Retrieves the setting for SO_TIMEOUT. A return of zero means that the
     * option is disabled (that is, a timeout of infinity).
     *
     * @return The value of SO_TIMEOUT.
     * @exception  IOException  If an input-output error occurs.
     */
    public int getSoTimeout() throws IOException {
        return this.socksServerSocket.getSoTimeout();
    }
    
    /**
     * Allows you to enable or disable SO_TIMEOUT with the specified timeout, in
     * milliseconds. With this option set to a non-zero timeout,
     * a call to accept() for the current ServerSocket
     * blocks for only the specified amount of time. If the timeout expires,
     * a <B>java.io.InterruptedIOException</B> is raised, but the
     * ServerSocket is still valid. The option <B>must</B> be enabled
     * before entering the blocking operation or the method does not work.  The
     * timeout must be greater than 0.
     * A timeout of zero is interpreted as an infinite timeout.
     *
     * @param timeout  The SO_TIMEOUT value.
     * @exception  SocketException  If a socket error occurs.
     */
    public void setSoTimeout(int timeout) throws SocketException {
        this.socksServerSocket.setSoTimeout(timeout);
    }
    
    /**
     * Returns the implementation address and implementation port of
     * the current socket as a <code>string</code>.
     *
     * @return  A string representation of the current socket.
     */
    public String toString() {
        return this.socksServerSocket.toString();
    }
    
}
