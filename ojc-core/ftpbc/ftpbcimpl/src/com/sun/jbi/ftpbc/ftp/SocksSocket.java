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
 * @(#)SocksSocket.java 
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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketImplFactory;
import java.net.UnknownHostException;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;


/**
 * This class represents a socket through the SOCKS server.
 *
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */

public class SocksSocket extends Socket {
    private static final Messages mMessages =
            Messages.getMessages(SocksSocket.class);
    private static final Logger mLogger =
            Messages.getLogger(SocksSocket.class);
    
    private Socks socks = null;
    private SocksChain socksList = null;
    private String remoteHost = null;
    private InetAddress remoteIP = null;
    private int remotePort = 0;
    private String localHost = null;
    private InetAddress localIP = null;
    private int localPort = 0;
    
    private Socket socksSocket = null;
    private int methodAllowed = 0;
    private String defaultEncoding = "ISO-8859-1";
    private Socks workingSocks = null;
    private byte[] workingHostBytes = null;
    private byte[] workingIPBytes = {0, 0, 0, 1};
    private byte[] workingPortBytes = new byte[2];
    
    // Are we trying version for SOCKS server with "Unknown" version?
    // It is used to differentiate trace log level. If we are trying
    // to determine the unknown version by trying some command, we
    // don't like to treat the error code as a real error.
    //private boolean tryingVersion = false;
    
    // We have two options for some methods, that depends
    // on how to define and document the behavior of these
    // methods. overwriteBehavior controls methods' behaviors.
    private boolean overwriteBehavior = true;
    
    /**
     * Constructor.
     * Creates a socket and connects it to the specified remote host
     * on the specified remote port. The socket also binds to
     * the local address and port supplied.
     * @param socks Represent a SOCKS server.
     * @param remoteHost The remote host to connect to.
     * @param remotePort The port number to connect to on the remote host.
     * @param localIP The IP address of the local host that the socket is bound to.
     * @param localPort The local port (on the local host) that the socket is bound to.
     * @throws SocksException If something is wrong with SOCKS.
     * @throws UnknownHostException If the IP address of the host cannot be determined.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksSocket(
            Socks socks,
            String remoteHost,
            int remotePort,
            InetAddress localIP,
            int localPort)
            throws SocksException, UnknownHostException, IOException {
        
        String msg = null;
        this.socks = socks;
        this.remoteHost = Socks.resolveHostName(remoteHost);
        this.remoteIP = InetAddress.getByName(this.remoteHost);
        this.remotePort = remotePort;
        this.localIP = localIP;
        this.localPort = localPort;
        if (this.localIP == null) {
            this.localHost = null;
        } else {
            this.localHost = this.localIP.getHostAddress();
        }
        
        if (this.socks == null) {
            
            msg = mMessages.getString("FTPBC-W006017.WRN_EXT_FTP_NO_SOCKS_SERVER_SPECIFIED_REG_SOCKET_CREATED",
                    new Object[] {"SocksSocket(Socks socks,String remoteHost,int remotePort,InetAddress localIP,int localPort)"});
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, msg);
            }
            this.socksSocket = new Socket(this.remoteHost, this.remotePort, this.localIP, this.localPort);
            return;
        }
        
        try {
            this.workingSocks = this.socks;
            
            // Create this.socksSocket
            this.createSocksSocket();
            
            // Talk to remote server
            this.connectRemote();
        } catch (Exception e) {
            if (this.socksSocket != null) {
                this.socksSocket.close();
            }
            msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION",
                    new Object[] {"SocksSocket(Socks socks,String remoteHost,int remotePort,InetAddress localIP,int localPort)", e});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, e);
            }
            throw new SocksException(msg, e);
        }
    }
    
    /**
     * Constructor.
     * Creates a socket and connects it to the specified remote address
     * on the specified remote port. The socket also binds to
     * the local address and port supplied.
     * @param socks Represent a SOCKS server.
     * @param remoteIP The IP address of the remote host to connect to.
     * @param remotePort The port number to connect to on the remote host.
     * @param localIP The IP address of the local host that the socket is bound to.
     * @param localPort The local port on the local host that the socket is bound to.
     * @throws SocksException If something is wrong with SOCKS.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksSocket(
            Socks socks,
            InetAddress remoteIP,
            int remotePort,
            InetAddress localIP,
            int localPort)
            throws SocksException, IOException {
        this(socks, remoteIP.getHostName(), remotePort, localIP, localPort);
    }
    
    /**
     * Constructor.
     * Creates a data-streaming socket and connects it to the specified port number
     * at the specified IP address.
     * @param socks Represent a SOCKS server.
     * @param remoteIP The IP address of the remote host to connect to.
     * @param remotePort The port number to connect to on the remote host.
     * @throws SocksException If something is wrong with SOCKS.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksSocket(Socks socks, InetAddress remoteIP, int remotePort)
    throws SocksException, IOException {
        this(socks, remoteIP.getHostName(), remotePort, null, 0);
    }
    
    /**
     * Constructor.
     * Creates a data-streaming socket and connects it to the specified port number on the named host.
     * @param socks Represent a SOCKS server.
     * @param remoteHost The remote host to connect to.
     * @param remotePort The port number to connect to on the remote host.
     * @throws SocksException If something is wrong with SOCKS.
     * @throws UnknownHostException If the IP address of the host cannot be determined.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksSocket(Socks socks, String remoteHost, int remotePort)
    throws SocksException, UnknownHostException, IOException {
        this(socks, remoteHost, remotePort, null, 0);
    }
    
    /**
     * Constructor.
     * Creates a data-streaming socket and connects it to the specified port number on the named host.
     * @param remoteHost The remote host to connect to.
     * @param remotePort The port number to connect to on the remote host.
     * @throws SocksException If something is wrong with SOCKS.
     * @throws UnknownHostException If the IP address of the host cannot be determined.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksSocket(String remoteHost, int remotePort)
    throws SocksException, UnknownHostException, IOException {
        this(null, remoteHost, remotePort, null, 0);
    }
    
    /**
     * Constructor.
     * Creates a socket and connects it to the specified remote host
     * on the specified remote port. The socket also binds to
     * the local address and port supplied.
     * @param remoteHost The remote host to connect to.
     * @param remotePort The port number to connect to on the remote host.
     * @param localIP The IP address of the local host that the socket is bound to.
     * @param localPort The local port on the local host that the socket is bound to.
     * @throws SocksException If something is wrong with SOCKS.
     * @throws UnknownHostException If the IP address of the host cannot be determined.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksSocket(
            String remoteHost,
            int remotePort,
            InetAddress localIP,
            int localPort)
            throws SocksException, UnknownHostException, IOException {
        this(null, remoteHost, remotePort, localIP, localPort);
    }
    
    /**
     * Constructor.
     * Creates a data-streaming socket and connects it to the specified port number
     * at the specified IP address.
     * @param remoteIP The IP address of the remote host to connect to.
     * @param remotePort The port number to connect to on the remote host.
     * @throws SocksException If something is wrong with SOCKS.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksSocket(InetAddress remoteIP, int remotePort)
    throws SocksException, IOException {
        this(null, remoteIP.getHostName(), remotePort, null, 0);
    }
    
    /**
     * Constructor.
     * Creates a socket and connects it to the specified remote host
     * on the specified remote port. The socket also binds to
     * the local address and port supplied.
     * @param remoteIP The IP address of the remote host to connect to.
     * @param remotePort The port number to connect to on the remote host.
     * @param localIP The IP address of the local host that the socket is bound to.
     * @param localPort The local port on the local host that the socket is bound to.
     * @throws SocksException If something is wrong with SOCKS.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksSocket(
            InetAddress remoteIP,
            int remotePort,
            InetAddress localIP,
            int localPort)
            throws SocksException, IOException {
        this(null, remoteIP.getHostName(), remotePort, localIP, localPort);
    }
    
    /**
     * Sets the client socket implementation factory for the current
     * application.
     *
     * @param      factory   The desired factory.
     * @exception  IOException  If an input-output error occurs when setting the
     *             socket factory.
     */
    public static void setSocketImplFactory(SocketImplFactory factory)
    throws IOException {
        // this.socksSocket.setSocketImplFactory(factory);
        Socket.setSocketImplFactory(factory);
    }
    
    /**
     * Closes the current socket.
     *
     * @exception  IOException  If an input-output error occurs when closing the current socket.
     */
    public void close() throws IOException {
        this.socksSocket.close();
        // More reliable
        this.socksSocket = null;
    }
    /**
     * Retrieves the address to which the current socket is connected.
     *
     * @return  The remote IP address to which the socket is connected.
     */
    public InetAddress getInetAddress() {
        // We have two options for this method, that depends
        // on how to define and document the behavior of this method.
        // The related methods are getPort() and toString(),
        // these methods have to behave the same way.
        
        if (!this.overwriteBehavior) {
            // 1. We can always return the address of SOCKS server
            // if it is used no matter what the final destination
            // host is. Like following line:
            return this.socksSocket.getInetAddress();
        } else {
            // 2. We can return the real destination host address, so
            // SOCKS server is hidden from users. Like following line:
            return this.remoteIP;
        }
    }
    /**
     * Returns an input stream for the current socket.
     *
     * @return     An input stream for reading bytes from the socket.
     * @exception  IOException  If an input-output error occurs when creating the input stream.
     */
    public InputStream getInputStream() throws IOException {
        return this.socksSocket.getInputStream();
    }
    /**
     * Gets the local address to which the current socket is bound.
     * @return  The local IP address which binds to the socket.
     */
    public InetAddress getLocalAddress() {
        return this.socksSocket.getLocalAddress();
    }
    /**
     * Returns the local port to which the current socket is bound.
     *
     * @return  The local port number to which the socket is bound.
     */
    public int getLocalPort() {
        return this.socksSocket.getLocalPort();
    }
    /**
     * Returns an output stream for the current socket.
     *
     * @return     An output stream for writing bytes to the socket.
     * @exception  IOException  If an input-output error occurs when creating the output stream.
     */
    public OutputStream getOutputStream() throws IOException {
        return this.socksSocket.getOutputStream();
    }
    /**
     * Returns the remote port to which the current socket is connected.
     *
     * @return  The remote port number to which the socket is connected.
     */
    public int getPort() {
        // We have two options for this method, that depends
        // on how to define and document the behavior of this method.
        // The related methods are getInetAddress() and toString(),
        // these methods have to behave the same way.
        
        if (!this.overwriteBehavior) {
            // 1. We can always return the port number of SOCKS server
            // if it is used no matter what the final destination
            // host is. Like following line:
            return this.socksSocket.getPort();
        } else {
            // 2. We can return the real destination host port number, so
            // SOCKS server is hidden from users. Like following line:
            return this.remotePort;
        }
    }
    /**
     * Gets the value of the SO_RCVBUF option for the current socket, that is the
     * buffer size used by the platform for input on this socket.
     *
     * @return The buffer size (an integer).
     * @exception SocketException If there is an error in the underlying protocol, such as a TCP error.
     */
    public int getReceiveBufferSize() throws SocketException {
        return this.socksSocket.getReceiveBufferSize();
    }
    /**
     * Gets the value of the SO_SNDBUF option for the current socket, that is, the
     * buffer size used by the platform for output on the current socket.
     *
     * @return The buffer size (an integer).
     * @exception SocketException If there is an error in the underlying protocol, such as a TCP error.
     */
    public int getSendBufferSize() throws SocketException {
        return this.socksSocket.getSendBufferSize();
    }
    /**
     * Gets the setting for the SO_LINGER option. A -1 return means that the
     * option is disabled.
     * @return The SO_LINGER value (an integer).
     * @exception SocketException If there is an error in the underlying protocol, such as a TCP error.
     */
    public int getSoLinger() throws SocketException {
        return this.socksSocket.getSoLinger();
    }
    /**
     * Gets the setting for the SO_TIMEOUT option. A zero return means that the
     * option is disabled (that is, a timeout of infinity).
     * @return The SO_TIMEOUT value (an integer).
     * @exception SocketException If there is an error in the underlying protocol, such as a TCP error.
     */
    public int getSoTimeout() throws SocketException {
        return this.socksSocket.getSoTimeout();
    }
    /**
     * Tests whether the TCP_NODELAY option is enabled.
     * @return true or false.
     * @exception SocketException If there is an error in the underlying protocol, such as a TCP error.
     */
    public boolean getTcpNoDelay() throws SocketException {
        return this.socksSocket.getTcpNoDelay();
    }
    /**
     * Sets the SO_RCVBUF option to the specified value for the current
     * DatagramSocket. The SO_RCVBUF option is used by the platform's
     * networking code as a hint for the size to use to allocate and set
     * the underlying network input-output buffers.
     *
     * <p>Increasing the buffer size can improve the performance of
     * network input-output for a high-volume connection. However, decreasing the size can
     * help reduce the backlog of incoming data. For UDP, this method sets
     * the maximum size of a packet that can be sent on the current socket.
     *
     * <p>Because SO_RCVBUF is a hint, applications that want to
     * verify what size the buffers were set to must call
     * <href="#getReceiveBufferSize>getReceiveBufferSize</a>.
     *
     * @param size The size to use in setting the receive buffer.
     * size. This value must be greater than zero.
     *
     * @exception IllegalArgumentException If the value is zero or negative.
     * @exception SocketException If there is an error in the underlying protocol, such as a TCP error.
     */
    public void setReceiveBufferSize(int size) throws SocketException {
        this.socksSocket.setReceiveBufferSize(size);
    }
    /**
     * Sets the SO_SNDBUF option to the specified value for the current
     * DatagramSocket. The SO_SNDBUF option is used by the platform's
     * networking code as a hint for the size to use to allocate and set
     * the underlying network input-output buffers.
     *
     * <p>Increasing the buffer size can improve the performance of
     * network input-output for a high-volume connection. However, decreasing the size can
     * help reduce the backlog of incoming data. For UDP, this method sets
     * the maximum size of a packet that can be sent on the current socket.
     *
     * <p>Because SO_SNDBUF is a hint, applications that want to
     * verify what size the buffers were set to must call
     * <href="#getSendBufferSize>getSendBufferSize</a>.
     *
     * @param size The size to which to set the send buffer.
     * size. This value must be greater than zero.
     *
     * @exception IllegalArgumentException If the value is zero or negative.
     * @exception SocketException If there is an error in the underlying protocol, such as a TCP error.
     */
    public void setSendBufferSize(int size) throws SocketException {
        this.socksSocket.setSendBufferSize(size);
    }
    
    /**
     * Allows you to enable or disable the SO_LINGER option, using the specified linger time, in seconds.
     * If the specified timeout value exceeds 65,535, is reduced to
     * 65,535.
     *
     * @param on     Enables the SO_LINGER option.
     * @param linger How long to linger, if SO_LINGER is enabled.
     * @exception IllegalArgumentException If the linger value is negative.
     * @exception SocketException If there is an error in the underlying protocol, such as a TCP error.
     */
    public void setSoLinger(boolean on, int linger) throws SocketException {
        this.socksSocket.setSoLinger(on, linger);
    }
    /**
     * Allows you to enable or disable the SO_TIMEOUT option, using the specified timeout, in
     * milliseconds. With this option set to a non-zero timeout,
     * a read() call on the InputStream associated with the current socket
     * blocks for only the specified amount of time. If the timeout expires,
     * a <B>java.io.InterruptedIOException</B> is raised, though the
     * socket is still valid. This option <B>must</B> be enabled
     * before entering the blocking operation or it does not work. The
     * timeout must be greater than 0.
     * A timeout of zero is interpreted as an infinite timeout.
     *
     * @param timeout   The timeour time, in milliseconds (an integer).
     * @exception SocketException If there is an error in the underlying protocol, such as a TCP error.
     */
    public void setSoTimeout(int timeout) throws SocketException {
        this.socksSocket.setSoTimeout(timeout);
    }
    /**
     * Allows you to enable or disable TCP_NODELAY (Nagle's algorithm).
     * @param on  true or false.
     * @exception SocketException If there is an error in the underlying protocol, such as a TCP error.
     */
    public void setTcpNoDelay(boolean on) throws SocketException {
        this.socksSocket.setTcpNoDelay(on);
    }
    /**
     * Converts the current socket to a <code>String</code>.
     *
     * @return  A string representation of the current socket.
     */
    public String toString() {
        // We have two options for this method, that depends
        // on how to define and document the behavior of this method.
        // The related methods are getInetAddress() and getPort(),
        // these methods have to behave the same way.
        
        if (!this.overwriteBehavior) {
            // 1. We can always return the addr/port of SOCKS server
            // if it is used no matter what the final destination
            // host is. Like following line:
            return this.socksSocket.toString();
        } else {
            // 2. We can return the real destination host addr/port, so
            // SOCKS server is hidden from users. Like following line:
            return "SocksSocket[addr=" + this.remoteIP +
                    ",port=" + this.remotePort +
                    ",localport=" + this.localPort + "]";
        }
    }
    
    /**
     * Allows you to use the existing socket to connect to a remote FTP host via a specific host name and port.
     * @param host  The remote host name.
     * @param port  The remote host port number.
     * @exception  SocksException   If some error occurs.
     */
    private void connectRemote() throws SocksException {
        this.connect(this.remoteHost, this.remotePort);
    }
    
    
    
    
    
    /**
     * Tests whether the SO_KEEPALIVE option is enabled.
     * @return true or false.
     * @exception SocketException If there is an error in the underlying protocol, such as a TCP error.
     */
    public boolean getKeepAlive() throws SocketException {
        return this.socksSocket.getKeepAlive();
    }
    
    /**
     * Allows you to enable or disable the SO_KEEPALIVE option.
     * @param on  true or false.
     * @exception SocketException If there is an error in the underlying protocol, such as a TCP error.
     */
    public void setKeepAlive(boolean on) throws SocketException {
        this.socksSocket.setKeepAlive(on);
    }
    
    /**
     * Places the input data stream for the current socket at the "end of the stream."
     * Any data sent to the input stream side of a socket is
     * acknowledged, then silently discarded. If you read from
     * a socket's input stream after invoking shutdownInput() on
     * the socket, the stream returns EOF.
     * @exception IOException If an input-output error occurs when shutting down the current socket.
     */
    public void shutdownInput() throws IOException {
        this.socksSocket.shutdownInput();
    }
    
    /**
     * Disables the output data stream for the current socket. For a TCP socket,
     * any previously written data is sent followed by TCP's
     * normal connection termination sequence. If you write to a
     * socket output stream after invoking shutdownOutput() on
     * the socket, the stream throws an IOException.
     * @exception IOException If an input-output error occurs when shutting down the current socket.
     */
    public void shutdownOutput() throws IOException {
        this.socksSocket.shutdownOutput();
    }
    
    /**
     * Checks on the reply from the remote FTP host through SOCKS 4.
     * @exception SocksException If some error occurs.
     */
    private void V4_checkReply() throws SocksException {
        String msg = null;
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"SocksSocket.V4_checkReply()"}));

        String errMsg = null;
        // 1 byte (version) + 1 byte (status) + 2 bytes (port) + 4 bytes (address)
        byte[] reply = new byte[1 + 1 + 2 + 4];
        
        try {
            InputStream is = this.socksSocket.getInputStream();
            int received = 0;
            int oneByte = 0;
            //?Note: Some SOCKS servers (e.g. winproxy socks4) don't return the whole reply in
            //       one blob, for example, only after the first 2 bytes are read out, it
            //       returns the remaining 6 bytes. Let's read these bytes one by one.
            //? received += is.read(reply);
            while (received < reply.length && (oneByte = is.read()) != -1) {
                reply[received++] = (byte)oneByte;
            }
            
            while (errMsg == null) {
                if (received != reply.length) {
                    // Some error - cannot parse further
                    errMsg = mMessages.getString("FTPBC-E006065.ERR_EXT_FTP_SOCKS_SERVER_RETURN_BAD_STREAM",
                            new Object[] {"V4_checkReply()", received, reply.length});
                    break;
                }
                // Check version
                switch (reply[0]) {
                    case 0 :
                    {
                        break;
                    }
                    case Socks.VERSION_4 :
                    {
                        break;
                    }
                    case -1 :
                    {
                        errMsg = mMessages.getString("FTPBC-E006066.ERR_EXT_FTP_SOCKS_CHK_VER_CONN_REFUSED",
                                new Object[] {"V4_checkReply()", reply[0]});
                        break;
                    }
                    default :
                    {
                        errMsg = mMessages.getString("FTPBC-E006067.ERR_EXT_FTP_SOCKS_CHK_VER_INVALID",
                                new Object[] {"V4_checkReply()", reply[0]});
                        break;
                    }
                } // End of switch
                
                if (errMsg != null) {
                    break;
                }
                
                // Check status
                switch (reply[1]) {
                    case Socks.REPLY_V4_GRANTED :
                    {
                        break;
                    }
                    case Socks.REPLY_V4_REJECTED :
                    {
                        errMsg = mMessages.getString("FTPBC-E006068.ERR_EXT_FTP_SOCKS_CHK_STAT_REQ_FAIL",
                                new Object[] {"V4_checkReply()", reply[1]});
                        break;
                    }
                    case Socks.REPLY_V4_REJECTED_DIFF_IDENTS :
                    {
                        errMsg = mMessages.getString("FTPBC-E006069.ERR_EXT_FTP_SOCKS_CHK_STAT_REQ_FAIL_DIFF_USERID",
                                new Object[] {"V4_checkReply()", reply[1]});
                        break;
                    }
                    case Socks.REPLY_V4_REJECTED_NO_IDENTD :
                    {
                        errMsg = mMessages.getString("FTPBC-E006070.ERR_EXT_FTP_SOCKS_CHK_STAT_REQ_FAIL_CANNOT_CONN2DEMON",
                                new Object[] {"V4_checkReply()", reply[1]});
                        break;
                    }
                    default :
                    {
                        errMsg = mMessages.getString("FTPBC-E006071.ERR_EXT_FTP_SOCKS_CHK_STAT_REQ_FAIL_UNKWN",
                                new Object[] {"V4_checkReply()", reply[1]});
                        break;
                    }
                } // End of switch
                
                break;
            } // End of while
            
        } // End of try
        catch (Exception e) {
            msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"V4_checkReply()", e});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, e);
            }
            throw new SocksException(msg, e);
        }
        
        // Check errors
        if (errMsg != null) {
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new SocksException(msg);
        }
    }
    
    /**
     * Sends a request to the remote FTP host through SOCKS 4.
     * @param command   A SOCKS command (for example, CONNECT or BIND)
     * @exception SocksException If some error occurs.
     */
    private void V4_sendCommand(int command) throws SocksException {
        String msg = null;
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"SocksSocket.V4_sendCommand(int command)"}));
        
        try {
            ByteArrayOutputStream request = new ByteArrayOutputStream(1024);
            request.reset();
            request.write(Socks.VERSION_4);
            request.write(command);
            request.write(this.workingPortBytes);
            request.write(this.workingIPBytes);
            byte[] user = null;
            try {
                user = this.workingSocks.getSocksUser().getBytes(this.defaultEncoding);
            } catch (UnsupportedEncodingException e) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"V4_sendCommand(int command)", e}));
                }
                user = this.workingSocks.getSocksUser().getBytes();
            }
            
            if (user != null) {
                request.write(user);
            }
            //terminate in NULL byte
            request.write(Socks.NULL_IND);
            if (this.workingIPBytes[0] == 0) {
                // This is an impossible IP format, it is used for SOCKS version 4A.
                request.write(this.workingHostBytes);
                // terminate in NULL byte
                request.write(Socks.NULL_IND);
            }
            
            OutputStream os = this.socksSocket.getOutputStream();
            request.writeTo(os);
            os.flush();
            
            request.flush();
            request.close();
        } catch (Exception e) {
            msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"V4_sendCommand(int command)", e});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, e);
            }
            throw new SocksException(msg, e);
        }
    }
    
    /**
     * Check on the reply from the remote FTP host through SOCKS 5.
     * @exception SocksException If some error occurs.
     */
    private void V5_checkReply() throws SocksException {
        String msg = null;
        if (mLogger.isLoggable(Level.FINE)) 
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"SocksSocket.V5_checkReply()"}));
        
        String errMsg = null;
        // 1 byte (version) + 1 byte (status) + 1 byte (reserved) + 1 byte (address type)
        byte[] reply = new byte[1 + 1 + 1 + 1];
        
        try {
            InputStream is = this.socksSocket.getInputStream();
            int received = 0;
            int oneByte = 0;
            while (received < reply.length && (oneByte = is.read()) != -1) {
                reply[received++] = (byte)oneByte;
            }
            
            while (errMsg == null) {
                if (received != reply.length) {
                    // Some error - cannot parse further
                    errMsg = mMessages.getString("FTPBC-E006065.ERR_EXT_FTP_SOCKS_SERVER_RETURN_BAD_STREAM",
                            new Object[] {"V5_checkReply()", received, reply.length});
                    break;
                }

                // Check version
                if (reply[0] != Socks.VERSION_5) {
                    errMsg = mMessages.getString("FTPBC-E006067.ERR_EXT_FTP_SOCKS_CHK_VER_INVALID",
                            new Object[] {"V5_checkReply()", reply[0]});
                    break;
                }
                
                // Check status
                switch (reply[1]) {
                    case Socks.REPLY_V5_SUCCEEDED :
                    {
                        break;
                    }
                    case Socks.REPLY_V5_FAILURE :
                    {
                        errMsg = mMessages.getString("FTPBC-E006068.ERR_EXT_FTP_SOCKS_CHK_STAT_REQ_FAIL",
                                new Object[] {"V5_checkReply()", reply[1]});
                        break;
                    }
                    case Socks.REPLY_V5_NOT_ALLOWED :
                    {
                        errMsg = mMessages.getString("FTPBC-E006072.ERR_EXT_FTP_SOCKS5_CONN_NOT_ALLOWED_BY_RULESET",
                                new Object[] {"V5_checkReply()", reply[1]});
                        break;
                    }
                    case Socks.REPLY_V5_NETWORK_UNREACHABLE :
                    {
                        errMsg = mMessages.getString("FTPBC-E006073.ERR_EXT_FTP_SOCKS5_NETWORK_UNREACHABLE",
                                new Object[] {"V5_checkReply()", reply[1]});
                        break;
                    }
                    case Socks.REPLY_V5_HOST_UNREACHABLE :
                    {
                        errMsg = mMessages.getString("FTPBC-E006074.ERR_EXT_FTP_SOCKS5_HOST_UNREACHABLE",
                                new Object[] {"V5_checkReply()", reply[1]});
                        break;
                    }
                    case Socks.REPLY_V5_REFUSED :
                    {
                        errMsg = mMessages.getString("FTPBC-E006066.ERR_EXT_FTP_SOCKS_CHK_VER_CONN_REFUSED",
                                new Object[] {"V5_checkReply()", reply[1]});
                        break;
                    }
                    case Socks.REPLY_V5_TTL_EXPIRED :
                    {
                        errMsg = mMessages.getString("FTPBC-E006075.ERR_EXT_FTP_SOCKS5_TTL_EXPIRED",
                                new Object[] {"V5_checkReply()", reply[1]});
                        break;
                    }
                    case Socks.REPLY_V5_COMMAND_NOT_SUPPORTED :
                    {
                        errMsg = mMessages.getString("FTPBC-E006076.ERR_EXT_FTP_SOCKS5_CMD_NOT_SUPPORTED",
                                new Object[] {"V5_checkReply()", reply[1]});
                        break;
                    }
                    case Socks.REPLY_V5_ADDRESS_TYPE_NOT_SUPPORTED :
                    {
                        errMsg = mMessages.getString("FTPBC-E006077.ERR_EXT_FTP_SOCKS5_ADDRESS_NOT_SUPPORTED",
                                new Object[] {"V5_checkReply()", reply[1]});
                        break;
                    }
                    case Socks.REPLY_V5_INVALID_ADDRESS :
                    {
                        errMsg = mMessages.getString("FTPBC-E006078.ERR_EXT_FTP_SOCKS5_INVALID_ADDRESS",
                                new Object[] {"V5_checkReply()", reply[1]});
                        break;
                    }
                    default:
                    {
                        errMsg = mMessages.getString("FTPBC-E006071.ERR_EXT_FTP_SOCKS_CHK_STAT_REQ_FAIL_UNKWN",
                                new Object[] {"V5_checkReply()", reply[1]});
                        break;
                    }
                } // End of switch
                
                if (errMsg != null) {
                    break;
                }
                
                // Get address length
                int addrLen = 0;
                switch (reply[3]) {
                    case Socks.ADDR_TYPE_IP_V6 :
                    {
                        addrLen = 16;
                        break;
                    }
                    case Socks.ADDR_TYPE_IP_V4 :
                    {
                        addrLen = 4;
                        break;
                    }
                    case Socks.ADDR_TYPE_DOMAINNAME :
                    {
                        addrLen = is.read();
                        break;
                    }
                    default:
                    {
                        errMsg = mMessages.getString("FTPBC-E006079.ERR_EXT_FTP_SOCKS5_INVALID_ADDRESS_TYPE",
                                new Object[] {"V5_checkReply()", reply[3]});
                        break;
                    }
                } // End of switch
                
                if (errMsg != null) {
                    break;
                }
                
                // address length + 2 bytes (port)
                reply = new byte[addrLen + 2];
                received = 0;
                while (received < reply.length && (oneByte = is.read()) != -1) {
                    reply[received++] = (byte)oneByte;
                }
                
                if (received != reply.length) {
                    // Some error - cannot parse further
                    errMsg = mMessages.getString("FTPBC-E006065.ERR_EXT_FTP_SOCKS_SERVER_RETURN_BAD_STREAM",
                            new Object[] {"V5_checkReply()", received, reply.length});
                    break;
                }
                // Just skip length [addrLen + 2], no further check is needed.
                
                break;
                
            } // End of while
        } // End of try
        catch (Exception e) {
            msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"SocksSocket.V5_checkReply()", e});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, e);
            }
            throw new SocksException(msg, e);
        }
        
        // Check errors
        if (errMsg != null) {
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new SocksException(msg);
        }
    }
    
    
    
    /**
     * Sends a request to the remote FTP host through SOCKS 5.
     * @param command   A SOCKS command (for example, CONNECT or BIND).
     * @param addrType  The address type (for example, DOMAINNAME, IP_V4, IP_V6).
     * @exception SocksException If some error occurs.
     */
    private void V5_sendCommand(int command, int addrType) throws SocksException {
        String msg = null;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"SocksSocket.V5_sendCommand(int command, int addrType)"}));
        }
        try {
            ByteArrayOutputStream request = new ByteArrayOutputStream(1024);
            request.reset();
            request.write(Socks.VERSION_5);
            request.write(command);
            request.write(Socks.CMD_FLAG);
            request.write(addrType);
            switch (addrType) {
                case Socks.ADDR_TYPE_DOMAINNAME : {
                    request.write(this.workingHostBytes.length);
                    request.write(this.workingHostBytes);
                    break;
                }
                case Socks.ADDR_TYPE_IP_V4 : {
                    request.write(this.workingIPBytes);
                    break;
                }
                case Socks.ADDR_TYPE_IP_V6 : {
                    // ? Need to verify
                    request.write(this.workingIPBytes);
                    break;
                }
                default: {
                    msg = mMessages.getString("FTPBC-E006080.ERR_EXT_FTP_SOCKS5_INVALID_ADDRESS_TYPE_SENT",
                            new Object[] {"V5_sendCommand()", addrType});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg);
                    }
                    throw new SocksException(msg);
                }
            }
            request.write(this.workingPortBytes);
            OutputStream os = this.socksSocket.getOutputStream();
            request.writeTo(os);
            os.flush();
            
            request.flush();
            request.close();
        } catch (Exception e) {
            msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"SocksSocket.V5_sendCommand(int command, int addrType)", e});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, e);
            }
            throw new SocksException(msg, e);
        }
    }
    
    /**
     * Negotiates the use of the method CHAP.
     * NOTE: This method is not implemented yet.
     * @return  A message (a null means success, but a non-null means failure).
     * @exception SocksException If some error occurs.
     */
    private String V5_NegotiateCHAP() throws SocksException {
        String msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"SocksSocket.V5_NegotiateCHAP()"});
        if (mLogger.isLoggable(Level.SEVERE)) {
            mLogger.log(Level.SEVERE, msg);
        }
        //throw new SocksException(msg);
        // Don't throw exception
        // Defer the exception to caller
        return msg;
    }
    
    /**
     * Negotiates the use of the method GSS-API.
     * NOTE: This method is not implemented yet.
     * @return  A message (a null means success, but a non-null means failure).
     * @exception SocksException If some error occurs.
     */
    private String V5_NegotiateGSSAPI() throws SocksException {
        String msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"SocksSocket.V5_NegotiateGSSAPI()"});
        if (mLogger.isLoggable(Level.WARNING)) {
            mLogger.log(Level.WARNING, msg);
        }
        return msg;
    }
    
    /**
     * Negotiates an authentication using the method USER/PASSWORD (rfc1929).
     * @return  A message (a null means success, but a non-null means failure).
     * @exception SocksException If some error occurs.
     */
    private String V5_NegotiateUserPassword() {
        String msg = null;
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"SocksSocket.V5_NegotiateUserPassword()"}));
        
        String errMsg = null;
        
        // 1 byte (version) + 1 byte (status)
        byte[] reply = new byte[1 + 1];
        
        try {
            // Send sub-negociate request
            ByteArrayOutputStream request = new ByteArrayOutputStream(1024);
            request.reset();
            // Sub-negociation version 1
            request.write(0x01);
            byte[] user = null;
            try {
                user = this.workingSocks.getSocksUser().getBytes(this.defaultEncoding);
            } catch (UnsupportedEncodingException e) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006018.WRN_EXT_FTP_UNSUPPORTED_ENCODING", new Object[] {"SocksSocket.V5_NegotiateUserPassword()",
                    defaultEncoding}));
                }
                user = this.workingSocks.getSocksUser().getBytes();
            }
            
            if (user == null) {
                request.write(0x0);
            } else {
                request.write(user.length);
                request.write(user);
            }
            String pass = null;
            try {
                pass =
                        (this.workingSocks.getSocksPassword() == null ||
                        this.workingSocks.getSocksPassword().equals(""))
                        ? ""
                        : NonEmptyScEncrypt.decrypt(this.workingSocks.getSocksUser(),
                        this.workingSocks.getSocksPassword());
            } catch (Exception e) {
                errMsg = mMessages.getString("FTPBC-E006081.ERR_EXT_FTP_FAIL_DECRYPT_PASSWORD", new Object[] {"V5_NegotiateUserPassword()", e});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, errMsg, e);
                }
                // Don't throw exception
                // Defer the exception to caller
                //throw new SocksException(errMsg, e);
                return errMsg;
            }
            
            byte[] password = null;
            try {
                password = pass.getBytes(this.defaultEncoding);
            } catch (UnsupportedEncodingException e) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006018.WRN_EXT_FTP_UNSUPPORTED_ENCODING", new Object[] {"SocksSocket.V5_NegotiateUserPassword()",
                    defaultEncoding}));
                }
                password = pass.getBytes();
            }
            
            if (password == null) {
                request.write(0x0);
            } else {
                request.write(password.length);
                request.write(password);
            }
            
            // We have to validate this condition because SOCKS server will not
            // return error code under that condition
            if ((user == null || user.length == 0) &&
                    (password == null || password.length == 0)) {
                errMsg = mMessages.getString("FTPBC-E006082.ERR_EXT_FTP_SOCKS_MISSING_USER_PASSWORD",
                        new Object[] {"SocksSocket.V5_NegotiateUserPassword()"});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, errMsg);
                }
                // Don't throw exception
                // Defer the exception to caller
                //throw new SocksException(errMsg);
                return errMsg;
            }
            
            OutputStream os = this.socksSocket.getOutputStream();
            request.writeTo(os);
            os.flush();
            
            request.flush();
            request.close();
            
            // Get reply
            InputStream is = this.socksSocket.getInputStream();
            int received = 0;
            int oneByte = 0;
            while (received < reply.length && (oneByte = is.read()) != -1) {
                reply[received++] = (byte)oneByte;
            }
            
            while (errMsg == null) {
                if (received != reply.length) {
                    // Some error - cannot parse further
                    errMsg = mMessages.getString("FTPBC-E006065.ERR_EXT_FTP_SOCKS_SERVER_RETURN_BAD_STREAM",
                            new Object[] {"V5_NegotiateUserPassword()", received, reply.length});
                    break;
                }
                //?? We accept both 0x05 and 0x01 as version here. 0x01
                // is the right response but some buggy servers may
                // respond with 0x05 (i.e. not comply with rfc1929).
                
                //??if (reply[0] != 0x01) {
                if (reply[0] != 0x01 && reply[0] != 0x05) {
                    errMsg = mMessages.getString("FTPBC-E006083.ERR_EXT_FTP_SOCKS_WRONG_VER_SUB_NEGOTIATION",
                            new Object[] {"V5_NegotiateUserPassword()", reply[0]});
                    break;
                }
                if (reply[1] != 0x0) {
                    errMsg = mMessages.getString("FTPBC-E006084.ERR_EXT_FTP_SOCKS_AUTH_FAIL",
                            new Object[] {"V5_NegotiateUserPassword()", reply[1]});
                    break;
                }
                break;
            } // End of while
        } // End of try
        catch (Exception e) {
            msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"SocksSocket.V5_NegotiateUserPassword()", e});
            // Defer the exception to caller
        }
        
        if (errMsg != null) {
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, errMsg);
            }
            // Defer the exception to caller
        }
        
        return errMsg;
        
    }
    
    /**
     * Allows you to determine the negotiation method.
     * @return  A negotiation method (an integer) used
     *          for keeping track of communications.
     * @exception SocksException If some error occurs.
     */
    private int V5_selectMethod(byte[] methods) throws SocksException {
        String msg = null;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"SocksSocket.V5_selectMethod()"}));
        }
        
        String errMsg = null;
        // 1 byte (version) + 1 byte (method)
        byte[] reply = new byte[1 + 1];
        
        try {
            ByteArrayOutputStream request = new ByteArrayOutputStream(1024);
            request.reset();
            request.write(Socks.VERSION_5);
            request.write(methods.length);
            request.write(methods);
            
            OutputStream os = this.socksSocket.getOutputStream();
            request.writeTo(os);
            os.flush();
            
            request.flush();
            request.close();
            
            InputStream is = this.socksSocket.getInputStream();
            int received = 0;
            int oneByte = 0;
            while (received < reply.length && (oneByte = is.read()) != -1) {
                reply[received++] = (byte)oneByte;
            }
            
            while(errMsg == null) {
                if (received != reply.length) {
                    // Some error - cannot parse further
                    errMsg = mMessages.getString("FTPBC-E006065.ERR_EXT_FTP_SOCKS_SERVER_RETURN_BAD_STREAM",
                            new Object[] {"SocksSocket.V5_selectMethod()", received, reply.length});
                    break;
                }
                // Check version
                switch (reply[0]) {
                    case Socks.VERSION_5 :
                    {
                        break;
                    }
                    case -1 :
                    {
                        errMsg = mMessages.getString("FTPBC-E006066.ERR_EXT_FTP_SOCKS_CHK_VER_CONN_REFUSED",
                                new Object[] {"SocksSocket.V5_selectMethod()", reply[0]});
                        break;
                    }
                    default :
                    {
                        errMsg = mMessages.getString("FTPBC-E006067.ERR_EXT_FTP_SOCKS_CHK_VER_INVALID",
                                new Object[] {"SocksSocket.V5_selectMethod()", reply[0]});
                        break;
                    }
                } // End of switch
                
                if (errMsg != null) {
                    break;
                }
                
                // Sub-negotiation - check authentication method
                String method = null;
                switch ((int)reply[1]) {
                    case Socks.METHOD_NO_AUTHENTICATION_REQUIRED :
                    {
                        method = "METHOD_NO_AUTHENTICATION_REQUIRED";
                        errMsg = V5_NegotiateNoAuthentication();
                        break;
                    }
                    case Socks.METHOD_GSSAPI :
                    {
                        method = "METHOD_GSSAPI";
                        errMsg = V5_NegotiateGSSAPI();
                        break;
                    }
                    case Socks.METHOD_USERNAME_PASSWORD :
                    {
                        method = "METHOD_USERNAME_PASSWORD";
                        errMsg = V5_NegotiateUserPassword();
                        break;
                    }
                    case Socks.METHOD_CHAP :
                    {
                        method = "METHOD_CHAP";
                        errMsg = V5_NegotiateCHAP();
                        break;
                    }
                    case Socks.METHOD_NO_ACCEPTABLE_METHODS :
                    {
                        method = "METHOD_NO_ACCEPTABLE_METHODS";
                        errMsg = mMessages.getString("FTPBC-E006085.ERR_EXT_FTP_SOCKS_NO_ACCEPTABLE_NEG_METHOD", new Object[] {"SocksSocket.V5_selectMethod()"});
                        break;
                    }
                    default :
                    {
                        method = "No method";
                        errMsg = mMessages.getString("FTPBC-E006086.ERR_EXT_FTP_SOCKS_CAN_NOT_PROCESS_METHOD", new Object[] {"SocksSocket.V5_selectMethod()", reply[1]});
                        break;
                    }
                } // End of switch
                
                if (mLogger.isLoggable(Level.INFO)) {
                    mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006057.DBG_EXT_FTP_SOCKS_METHOD_SELECT", new Object[] {"SocksSocket.V5_selectMethod()", method}));
                }
                
                break;
                
            } // End of while
        } // End of try
        catch (Exception e) {
            msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"SocksSocket.V5_selectMethod()", e});
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, msg, e);
            }
            throw new SocksException(msg, e);
        }
        
        // Check errors
        if (errMsg != null) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, msg);
            }
            throw new SocksException(msg);
        }
        
        return reply[1];
    }
    
    
    
    /**
     * Returns the socket.
     *
     * @return  The socket.
     */
    // We can change it to public if we want to get a
    // socket with standard behaviors without any overwrite
    // features. Note: the current socket is connected to SOCKS
    // server and pointed to remote destination host.
    private Socket getSocksSocket() {
        return this.socksSocket;
    }
    
    /**
     * Used to do stand-alone testing.
     * @param       args  Command line parameters.
     */
    public static void main(String args[]) {
        try {
            Socks s = new Socks("hliu2", 1080, "", "", 4);
            SocksSocket ss = new SocksSocket(s, "pluto", 21);
            byte[] b = new byte[100];
            ss.getInputStream().read(b);
            System.out.println("SocksSocket.getInputStream() returns " + new String(b));
            ss.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return;
    }
    
    /**
     * Constructor.
     * Creates a data-stream socket and connects it to the specified port number on the named host.
     * @param socksList Represents a SOCKS server chain.
     * @param remoteHost The remote FTP host to connect to.
     * @param remotePort The port number to connect to on remote host.
     * @throws SocksException If something is wrong with SOCKS.
     * @throws UnknownHostException If the IP address of the host could not be determined.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksSocket(String remoteHost, int remotePort, SocksChain socksList)
    throws SocksException, UnknownHostException, IOException {
        this(remoteHost, remotePort, null, 0, socksList);
    }
    
    /**
     * Constructor.
     * Creates a socket and connects it to the specified remote FTP host
     * on the specified remote port. The socket also binds to
     * the local address and port supplied.
     * @param socksList Represents a SOCKS server chain.
     * @param remoteHost The remote host to connect to.
     * @param remotePort The port number to connect to on remote host.
     * @param localIP The IP address of the local host that the socket is bound to.
     * @param localPort The local port (on the local host) that the socket is bound to.
     * @throws SocksException If something is wrong with SOCKS.
     * @throws UnknownHostException If the IP address of the host could not be determined.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksSocket(
            String remoteHost,
            int remotePort,
            InetAddress localIP,
            int localPort,
            SocksChain socksList)
            throws SocksException, UnknownHostException, IOException {
        
        String msg = null;
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"SocksSocket.SocksSocket(..., SocksChain)"}));
        
        this.socksList = socksList;
        this.remoteHost = Socks.resolveHostName(remoteHost);
        this.remoteIP = InetAddress.getByName(this.remoteHost);
        this.remotePort = remotePort;
        this.localIP = localIP;
        this.localPort = localPort;
        if (this.localIP == null) {
            this.localHost = null;
        } else {
            this.localHost = this.localIP.getHostAddress();
        }
        
        if (this.socksList == null) {
            if (mLogger.isLoggable(Level.WARNING))
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006017.WRN_EXT_FTP_NO_SOCKS_SERVER_SPECIFIED_REG_SOCKET_CREATED", new Object[] {"SocksSocket.SocksSocket()"}));
            this.socksSocket = new Socket(this.remoteHost, this.remotePort, this.localIP, this.localPort);
            return;
        }
        
        try {
            // The first SOCKS server if the SOCKS chain exists, otherwise, it's the only one.
            this.workingSocks = this.socksList.get(0);
            
            // Create this.socksSocket
            this.createSocksSocket();
            
            // Talk to SOCKS server chain
            this.connectSocksChain();
            
            // Talk to remote server
            this.connectRemote();
        } catch (Exception e) {
            if (this.socksSocket != null) {
                this.socksSocket.close();
            }
            msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"SocksSocket.SocksSocket()", e});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, e);
            }
            throw new SocksException(msg, e);
        }
    }
    
    /**
     * Constructor.
     * Creates a data-stream socket and connects it to the specified port number
     * at the specified IP address.
     * @param socksList Represents a SOCKS server chain.
     * @param remoteIP The IP address of the remote host to connect to.
     * @param remotePort The port number to connect to on remote host.
     * @throws SocksException If something is wrong with SOCKS.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksSocket(InetAddress remoteIP, int remotePort, SocksChain socksList)
    throws SocksException, IOException {
        this(remoteIP.getHostName(), remotePort, null, 0, socksList);
    }
    
    /**
     * Constructor.
     * Creates a socket and connects it to the specified remote host address
     * on the specified remote port. The socket also binds to
     * the local address and port supplied.
     * @param socksList Represents a SOCKS server chain.
     * @param remoteIP The IP address of the remote host to connect to.
     * @param remotePort The port number to connect to on remote host.
     * @param localIP The IP address of the local host that the socket is bound to.
     * @param localPort The local port (on the local host) that the socket is bound to.
     * @throws SocksException If something is wrong with SOCKS.
     * @throws IOException If an input-output error occurs when creating the socket.
     */
    public SocksSocket(
            InetAddress remoteIP,
            int remotePort,
            InetAddress localIP,
            int localPort,
            SocksChain socksList)
            throws SocksException, IOException {
        this(remoteIP.getHostName(), remotePort, localIP, localPort, socksList);
    }
    
    /**
     * Returns the SOCKS server to which the socket is connected.
     *
     * @return  The SOCKS server to which the current socket is connected.
     */
    // This is a specific method for this class, it is not
    // a overwrite method from super class java.net.Socket.
    public Socks getSocks() {
        return this.socks;
    }
    
    /**
     * Create a socket on the SOCKS server.
     * @exception  SocksException   If some error occurs.
     */
    // This method takes this.workingSocks as input information.
    private void createSocksSocket() throws SocksException {
        String msg = null;
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"SocksSocket.createSocksSocket()"}));
        
        String socksHost = this.workingSocks.getSocksHost();
        int socksPort = this.workingSocks.getSocksPort();
        InetAddress[] socksIPs = null;
        try {
            // Get all IP addresses that associate with the host name
            socksIPs = InetAddress.getAllByName(socksHost);
        } catch (Exception e) {
            msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"SocksSocket.createSocksSocket()", e});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, e);
            }
            throw new SocksException(msg, e);
        }
        
        // Try all IP addresses
        for (int i = 0; i < socksIPs.length; i++) {
            try {
                if (this.localIP == null) {
                    this.socksSocket = new Socket(socksIPs[i], socksPort);
                    //this.socksSocket = new Socket(socksHost, socksPort);
                } else {
                    this.socksSocket =
                            new Socket(socksIPs[i], socksPort, this.localIP, this.localPort);
                }
                break;
            } catch (Exception e) {
                if (i >= socksIPs.length - 1) {
                    msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"SocksSocket.createSocksSocket()", e});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg, e);
                    }
                    throw new SocksException(msg, e);
                } else {
                    // Try next IP address
                    continue;
                }
            }
        }
    }
    
    
    
    /**
     * Allows you to use the existing socket to connect through the SOCKS chain.
     * @exception  SocksException   If some error occurs.
     */
    private void connectSocksChain() throws SocksException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"SocksSocket.connectSocksChain()"}));
        }
        
        if (this.socksList == null) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006019.WRN_EXT_FTP_NO_SOCKS_SERVER_SPECIFIED", new Object[] {"SocksSocket.connectSocksChain()"}));
            }
            return;
            // Don't throw exception
            //mLogger.log(Level.SEVERE, msg);
            //throw new SocksException(msg);
        }
        
        // Note: Following loop starts with i = 1 instead of i = 0.  -- Tricky but effective.
        //       The first one has been assigned to this.workingSocks already.
        //       In the chain, the next SOCKS is just a remote host from the point of
        //       current SOCKS.
        for (int i = 1; i < this.socksList.chainSize(); i++){
            Socks nextSocks = this.socksList.get(i);
            this.connect(nextSocks.getSocksHost(), nextSocks.getSocksPort());
            this.workingSocks = nextSocks;
        }
    }
    
    /**
     * Returns the SOCKS chain to which the current socket is connected.
     *
     * @return  The SOCKS chain to which the socket is connected.
     */
    // This is a specific method for this class, it is not
    // a overwrite method from super class java.net.Socket.
    public SocksChain getSocksList() {
        return this.socksList;
    }
    
    /**
     * Allows you to use the existing socket to connect to a remote FTP host using a specified host name and port.
     * @param host  The remote host name.
     * @param port  The remote host port number.
     * @exception  SocksException   If some error occurs.
     */
    private void connect(String host, int port) throws SocksException {
        String msg = null;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006007.DBG_EXT_FTP_SOCKS_INFO", new Object[] {"connect(String host, int port)", 
            host, port, workingSocks.getSocksHost(), workingSocks.getSocksPort()}));
        }
        
        // Set working bytes variables: workingIPBytes, workingPortBytes, workingHostBytes
        try {
            this.workingIPBytes = InetAddress.getByName(host).getAddress();
        }
        // Note: This is the difference with normal socket behavior.
        //       If the host name cannot be resolved, don't throw exception.
        //       According to SOCKS version 4A, we will ask SOCKS server to
        //       resolve the host name.
        //catch (SecurityException e) {
        //catch (UnknownHostException e) {
        catch (Exception e) {
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006058.DBG_EXT_FTP_SOCKS_HOST_CANNOT_RESOLVED_LOCALLY", new Object[] {"SocksSocket.connect()"}));
            }
            // do nothing - keep initial value
            this.workingIPBytes[0] = (byte)0;
            this.workingIPBytes[1] = (byte)0;
            this.workingIPBytes[2] = (byte)0;
            this.workingIPBytes[3] = (byte)1;
        }
        
        this.workingPortBytes[0] = (byte) ((port >> 8) & 0xff);
        this.workingPortBytes[1] = (byte) (port & 0xff);
        try {
            this.workingHostBytes = host.getBytes(this.defaultEncoding);
        } catch (UnsupportedEncodingException e) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006018.WRN_EXT_FTP_UNSUPPORTED_ENCODING", new Object[] {"SocksSocket.connect()",
                defaultEncoding}));
            }
            this.workingHostBytes = host.getBytes();
        }
        
        // OK, now all working bytes are available to use directly
        // (by method V4_sendCommand(), V5_sendCommand(), etc.
        
        
        
        switch (this.workingSocks.getSocksVersion()) {
            case Socks.VERSION_4 :
            {
                // Version 4A is included
                this.V4_sendCommand(Socks.COMMAND_CONNECT);
                this.V4_checkReply();
                break;
            }
            case Socks.VERSION_5 :
            {
                byte[] methods = {(byte) Socks.METHOD_NO_AUTHENTICATION_REQUIRED,
                // (byte) Socks.METHOD_GSSAPI,
                // (byte) Socks.METHOD_CHAP,
                (byte) Socks.METHOD_USERNAME_PASSWORD
                };
                // A set of methods are sent to SOCKS server, SOCKS server will
                // determine the negotiation method according to the sequence of
                // the methods that are configured in SOCKS server configuration
                // file (such as socks5.conf). -- get the first matching ??!!
                this.methodAllowed = this.V5_selectMethod(methods);
                this.V5_sendCommand(Socks.COMMAND_CONNECT, Socks.ADDR_TYPE_DOMAINNAME);
                // Note: You can also consider to use other address types as below.
                //       If you'd like to use address type IP_V6, you should make sure
                //       that yuor JVM and your SOCKS server supports it.
                //       IP_V6 is supported by JDK1.4.
                //       You can always use address type DOMAINNAME safely.
                //this.V5_sendCommand(Socks.COMMAND_CONNECT, Socks.ADDR_TYPE_IP_V4);
                //this.V5_sendCommand(Socks.COMMAND_CONNECT, Socks.ADDR_TYPE_IP_V6);
                this.V5_checkReply();
                break;
            }
            case Socks.VERSION_UNKNOWN :
            {
                this.workingSocks.setSocksVersion(this.findVersion(host, port));
                break;
            }
            default :
            {
                msg = mMessages.getString("FTPBC-E006027.ERR_EXT_FTP_INVALID_SOCKS_VER", new Object[] {workingSocks.getSocksVersion(),
                    new Integer(Socks.VERSION_4), new Integer(Socks.VERSION_5), new Integer(Socks.VERSION_UNKNOWN)

                });
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new SocksException(msg);
            }
        }
        
    }
    
    /**
     * Allows you to determine the SOCKS server version according to
     * the response of the SOCKS server.
     * @param host    The SOCKS server host name.
     * @param port    The SOCKS server port number.
     * @return  The version of the SOCKS server.
     * @exception  SocksException   If some error occurs.
     */
    private int findVersion(String host, int port) throws SocksException {
        String msg = null;
        int version = -1;
        try {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006059.DBG_EXT_FTP_SOCKS_DETERMINE_SERVER_VER_ATTEMPT_V5", new Object[] {"SocksSocket.findVersion()", workingSocks.getSocksHost()}));
            }
            // Try version 5 first
            version = Socks.VERSION_5;
            this.workingSocks.setSocksVersion(version);
            // this.tryingVersion will only work in the scope of following this.connect(host, port).
            //this.tryingVersion = true;
            // Invoke method connect() recursively using version 5
            this.connect(host, port);
            //this.tryingVersion = false;
        } catch (Exception e_v5) {
            try {
                //this.tryingVersion = false;
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006060.DBG_EXT_FTP_SOCKS_DETERMINE_SERVER_VER_NOT_V5_ASSUME_V4", new Object[] {"SocksSocket.findVersion()", workingSocks.getSocksHost()}));
                }
                
                // Version 4 is assumed and update the socks with new version number.
                // In SOCKS chain, one unknown version SOCKS server gets the real version
                // and is updated, so next time this SOCKS server will have known version
                // number.
                version = Socks.VERSION_4;
                this.workingSocks.setSocksVersion(version);
                
                // From beginning
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006061.DBG_EXT_FTP_SOCKS_DETERMINE_SERVER_VER_RECONNECT", new Object[] {"SocksSocket.findVersion()"}));
                }
                
                // If last try fails, the SOCKS server might close the socket already,
                // we should create the socket again.
                // In case of the socket is not closed, do a clean up (close) before we
                // create the new socket
                try {
                    this.close();
                } catch (Exception nothing) {
                    // do nothing
                }
                
                // Invoke method connect() RECURSIVELY again using version 4.
                // createSocksSocket() and connectRemote() --> connect() --> findVersion()
                //         A                     A                                 |
                //         |                     |                                 |
                //         |                     A                                 V
                //         |--------<<---<<------|-------<<-------<<------<<-------|
                //
                // This is a new socket, we have to do everything from beginning,
                // but the progress is we have resolved an unknown SOCKS server version.
                if (this.socksList != null) {
                    this.workingSocks = this.socksList.get(0);
                } else {
                    this.workingSocks = this.socks;
                }
                
                // Create this.socksSocket
                this.createSocksSocket();
                
                // Talk to SOCKS server chain
                this.connectSocksChain();
                
                // Talk to remote server
                this.connectRemote();
            } catch (Exception e_v4) {
                msg = mMessages.getString("FTPBC-E006087.ERR_EXT_FTP_SOCKS_CAN_NOT_DETERMINE_SERVER_VER",
                        new Object[] {
                    "SocksSocket.findVersion()",
                    workingSocks.getSocksHost(),
                    e_v4.toString(),
                    e_v5.toString()
                });
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg, e_v4);
                }
                throw new SocksException(msg, e_v4);
            }
        }
        
        //this.tryingVersion = false;
        if (mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006062.DBG_EXT_FTP_SOCKS_SERVER_VER_DETERMINED", new Object[] {"SocksSocket.findVersion()", workingSocks.getSocksHost(), version}));
        
        return version;
    }
    
    /**
     * Allows you to negotiate the use of the method NO-AUTH.
     * @return  A message (a null means success, but a non-null means failure).
     * @exception SocksException If some error occurs.
     */
    private String V5_NegotiateNoAuthentication() throws SocksException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"SocksSocket.V5_NegotiateNoAuthentication()"}));
        }
        
        // Must return null always for method NO_AUTHENTICATION_REQUIRED
        return null;
    }
}
