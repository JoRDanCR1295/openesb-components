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
 * @(#)FtpFileClientImpl.java 
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

import com.sun.jbi.ftpbc.ftp.exception.FtpInterfaceException;
import com.sun.jbi.ftpbc.ftp.exception.FtpFileException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Properties;
import java.util.StringTokenizer;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.ftpbc.ftp.io.stream.InputStreamAdapter;
import com.sun.jbi.ftpbc.ftp.io.stream.OutputStreamAdapter;
import com.sun.jbi.ftpbc.ftp.io.stream.StreamingException;

/**
 * this class represents a ftp client with configurable parameters.
 * @author Harry Liu
 * @author jfu
 * @version cvs revision:    Last Modified: 
 */

public class FtpFileClientImpl implements FtpFileClient {
    private static final Messages mMessages =
            Messages.getMessages(FtpFileClientImpl.class);
    private static final Logger mLogger =
            Messages.getLogger(FtpFileClientImpl.class);
    
    private String msg = null;
    private byte[] workingPayload = null;
    private TransferNamesAndCommands tnc = null;
    
    private FtpFileConfiguration configuration = null;
    private FtpInterface intf = null;

    private String openHostName = null;
    private int openServerPort = 0;
    private boolean openUsePASV = true;
    private boolean openRemoteVerify = true;
    
    private byte[] payload = null;
    
    private InputStreamAdapter insa = null;
    private OutputStreamAdapter outsa = null;
    private InputStream is = null;
    private OutputStream os = null;
    private boolean noWarning;
    
    /**
     * Constructor.
     */
    public FtpFileClientImpl() {
        logMethodCalled("FtpFileClientImpl()");
    }
    
    /**
     * Initializes ftp client.
     * @param intf   reference to the FtpInterface object.
     * @exception FtpFileException  If some error occurs.
     */
    public void initialize(FtpInterface intf) throws FtpFileException {
        logMethodCalled("initialize()");
                
        this.intf = intf;
        this.configuration = this.intf.getConfiguration();
        //!! don't get then keep the provider instance because
        //   1. At this point, the provider maybe not created yet, so you'll get null.
        //   2. After this point, the provider maybe re-created, so you'll keep an old one.
        //   3. We need to call connector.setLastActivityTime() on each connection activity.
        // We should always use this.intf.getProvider() to get updated provider instance.
        //!! this.provider = this.intf.getProvider();
        
        insa = null;
        outsa = null;
    }
    
    /**
     * Performs FTP pre and post transfer raw commands, for example:
     * SITE RECFM=FB;SITE LRECL=50;SITE BLOCKSIZE=32750;SITE TRACKS;SITE PRI=5;SITE SEC=5
     * NOTE: The commands are separated by a semicolon (;), and
     * only FTP raw commands are expected.
     * @param       commands  The FTP raw command set.
     * @param       commands  The raw command set.
     *
     * @exception   FtpFileException  If some error occurs.
     */
    public void doRawCommands(String commands) throws FtpFileException {
        logMethodCalled("doRawCommands(String commands)");

        if (commands == null || commands.equals(""))
            return;
        
        String oneCommand = null;
        try {
            StringTokenizer tokens = new StringTokenizer(commands, ";");
            while (tokens.hasMoreTokens()) {
                oneCommand = tokens.nextToken().trim();
                
                if (oneCommand == null || oneCommand.equals("")) 
                    continue;

                if (mLogger.isLoggable(Level.FINE))
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006004.DBG_EXT_FTP_SEND_RAW_CMD", new Object[] {oneCommand}));
                
                if (this.intf.getProvider().isNegativePermanent(this.intf.getProvider().sendCommand(oneCommand))) {
                    this.msg = mMessages.getString("FTPBC-E006002.ERR_EXT_FTP_RAW_CMD_FAIL", new Object[] {oneCommand, intf.getProvider().getReplyString()});
                    if (mLogger.isLoggable(Level.SEVERE))
                        mLogger.log(Level.SEVERE, this.msg);
                    
                    // in put(), we might need to deleteTempFile() for any failure,
                    // so don't do this.close() here.
                    // this.close();
                    throw new FtpFileException(this.msg);
                } else {
                    if (mLogger.isLoggable(Level.FINE))
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006005.DBG_EXT_FTP_RAW_CMD_OK", new Object[] {oneCommand, intf.getProvider().getReplyString()}));
                }
            }
        } catch (Exception e) {
            this.msg = mMessages.getString("FTPBC-E006003.ERR_EXT_FTP_RAW_CMD_EXCEPTION", new Object[] {oneCommand, e.getMessage()});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg, e);
            }
            // in put(), we might need to deleteTempFile() for any failure,
            // so don't do this.close() here.
            // this.close();
            throw new FtpFileException(this.msg, e);
        }
        
        return;
    }
    
    /**
     * Use this method to retrieve the new information when
     * FTP server settings (for example, host name or port number) have been changed.
     */
    private void obtainFtpServerInfo() {
        logMethodCalled("obtainFtpServerInfo()");
        
        try {
            // Can we use this systemName to derive the directoryListingStyle
            // so we don't need user to config directoryListingStyle?
            // According to RFC-959, SYST should return the system name, but not all
            // Ftp Servers implement this already. So we can treat it as a useful
            // information rather than a reliable resource.
            // this.systemName = this.intf.getProvider().getSystemName();
            if (mLogger.isLoggable(Level.FINER))
                mLogger.log(Level.FINER, mMessages.getString("FTPBC-D006006.DBG_EXT_FTP_SERVER_HELP", new Object[] {intf.getProvider().getSystemName(), intf.getProvider().listHelp()}));
        } catch (Exception e) {
            // ignored on purpose
        }
        this.intf.getConfiguration().serverChanged = false;
    }
    
    /**
     * Performs SOCKS related configurations (No JSSE).
     * @exception  SocksException  If some error occurs.
     */
    private void configureSocks() throws SocksException {
        
        if (this.intf.getConfiguration().getSocksHostName() == null ||
                this.intf.getConfiguration().getSocksHostName().equalsIgnoreCase("")) {
            this.msg = mMessages.getString("FTPBC-E006004.ERR_EXT_FTP_NO_SOCKS");
            if (mLogger.isLoggable(Level.SEVERE))
                mLogger.log(Level.SEVERE, this.msg);
            throw new SocksException(this.msg);
        }
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006007.DBG_EXT_FTP_SOCKS_INFO", new Object[] {intf.getConfiguration().getSocksHostName(), 
                    intf.getConfiguration().getSocksServerPort(), 
                    intf.getConfiguration().getSocksUserName(),
            intf.getConfiguration().getSocksVersion(),
            new Integer(Socks.VERSION_UNKNOWN)}));
        }
        
        // Create Socks instance
        Socks socks = new Socks(
                this.intf.getConfiguration().getSocksHostName(),
                this.intf.getConfiguration().getSocksServerPort(),
                this.intf.getConfiguration().getSocksUserName(),
                this.intf.getConfiguration().getSocksPassword(),
                this.intf.getConfiguration().getSocksVersion());
        
        // Create SocksSocketFactory instance
        SocksSocketFactory factory = null;
        factory = new SocksSocketFactory(socks);
        
        // Always enter passive local connection mode (we will
        // restore the setting according to configuration
        // in method close()) because:
        // 1. If class SocksServerSocket has not been implemented completedly,
        //    we will not use method factory.createServerSocket().
        //    In com.oroinc.ftp.FTPClient, factory.createServerSocket() is used
        //    only for active local connection mode (in method
        //    FTPClient.__openDataConnection()). We'll not need to create
        //    ServerSocket if we don't use active local connection mode.
        //    So we can enter passive mode to avoid the use of method
        //    factory.createServerSocket() if we have not implemented
        //    class SocksServerSocket completedly.
        // 2. Some SOCKS servers may not support BIND request, that means
        //    we have to use passive mode for those SOCKS servers.
        
        this.openUsePASV = true;
        this.intf.getProvider().setSocketFactory(factory);
    }
    
    /**
     * Does an FTP log-out and disconnects.
     */
    public void close() {
        logMethodCalled("close()");
                
        try {
            this.intf.getProvider().logout();
        } catch (Exception e) {
            // do nothing
        }
        
        try {
            if (this.intf.getProvider().isConnected()) {
                this.intf.getProvider().disConnect();
            }
        } catch (Exception e) {
            // do nothing
        }
        
        // In open() method, SOCKS, JSSE may have configured ftp
        // based on some non-configuration information, here just try
        // to restore the default behaviors based on configuration
        // information.
        if (this.intf.getConfiguration().isUsePASV()) {
            this.intf.getProvider().usePassive();
        } else {
            this.intf.getProvider().useActive();
        }
        this.intf.getProvider().setRemoteVerificationEnabled(true);
    }
    
    /**
     * Performs an FTP connection, log-in and switch modes, and so on.
     * @exception   FtpFileException  If some error occurs.
     */
    public void open() throws FtpFileException {
        open(FtpFileConfigConstants.DEFAULT_CNTRL_ENCODING);
    }
    
    /**
     * Performs an FTP connection, log-in and switch modes, and so on.
     * @param encoding The encoding for the server.
     * @exception   FtpFileException  If some error occurs.
     */
    public void open(String encoding) throws FtpFileException {
        logMethodCalled("open(String encoding)");
        
        String action = null;
        
        try {
            // Give default settings for some parameters that only are used in this method open().
            // They might be updated in deeper method configure????().
            this.openHostName = this.intf.getConfiguration().getHostName();
            this.openServerPort = this.intf.getConfiguration().getServerPort();
            this.openUsePASV = this.intf.getConfiguration().isUsePASV();
            this.openRemoteVerify = true;
            
            try {
                if (this.intf.getConfiguration().isSocksEnabled()) {
                    // SOCKS only
                    this.configureSocks();
                }
            } catch (SocksException e) {
                this.close();
                throw new FtpFileException(e.toString(), e);
            }
            
            try {
                if (mLogger.isLoggable(Level.FINE)) 
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006008.DBG_EXT_FTP_CALL_PROVIDER_CONNECT", new Object[] {this.openHostName, new Integer(this.openServerPort)}));
                
                this.intf.getProvider().connect(this.openHostName, this.openServerPort, encoding);
                if (!this.intf.getProvider().isPositiveCompletion(this.intf.getProvider().getReplyCode())) {
                    this.msg = mMessages.getString("FTPBC-E006005.ERR_EXT_FTP_CALL_PROVIDER_CONNECT_FAIL", new Object[] {intf.getProvider().getReplyString()});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    this.close();
                    throw new FtpFileException(this.msg);
                } else if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006009.DBG_EXT_FTP_CALL_PROVIDER_CONNECT_OK", new Object[] {intf.getProvider().getReplyString()}));
                }
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {e, "open()"});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                this.close();
                throw new FtpFileException(this.msg, e);
            }
            
            // once connection is established
            this.intf.getConfiguration().connectionChanged = false;
            
            // Set timeout for command and data connection sockets
            try {
                // Set timeout for command connection socket - if you want this method to
                // take effect, you have to call it after connect(); otherwise, the timeout
                // value would be overwritten by value set by connect().
                this.intf.getProvider().setSoTimeout(this.intf.getConfiguration().getCommandConnectionTimeout());
                
                // Set timeout for data connection socket - it will take effect when the data
                // connection is being established.
                this.intf.getProvider().setDataSocketTimeout(this.intf.getConfiguration().getDataConnectionTimeout());
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {e, "open()"});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                this.close();
                throw new FtpFileException(this.msg, e);
            }
            
            // login
            String pass = null;
            pass = (this.intf.getConfiguration().getPassword() == null ? "" : this.intf.getConfiguration().getPassword());
            
            try {
                if (mLogger.isLoggable(Level.FINE))
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006010.DBG_EXT_FTP_CALL_PROVIDER_LOGIN", new Object[] {intf.getConfiguration().getUserName()}));
                
                if (!this.intf.getProvider().login(this.intf.getConfiguration().getUserName(), pass)) {
                    this.msg = mMessages.getString("FTPBC-E006006.ERR_EXT_FTP_CALL_PROVIDER_LOGIN_FAIL", new Object[] {intf.getProvider().getReplyString()});
                    if (mLogger.isLoggable(Level.SEVERE))
                        mLogger.log(Level.SEVERE, this.msg);
                    this.close();
                    throw new FtpFileException(this.msg);
                } else if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006011.DBG_EXT_FTP_CALL_PROVIDER_LOGIN_OK", new Object[] {intf.getProvider().getReplyString()}));
                }
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {e, "open()"});
                if (mLogger.isLoggable(Level.SEVERE)) 
                    mLogger.log(Level.SEVERE, this.msg, e);
                
                this.close();
                throw new FtpFileException(this.msg, e);
            }
            
            if ( this.intf.getConfiguration().getSecureFTPType() != null
                    && this.intf.getConfiguration().getSecureFTPType().equalsIgnoreCase(FtpFileConfigConstants.FTP_SECURE_EXPLICITSSL) ) {
                if ( this.intf.getConfiguration().getEnableCCC() )
                    this.intf.getProvider().clearCommandChannel();
            }
            
            // Switch local connection mode - passive or active. No socket operation.
            try {
                action = this.openUsePASV ? "usePassive()" : "useActive()";
                if (mLogger.isLoggable(Level.FINE))
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006012.DBG_EXT_FTP_ACTION", new Object[] {"open()", action}));
                
                if (this.openUsePASV)
                    this.intf.getProvider().usePassive();
                else
                    this.intf.getProvider().useActive();
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {e, "open()"});
                if (mLogger.isLoggable(Level.SEVERE))
                    mLogger.log(Level.SEVERE, this.msg, e);
                
                this.close();
                throw new FtpFileException(this.msg, e);
            }
            
            // Switch file transfer type - ascii, ebcdic or binary mode
            try {
                action = this.intf.getConfiguration().getMode().equalsIgnoreCase("Ascii") ? "ascii()" :
                    this.intf.getConfiguration().getMode().equalsIgnoreCase("Ebcdic") ? "ebcdic()" : "binary()";
                
                if (mLogger.isLoggable(Level.FINE))
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006012.DBG_EXT_FTP_ACTION", new Object[] {action}));
                
                if (this.intf.getConfiguration().getMode().equalsIgnoreCase("Ascii")
                ? !this.intf.getProvider().ascii() :
                    this.intf.getConfiguration().getMode().equalsIgnoreCase("Ebcdic")
                    ? !this.intf.getProvider().ebcdic()
                    : !this.intf.getProvider().binary()) {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", new Object[] {"open()", action, intf.getProvider().getReplyString()});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    this.close();
                    throw new FtpFileException(this.msg);
                } else if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {"open()", action, intf.getProvider().getReplyString()}));
                }
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {e, "open()"});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                this.close();
                throw new FtpFileException(this.msg, e);
            }
            
            // Enable or disable the remote verification
            this.intf.getProvider().setRemoteVerificationEnabled(this.openRemoteVerify);
            
            // collect Ftp Server System information (it'll be helpful for debugging).
            if (this.intf.getConfiguration().serverChanged) {
                this.obtainFtpServerInfo();
            }
        } catch (Exception e) {
            throw new FtpFileException(e.getMessage(), e);
        }
    }
    
    /**
     * Resets data content and flags of this ftp client.
     * @exception   FtpFileException  If some error occurs.
     */
    public void reset() throws FtpFileException {
        logMethodCalled("reset()");

        this.tnc = null;
        this.payload = null;
        
        releaseInput(false);
        releaseOutput(false);
        
        insa = null;
        outsa = null;
        
        this.intf.getConfiguration().relationChanged = false;
        this.intf.getConfiguration().connectionChanged = false;
        this.intf.getConfiguration().serverChanged = false;
        this.intf.getConfiguration().clientChanged = false;
        this.intf.getConfiguration().providerChanged = false;
    }
    
    /**
     * Constructor.
     * 
     * 
     * @param intf  FtpInterface instance.
     * @exception FtpFileException  If some error occurs.
     */
    public FtpFileClientImpl(FtpInterface intf) throws FtpFileException {
        logMethodCalled("FtpFileClientImpl(FtpInterface intf)");
        this.initialize(intf);
    }
    
    /**
     * get payload from the ftp interface.
     * <p>The data payload is a blob (byte array) used to store the raw content of a file.
     * @return    The data payload.
     */
    public byte[] getPayload() {
        return payload;
    }
    
    /**
     * set payload.
     * <p>The data payload is a blob (byte array) used to store the raw content of a file.
     * @param       newPayload  The data payload.
     */
    public void setPayload(byte[] newPayload) {
        payload = newPayload;
    }
    
    /**
     * Initializes configuration properties from specified properties.
     * @param  props   The specified properties.
     * @exception   FtpFileException   If some error occurs.
     */
    public void initialConfigValues(Properties props) throws FtpFileException {
        logMethodCalled("initialConfigValues(Properties props)");
        this.intf.getConfiguration().initialConfigValues(props);
    }
    
    /**
     * check connection is still connected.
     *
     * @return  <code>true</code>  if the connection is still open and available;
     *          <code>false</code> if otherwise.
     */
    public boolean isOpen() {
        if (!this.intf.getProvider().isConnected()) {
            return false;
        } else {
            try {
                return this.intf.getProvider().isPositiveCompletion(this.intf.getProvider().sendCommand("NOOP"));
            } catch (Exception e) {
                return false;
            }
        }
    }
    
    /**
     * Restores all configuration properties from the related e*Way Connection's configuration file.
     * @exception   FtpFileException   If some error occurs.
     */
    public void restoreConfigValues() throws FtpFileException {
        logMethodCalled("restoreConfigValues()");
        this.intf.getConfiguration().reset();
    }
    
    /**
     *
     */
    public void connect() throws FtpFileException {
        connect(FtpFileConfigConstants.DEFAULT_CNTRL_ENCODING);
    }
    
    /**
     *
     */
    public void connect(String encoding) throws FtpFileException {
        logMethodCalled("connect(String encoding)");
        try {
            if (this.isOpen()) {
                this.close();
            }
            this.open(encoding);
        } catch (Exception e) {
            throw new FtpFileException(e.getMessage(), e);
        }
    }
    
    /**
     * Does FTP log-out and disconnects.
     */
    public void disconnect() {
        logMethodCalled("disconnect()");
        this.close();
    }
    
    /**
     * Verifies that the e*Way Connection to the external system is still available.
     *
     * @return  <code>true</code>  if the connection is still open and available;
     *          <code>false</code> if otherwise.
     */
    public boolean isConnected() {
        return this.isOpen();
    }
    
    /**
     * accept an input streams from caller.
     * @param     isa   An InputStreamAdapter object.
     */
    public void setInputStreamAdapter(InputStreamAdapter isa) {
        insa = isa;
        payload = null;
    }
    
    /**
     * accept an output stream from caller.
     * @param     osa   An OutputStreamAdapter object.
     */
    public void setOutputStreamAdapter(OutputStreamAdapter osa) {
        outsa = osa;
        payload = null;
    }
    
    private void releaseInput(boolean success) throws FtpFileException {
        String msg = null;
        if (null != is) {
            if (null != insa) {
                is = null;
                try {
                    insa.releaseInputStream(success);
                } catch (StreamingException sx) {
                    msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {sx, "releaseInput(boolean success)"});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg, sx);
                    }
                    throw new FtpFileException(msg, sx);
                } finally {
                    insa = null;
                }
            } else {
                ByteArrayInputStream bas = (ByteArrayInputStream) is;
                is = null;
                try {
                    bas.close();
                } catch (IOException ioex) {
                    msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {ioex, "releaseInput(boolean success)"});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg, ioex);
                    }
                    throw new FtpFileException(msg, ioex);
                }
            }
        }
    }
    
    private void releaseOutput(boolean success) throws FtpFileException {
        String msg = null;
        if (null != os) {
            if (null != outsa) {
                os = null;
                try {
                    outsa.releaseOutputStream(success);
                } catch (StreamingException sx) {
                    msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {sx, "releaseOutput(boolean success)"});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg, sx);
                    }
                    throw new FtpFileException(msg, sx);
                } finally {
                    outsa = null;
                }
            } else {
                ByteArrayOutputStream bas = (ByteArrayOutputStream) os;
                os = null;
                try {
                    workingPayload = null;
                    bas.close();
                    if (success) {
                        workingPayload = bas.toByteArray();
                    }
                } catch (IOException ioex) {
                    msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {ioex, "releaseOutput(boolean success)"});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg, ioex);
                    }
                    throw new FtpFileException(msg, ioex);
                }
            }
        }
    }
    
    
    /**
     * Checks to determine whether a given file exists on an FTP server.
     * @param         dirName  The directory name.
     * @param        fileName  The file name.
     * @return     true or false.
     * @excetpion  FtpFileException   If some error occurs.
     */
    public boolean isFileExist(String dirName, String fileName) throws FtpFileException {
        try {
            Object[] list = this.intf.getProvider().listFiles(
            		dirName, 
            		this.intf.getProvider().getHeuristics().escapeRegExpChars(fileName));
            if (list == null || list.length == 0) {
                return false;
            } else {
                return true;
            }
        } catch (Exception e) {
            msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {e, "isFileExist(String dirName, String fileName)"});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg, e);
            }
            throw new FtpFileException(this.msg, e);
        }
    }
    
    /**
     * Resolves names from patterns for the FTP get operation.
     * @return    An object of TransferNamesAndCommands.
     * @exception  FtpFileException    If some error occurs.
     */
    public TransferNamesAndCommands getResolvedNamesForGet() throws FtpFileException {
        this.validate(FtpFileClient.TC_GET);
        if (this.tnc == null ||
                !(this.tnc instanceof FtpFileTransferNamesAndCommandsGet)) {
            this.tnc = new FtpFileTransferNamesAndCommandsGet(this.intf);
        }
        return this.tnc;
    }
    
    /**
     * Resolves names from patterns for the FTP put operation.
     * @return    An object of TransferNamesAndCommands.
     * @exception  FtpFileException    If some error occurs.
     */
    public TransferNamesAndCommands getResolvedNamesForPut() throws FtpFileException {
        this.validate(FtpFileClient.TC_PUT);
        if (this.tnc == null ||
                !(this.tnc instanceof FtpFileTransferNamesAndCommandsPut)) {
            this.tnc = new FtpFileTransferNamesAndCommandsPut(this.intf);
        }
        return this.tnc;
    }
    
    /**
     * Performs Post Transfer Command for FTP get operation.
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void doPostTransferGet(TransferNamesAndCommands tncg) throws Exception {
        String workingDirectoryName = null;
        String workingFileName = null;
        
        if (this.intf.getConfiguration().isPreTransferCommandRename()) {
            workingDirectoryName = tncg.getPreDirectoryName();
            workingFileName = tncg.getPreFileName();
        } else {
            workingDirectoryName = tncg.getTargetDirectoryName();
            workingFileName = tncg.getTargetFileName();
        }
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006014.DBG_EXT_FTP_DUMP_DIRS_4_GET", new Object[] {
            		"doPostTransferGet()",
            		tncg.getPostTransferCommand(),
            		workingDirectoryName,
            		workingFileName,
            		tncg.getPostDirectoryName(),
            		tncg.getPostFileName()}));
        }
        
        // No qualified file is available to get. Nothing needs to do.
        if (workingDirectoryName.length() == 0 &&
                workingFileName.length() == 0) {
            return;
        }
        
        // 'None'
        if (tncg.getPostTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_NONE)) {
            return;
        }
        
        // 'Delete'
        if (tncg.getPostTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_DELETE)) {
            try {
                // In case we need it for buggy NT ftp server, just uncomment it
                /*
                if (tncg.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
                    this.safeDelete(workingDirectoryName, workingFileName);
                    return;
                }
                 */
                if (this.intf.getProvider().deleteFile(workingDirectoryName, workingFileName)) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                        		"doPostTransferGet()",
                        		"delete",
                        		intf.getProvider().getReplyString()
                        }));
                    }
                    return;
                } else {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"doPostTransferGet()",
                    		"delete",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                		new Object[] {
                		e,
                		"doPostTransferGet()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                throw new FtpFileException(this.msg, e);
            }
        }
        
        if (workingDirectoryName.equals(tncg.getPostDirectoryName()) &&
                workingFileName.equals(tncg.getPostFileName())) {
            if (mLogger.isLoggable(Level.WARNING))
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006001.WRN_EXT_FTP_OP_TO_SELF", new Object[] {
                		"doPostTransferGet()", 
                		tncg.getPostTransferCommand()}));
            return;
        }
        
        // 'Rename'
        // note: for ftp rename function, if the target file exists,
        //       different ftp servers behave differently.
        //       For UNIX ftp server, the target file just is overwritten without extra message.
        //       For NT ftp server, we'll fail and get exception.
        //       Now we don't do extra work for this, we don't want to define unified behavior,
        //       we just follow the native behavior of the corresponding ftp server.
        if (tncg.getPostTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME)) {
            try {
                if (this.intf.getProvider().archiveFile(
                        workingDirectoryName,
                        workingFileName,
                        tncg.getPostDirectoryName(),
                        tncg.getPostFileName())) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                        		"doPostTransferGet()",
                        		"rename",
                        		intf.getProvider().getReplyString()
                        }));
                    }
                    return;
                } else {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"doPostTransferGet()",
                    		"rename",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                		new Object[] {
                		e,
                		"doPostTransferGet()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                throw new FtpFileException(this.msg, e);
            }
        }
        
        // invalid command
        this.msg = mMessages.getString("FTPBC-E006008.ERR_EXT_FTP_INVALID_PRE_POST_CMD", 
        		new Object[] {
        		"doPostTransferGet()",
        		intf.getConfiguration().getPreTransferCommand()
        		});
        if (mLogger.isLoggable(Level.SEVERE)) {
            mLogger.log(Level.SEVERE, this.msg);
        }
        throw new FtpFileException(this.msg);
    }
    
    /**
     * Performs Pre Transfer Command for FTP get operation.
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void doPreTransferGet(TransferNamesAndCommands tncg) throws Exception {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006015.DBG_EXT_FTP_DUMP_DIRS_4_PRE_GET", new Object[] {
            		"doPreTransferGet()",
            		tncg.getPreTransferCommand(),
            		tncg.getTargetDirectoryName(),
            		tncg.getTargetFileName(),
            		tncg.getPreDirectoryName(),
            		tncg.getPreFileName()
            }));
        }
        
        // 'None'
        if (tncg.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_NONE)) {
            return;
        }
        
        // No qualified file is available to get. Nothing needs to do.
        if (tncg.getTargetDirectoryName().length() == 0 ||
                tncg.getTargetFileName().length() == 0) {
            return;
        }
        
        if (!this.isFileExist(tncg.getTargetDirectoryName(), tncg.getTargetFileName())) {
            return;
        }
        
        if (tncg.getTargetDirectoryName().equals(tncg.getPreDirectoryName()) &&
                tncg.getTargetFileName().equals(tncg.getPreFileName())) {
            if (mLogger.isLoggable(Level.WARNING))
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006001.WRN_EXT_FTP_OP_TO_SELF", new Object[] {
                		"doPreTransferGet()", 
                		tncg.getPreTransferCommand()}));
            return;
        }
        
        // 'Rename'
        // note: for ftp rename function, if the target file exists,
        //       different ftp servers behave differently.
        //       For UNIX ftp server, the target file just is overwritten without extra message.
        //       For NT ftp server, we'll fail and get exception.
        //       Now we don't do extra work for this, we don't want to define unified behavior,
        //       we just follow the native behavior of the corresponding ftp server.
        if (tncg.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME)) {
            try {
                if (this.intf.getProvider().archiveFile(
                        tncg.getTargetDirectoryName(),
                        tncg.getTargetFileName(),
                        tncg.getPreDirectoryName(),
                        tncg.getPreFileName())) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                        		"doPreTransferGet()",
                        		"rename",
                        		intf.getProvider().getReplyString()
                        }));
                    }
                    return;
                } else {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"doPreTransferGet()",
                    		"rename",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                		new Object[] {
                		e,
                		"doPreTransferGet()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                throw new FtpFileException(this.msg, e);
            }
        }
        
        // 'Copy'
        if (tncg.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
            // make sure the destination directory exists
            if (!this.intf.getProvider().mkdirs(tncg.getPreDirectoryName())) {
                this.msg = mMessages.getString("FTPBC-E006009.ERR_EXT_FTP_CREATE_DEST_FAIL", 
                		new Object[] {
                		"doPreTransferGet()",
                		tncg.getPreDirectoryName()
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg);
                }
                throw new FtpFileException(this.msg);
            }
            
            InputStream is = null;
            OutputStream os = null;
            ByteArrayOutputStream baos = null;
            try {
                is = this.intf.getProvider().retrieveFileStream(tncg.getTargetDirectoryName(), tncg.getTargetFileName());
                baos = new ByteArrayOutputStream();
                org.apache.commons.net.io.Util.copyStream(is, baos);
                is.close();
                if (!this.intf.getProvider().completePendingCommand()) {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"doPreTransferGet()",
                    		"retrieve",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
                os = this.intf.getProvider().storeFileStream(tncg.getPreDirectoryName(), tncg.getPreFileName());
                baos.writeTo(os);
                baos.flush();
                baos.close();
                os.flush();
                os.close();
                if (!this.intf.getProvider().completePendingCommand()) {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"doPreTransferGet()",
                    		"store",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
                return;
            } catch (Exception e) {
                try {
                    is.close();
                } catch (Exception ee) {
                    // do nothing
                }
                try {
                    os.close();
                } catch (Exception ee) {
                    // do nothing
                }
                try {
                    baos.close();
                } catch (Exception ee) {
                    // do nothing
                }
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                		new Object[] {
                		e,
                		"doPreTransferGet()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                throw new FtpFileException(this.msg, e);
            }
        }
        
        this.msg = mMessages.getString("FTPBC-E006008.ERR_EXT_FTP_INVALID_PRE_POST_CMD", 
        		new Object[] {
        		"doPreTransferGet()",
        		tncg.getPreTransferCommand()
        		});
        if (mLogger.isLoggable(Level.SEVERE)) {
            mLogger.log(Level.SEVERE, this.msg);
        }
        throw new FtpFileException(this.msg);
    }
    
    /**
     * Performs the real FTP get transfer.
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void doTransferGet(TransferNamesAndCommands tncg) throws Exception {
        String workingDirectoryName = null;
        String workingFileName = null;
        
        if (this.intf.getConfiguration().isPreTransferCommandRename()) {
            workingDirectoryName = tncg.getPreDirectoryName();
            workingFileName = tncg.getPreFileName();
        } else {
            workingDirectoryName = tncg.getTargetDirectoryName();
            workingFileName = tncg.getTargetFileName();
        }
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006016.DBG_EXT_FTP_DUMP_WORKING_DIRS", new Object[] {
            		"doTransferGet()",
            		workingDirectoryName,
            		workingFileName
            		           }));
        }
        
        // No qualified file is available to get. Nothing needs to do.
        //if (workingDirectoryName.length() == 0 || workingFileName.length() == 0)
        if (workingDirectoryName.length() == 0 && workingFileName.length() == 0)
            return;
        
        if (!this.isFileExist(workingDirectoryName, workingFileName)) {
            // Possible reasons:
            // (1). after resolve names, the file is deleted (race condition).
            // (2). call get() repeatedly without reset() in collaboration.
            // (3). ftp server's OS io blocking/caching/multi-tasking issues (???)
            //      which may cause ftp LIST to behave abnormally: even there are
            //      files under current directory, LIST gives a normal return code
            //      but doesn't return the list of files.
            //      Thus, sometimes isFileExist() may return false even the file
            //      does exist. We need to consider this kind of case.
            //
            // Solutions:
            // (a). throw exception: covers (1)/(2)/(3)
            // (b). warn and return: covers (1)/(2)
            // (c). warn and continue: covers (1)/(2)/(3)
            //      Defer the exception (if any) to retrieveFile() action later.
            //      Seems better user's experience than (b).
            //
            if ( !noWarning ) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006002.WRN_EXT_FTP_WORKING_FILE_NOT_EXIST", new Object[] {
                    		"doTransferGet()",
                    		workingDirectoryName,
                    		workingFileName
                    }));
                }
            }
        }
        
        // get file from ftp server to payload
        try {
            if (null == os) {
                if (null != outsa) {
                    try {
                        os = outsa.requestOutputStream();
                    } catch (StreamingException sx) {
                        this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                        		new Object[] {
                        		sx,
                        		"doTransferGet()"
                        		});
                        if (mLogger.isLoggable(Level.SEVERE)) {
                            mLogger.log(Level.SEVERE, msg, sx);
                        }
                        throw new Exception(msg, sx);
                    }
                } else {
                    os = new ByteArrayOutputStream();
                }
            } else {
                // This should never happen!
                this.msg = mMessages.getString("FTPBC-E006010.ERR_EXT_FTP_OUTPUT_STREAM_EXISTS", 
                		new Object[] {
                		"doTransferGet()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new Exception(msg);
            }
            
            if (this.intf.getProvider().retrieveFile(workingDirectoryName, workingFileName, os)) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                    		"doTransferGet()",
                    		"retrieve",
                    		intf.getProvider().getReplyString()
                    		}));
                }
                // This should take care of the clean up
                // and set the workingPayload.
                releaseOutput(true);
                // we have to delay this call until the payload should be visible to collab.
                //this.setPayload(this.workingPayload);
            } else {
                this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                		new Object[] {
                		"doTransferGet()",
                		"retrieve",
                		intf.getProvider().getReplyString()
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg);
                }
                throw new FtpFileException(this.msg);
            }
        } catch (Exception e) {
            try {
                // This should take care of the clean up
                // including the workingPayload.
                releaseOutput(false);
            } catch (Exception ie) {
                // do nothing
            }
            this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
            		new Object[] {
            		e,
            		"doTransferGet()"
            		});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg, e);
            }
            throw new FtpFileException(this.msg, e);
        }
        
    }
    
    /**
     * Performs Post Transfer Command for FTP put operation.
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void doPostTransferPut(TransferNamesAndCommands tncp) throws Exception {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006017.DBG_EXT_FTP_DUMP_DIRS_4_POST_PUT", new Object[] {
            	"doPostTransferPut",
            	tncp.getPostTransferCommand(),
            	tncp.getTargetDirectoryName(),
            	tncp.getTargetFileName(),
            	tncp.getPostDirectoryName(),
            	tncp.getPostFileName()
            }));
        }
        
        // 'None'
        if (tncp.getPostTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_NONE)) {
            return;
        }
        
        if (tncp.getTargetDirectoryName().equals(tncp.getPostDirectoryName()) &&
                tncp.getTargetFileName().equals(tncp.getPostFileName())) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006001.WRN_EXT_FTP_OP_TO_SELF", new Object[] {
                		"doPostTransferPut()", 
                		tncp.getPostTransferCommand()}));
            }
            return;
        }
        
        // 'Rename'
        // note: for ftp rename function, if the target file exists,
        //       different ftp servers behave differently.
        //       For UNIX ftp server, the target file just is overwritten without extra message.
        //       For NT ftp server, we'll fail and get exception.
        //       Now we don't do extra work for this, we don't want to define unified behavior,
        //       we just follow the native behavior of the corresponding ftp server.
        if (tncp.getPostTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME)) {
            try {
                if (this.intf.getProvider().archiveFile(
                        tncp.getTargetDirectoryName(),
                        tncp.getTargetFileName(),
                        tncp.getPostDirectoryName(),
                        tncp.getPostFileName())) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                        		"doPostTransferPut()",
                        		"rename",
                        		intf.getProvider().getReplyString()
                        		}));
                    }
                    return;
                } else {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"doPostTransferPut()",
                    		"rename",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                		new Object[] {
                		e,
                		"doPostTransferPut()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                throw new FtpFileException(this.msg, e);
            }
        }
        
        // invalid command
        this.msg = mMessages.getString("FTPBC-E006008.ERR_EXT_FTP_INVALID_PRE_POST_CMD", 
        		new Object[] {
        		"doPostTransferPut()",
        		intf.getConfiguration().getPreTransferCommand()
        		});
        if (mLogger.isLoggable(Level.SEVERE)) {
            mLogger.log(Level.SEVERE, this.msg);
        }
        throw new FtpFileException(this.msg);
        
    }
    
    /**
     * Performs Pre Transfer Command for FTP put operation.
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void doPreTransferPut(TransferNamesAndCommands tncp) throws Exception {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006017.DBG_EXT_FTP_DUMP_DIRS_4_POST_PUT", new Object[] {
            	"doPreTransferPut",
            	tncp.getPreTransferCommand(),
            	tncp.getTargetDirectoryName(),
            	tncp.getTargetFileName(),
            	tncp.getPreDirectoryName(),
            	tncp.getPreFileName()
            }));
        }
        
        // 'None'
        if (tncp.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_NONE)) {
            return;
        }
        
        // No existing file is available to delete/rename/copy - nothing needs to backup/cleanup.
        if (!this.isFileExist(tncp.getTargetDirectoryName(), tncp.getTargetFileName())) {
            // need to do nothing even option "Delete" for this situation
            return;
        }
        
        if (tncp.getTargetDirectoryName().equals(tncp.getPreDirectoryName()) &&
                tncp.getTargetFileName().equals(tncp.getPreFileName())) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006001.WRN_EXT_FTP_OP_TO_SELF", new Object[] {
                		"doPreTransferPut()", 
                		tncp.getPreTransferCommand()}));
            }
            return;
        }
        
        // 'Rename'
        // note: for ftp rename function, if the target file exists,
        //       different ftp servers behave differently.
        //       For UNIX ftp server, the target file just is overwritten without extra message.
        //       For NT ftp server, we'll fail and get exception.
        //       Now we don't do extra work for this, we don't want to define unified behavior,
        //       we just follow the native behavior of the corresponding ftp server.
        if (tncp.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME)) {
            try {
                if (this.intf.getProvider().archiveFile(
                        tncp.getTargetDirectoryName(),
                        tncp.getTargetFileName(),
                        tncp.getPreDirectoryName(),
                        tncp.getPreFileName())) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", new Object[] {
                        		"doPreTransferPut()",
                        		"rename",
                        		intf.getProvider().getReplyString()
                        		}));
                    }
                    return;
                } else {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"doPreTransferPut()",
                    		"rename",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                		new Object[] {
                		e,
                		"doPreTransferPut()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                throw new FtpFileException(this.msg, e);
            }
        }
        
        // 'Copy'
        if (tncp.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
            // make sure the destination directory exists
            if (!this.intf.getProvider().mkdirs(tncp.getPreDirectoryName())) {
                this.msg = mMessages.getString("FTPBC-E006009.ERR_EXT_FTP_CREATE_DEST_FAIL", 
                		new Object[] {
                		"doPreTransferPut()",
                		tncp.getPreDirectoryName()
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg);
                }
                throw new FtpFileException(this.msg);
            }
            
            InputStream is = null;
            OutputStream os = null;
            ByteArrayOutputStream baos = null;
            try {
                is = this.intf.getProvider().retrieveFileStream(tncp.getTargetDirectoryName(), tncp.getTargetFileName());
                baos = new ByteArrayOutputStream();
                org.apache.commons.net.io.Util.copyStream(is, baos);
                is.close();
                if (!this.intf.getProvider().completePendingCommand()) {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"doPreTransferPut()",
                    		"retrieve",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
                os = this.intf.getProvider().storeFileStream(tncp.getPreDirectoryName(), tncp.getPreFileName());
                baos.writeTo(os);
                baos.flush();
                baos.close();
                os.flush();
                os.close();
                if (!this.intf.getProvider().completePendingCommand()) {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"doPreTransferPut()",
                    		"store",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
                return;
            } catch (Exception e) {
                try {
                    is.close();
                } catch (Exception ee) {
                    // do nothing
                }
                try {
                    os.close();
                } catch (Exception ee) {
                    // do nothing
                }
                try {
                    baos.close();
                } catch (Exception ee) {
                    // do nothing
                }
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                		new Object[] {
                		e,
                		"doPreTransferPut()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                throw new FtpFileException(this.msg, e);
            }
        }
        
        this.msg = mMessages.getString("FTPBC-E006008.ERR_EXT_FTP_INVALID_PRE_POST_CMD", 
        		new Object[] {
        		"doPreTransferPut()",
        		tncp.getPreTransferCommand()
        		});
        if (mLogger.isLoggable(Level.SEVERE)) {
            mLogger.log(Level.SEVERE, this.msg);
        }
        throw new FtpFileException(this.msg);
    }
    
    /**
     * Performs the real FTP put transfer.
     * @param    tnc   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void doTransferPut(TransferNamesAndCommands tnc) throws Exception {
        FtpFileTransferNamesAndCommandsPut tncp = (FtpFileTransferNamesAndCommandsPut) tnc;
        
        tncp.setCleanupOnPutFailure(false);
        
        String workingDirectoryName = null;
        String workingFileName = null;
        
        if (this.intf.getConfiguration().isStageEnabled()) {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006018.DBG_EXT_FTP_DUMP_STAGE_DIRS", new Object[] {
                		"doTransferPut()",
                		tncp.getStageDirectoryName(),
                		tncp.getStageFileName()
                }));
            }
            workingDirectoryName = tncp.getStageDirectoryName();
            workingFileName = tncp.getStageFileName();
        } else {
            workingDirectoryName = tncp.getTargetDirectoryName();
            workingFileName = tncp.getTargetFileName();
        }
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006019.DBG_EXT_FTP_DUMP_TGT_DIRS", new Object[] {
            		"doTransferPut()",
            		tncp.getTargetDirectoryName(),
            		tncp.getTargetFileName()
            }));
        }
        
        // VOSK doesn't support command MKD ==> the dir tree should exist already
        if (!intf.getConfiguration().getDirectoryListingStyle().equalsIgnoreCase("VOSK (Hitachi)")) { // see QAI53881
            // make sure the destination directory exists
            if (!this.intf.getProvider().mkdirs(workingDirectoryName)) {
                this.msg = mMessages.getString("FTPBC-E006011.ERR_EXT_FTP_CREATE_WORKING_DIR_FAIL", 
                		new Object[] {
                		"doTransferPut()",
                		workingDirectoryName
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg);
                }
                throw new FtpFileException(this.msg);
            }
        }
        
        try {
            String action = tncp.getAppend() ? "appendFile" : "storeFile";
            if (null == is) {
                if (null != insa) {
                    try {
                        is = insa.requestInputStream();
                    } catch (StreamingException sx) {
                        this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                        		new Object[] {
                        		sx,
                        		"doTransferPut()"
                        		});
                        if (mLogger.isLoggable(Level.SEVERE)) {
                            mLogger.log(Level.SEVERE, msg, sx);
                        }
                        throw new FtpFileException(msg, sx);
                    }
                } else {
                    is = new ByteArrayInputStream(payload);
                }
            } else {
                // This should never happen!
                this.msg = mMessages.getString("FTPBC-E006012.ERR_EXT_FTP_INPUT_STREAM_EXISTS", 
                		new Object[] {
                		"doTransferPut()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new FtpFileException(msg);
            }
            
            // Note: Even in this put transfer, you still may get exception containing
            //       java.io.FileNotFoundException as a nested exception because the
            //       streaming interface may throw that exception from BatchLocal.
            
            tncp.setCleanupOnPutFailure(true);
            if (tncp.getAppend()
            ? this.intf.getProvider().appendFile(workingDirectoryName, workingFileName, is)
            : this.intf.getProvider().storeFile(workingDirectoryName, workingFileName, is)) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                    		"doTransferPut()",
                    		action,
                    		intf.getProvider().getReplyString()
                    		}));
                }
                releaseInput(true);
            } else {
                this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                		new Object[] {
                		"doTransferPut()",
                		action,
                		intf.getProvider().getReplyString()
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg);
                }
                throw new FtpFileException(this.msg);
            }
        } catch (Exception e) {
            try {
                releaseInput(false);
            } catch (Exception ee) {
                // do nothing
            }
            this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
            		new Object[] {
            		e,
            		"doTransferPut()"
            		});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg, e);
            }
            throw new FtpFileException(this.msg, e);
        }
        
        if (this.intf.getConfiguration().isStageEnabled()) {
            try {
                if (this.intf.getProvider().archiveFile(
                        workingDirectoryName,
                        workingFileName,
                        tncp.getTargetDirectoryName(),
                        tncp.getTargetFileName())) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                        		"doTransferPut()",
                        		"rename",
                        		intf.getProvider().getReplyString()
                        		}));
                    }
                    return;
                } else {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"doTransferPut()",
                    		"rename",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                		new Object[] {
                		e,
                		"doTransferPut()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                throw new FtpFileException(this.msg, e);
            }
        }
    }
    
    /**
     * Stores a remote FTP file.
     * @exception   FtpFileException  If some error occurs.
     */
    public void put() throws FtpFileException {
    	logMethodCalled("put()");
        try {
            if ( this.isConnected() ) {
                if (this.intf.getConfiguration().connectionChanged) {
                    //? What is the desired behavior?
                    // 1. re-connect here ?
                    // 2. reject ?
                    // 3. ignore/discard ?
                    // 4. accept the changes until next connect time ?
                    //
                    if (mLogger.isLoggable(Level.WARNING)) {
                        mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006003.WRN_EXT_FTP_PARAM_CHANGED"));
                    }
                }
            } else {
                String key = intf.getConfiguration().isConnectionEstablishmentModeManual() ? "FTPBC-E006013.ERR_EXT_FTP_NOT_CONNECTED" : "FTPBC-E006014.ERR_EXT_FTP_NOT_CONNECTED_MANUAL_MODE";
                this.msg = mMessages.getString(key, new Object[] {intf.getConfiguration().getConnectionEstablishmentMode()});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg);
                }
                throw new FtpFileException(this.msg);
            }
            
            // perform non-transactional transfer
            // pre raw commands
            this.doRawCommands(this.intf.getConfiguration().getPreTransferRawCommands());
            
            // pre transfer
            try {
                this.doPreTransferPut(this.getResolvedNamesForPut());
            } catch (Exception e) {
                try {
                    if (mLogger.isLoggable(Level.INFO))
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006020.DBG_EXT_FTP_PROCESS_STARTED", new Object[] {"put()", "cleanup"}));
                    this.cleanupPreTransferPut(this.getResolvedNamesForPut());
                    if (mLogger.isLoggable(Level.INFO))
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006021.DBG_EXT_FTP_PROCESS_SUCCEEDED", new Object[] {"put()", "cleanup"}));
                } catch (Exception ee) {
                    throw new FtpFileException(ee.toString(), ee);
                }
                throw new FtpFileException(e.toString(), e);
            }
            
            // transfer
            try {
                this.doTransferPut(this.getResolvedNamesForPut());
            } catch (Exception e) {
                try {
                    if (mLogger.isLoggable(Level.INFO))
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006020.DBG_EXT_FTP_PROCESS_STARTED", new Object[] {"put()", "cleanup/undo"}));
                    this.cleanupTransferPut(this.getResolvedNamesForPut());
                    this.undoPreTransferPut(this.getResolvedNamesForPut());
                    if (mLogger.isLoggable(Level.INFO))
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006021.DBG_EXT_FTP_PROCESS_SUCCEEDED", new Object[] {"put()", "cleanup/undo"}));
                } catch (Exception ee) {
                    throw new FtpFileException(ee.toString(), ee);
                }
                throw new FtpFileException(e.toString(), e);
            }
            
            // post transfer
            try {
                this.doPostTransferPut(this.getResolvedNamesForPut());
            } catch (Exception e) {
                try {
                    if (mLogger.isLoggable(Level.INFO))
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006020.DBG_EXT_FTP_PROCESS_STARTED", new Object[] {"put()", "cleanup/undo/undo"}));
                    this.cleanupPostTransferPut(this.getResolvedNamesForPut());
                    this.undoTransferPut(this.getResolvedNamesForPut());
                    this.undoPreTransferPut(this.getResolvedNamesForPut());
                    if (mLogger.isLoggable(Level.INFO))
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006021.DBG_EXT_FTP_PROCESS_SUCCEEDED", new Object[] {"put()", "cleanup/undo/undo"}));
                } catch (Exception ee) {
                    throw new FtpFileException(ee.toString(), ee);
                }
                throw new FtpFileException(e.toString(), e);
            }
            
            // post raw commands
            try {
                this.doRawCommands(this.intf.getConfiguration().getPostTransferRawCommands());
                this.doUpdateState();
            } catch (Exception e) {
                try {
                    if (mLogger.isLoggable(Level.INFO))
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006020.DBG_EXT_FTP_PROCESS_STARTED", new Object[] {"put()", "cleanup/undo/undo/undo"}));
                    this.cleanupRawCommands(this.getResolvedNamesForPut());
                    this.undoPostTransferPut(this.getResolvedNamesForPut());
                    this.undoTransferPut(this.getResolvedNamesForPut());
                    this.undoPreTransferPut(this.getResolvedNamesForPut());
                    if (mLogger.isLoggable(Level.INFO))
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006021.DBG_EXT_FTP_PROCESS_SUCCEEDED", new Object[] {"put()", "cleanup/undo/undo/undo"}));
                } catch (Exception ee) {
                    throw new FtpFileException(ee.toString(), ee);
                }
                throw new FtpFileException(e.toString(), e);
            }
        } catch (Exception e) {
            throw new FtpFileException(e.getMessage(), e);
        }
    }
    
    /**
     * Undoes Pre Transfer for FTP get operation.
     * @param    tncg  An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void undoPreTransferGet(TransferNamesAndCommands tncg) throws Exception {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006015.DBG_EXT_FTP_DUMP_DIRS_4_PRE_GET", new Object[] {
            	"undoPreTransferGet",
            	tncg.getPreTransferCommand(),
            	tncg.getTargetDirectoryName(),
            	tncg.getTargetFileName(),
            	tncg.getPreDirectoryName(),
            	tncg.getPreFileName()
            }));
        }
        
        // 'None'
        if (tncg.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_NONE)) {
            return;
        }
        
        // 'Rename' or 'Copy'
        if (tncg.getPreDirectoryName().length() == 0 ||
                tncg.getPreFileName().length() == 0) {
            return;
        }
        
        if (!this.isFileExist(tncg.getPreDirectoryName(), tncg.getPreFileName())) {
            return;
        }
        
        // 'Rename'
        // note: for ftp rename function, if the target file exists,
        //       different ftp servers behave differently.
        //       For UNIX ftp server, the target file just is overwritten without extra message.
        //       For NT ftp server, we'll fail and get exception.
        //       Now we don't do extra work for this, we don't want to define unified behavior,
        //       we just follow the native behavior of the corresponding ftp server.
        if (tncg.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME)) {
            //? if pre rename/copy overwrote a file, the overwritten file will not be recovered.
            //? warning msg
            
            try {
                if (this.intf.getProvider().rename(
                        tncg.getPreDirectoryName(),
                        tncg.getPreFileName(),
                        tncg.getTargetDirectoryName(),
                        tncg.getTargetFileName())) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                        		"undoPreTransferGet()",
                        		"rename",
                        		intf.getProvider().getReplyString()
                        		}));
                    }
                    return;
                } else {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"undoPreTransferGet()",
                    		"rename",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                		new Object[] {
                		e,
                		"undoPreTransferGet()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                throw new FtpFileException(this.msg, e);
            }
        }
        
        // 'Copy'
        if (tncg.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
            try {
                if (this.intf.getProvider().deleteFile(tncg.getPreDirectoryName(), tncg.getPreFileName())) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                        		"undoPreTransferGet()",
                        		"delete",
                        		intf.getProvider().getReplyString()
                        		}));
                    }
                    return;
                } else {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"undoPreTransferGet()",
                    		"delete",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                		new Object[] {
                		e,
                		"undoPreTransferGet()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                throw new FtpFileException(this.msg, e);
            }
        }
        
        this.msg = mMessages.getString("FTPBC-E006008.ERR_EXT_FTP_INVALID_PRE_POST_CMD", 
        		new Object[] {
        		"undoPreTransferGet()",
        		tncg.getPreTransferCommand()
        		});
        if (mLogger.isLoggable(Level.SEVERE)) {
            mLogger.log(Level.SEVERE, this.msg);
        }
        throw new FtpFileException(this.msg);
    }
    
    /**
     * Undoes ftp get transfer.
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void undoTransferGet(TransferNamesAndCommands tncg) throws Exception {
        logMethodCalled("undoTransferGet()");
        this.workingPayload = null;
        this.payload = null; // just in case.
    }
    
    /**
     * Undoes Pre Transfer for FTP put operation.
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void undoPreTransferPut(TransferNamesAndCommands tncp) throws Exception {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006022.DBG_EXT_FTP_DUMP_DIRS_4_PRE_PUT", new Object[] {
            	"undoPreTransferPut",
            	tncp.getPreTransferCommand(),
            	tncp.getTargetDirectoryName(),
            	tncp.getTargetFileName(),
            	tncp.getPreDirectoryName(),
            	tncp.getPreFileName()
            }));
        }
        
        // 'None'
        if (tncp.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_NONE)) {
            return;
        }
        
        // 'Rename' or 'Copy'
        if (tncp.getPreDirectoryName().length() == 0 ||
                tncp.getPreFileName().length() == 0) {
            return;
        }
        
        if (!this.isFileExist(tncp.getPreDirectoryName(), tncp.getPreFileName())) {
            return;
        }
        
        // note: for ftp rename function, if the target file exists,
        //       different ftp servers behave differently.
        //       For UNIX ftp server, the target file just is overwritten without extra message.
        //       For NT ftp server, we'll fail and get exception.
        //       Now we don't do extra work for this, we don't want to define unified behavior,
        //       we just follow the native behavior of the corresponding ftp server.
        if (tncp.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME) ||
                tncp.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
            //? we will not try to undo the creation of the destination directory tree - too expensive!
            //? warning msg
            
            //? if pre rename/copy overwrote a file, the overwritten file will not be recovered.
            //? warning msg
            
            try {
                if (this.intf.getProvider().rename(
                        tncp.getPreDirectoryName(),
                        tncp.getPreFileName(),
                        tncp.getTargetDirectoryName(),
                        tncp.getTargetFileName())) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                        		"undoPreTransferPut()",
                        		"rename",
                        		intf.getProvider().getReplyString()
                        		}));
                    }
                    return;
                } else {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"undoPreTransferPut()",
                    		"rename",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                		new Object[] {
                		e,
                		"undoPreTransferPut()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                throw new FtpFileException(this.msg, e);
            }
        }
        
        this.msg = mMessages.getString("FTPBC-E006008.ERR_EXT_FTP_INVALID_PRE_POST_CMD", 
        		new Object[] {
        		"undoPreTransferPut()",
        		tncp.getPreTransferCommand()
        		});
        if (mLogger.isLoggable(Level.SEVERE)) {
            mLogger.log(Level.SEVERE, this.msg);
        }
        throw new FtpFileException(this.msg);
    }
    
    /**
     * Undoes ftp put transfer.
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void undoTransferPut(TransferNamesAndCommands tncp) throws Exception {
        //? we will not try to undo the creation of the destination directory tree - too expensive!
        //? warning msg
        
        //? if after pre command, ftp 'put' overwrote a file, the overwritten file will not be recovered.
        //? warning msg
        
        if (tncp.getTargetDirectoryName().length() == 0 ||
                tncp.getTargetFileName().length() == 0) {
            return;
        }
        
        if (!this.isFileExist(tncp.getTargetDirectoryName(), tncp.getTargetFileName())) {
            return;
        }
        
        try {
            // In case we need it for buggy NT ftp server, just uncomment it
            /*
            if (tncp.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
                this.safeDelete(tncp.getTargetDirectoryName(), tncp.getTargetFileName());
                return;
            }
             */
            if (this.intf.getProvider().deleteFile(tncp.getTargetDirectoryName(), tncp.getTargetFileName())) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                    		"undoTransferPut()",
                    		"delete",
                    		intf.getProvider().getReplyString()
                    		}));
                }
                return;
            } else {
                this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                		new Object[] {
                		"undoTransferPut()",
                		"delete",
                		intf.getProvider().getReplyString()
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg);
                }
                throw new FtpFileException(this.msg);
            }
        } catch (Exception e) {
            this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
            		new Object[] {
            		e,
            		"undoTransferPut()"
            		});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg, e);
            }
            throw new FtpFileException(this.msg, e);
        }
        
    }
    
    /**
     * Undoes Post Transfer for FTP get operation.
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void undoPostTransferGet(TransferNamesAndCommands tncg) throws Exception {
        String workingDirectoryName = null;
        String workingFileName = null;
        
        if (this.intf.getConfiguration().isPreTransferCommandRename()) {
            workingDirectoryName = tncg.getPreDirectoryName();
            workingFileName = tncg.getPreFileName();
        } else {
            workingDirectoryName = tncg.getTargetDirectoryName();
            workingFileName = tncg.getTargetFileName();
        }
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006014.DBG_EXT_FTP_DUMP_DIRS_4_GET", new Object[] {
            		"undoPostTransferGet()",
            		tncg.getPostTransferCommand(),
            		workingDirectoryName,
            		workingFileName,
            		tncg.getPostDirectoryName(),
            		tncg.getPostFileName()}));
        }
        
        // 'None'
        if (tncg.getPostTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_NONE)) {
            return;
        }
        
        // 'Delete'
        if (tncg.getPostTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_DELETE)) {
            // undo only for pre 'Copy'
            if (!tncg.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006004.WRN_EXT_FTP_CAN_NOT_RECOVER_DELETED_FILE", new Object[] {"undoPostTransferGet"}));
                }
                return;
            }
            try {
                if (this.intf.getProvider().rename(
                        tncg.getPreDirectoryName(),
                        tncg.getPreFileName(),
                        workingDirectoryName,
                        workingFileName)) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                        		"undoPostTransferGet()",
                        		"rename",
                        		intf.getProvider().getReplyString()
                        		}));
                    }
                    return;
                } else {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"undoPostTransferGet()",
                    		"rename",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                		new Object[] {
                		e,
                		"undoPostTransferGet()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                throw new FtpFileException(this.msg, e);
            }
        }
        
        // 'Rename'
        if (tncg.getPostDirectoryName().length() == 0 ||
                tncg.getPostFileName().length() == 0) {
            return;
        }
        
        if (!this.isFileExist(tncg.getPostDirectoryName(), tncg.getPostFileName())) {
            return;
        }
        
        if (tncg.getPostTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME)) {
            try {
                if (this.intf.getProvider().rename(
                        tncg.getPostDirectoryName(),
                        tncg.getPostFileName(),
                        workingDirectoryName,
                        workingFileName)) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                        		"undoPostTransferGet()",
                        		"rename",
                        		intf.getProvider().getReplyString()
                        		}));
                    }
                    return;
                } else {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"undoPostTransferGet()",
                    		"rename",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                		new Object[] {
                		e,
                		"undoPostTransferGet()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                throw new FtpFileException(this.msg, e);
            }
        }
        
        this.msg = mMessages.getString("FTPBC-E006008.ERR_EXT_FTP_INVALID_PRE_POST_CMD", 
        		new Object[] {
        		"undoPostTransferGet()",
        		tncg.getPostTransferCommand()
        		});
        if (mLogger.isLoggable(Level.SEVERE)) {
            mLogger.log(Level.SEVERE, this.msg);
        }
        throw new FtpFileException(this.msg);
    }
    
    /**
     * Undoes Post Transfer for FTP put operation.
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void undoPostTransferPut(TransferNamesAndCommands tncp) throws Exception {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006017.DBG_EXT_FTP_DUMP_DIRS_4_POST_PUT", new Object[] {
            	"undoPostTransferPut",
            	tncp.getPostTransferCommand(),
            	tncp.getTargetDirectoryName(),
            	tncp.getTargetFileName(),
            	tncp.getPostDirectoryName(),
            	tncp.getPostFileName()
            }));
        }
        
        // 'None'
        if (tncp.getPostTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_NONE)) {
            return;
        }
        
        // 'Rename'
        if (tncp.getPostDirectoryName().length() == 0 ||
                tncp.getPostFileName().length() == 0) {
            return;
        }
        
        if (!this.isFileExist(tncp.getPostDirectoryName(), tncp.getPostFileName())) {
            return;
        }
        
        if (tncp.getPostTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME)) {
            try {
                if (this.intf.getProvider().rename(
                        tncp.getPostDirectoryName(),
                        tncp.getPostFileName(),
                        tncp.getTargetDirectoryName(),
                        tncp.getTargetFileName())) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                        		"undoPostTransferPut()",
                        		"rename",
                        		intf.getProvider().getReplyString()
                        		}));
                    }
                    return;
                } else {
                    this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                    		new Object[] {
                    		"undoPostTransferPut()",
                    		"rename",
                    		intf.getProvider().getReplyString()
                    		});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, this.msg);
                    }
                    throw new FtpFileException(this.msg);
                }
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                		new Object[] {
                		e,
                		"undoPostTransferPut()"
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                throw new FtpFileException(this.msg, e);
            }
        }
        
        this.msg = mMessages.getString("FTPBC-E006008.ERR_EXT_FTP_INVALID_PRE_POST_CMD", 
        		new Object[] {
        		"undoPostTransferPut()",
        		tncp.getPostTransferCommand()
        		});
        if (mLogger.isLoggable(Level.SEVERE)) {
            mLogger.log(Level.SEVERE, this.msg);
        }
        throw new FtpFileException(this.msg);   }
    
    /**
     * Cleans up any failures during Post Transfer Command for "get".
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void cleanupPostTransferGet(TransferNamesAndCommands tncg) throws Exception {
        // need to do nothing now.
    }
    
    /**
     * Cleans up any failures during Post Transfer Command for "put".
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void cleanupPostTransferPut(TransferNamesAndCommands tncp) throws Exception {
        // need to do nothing now.
    }
    
    /**
     * Cleans up any failures during Pre Transfer Command for "get".
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void cleanupPreTransferGet(TransferNamesAndCommands tncg) throws Exception {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006015.DBG_EXT_FTP_DUMP_DIRS_4_PRE_GET", new Object[] {
            		"cleanupPreTransferGet()",
            		tncg.getPreTransferCommand(),
            		tncg.getTargetDirectoryName(),
            		tncg.getTargetFileName(),
            		tncg.getPreDirectoryName(),
            		tncg.getPreFileName()
            }));
        }
        
        // only for 'Copy'
        if (!tncg.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
            return;
        }
        
        if (tncg.getPreDirectoryName().length() == 0 ||
                tncg.getPreFileName().length() == 0) {
            return;
        }
        
        if (!this.isFileExist(tncg.getPreDirectoryName(), tncg.getPreFileName())) {
            return;
        }
        
        try {
            if (this.intf.getProvider().deleteFile(tncg.getPreDirectoryName(), tncg.getPreFileName())) {
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                    		"cleanupPreTransferGet()",
                    		"delete",
                    		intf.getProvider().getReplyString()
                    		}));
                }
            } else {
                this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                		new Object[] {
                		"cleanupPreTransferGet()",
                		"delete",
                		intf.getProvider().getReplyString()
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg);
                }
                throw new FtpFileException(this.msg);
            }
        } catch (Exception e) {
            this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
            		new Object[] {
            		e,
            		"cleanupPreTransferGet()"
            		});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg, e);
            }
            throw new FtpFileException(this.msg, e);
        }
    }
    
    /**
     * Cleans up any failures during Pre Transfer Command for "put".
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void cleanupPreTransferPut(TransferNamesAndCommands tncp) throws Exception {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006022.DBG_EXT_FTP_DUMP_DIRS_4_PRE_PUT", new Object[] {
            		"cleanupPreTransferPut()",
            		tncp.getPreTransferCommand(),
            		tncp.getTargetDirectoryName(),
            		tncp.getTargetFileName(),
            		tncp.getPreDirectoryName(),
            		tncp.getPreFileName()
            }));
        }
        
        // only for 'Copy'
        if (!tncp.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
            return;
        }
        
        if (tncp.getPreDirectoryName().length() == 0 ||
                tncp.getPreFileName().length() == 0) {
            return;
        }
        
        if (!this.isFileExist(tncp.getPreDirectoryName(), tncp.getPreFileName())) {
            return;
        }
        
        try {
            if (this.intf.getProvider().deleteFile(tncp.getPreDirectoryName(), tncp.getPreFileName())) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                    		"cleanupPreTransferPut()",
                    		"delete",
                    		intf.getProvider().getReplyString()
                    		}));
                }
            } else {
                this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                		new Object[] {
                		"cleanupPreTransferPut()",
                		"delete",
                		intf.getProvider().getReplyString()
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg);
                }
                throw new FtpFileException(this.msg);
            }
        } catch (Exception e) {
            this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
            		new Object[] {
            		e,
            		"cleanupPreTransferPut()"
            		});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg, e);
            }
            throw new FtpFileException(this.msg, e);
        }
    }
    
    /**
     * Cleans up any failures during pre/post ftp raw commands.
     * @param    tncr   An instance of TransferNamesAndCommands class.
     * @exception   FtpFileException  If some error occurs.
     */
    public void cleanupRawCommands(TransferNamesAndCommands tncr) throws FtpFileException {
        // we are able to do nothing because we are out of control of the raw commands.
    }
    
    /**
     * Cleans up any failures during ftp Transfer "get".
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void cleanupTransferGet(TransferNamesAndCommands tncg) throws Exception {
        // need to do nothing now.
    }
    
    /**
     * Cleans up any failures during ftp Transfer "put".
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void cleanupTransferPut(TransferNamesAndCommands tncp) throws Exception {
        //? we will not try to undo the creation of the destination directory tree - too expensive!
        //? warning msg
        
        //? if after pre command, ftp 'put' overwrote a file, the overwritten file will not be recovered.
        //? warning msg
        
        if (!((FtpFileTransferNamesAndCommandsPut)tncp).getCleanupOnPutFailure()) {
            // don't do cleanup because no actual ftp put transfer occurred.
            return;
        }
        
        if (tncp.getTargetDirectoryName().length() == 0 ||
                tncp.getTargetFileName().length() == 0) {
            return;
        }
        
        if (!this.isFileExist(tncp.getTargetDirectoryName(), tncp.getTargetFileName())) {
            return;
        }
        
        try {
            // In case we need it for buggy NT ftp server, just uncomment it
            /*
            if (tncp.getPreTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_COPY)) {
                this.safeDelete(tncp.getTargetDirectoryName(), tncp.getTargetFileName());
                return;
            }
             */
            if (this.intf.getProvider().deleteFile(tncp.getTargetDirectoryName(), tncp.getTargetFileName())) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006013.DBG_EXT_FTP_ACTION_OK", new Object[] {
                    		"cleanupTransferPut()",
                    		"delete",
                    		intf.getProvider().getReplyString()
                    		}));
                }
                return;
            } else {
                this.msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                		new Object[] {
                		"cleanupTransferPut()",
                		"delete",
                		intf.getProvider().getReplyString()
                		});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg);
                }
                throw new FtpFileException(this.msg);
            }
        } catch (Exception e) {
            this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
            		new Object[] {
            		e,
            		"cleanupTransferPut()"
            		});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg, e);
            }
            throw new FtpFileException(this.msg, e);
        }
        
    }
    
    /**
     * Undo pre/post ftp raw commands.
     * @param    tncr   An instance of TransferNamesAndCommands.
     * @exception   FtpFileException  If some error occurs.
     */
    public void undoRawCommands(TransferNamesAndCommands tncr) throws FtpFileException {
        // we are able to do nothing because we are out of control of the raw commands.
    }
    
    /**
     * Validates against the configuration parameters' relationships.
     * @param       mode  "get" or "put".
     *
     * @exception   FtpFileException   If some error occurs.
     */
    // For the single parameter validation, we have
    // done them in property setters. Here is for the
    // relationship validation.
    // These parameters can be set on GUI screen or
    // can be modified in collaboration anytime.
    private void validate(String mode) throws FtpFileException {
        logMethodCalled("validate()");
        if (!this.intf.getConfiguration().relationChanged) {
            return;
        }
        
        if ( this.tnc != null && this.configuration != null ) {
            // a temp fix for QAI 83764, a cleaner fix will come in 5.1.0
            if ( this.tnc.getAppend() != this.intf.getConfiguration().getAppend() ) {
                // need to update tnc, otherwise, in certain case
                // dynamic setting of append mode does not take
                // effect
                if (this.tnc instanceof FtpFileTransferNamesAndCommandsPut ) {
                    this.tnc = new FtpFileTransferNamesAndCommandsPut((FtpFileTransferNamesAndCommandsPut)tnc, this.configuration);
                } else if (this.tnc instanceof FtpFileTransferNamesAndCommandsGet ) {
                    this.tnc = new FtpFileTransferNamesAndCommandsGet((FtpFileTransferNamesAndCommandsGet)tnc, this.configuration);
                }
            }
        }
        
        new FtpFileValidator(this.configuration, mode).validate(noWarning);
        this.intf.getConfiguration().relationChanged = false;
        return;
    }
    
    /**
     * Updates the persistent state.
     * @exception  Exception  If some error occurs.
     */
    private void doUpdateState() throws Exception {
        // only if the state is really changed, we perform the physical update.
        if ( this.intf.getConfiguration().getSynchronized() ) {
            if (!this.intf.isStateChanged()) {
                return;
            }
            
            logMethodCalled("doUpdateState()");
            
            this.intf.setStateChanged(false);
            try {
                this.intf.getStateManager().store();
            } catch (Exception e) {
                this.msg = mMessages.getString("FTPBC-E006015.ERR_EXT_FTP_STATE_PERSIST_EXCEPTION", new Object[] {"doUpdateState()", e});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg, e);
                }
                throw new FtpFileException(this.msg, e);
            }
        }
    }
    
    /**
     * Retrieves a file from the remote FTP server.
     * This method retrieves the first matching entry under your directory and file name.
     * it takes the configuration parameters when performing get operation and pre/post operations.
     * Note: If no qualified file is available for retrieving, you will get
     *       the exception containing java.io.FileNotFoundException as a nested exception.
     * @exception   FtpFileException  If some error occurs.
     */
    public void get() throws FtpFileException {
    	logMethodCalled("get()");
        
        try {
            if (!this.isConnected()) {
                String key = intf.getConfiguration().isConnectionEstablishmentModeManual() ? "FTPBC-E006013.ERR_EXT_FTP_NOT_CONNECTED" : "FTPBC-E006014.ERR_EXT_FTP_NOT_CONNECTED_MANUAL_MODE";
                this.msg = mMessages.getString(key, new Object[] {intf.getConfiguration().getConnectionEstablishmentMode()});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg);
                }
                throw new FtpFileException(this.msg);
            } else {
                if (this.intf.getConfiguration().connectionChanged) {
                    //? What is the desired behavior?
                    // 1. re-connect here ?
                    // 2. reject ?
                    // 3. ignore/discard ?
                    // 4. accept the changes until next connect time ?
                    //
                    if (mLogger.isLoggable(Level.WARNING)) {
                        mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006003.WRN_EXT_FTP_PARAM_CHANGED"));
                    }
                }
            }
            
            this.payload = null;
            this.workingPayload = null;
            
            // pre raw commands
            this.doRawCommands(this.intf.getConfiguration().getPreTransferRawCommands());
            
            // pre transfer
            try {
                this.doPreTransferGet(this.getResolvedNamesForGet());
            } catch (Exception e) {
                try {
                    if (mLogger.isLoggable(Level.INFO)) {
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006020.DBG_EXT_FTP_PROCESS_STARTED", new Object[] {"get()", "cleanup"}));
                    }
                    this.cleanupPreTransferGet(this.getResolvedNamesForGet());
                    if (mLogger.isLoggable(Level.INFO)) {
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006021.DBG_EXT_FTP_PROCESS_SUCCEEDED", new Object[] {"get()", "cleanup"}));
                    }
                } catch (Exception ee) {
                    throw new FtpFileException(ee.toString(), ee);
                }
                throw new FtpFileException(e.toString(), e);
            }
            
            // transfer
            try {
                this.doTransferGet(this.getResolvedNamesForGet());
            } catch (Exception e) {
                try {
                    if (mLogger.isLoggable(Level.INFO)) {
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006020.DBG_EXT_FTP_PROCESS_STARTED", new Object[] {"get()", "cleanup/undo"}));
                    }
                    this.cleanupTransferGet(this.getResolvedNamesForGet());
                    this.undoPreTransferGet(this.getResolvedNamesForGet());
                    if (mLogger.isLoggable(Level.INFO)) {
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006021.DBG_EXT_FTP_PROCESS_SUCCEEDED", new Object[] {"get()", "cleanup/undo"}));
                    }
                } catch (Exception ee) {
                    throw new FtpFileException(ee.toString(), ee);
                }
                throw new FtpFileException(e.toString(), e);
            }
            
            // post transfer
            try {
                this.doPostTransferGet(this.getResolvedNamesForGet());
            } catch (Exception e) {
                try {
                    if (mLogger.isLoggable(Level.INFO)) {
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006020.DBG_EXT_FTP_PROCESS_STARTED", new Object[] {"get()", "cleanup/undo/undo"}));
                    }
                    this.cleanupPostTransferGet(this.getResolvedNamesForGet());
                    this.undoTransferGet(this.getResolvedNamesForGet());
                    this.undoPreTransferGet(this.getResolvedNamesForGet());
                    if (mLogger.isLoggable(Level.INFO)) {
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006021.DBG_EXT_FTP_PROCESS_SUCCEEDED", new Object[] {"get()", "cleanup/undo/undo"}));
                    }
                } catch (Exception ee) {
                    throw new FtpFileException(ee.toString(), ee);
                }
                throw new FtpFileException(e.toString(), e);
            }
            
            // post raw commands
            try {
                this.doRawCommands(this.intf.getConfiguration().getPostTransferRawCommands());
                this.doUpdateState();
            } catch (Exception e) {
                try {
                    if (mLogger.isLoggable(Level.INFO)) {
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006020.DBG_EXT_FTP_PROCESS_STARTED", new Object[] {"get()", "cleanup/undo/undo/undo"}));
                    }
                    this.cleanupRawCommands(this.getResolvedNamesForGet());
                    this.undoPostTransferGet(this.getResolvedNamesForGet());
                    this.undoTransferGet(this.getResolvedNamesForGet());
                    this.undoPreTransferGet(this.getResolvedNamesForGet());
                    if (mLogger.isLoggable(Level.INFO)) {
                        mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006021.DBG_EXT_FTP_PROCESS_SUCCEEDED", new Object[] {"get()", "cleanup/undo/undo/undo"}));
                    }
                } catch (Exception ee) {
                    throw new FtpFileException(ee.toString(), ee);
                }
                throw new FtpFileException(e.toString(), e);
            }
            
            // only when everything goes well, the payload become visible to collab.
            this.setPayload(this.workingPayload);
            // Release the reference to the payload, so it can be
            // garbage collected properly. Otherwise, it will be
            // sitting in memory unnecessarily until the next get().
            this.workingPayload = null;
            
            // No original qualified file is available to get - special handling (throw special nested exception).
            // We only need to check TargetDirectoryName and TargetFileName
            // (Pre names are not necessary to verify.)
            String resolvedDir  = ((FtpFileTransferNamesAndCommandsGet) this.getResolvedNamesForGet()).getTargetDirectoryName();
            String resolvedFile = ((FtpFileTransferNamesAndCommandsGet) this.getResolvedNamesForGet()).getTargetFileName();
//            if (resolvedDir.length() == 0 ||
//                    resolvedFile.length() == 0) {
            if (resolvedDir.length() == 0 &&
                    resolvedFile.length() == 0) {
                //return false;
                this.msg = mMessages.getString("FTPBC-W006005.WRN_EXT_FTP_NO_FILE_RESOLVED",
                		new Object[] {
                		"get()"
                });
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, this.msg);
                }
                throw new FtpFileException("", new FileNotFoundException(this.msg));
            } else {
            }
        } catch (Exception e) {
            throw new FtpFileException(e.getMessage(), e);
        }
    }
    
    
    /**
     * same as get() but will return siliently if the target does not
     * exists;
     */
    public void getIfExists() throws FtpFileException {
        String file = null;
        TransferNamesAndCommands tnc = this.getResolvedNamesForGet();
        try {
            tnc.resolveTargetLocation();
            file = tnc.getTargetFileName();
        } catch (FtpInterfaceException e) {
            throw new FtpFileException(mMessages.getString("FTPBC-E006016.ERR_EXT_FTP_EXCEPTION_RESOLVING_TARGET",
            		new Object[] {"getIfExists()", e}), e);
        }
        if ( file == null || file.trim().length() == 0 )
            return; // does not resolve to a target file name <==> file not found
        // since the target dir/file names are resolved
        // do not resolve them again - could cause the resolved literal
        // escaped again - hence result in non-match of a already detected
        // target in get.
        //
        // wipe out tnc - will make the get() do the resolving all over again
        tnc = null;
        get();
    }
    
    public void setWarningOff(boolean b) {
        noWarning = b;
    }
    
    public boolean getWarningOff() {
        return noWarning;
    }
    
    /**
     */
    public int getFile(long leasePeriodMillis, String[] content, String[] target) throws Exception {
        String dir = intf.getConfiguration().getTargetDirectoryName();
        String file = intf.getConfiguration().getTargetFileName();
        FtpFileProvider provider = this.intf.getProvider();
        int seqVal = -1;
        // the file is not created yet - return -1 to indicate this
        String seqFilePattern = file + "|" + file + "\\.T\\d+";
        String firstFile = null;
        String fn = null;
        long stampVal = 0;
        String[] seqParts = null;
        String stampStr = null;
        boolean renameOK = false;
        String destFile = null;
        String absoluteDir = null;
        String seqEntry = null;
        int attemptCount = 0;
        long leaseExpire = 0;
        firstFile = provider.getFirstFileName(dir, false, seqFilePattern, true);
        
        if ( firstFile != null && firstFile.trim().length() > 0 ) {
            fn = this.intf.getProvider().getHeuristics().getBaseFileName(firstFile);
            absoluteDir = this.intf.getProvider().getHeuristics().getDirectoryName(firstFile);
            seqParts = fn.split("\\.");
            if ( seqParts != null && seqParts.length >= 2 && seqParts.length <=3 ) {
                try {
                    seqVal = Integer.parseInt(seqParts[1]);
                } catch (Exception e) {
                    throw new FtpFileException("Invalid sequence file [ " + firstFile + " ]", e);
                }
                
                if ( seqParts.length == 3 ) {
                    if ( seqParts[2] != null && seqParts[2].startsWith("T") && seqParts[2].length() > 2 ) {
                        stampStr = seqParts[2].substring(1);
                        try {
                            stampVal = Long.parseLong(stampStr);
                            if ( stampVal > System.currentTimeMillis() ) {
                                // lease still valid
                                // caller might want to retry
                                return -2;
                            }
                        } catch (Exception e) {
                            throw new FtpFileException("Invalid timestamp in sequence file [ " + firstFile + " ]", e);
                        }
                    } else {
                        throw new FtpFileException("Invalid timestamp in sequence file [ " + firstFile + " ]");
                    }
                }
            }
            else {
                throw new FtpFileException("Invalid sequence file [ " + firstFile + " ]");
            }

            try {
                leaseExpire = System.currentTimeMillis() + leasePeriodMillis;
                destFile = file.concat("." + seqVal + ".T" + leaseExpire);
                renameOK = provider.rename(absoluteDir, fn, absoluteDir, destFile);
            } catch (Exception e) {
                throw new FtpFileException("Exception when tagging sequence file with timestamp [ " + firstFile + " ] => [" + destFile + "]", e);
            }
            
            // lease failed
            if ( !renameOK ) {
                int retCode = provider.getReplyCode();
                if ( retCode == 550 ) {
                    // if this happens, it can be caused by
                    // racing condition, i.e., a competing client
                    // renamed successfully before this client,
                    // the caller might want to retry.
                    seqVal = -3;
                }
                else {
                    // error
                    seqVal = -1;
                }
            }
            else {
                String releasedFile = file.concat("." + (seqVal + 1) + ".T" + System.currentTimeMillis());
                // note, it is not necessary to check if lease is still valid - since
                // even at this point lease is valid, it could be invalid when the next
                // api is called, so just do renaming and check return code.
                renameOK = provider.rename(absoluteDir, destFile, absoluteDir, releasedFile);
                if ( !renameOK ) {
                    if ( provider.getReplyCode() == 550 ) {
                        // assume it is caused by destFile renamed by other competing client (because release just expired)
                        seqVal = -4;
                    }
                    else {
                        // error condition;
                        seqVal = -1;
                    }
                }
            }
        } else {
            // theoretically this can happen if RNFR and RNTO is not atomic
            // i.e. there is a tiny period when either the source file and
            // the dest file are not visible;
            // the caller can retry
            seqVal = -5;
        }
        return seqVal;
    }
    
    /**
     * prototyping persisted sequencing on FTP - this approach manipulates
     * remote file name as sequence persistence - instead of storing sequence info 
     * as file content which needs more file IO.
     * 
     * file is either <seq-ref>.<curr_seq> or <seq-ref>.<curr_seq>.<timestamp>
     * where <seq-ref> is a sequence name, e.g. s1, s2, etc. and <curr_seq> is the current sequence
     * value and <timestamp> is a timestamp tag added by the client currently fetching
     * the sequence (sequence is leased until the tagged time), competing client will
     * check this tag, if the current system time
     * > the tag, the lease expired, the competing client can re-tag (lease) the sequence.
     * after the current lease holder has done its sequence fetching, and if the lease is not
     * expired yet, it will rename the sequence file name to <seq-ref>.<curr_seq+1>.<current_time>,
     * if the rename is successful, return the sequence number to the caller, otherwise,
     * it could be that lease expired before renaming (ftp return 550 - file does not exist), 
     * in this case, this sequence fetching will be voided and the caller might want to 
     * try again after a retry interval, for other cases, it is an error, and caller will
     * be notified of the error.
     * 
     * @param leaseExpire - lease period in milli-seconds
     * @return the current value of the specified sequence <code>seq</code> or
     * -1 error condition
     * -2 still leased by others
     * -3 found sequence but failed to lease it (assuming leased by others)
     * -4 found sequence but failed to persist sequence (assuming leased by others before sequence update committed)
     * -5 does not find sequence (if ever occurs, assuming this is caused by slight chance of transient state during renaming)
     */
    public int getSequence(long leasePeriodMillis) throws Exception {
        String dir = intf.getConfiguration().getTargetDirectoryName();
        String seq = intf.getConfiguration().getTargetFileName();
        FtpFileProvider provider = this.intf.getProvider();
        int seqVal = -1;
        // the seq is not created yet - return -1 to indicate this
        String seqFilePattern = seq + "\\.\\d+|" + seq + "\\.\\d+\\.T\\d+";
        String firstFile = null;
        String fn = null;
        long stampVal = 0;
        String[] seqParts = null;
        String stampStr = null;
        boolean renameOK = false;
        String destFile = null;
        String absoluteDir = null;
        String seqEntry = null;
        int attemptCount = 0;
        long leaseExpire = 0;
        firstFile = provider.getFirstFileName(dir, false, seqFilePattern, true);
        
        if ( firstFile != null && firstFile.trim().length() > 0 ) {
            fn = this.intf.getProvider().getHeuristics().getBaseFileName(firstFile);
            absoluteDir = this.intf.getProvider().getHeuristics().getDirectoryName(firstFile);
            seqParts = fn.split("\\.");
            if ( seqParts != null && seqParts.length >= 2 && seqParts.length <=3 ) {
                try {
                    seqVal = Integer.parseInt(seqParts[1]);
                } catch (Exception e) {
                    throw new FtpFileException("Invalid sequence file [ " + firstFile + " ]", e);
                }
                
                if ( seqParts.length == 3 ) {
                    if ( seqParts[2] != null && seqParts[2].startsWith("T") && seqParts[2].length() > 2 ) {
                        stampStr = seqParts[2].substring(1);
                        try {
                            stampVal = Long.parseLong(stampStr);
                            if ( stampVal > System.currentTimeMillis() ) {
                                // lease still valid
                                // caller might want to retry
                                return -2;
                            }
                        } catch (Exception e) {
                            throw new FtpFileException("Invalid timestamp in sequence file [ " + firstFile + " ]", e);
                        }
                    } else {
                        throw new FtpFileException("Invalid timestamp in sequence file [ " + firstFile + " ]");
                    }
                }
            }
            else {
                throw new FtpFileException("Invalid sequence file [ " + firstFile + " ]");
            }

            try {
                leaseExpire = System.currentTimeMillis() + leasePeriodMillis;
                destFile = seq.concat("." + seqVal + ".T" + leaseExpire);
                renameOK = provider.rename(absoluteDir, fn, absoluteDir, destFile);
            } catch (Exception e) {
                throw new FtpFileException("Exception when tagging sequence file with timestamp [ " + firstFile + " ] => [" + destFile + "]", e);
            }
            
            // lease failed
            if ( !renameOK ) {
                int retCode = provider.getReplyCode();
                if ( retCode == 550 ) {
                    // if this happens, it can be caused by
                    // racing condition, i.e., a competing client
                    // renamed successfully before this client,
                    // the caller might want to retry.
                    seqVal = -3;
                }
                else {
                    // error
                    seqVal = -1;
                }
            }
            else {
                String releasedFile = seq.concat("." + (seqVal + 1) + ".T" + System.currentTimeMillis());
                // note, it is not necessary to check if lease is still valid - since
                // even at this point lease is valid, it could be invalid when the next
                // api is called, so just do renaming and check return code.
                renameOK = provider.rename(absoluteDir, destFile, absoluteDir, releasedFile);
                if ( !renameOK ) {
                    if ( provider.getReplyCode() == 550 ) {
                        // assume it is caused by destFile renamed by other competing client (because release just expired)
                        seqVal = -4;
                    }
                    else {
                        // error condition;
                        seqVal = -1;
                    }
                }
            }
        } else {
            // theoretically this can happen if RNFR and RNTO is not atomic
            // i.e. there is a tiny period when either the source file and
            // the dest file are not visible;
            // the caller can retry
            seqVal = -5;
        }
        return seqVal;
    }

    private void logMethodCalled(String methodName) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {methodName}));
        }
    }

    public boolean login(String user, String password) throws IOException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public boolean logout() throws IOException {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
