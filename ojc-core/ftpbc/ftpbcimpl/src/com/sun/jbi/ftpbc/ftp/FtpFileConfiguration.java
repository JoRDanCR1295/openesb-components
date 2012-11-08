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
 * @(#)FtpFileConfiguration.java 
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

import com.sun.jbi.ftpbc.Endpoint;
import com.sun.jbi.ftpbc.ftp.exception.FtpFileException;

import java.util.Properties;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;
import java.nio.charset.Charset;
import javax.xml.namespace.QName;

/**
 * This class represents ftp client configuration
 * @author Harry Liu
 * @author jfu
 * @version cvs revision:    Last Modified: 
 */
public class FtpFileConfiguration {
    private static final Messages mMessages =
            Messages.getMessages(FtpFileConfiguration.class);
    private static final Logger mLogger =
            Messages.getLogger(FtpFileConfiguration.class);
    
    public static final String CMD_NONE = "None";
    public static final String CMD_RENAME = "Rename";
    public static final String CMD_COPY = "Copy";
    public static final String CMD_DELETE = "Delete";
    
    public static final String MEDIA_FLAT_FILE = "FlatFile";
    public static final String MEDIA_BINARY_FILE = "BinaryFile";
    public static final String MEDIA_DB = "DB";
    
    /* Working variables */
    private String msg = null;
    private FtpInterface intf = null;
    private Properties mConfigProps;
    
    // maintain a set of states (they will be checked/set by multiple classes)
    boolean relationChanged = false;  // for validate()
    boolean connectionChanged = false;  // for close()/open() again?
    boolean serverChanged = false;  // for server info display
    boolean clientChanged = false;  // for future usage
    boolean providerChanged = false;  // for future usage (changing provider at runtime may lose the old ftp connection)
    
    // section "General Settings"
    private String statePersistenceBaseLocation = "";
    
    // section "FTP"
    private String directoryListingStyle = null;
    private String userDefinedDirectoryListingStyle = null;
    private String hostName = null;
    private int serverPort = 0;
    private String userName = null;
    private String password = null;
    private String mode = null;
    
    private boolean usePASV = false;
    private int dataConnectionTimeout = FtpFileProviderImpl.DEFAULT_TIMEOUT_DATA_CONNECTION;
    private int commandConnectionTimeout = FtpFileProviderImpl.DEFAULT_TIMEOUT_COMMAND_CONNECTION;
    
    private String controlChannelEncoding;

    // section "Target Location"
    private String targetDirectoryName = null;
    private boolean targetDirectoryNameIsPattern = false;
    private String targetFileName = null;
    private boolean targetFileNameIsPattern = false;
    private boolean append = false;
    
    // section "Pre Transfer"
    private String preTransferCommand = null;
    private String preDirectoryName = null;
    private boolean preDirectoryNameIsPattern = false;
    private String preFileName = null;
    private boolean preFileNameIsPattern = false;
    
    // section "Post Transfer"
    private String postTransferCommand = null;
    private String postDirectoryName = null;
    private boolean postDirectoryNameIsPattern = false;
    private String postFileName = null;
    private boolean postFileNameIsPattern = false;
    
    // section "Stage Transfer" for "put" only"
    private boolean stageEnabled = false;
    private String stageDirectoryName = null;
    private String stageFileName = null;
    
    // section "FTP Raw Commands"
    private String preTransferRawCommands = null;
    private String postTransferRawCommands = null;
    
    // section "Sequence Numbering"
    private long startingSequenceNumber = 0;
    private long maxSequenceNumber = 0;
    
    // section "SOCKS"
    private boolean socksEnabled = false;
    private String socksHostName = null;
    private int socksServerPort = 0;
    private String socksUserName = null;
    private String socksPassword = null;
    private int socksVersion = 0;
    
    // section "Extensions"
    private String providerClassName = null;
    private String clientClassName = null;
    
    // section "connector"
    private String connectionEstablishmentMode = null;
    
    private String mConnMode;
    private boolean bSynchronized;
    private String mUserHeuristicsLoc;
    private String sequenceNumberPersistenceMedia;
    
    // added for FTP/TLS
    private String mSecureFTPType;
    private boolean bEnableCCC;
    private String mKeyStoreLoc;
    private String mKeyStorePassword;
    private String mTrustStoreLoc;
    private String mTrustStorePassword;
    private String mKeyAlias;
    private String mKeyPassword;
    
    // added for per comp app - persisted sequence
    private Endpoint mCurrEP;
    private QName mOpName;
    
    /**
     * Constructor.
     *
     *
     * @param intf reference to a FtpInterface object.
     * @exception FtpFileException  If some error occurs.
     */
    public FtpFileConfiguration(FtpInterface intf) throws FtpFileException {
        this.intf = intf;
    }
    
    /**
     * Gets the directory listing style.
     * <p>This style is the listing format that the FTP server displays
     * upon issuing a LIST command. FtpHeuristics defines
     * all the styles supported by the ftp client. The default value is UNIX.
     * @return    The directory listing style.
     */
    public String getDirectoryListingStyle() {
        return this.directoryListingStyle;
    }
    
    /**
     * Gets the user directory listing style.
     * <p>This style is the listing format that the FTP server displays
     * upon issuing a LIST command. a heuristics configuration file on the logical
     * host contains the heuristics information this parameter is referring to.
     * No default value, when left blank, the CM level "Directory Listing Style" is used.
     * @return    The user defined directory listing style.
     */
    public String getUserDefinedDirectoryListingStyle() {
        return this.userDefinedDirectoryListingStyle;
    }
    
    /**
     * Gets the remote FTP host name.
     * This value is the name of the FTP server that the ftp client connects to.
     * It can be either an IP address or a logical host name.
     * The default value is localhost.
     * @return    Host Name.
     */
    public String getHostName() {
        return this.hostName;
    }
    
    /**
     * Gets the FTP mode.
     * <p>This value describes the mode used to transfer data to and from the FTP server.
     * The method returns binary or ASCII. The default value is ASCII.
     * @return    The Mode.
     */
    public String getMode() {
        return this.mode;
    }
    
    /**
     * Gets the encrypted password.
     * <p>The encrypted password corresponds to the user name; there is no default value.
     * @return    The encrypted password.
     */
    String getPassword() {
        return this.password;
    }
    
    /**
     * Gets the target directory name.
     * <p>This value is the directory on the external FTP system where files are
     * retrieved from or sent to. The absolute directory name is preferred;
     * otherwise, this path is relative to the home directory where
     * you were when you logged in to the FTP server.
     * <p>For outbound data (publishing), the directory is created
     * if it does not exist.
     * @return    The Target Directory Name.
     */
    public String getTargetDirectoryName() {
        return this.targetDirectoryName;
    }
    
    /**
     * Gets the FTP server's port.
     * <p>This value is the port number used on the FTP server when the ftp client is connecting to it.
     * The default value is 21.
     * @return    The FTP server's port number.
     */
    public int getServerPort() {
        return this.serverPort;
    }
    
    /**
     * Gets the user name.
     * <p>This value is the user name given when logging in to the FTP server;
     * the default value is "anonymous."
     * @return    The user name.
     */
    public String getUserName() {
        return this.userName;
    }
    
    /**
     * Sets Directory Listing Style.
     * <p>This style is the listing format that the FTP server displays
     * upon issuing a LIST command. FtpHeuristics defines
     * all the styles supported by the ftp client. The default value is UNIX.
     * @param       newDirectoryListingStyle  The Directory Listing Style.
     *
     * @exception   FtpFileException   If some error occurs.
     */
    public void setDirectoryListingStyle(String newDirectoryListingStyle)
    throws FtpFileException {
        String oldDirectoryListingStyle = this.directoryListingStyle;
        this.directoryListingStyle = newDirectoryListingStyle;
        
        if (this.directoryListingStyle == null
                || this.directoryListingStyle.trim().length() == 0 ) {
            this.msg = mMessages.getString("FTPBC-E006017.ERR_EXT_FTP_MISSING_DIR_LIST_STYLE",
                    new Object[] {
                "setDirectoryListingStyle(String newDirectoryListingStyle)"
            });
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg);
            }
            throw new FtpFileException(this.msg);
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006023.DBG_EXT_FTP_DIR_LIST_STYLE", new Object[] {directoryListingStyle}));
            }
        }
        this.directoryListingStyle = this.directoryListingStyle.trim();
        
        if (oldDirectoryListingStyle == null ||
                !oldDirectoryListingStyle.equals(this.directoryListingStyle)) {
            this.connectionChanged = true;
        }
        
        // update heuristics info
        try {
            // with the introduction of non-trivial connection management
            // intf here could be null;
            if (this.intf != null && this.intf.getProvider() != null) {
                // At current method is called first time, provider is not initialized yet.
                // Actually, we don't need to execute following line at intf initialization time.
                // We need it only when directoryListingStyle is changed in collab at runtime.
                if (oldDirectoryListingStyle == null ||
                        !oldDirectoryListingStyle.equals(this.directoryListingStyle)) {
                    // really changed
                    this.intf.getProvider().setDirListingStyle(this.directoryListingStyle);
                }
            }
        } catch (Exception e) {
            this.msg = mMessages.getString("FTPBC-E006018.ERR_EXT_FTP_EXCEPTION_SET_DIR_LIST_STYLE",
                    new Object[] {
                this.directoryListingStyle,
                e
            });
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg, e);
            }
            throw new FtpFileException(this.msg, e);
        }
    }
    
    /**
     * Sets User Defined Directory Listing Style.
     * <p>This style is the listing format that the FTP server displays
     * upon issuing a LIST command. FtpHeuristics defines
     * all the styles supported by the ftp client.
     * the user can defined their own heurisitcs following the
     * same format as in FtpHeuristics.cfg.
     *
     * @param       newUserDefinedDirectoryListingStyle  The Directory Listing Style name.
     *
     * @exception   FtpFileException   If some error occurs.
     */
    public void setUserDefinedDirectoryListingStyle(String newUserDefinedDirectoryListingStyle)
    throws FtpFileException {
        String oldStyle = this.userDefinedDirectoryListingStyle;
        this.userDefinedDirectoryListingStyle = newUserDefinedDirectoryListingStyle;
        
        if (this.userDefinedDirectoryListingStyle == null )
            this.userDefinedDirectoryListingStyle = "";
        this.userDefinedDirectoryListingStyle = this.userDefinedDirectoryListingStyle.trim();
        
        if (oldStyle == null ||
                !oldStyle.equals(this.userDefinedDirectoryListingStyle)) {
            // really changed
            this.connectionChanged = true;
        }
        
        // update heuristics info
        if ( this.userDefinedDirectoryListingStyle.trim().length() != 0
                && ( this.mUserHeuristicsLoc == null
                || this.mUserHeuristicsLoc.trim().length() == 0 ))
            throw new FtpFileException(mMessages.getString("FTPBC-E006019.ERR_EXT_FTP_MISS_UD_HEURISTICS_FILE",
                    new Object[] {
                userDefinedDirectoryListingStyle
            }));
        
        try {
            // with the introduction of non-trivial connection management
            // intf here could be null;
            if (this.intf != null && this.intf.getProvider() != null) {
                // At current method is called first time, provider is not initialized yet.
                // Actually, we don't need to execute following line at intf initialization time.
                // We need it only when directoryListingStyle is changed in collab at runtime.
                if (oldStyle == null ||
                        !oldStyle.equals(this.userDefinedDirectoryListingStyle)) {
                    // really changed
                    this.intf.getProvider().setUserDefinedHeuristicsInfo(this.userDefinedDirectoryListingStyle, this.mUserHeuristicsLoc);
                }
            }
        } catch (Exception e) {
            this.msg = mMessages.getString("FTPBC-E006020.ERR_EXT_FTP_EXCEPTION_UPD_UD_HEURISTICS_FILE",
                    new Object[] {
                "setUserDefinedDirectoryListingStyle()",
                userDefinedDirectoryListingStyle,
                e
            });
            
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg, e);
            }
            throw new FtpFileException(this.msg, e);
        }
    }
    
    /**
     * Sets the host name.
     * <p>This value is the name of the FTP server that the ftp client connects to.
     * It can be either an IP address or a logical host name.
     * The default value is localhost.
     * @param       newHostName  The host name.
     *
     * @exception   FtpFileException   If some error occurs.
     */
    public void setHostName(String newHostName) throws FtpFileException {
        String oldHostName = this.hostName;
        this.hostName = newHostName;
        
        if (this.hostName == null || this.hostName.trim().equals("")) {
            this.msg = mMessages.getString("FTPBC-E006021.ERR_EXT_FTP_MISS_OR_BAD_PARAM", new Object[] {"HostName", hostName});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg);
            }
            throw new FtpFileException(this.msg);
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"HostName", hostName}));
            }
        }
        this.hostName = this.hostName.trim();
        if (oldHostName == null ||
                !oldHostName.equals(this.hostName)) {
            // really changed
            this.serverChanged = true;
            this.connectionChanged = true;
        }
    }
    
    /**
     * Sets the FTP mode.
     * <p>This value describes the mode used to transfer data to and from the FTP server.
     * The method sets binary, EBCIDC, or ASCII. The default value is ASCII.
     * @param       newMode  The mode.
     *
     * @exception   FtpFileException   If some error occurs.
     */
    public void setMode(String newMode) throws FtpFileException {
        this.mode = newMode;
        
        if (this.mode == null
                || !this.isModeAscii()
                && !this.isModeBinary()
                && !this.isModeEbcdic()) {
            this.msg = mMessages.getString("FTPBC-E006021.ERR_EXT_FTP_MISS_OR_BAD_PARAM", new Object[] {"Mode", mode});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg);
            }
            throw new FtpFileException(this.msg);
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"Mode", mode}));
            }
        }
        this.mode = this.mode.trim();
    }
    
    /**
     * Sets the password.
     * <p>The input is a non-encrypted password. You must use another method,
     * setEncryptedPassword(String password), if the input is an encrypted
     * password.
     * @param       newPassword  The non-encrypted password.
     */
    public void setPassword(String newPassword) {
        String oldPassword = this.password;
        this.password = newPassword;
        
        if (this.password == null || this.password.trim().equals("")) {
            this.password = "";
        }
        if (oldPassword == null ||
                !oldPassword.equals(this.password)) {
            this.connectionChanged = true;
        }
    }
    
    /**
     * Sets the target directory name.
     * <p>The directory on the external FTP system where files are
     * retrieved from or sent to. The absolute directory name is preferred;
     * otherwise, this path is relative to the directory where
     * you were when you logged in to the FTP server.
     * <p>For outbound data (publishing), the directory is created
     * if it does not exist.
     * @param       newTargetDirectoryName  The target directory name.
     */
    public void setTargetDirectoryName(String newTargetDirectoryName)
    throws FtpFileException {
        String oldTargetDirectoryName = this.targetDirectoryName;
        this.targetDirectoryName = newTargetDirectoryName;
        
        if (this.targetDirectoryName == null
                || this.targetDirectoryName.trim().equals("")) {
            this.msg = mMessages.getString("FTPBC-E006021.ERR_EXT_FTP_MISS_OR_BAD_PARAM",
                    new Object[] {"TargetDirectoryName", targetDirectoryName});
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, this.msg);
            }
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"TargetDirectoryName", targetDirectoryName}));
            }
        }
        if (this.targetDirectoryName == null) {
            this.targetDirectoryName = "";
        }
        this.targetDirectoryName = this.targetDirectoryName.trim();
        if (oldTargetDirectoryName == null ||
                !oldTargetDirectoryName.equals(this.targetDirectoryName)) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Sets the FTP server's port.
     * <p>This value is the port number used on the FTP server when the ftp client is connecting to it.
     * The default value is 21.
     * @param       newServerPort  The server port number.
     *
     * @exception   FtpFileException   If some error occurs.
     */
    public void setServerPort(int newServerPort) throws FtpFileException {
        int oldServerPort = this.serverPort;
        this.serverPort = newServerPort;
        
        if (this.serverPort < 1 || this.serverPort > 65535) {
            this.msg = mMessages.getString("FTPBC-E006022.ERR_EXT_FTP_INVALID_PORT",
                    new Object[] {new Integer(serverPort)});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg);
            }
            throw new FtpFileException(this.msg);
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"ServerPort", new Integer(serverPort)}));
            }
        }
        if (oldServerPort != this.serverPort) {
            this.serverChanged= true;
            this.connectionChanged = true;
        }
    }
    
    /**
     * Sets the user name.
     * <p>This value is the user name given when logging into the FTP server;
     * the default value is "anonymous."
     * @param       newUserName  The user name.
     *
     * @exception   FtpFileException   If some error occurs.
     */
    // Because the password is encryped using property UserName,
    // we should re-encrypt the password as long as UserName is changed.
    public void setUserName(String newUserName) throws FtpFileException {
        String oldUserName = this.userName;
        this.userName = newUserName;
        
        if (this.userName == null || this.userName.trim().equals("")) {
            this.msg = mMessages.getString("FTPBC-E006021.ERR_EXT_FTP_MISS_OR_BAD_PARAM", 
                    new Object[] {"UserName", userName});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg);
            }
            throw new FtpFileException(this.msg);
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"UserName", userName}));
            }
        }
        
        this.userName = this.userName.trim();
        if (oldUserName == null ||
                !oldUserName.equals(this.userName)) {
            this.connectionChanged = true;
        }
    }
    
    /**
     * Gets the post transfer FTP raw commands.
     * <p>This value represents the FTP raw commands issued after the file transfer command.
     * Note that some SITE commands use a semicolon (;) to separate the
     * command sets, for example:
     * <p>SITE RECFM=FB;SITE LRECL=50;SITE BLOCKSIZE=32750;SITE TRACKS;SITE PRI=5;SITE SEC=5
     * <p>NOTE: These commands are sent to FTP server directly,
     * so the commands must be FTP raw commands.
     * @return    The post transfer FTP raw commands.
     */
    public String getPostTransferRawCommands() {
        return this.postTransferRawCommands;
    }
    
    /**
     * Gets the pre transfer FTP raw commands.
     * <p>This value represents the FTP raw commands issued before the file transfer command.
     * Note that some SITE commands use a semicolon (;) to separate the
     * command sets, for example:
     * <p>SITE RECFM=FB;SITE LRECL=50;SITE BLOCKSIZE=32750;SITE TRACKS;SITE PRI=5;SITE SEC=5
     * <p>NOTE: These commands are sent to FTP server directly,
     * so the commands must be FTP raw commands.
     * @return    The pre transfer FTP raw commands.
     */
    public String getPreTransferRawCommands() {
        return this.preTransferRawCommands;
    }
    
    /**
     * Gets the target file name.
     * <p>This value is the FTP remote file name which is retrieved from or sent to.
     * <p>For outbound data (publishing), the file is created if it does not exist.
     * <p>It represents the base file name instead of full file name.
     * <p>For MVS GDG, this value can be the version of the dataset, for example:
     * <p>Target Directory Name = 'STC.SAMPLE.GDGSET'.
     * <p>Target File Name = (0) to indicate the current version.
     * @return    The target file name.
     */
    public String getTargetFileName() {
        return this.targetFileName;
    }
    
    /**
     * Gets information that tells you whether your connection is using the passive mode,
     * that is, whether the "Use PASV" configuration parameter status is YES or NO.
     * <p>Normally, when you connect to an FTP site, the site establishes the
     * data connection to your computer. However, some FTP sites allow passive data
     * transfers, meaning that your computer establishes the data connection.
     * By default, the passive mode is turned on; and it is recommended that you use it
     * for data transfers to and from FTP sites that support it.
     * <p>The passive mode may be required in the following instances:
     * <p>- For users on networks behind some types of router-based firewalls
     * <p>- For users on networks behind a gateway requiring passive data transfers
     * <p>- If transfers are erratic
     * <p>- If you keep getting failed data-channel errors
     * @return    The "Use PASV" parameter status.
     */
    public boolean isUsePASV() {
        return this.usePASV;
    }
    
    /**
     * Sets the post transfer FTP raw commands.
     * <p>This value represents the FTP raw commands issued after the file transfer command.
     * Note that some SITE commands use a semicolon (;) to separate the
     * command sets, for example:
     * <p>SITE RECFM=FB;SITE LRECL=50;SITE BLOCKSIZE=32750;SITE TRACKS;SITE PRI=5;SITE SEC=5
     * <p>NOTE: These commands are sent to FTP server directly,
     * so the commands must be FTP raw commands.
     * @param       newPostTransferRawCommands  The post transfer FTP raw commands.
     */
    public void setPostTransferRawCommands(
            String newPostTransferRawCommands) {
        this.postTransferRawCommands = newPostTransferRawCommands;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"PostTransferRawCommands", postTransferRawCommands}));
        }
        if (this.postTransferRawCommands == null) {
            this.postTransferRawCommands = "";
        }
        this.postTransferRawCommands = this.postTransferRawCommands.trim();
    }
    
    /**
     * Sets the pre transfer FTP raw commands.
     * <p>This value represents the FTP raw commands issued before the file transfer command.
     * Note that some SITE commands use a semicolon (;) to separate the
     * command sets, for example:
     * <p>SITE RECFM=FB;SITE LRECL=50;SITE BLOCKSIZE=32750;SITE TRACKS;SITE PRI=5;SITE SEC=5
     * <p>NOTE: These commands are sent to FTP server directly,
     * so the commands must be FTP raw commands.
     * @param       newPreTransferRawCommands  The pre transfer FTP raw commands.
     */
    public void setPreTransferRawCommands(
            String newPreTransferRawCommands) {
        this.preTransferRawCommands = newPreTransferRawCommands;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"PreTransferRawCommands", preTransferRawCommands}));
        }
        if (this.preTransferRawCommands == null) {
            this.preTransferRawCommands = "";
        }
        this.preTransferRawCommands = this.preTransferRawCommands.trim();
    }
    
    /**
     * Sets the target file name.
     * <p>This value is the FTP remote file name which is retrieved from or sent to.
     * <p>For outbound data (publishing), the file is created if it does not exist.
     * <p>It represents the base file name instead of full file name.
     * <p>For MVS GDG, this value can be the version of the dataset, for example:
     * <p>Target Directory Name = 'STC.SAMPLE.GDGSET'.
     * <p>Target File Name = (0) to indicate the current version.
     * @param       newTargetFileName  The target file name.
     *
     * @exception   FtpFileException   If some error occurs.
     */
    public void setTargetFileName(String newTargetFileName)
    throws FtpFileException {
        String oldTargetFileName = this.targetFileName;
        this.targetFileName = newTargetFileName;
        
        if (this.targetFileName == null || this.targetFileName.trim().equals("")) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-E006021.ERR_EXT_FTP_MISS_OR_BAD_PARAM", new Object[] {"TargetFileName", targetFileName}));
            }
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"TargetFileName", targetFileName}));
            }
        }
        if (this.targetFileName == null) {
            this.targetFileName = "";
        }
        this.targetFileName = this.targetFileName.trim();
        if (oldTargetFileName == null ||
                !oldTargetFileName.equals(this.targetFileName)) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Sets the parameter that tells the connection whether to use the passive mode,
     * that is, whether the "Use PASV" configuration parameter status is YES or NO.
     * <p>Normally, when you connect to an FTP site, the site establishes the
     * data connection to your computer. However, some FTP sites allow passive data
     * transfers, meaning that your computer establishes the data connection.
     * By default, the passive mode is turned on; and it is recommended that you use it
     * for data transfers to and from FTP sites that support it.
     * <p>The passive mode may be required in the following instances:
     * <p>- For users on networks behind some types of router-based firewalls
     * <p>- For users on networks behind a gateway requiring passive data transfers
     * <p>- If transfers are erratic
     * <p>- If you keep getting failed data-channel errors
     * @param       newUsePASV  The desired "Use PASV" parameter.
     */
    public void setUsePASV(boolean newUsePASV) {
        this.usePASV = newUsePASV;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"UsePASV", usePASV}));
        }
    }
    
    /**
     * Gets the maximum sequence number.
     * <p>This value is used for the name pattern %#. The value of this parameter
     * is used when you have set up the remote file name to
     * contain a sequence number. The value tells the ftp client that
     * when this value (the parameter Max Sequence Number) is reached,
     * to reset the sequence number to the starting sequence number.
     * <p>The value of the maximum sequence number MUST be greater than
     * the starting sequence number.
     * @return    The maximum sequence number.
     */
    public long getMaxSequenceNumber() {
        return this.maxSequenceNumber;
    }
    
    /**
     * Gets the starting sequence number.
     * <p>This value is used for the name pattern %#.
     * <p>The value of this parameter is used when you have set up
     * the remote file name to contain a sequence number.
     * The value tells the ftp client which value to start with in the absence
     * of a sequence number from a previous run.
     * <p>Also, when the maximum sequence number is reached, the sequence
     * number rolls over to the starting sequence number.
     * <p>The value of the starting sequence number MUST be less than
     * the maximum sequence number. The default value is 1.
     * @return    The starting sequence number.
     */
    public long getStartingSequenceNumber() {
        return this.startingSequenceNumber;
    }
    
    /**
     * Sets the maximum sequence number.
     * <p>This value is used for the name pattern %#. The value of this parameter
     * is used when you have set up the remote file name to
     * contain a sequence number. The value tells the ftp client that
     * when this value (the parameter Max Sequence Number) is reached,
     * to reset the sequence number to the starting sequence number.
     * <p>The value of the maximum sequence number MUST be greater than
     * the starting sequence number.
     * @param       newMaxSequenceNumber  The maximum sequence number.
     *
     * @exception   FtpFileException   If some error occurs.
     */
    public void setMaxSequenceNumber(long newMaxSequenceNumber)
    throws FtpFileException {
        long oldMaxSequenceNumber = this.maxSequenceNumber;
        this.maxSequenceNumber = newMaxSequenceNumber;
        
        if (this.maxSequenceNumber < 1 || this.maxSequenceNumber > 2147483647) {
            this.msg = mMessages.getString("FTPBC-E006023.ERR_EXT_FTP_INVALID_SEQ_MAX", 
                    new Object[] {new Long(maxSequenceNumber)});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg);
            }
            throw new FtpFileException(this.msg);
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"MaxSequenceNumber", maxSequenceNumber}));
            }
        }
        if (oldMaxSequenceNumber != this.maxSequenceNumber) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Sets the starting sequence number.
     * <p>This value is used for the name pattern %#.
     * <p>The value of this parameter is used when you have set up
     * the remote file name to contain a sequence number.
     * The value tells the ftp client which value to start with in the absence
     * of a sequence number from a previous run.
     * <p>Also, when the maximum sequence number is reached, the sequence
     * number rolls over to the starting sequence number.
     * <p>The value of the starting sequence number MUST be less than
     * the maximum sequence number. The default value is 1.
     * @param       newStartingSequenceNumber  The starting sequence number.
     *
     * @exception   FtpFileException   If some error occurs.
     */
    public void setStartingSequenceNumber(long newStartingSequenceNumber)
    throws FtpFileException {
        long oldstartingSequenceNumber = this.startingSequenceNumber;
        this.startingSequenceNumber = newStartingSequenceNumber;
        
        if (this.startingSequenceNumber < 0
                || this.startingSequenceNumber > 2147483647) {
            this.msg = mMessages.getString("FTPBC-E006024.ERR_EXT_FTP_INVALID_SEQ_MIN", 
                    new Object[] {new Long(startingSequenceNumber)});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg);
            }
            throw new FtpFileException(this.msg);
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"StartingSequenceNumber", startingSequenceNumber}));
            }
        }
        if (oldstartingSequenceNumber != this.startingSequenceNumber) {
            this.relationChanged = true;
        }
    }
    
    public void setSequenceNumberPersistenceMedia(String newMedia) throws FtpFileException {
        this.sequenceNumberPersistenceMedia = newMedia;
        
        if (!this.isSequenceNumberPersistenceFlatFile() &&
                !this.isSequenceNumberPersistenceBinaryFile() &&
                !this.isSequenceNumberPersistenceDB()) {
            this.msg = mMessages.getString("FTPBC-E006025.ERR_EXT_FTP_INVALID_PERSISTENCE_TYPE", new Object[] {
                MEDIA_BINARY_FILE, MEDIA_DB, MEDIA_FLAT_FILE
            });
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg);
            }
            throw new FtpFileException(this.msg);
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"SequenceNumberPersistenceMedia", sequenceNumberPersistenceMedia}));
            }
        }
        
        this.sequenceNumberPersistenceMedia = this.sequenceNumberPersistenceMedia.trim();
    }
    
    public String getSequenceNumberPersistenceMedia() {
        return this.sequenceNumberPersistenceMedia;
    }
    
    public boolean isSequenceNumberPersistenceFlatFile() {
        return MEDIA_FLAT_FILE.equalsIgnoreCase(this.sequenceNumberPersistenceMedia.trim());
    }
    
    public boolean isSequenceNumberPersistenceBinaryFile() {
        return MEDIA_BINARY_FILE.equalsIgnoreCase(this.sequenceNumberPersistenceMedia.trim());
    }
    
    public boolean isSequenceNumberPersistenceDB() {
        return MEDIA_DB.equalsIgnoreCase(this.sequenceNumberPersistenceMedia.trim());
    }
    
    /**
     * Sets the property password.
     * <p>The input is an encrypted password. You must use another method,
     * setPassword(String password), if the input is a non-encrypted password.
     * @param       newPassword  The encrypted password.
     */
    public void setEncryptedPassword(String newPassword) throws FtpFileException {
        String oldPassword = this.password;
        try {
            this.password = NonEmptyScEncrypt.decrypt(this.userName, newPassword);
        } catch (Exception e) {
            this.msg = mMessages.getString("FTPBC-E006026.ERR_EXT_FTP_DECRYPT_EXCEPTION", 
                    new Object[] {
                "setEncryptedPassword(encryptedPassword)",
                e
            });
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg, e);
            }
            throw new FtpFileException(this.msg, e);
        }
        
        if (oldPassword == null ||
                !oldPassword.equals(this.password)) {
            this.connectionChanged = true;
        }
    }
    
    /**
     * Gets the SOCKS server port.
     * <p>The port number to use on the SOCKS server when connecting to it.
     * The default value is 1080.
     * @return    The SOCKS server port.
     */
    public int getSocksServerPort() {
        return this.socksServerPort;
    }
    
    /**
     * Gets the SOCKS version.
     * <p> This value is the version of the SOCKS server. The valid values are 4, 5 and -1.
     * The default value is -1, indicating an unknown version.
     * @return    The SOCKS version.
     */
    public int getSocksVersion() {
        return this.socksVersion;
    }
    
    /**
     * Sets the SOCKS server port.
     * <p>The port number to use on the SOCKS server when connecting to it.
     * The default value is 1080.
     * @param       newSocksServerPort  The SOCKS server port.
     *
     * @exception   FtpFileException   If some error occurs.
     */
    public void setSocksServerPort(int newSocksServerPort)
    throws FtpFileException {
        int oldSocksServerPort = this.socksServerPort;
        this.socksServerPort = newSocksServerPort;
        
        if (this.socksServerPort < 1 || this.socksServerPort > 65535) {
            this.msg = mMessages.getString("FTPBC-E006022.ERR_EXT_FTP_INVALID_PORT", new Object[] {
                new Integer(socksServerPort)
            });
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg);
            }
            throw new FtpFileException(this.msg);
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"SocksServerPort", new Integer(socksServerPort)}));
            }
        }
        if (oldSocksServerPort != this.socksServerPort) {
            this.connectionChanged = true;
        }
    }
    
    /**
     * Sets the SOCKS version.
     * <p>This value is the version of the SOCKS server. The valid values are 4, 5 and -1.
     * The default value is -1, indicating an unknown version.
     * @param       newSocksVersion  The SOCKS Version.
     *
     * @exception   FtpFileException   If some error occurs.
     */
    public void setSocksVersion(int newSocksVersion) throws FtpFileException {
        int oldSocksVersion = this.socksVersion;
        this.socksVersion = newSocksVersion;
        
        if (!this.isSocksVersion4() &&
                !this.isSocksVersion5() &&
                !this.isSocksVersionUnknown()) {
            this.msg = mMessages.getString("FTPBC-E006027.ERR_EXT_FTP_INVALID_SOCKS_VER", new Object[] {socksVersion,
                new Integer(Socks.VERSION_4), new Integer(Socks.VERSION_5), new Integer(Socks.VERSION_UNKNOWN)
                
            });
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg);
            }
            throw new FtpFileException(this.msg);
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"SocksVersion", new Integer(socksVersion)}));
            }
        }
        if (oldSocksVersion != this.socksVersion) {
            this.connectionChanged = true;
        }
    }
    
    /**
     * Gets the client class name.
     * <p>This value is an advanced setting that allows for
     * user extensibility.
     * <p>You can provide your own implementation class
     * name. This class must implement the interface
     * com.sun.jbi.ftpbc.ftp.FtpFileClient or extend the class,
     * com.sun.jbi.ftpbc.ftp.FtpFileClientImpl. You can overwrite
     * or inherit any method you want.
     * <p>This value is optional but, if supplied, the full class
     * name must be given, for example:
     * com.mycompany.MyFtpFileClientImpl
     * @return    Client Class Name.
     */
    public String getClientClassName() {
        return this.clientClassName;
    }
    
    /**
     * Sets the client class name.
     * <p>This value is an advanced setting that allows for
     * user extensibility.
     * <p>You can provide your own implementation class
     * name. This class must implement the interface
     * com.sun.jbi.ftpbc.ftp.FtpFileClient or extend the class,
     * com.sun.jbi.ftpbc.ftp.FtpFileClientImpl. You can overwrite
     * or inherit any method you want.
     * <p>This value is optional but, if supplied, the full class
     * name must be given, for example:
     * com.mycompany.MyFtpFileClientImpl
     * @param    newClientClassName   The client class name.
     * @exception   FtpFileException   If some error occurs.
     */
    public void setClientClassName(String newClientClassName) throws FtpFileException {
        String oldClientClassName = this.clientClassName;
        
        this.clientClassName = newClientClassName;
        if (this.clientClassName == null || this.clientClassName.trim().equals("")) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-E006021.ERR_EXT_FTP_MISS_OR_BAD_PARAM", new Object[] {"ClientClassName", clientClassName}));
            }
            this.clientClassName = "com.sun.jbi.ftpbc.ftp.FtpFileClientImpl";
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"ClientClassName", clientClassName}));
            }
        }
        
        this.clientClassName = this.clientClassName.trim();
        
        if (oldClientClassName == null ||
                !oldClientClassName.equals(this.clientClassName)) {
            this.clientChanged = true;
            this.createClient();
        }
    }
    /**
     * Gets information that tells you whether your connection is using the passive mode,
     * that is, whether the "Use PASV" configuration parameter status is YES or NO.
     * <p>Normally, when you connect to an FTP site, the site establishes the
     * data connection to your computer. However, some FTP sites allow passive data
     * transfers, meaning that your computer establishes the data connection.
     * By default, the passive mode is turned on; and it is recommended that you use it
     * for data transfers to and from FTP sites that support it.
     * <p>The passive mode may be required in the following instances:
     * <p>- For users on networks behind some types of router-based firewalls
     * <p>- For users on networks behind a gateway requiring passive data transfers
     * <p>- If transfers are erratic
     * <p>- If you keep getting failed data-channel errors
     * @return    The "Use PASV" parameter status.
     */
    public boolean getUsePASV() {
        return this.usePASV;
    }
    
    /**
     * Gets the SOCKS host name.
     * This value is the name of the SOCKS server that the ftp client connects to.
     * It can be either an IP address or a logical host name.
     * The default value is empty, that is, no SOCKS server is used.
     * @return    The SOCKS host name.
     */
    public String getSocksHostName() {
        return this.socksHostName;
    }
    
    /**
     * Gets the encrypted SOCKS password.
     * <p>The encrypted SOCKS password corresponds to the SOCKS user name.
     * There is no default value.
     * @return    The encrypted SOCKS password.
     */
    String getSocksPassword() {
        return this.socksPassword;
    }
    
    /**
     * Gets the SOCKS user name.
     * <p> The SOCKS user name is the ID used to log into the SOCKS server.
     * The default value is empty.
     * @return    The SOCKS user name.
     */
    public String getSocksUserName() {
        return this.socksUserName;
    }
    
    /**
     * Sets the property socksPassword.
     * <p>The input password is an encrypted password. You must use another method,
     * setSocksPassword(String socksPassword), if the input is a non-encrypted password.
     * @param       newSocksPassword  The encrypted socksPassword.
     */
    public void setSocksEncryptedPassword(String newSocksPassword) throws FtpFileException {
        String oldSocksPassword = this.socksPassword;
        try {
            this.socksPassword = NonEmptyScEncrypt.decrypt(this.socksUserName, newSocksPassword);
        } catch (Exception e) {
            this.msg = mMessages.getString("FTPBC-E006026.ERR_EXT_FTP_DECRYPT_EXCEPTION", 
                    new Object[] {
                "setSocksEncryptedPassword(String newSocksPassword)",
                e
            });
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg, e);
            }
            throw new FtpFileException(this.msg, e);
        }
        if (oldSocksPassword == null ||
                !oldSocksPassword.equals(this.socksPassword)) {
            this.connectionChanged = true;
        }
    }
    
    /**
     * Sets the SOCKS host name.
     * <p>This value is the name of the SOCKS server that the ftp client connects to.
     * The value can be either an IP address or a logical host name.
     * The default value is empty, that is, no SOCKS server is used.
     * @param       newSocksHostName  The SOCKS host name.
     */
    public void setSocksHostName(String newSocksHostName) {
        String oldSocksHostName = this.socksHostName;
        this.socksHostName = newSocksHostName;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"SocksHostName", socksHostName}));
        }
        if (this.socksHostName == null) {
            this.socksHostName = "";
        }
        this.socksHostName = this.socksHostName.trim();
        if (oldSocksHostName == null ||
                !oldSocksHostName.equals(this.socksHostName)) {
            this.connectionChanged = true;
        }
    }
    
    /**
     * Sets the SOCKS password.
     * <p>The input password is a non-encrypted password. You must use another method,
     * setSocksEncryptedPassword(String socksPassword), if the input is an encrypted
     * password.
     * @param       newSocksPassword  The non-encrypted SOCKS password.
     */
    public void setSocksPassword(String newSocksPassword) {
        String oldSocksPassword = this.socksPassword;
        this.socksPassword = newSocksPassword;
        
        if (this.socksPassword == null || this.socksPassword.trim().equals("")) {
            this.socksPassword = "";
        }
        if (oldSocksPassword == null ||
                !oldSocksPassword.equals(this.socksPassword)) {
            // really changed
            this.connectionChanged = true;
        }
    }
    
    /**
     * Sets the SOCKS user name.
     * <p>This user name is the ID used to log into the SOCKS server.
     * The default value is empty.
     * @param       newSocksUserName  The SOCKS user name.
     *
     * @exception   FtpFileException   If some error occurs.
     */
    public void setSocksUserName(String newSocksUserName)
    throws FtpFileException {
        String oldSocksUserName = this.socksUserName;
        this.socksUserName = newSocksUserName;
        
        if (this.socksUserName == null) {
            this.socksUserName = "";
        }
        this.socksUserName = this.socksUserName.trim();
        
        if (oldSocksUserName == null ||
                !oldSocksUserName.equals(this.socksUserName)) {
            this.connectionChanged = true;
        }
    }
    
    /**
     * Checks to determine whether SOCKS is enabled.
     * @return    true or false
     */
    public boolean getSocksEnabled() {
        return this.socksEnabled;
    }
    
    /**
     * Checks to determine whether SOCKS is enabled.
     * @return    true or false
     */
    public boolean isSocksEnabled() {
        return this.socksEnabled;
    }
    
    /**
     * Allows you to enable or disable the SOCKS feature for the FTP ETD.
     * @param       newSocksEnabled  true or false
     */
    public void setSocksEnabled(boolean newSocksEnabled) {
        boolean oldSocksEnabled = this.socksEnabled;
        this.socksEnabled = newSocksEnabled;
        if (oldSocksEnabled != this.socksEnabled) {
            this.relationChanged = true;
            this.connectionChanged = true;
        }
    }
    
    /**
     * Gets the command connection timeout (in milliseconds).
     * A timeout of zero is interpreted as an infinite timeout.
     * @return    The command connection timeout.
     */
    public int getCommandConnectionTimeout() {
        return this.commandConnectionTimeout;
    }
    
    /**
     * Gets the data connection timeout (in milliseconds).
     * A timeout of zero is interpreted as an infinite timeout.
     * @return    The data connection timeout.
     */
    public int getDataConnectionTimeout() {
        return this.dataConnectionTimeout;
    }
    
    /**
     * Sets the command connection timeout (in milliseconds).
     * A timeout of zero is interpreted as an infinite timeout.
     * @param       newCommandConnectionTimeout  The command connection timeout.
     */
    public void setCommandConnectionTimeout(int newCommandConnectionTimeout) {
        this.commandConnectionTimeout = newCommandConnectionTimeout;
    }
    
    /**
     * Sets the data connection timeout (in milliseconds).
     * A timeout of zero is interpreted as an infinite timeout.
     * @param       newDataConnectionTimeout  The data connection timeout.
     */
    public void setDataConnectionTimeout(int newDataConnectionTimeout) {
        this.dataConnectionTimeout = newDataConnectionTimeout;
    }
    
    /**
     * Gets the control channel encoding.
     * @return  the char set name for control channel IO.
     */
    public String getControlChannelEncoding() {
        return this.controlChannelEncoding;
    }
    
    /**
     * Sets the control channel encoding.
     * @param  the char set name for FTP control channel IO
     */
    public void setControlChannelEncoding(String encoding) {
        if ( encoding == null || encoding.trim().length() == 0 ) {
            this.controlChannelEncoding = "ISO-8859-1";
        }
        else {
            if ( Charset.isSupported(encoding) ) {
                this.controlChannelEncoding = encoding;
            }
            else {
                throw new IllegalArgumentException("Invalid charset name [" + encoding + "] when set encoding for FTP control channel.");
            }
        }
    }
    
    /**
     * Gets the provider class name.
     * <p>This value is an advanced setting that allows for
     * user extensibility.
     * <p>You can provide your own implementation class
     * name. This class must implement the interface
     * com.sun.jbi.ftpbc.ftp.FtpFileProvider or extend the class,
     * com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl. You can overwrite
     * or inherit any method you want.
     * <p>This value is optional but, if supplied, the full class
     * name must be given, for example:
     * com.mycompany.MyFtpFileProviderImpl
     * @return    The provider class name.
     */
    public String getProviderClassName() {
        return this.providerClassName;
    }
    
    /**
     * Sets Pattern/Literal option for target directory name.
     * @param    newDirectoryPatternOrLiteral   The true or false
     */
    public void setTargetDirectoryNameIsPattern(boolean newTargetDirectoryNameIsPattern) {
        boolean oldTargetDirectoryNameIsPattern = this.targetDirectoryNameIsPattern;
        this.targetDirectoryNameIsPattern = newTargetDirectoryNameIsPattern;
        if (oldTargetDirectoryNameIsPattern != this.targetDirectoryNameIsPattern) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Gets the overwrite or append file operation status.
     * <p>This value is for outbound data transactions only. You can only
     * overwrite or append a remote file if it exists already. If a file with the same name
     * does not exist, both the append and overwrite operations create
     * a new file on the remote FTP system. The values are either Overwrite or Append.
     * The default value is Overwrite.
     * @return    The overwrite or append status.
     */
    public boolean getAppend() {
        return this.append;
    }
    
    /**
     * Gets the post transfer command.
     * <p>This command is executed after a file has been successfully retrieved from or sent to
     * a remote FTP host. The following actions can be performed
     * on the remote file copy:
     * <p>Delete: Deletes the file from the remote host.
     * <p>Rename: Renames the file.
     * <p>None: Does nothing, that is, leaves the file on the remote host intact.
     * @return    The post transfer command.
     */
    public String getPostTransferCommand() {
        return this.postTransferCommand;
    }
    
    /**
     * Gets the post directory name and
     * is only used for  the "Rename" option of  the post transfer command.
     * This value is the name of the directory on the external system where file is renamed.
     * The absolute directory name is expected.
     * <p>Special characters are allowed. The expansion of any
     * special characters is carried out each time this parameter
     * is used.
     * @return    The post directory name.
     */
    public String getPostDirectoryName() {
        return this.postDirectoryName;
    }
    
    /**
     * Checks whether the post directory name is a pattern or a literal name.
     * @return    Whether the post directory name is a pattern.
     */
    public boolean getPostDirectoryNameIsPattern() {
        return this.postDirectoryNameIsPattern;
    }
    
    /**
     * Gets the pre transfer command.
     * <p>This command is executed before a file has been successfully retrieved from or sent to
     * a remote FTP host. The following actions can be performed
     * on the remote file copy:
     * <p>Rename: Renames the file on the remote host.
     * <p>Copy: Copies the file.
     * <p>None: Does nothing, that is, leaves the file on the remote host intact.
     * @return    The pre transfer command.
     */
    public String getPreTransferCommand() {
        return this.preTransferCommand;
    }
    
    /**
     * Gets the pre directory name and is only used for the
     * "Rename" or "Copy" options of the pre transfer commands.
     * This value represents the directory on the remote FTP system where the file is renamed
     * or copied to. The absolute directory name is expected.
     * <p>Special characters are allowed. The expansion of any
     * special characters is carried out each time this parameter
     * is used.
     * @return    The pre directory name.
     */
    public String getPreDirectoryName() {
        return this.preDirectoryName;
    }
    
    /**
     * Checks whether the pre directory name is a pattern or a literal name.
     * @return    Whether the pre directory name is a pattern.
     */
    public boolean getPreDirectoryNameIsPattern() {
        return this.preDirectoryNameIsPattern;
    }
    
    /**
     * Checks whether the target directory name is a pattern.
     * @return    Whether the target directory name is a pattern.
     */
    public boolean getTargetDirectoryNameIsPattern() {
        return this.targetDirectoryNameIsPattern;
    }
    
    /**
     * Gets the state persistence base location.
     * @return       The state persistence base location.
     */
    public String getStatePersistenceBaseLocation() {
        return this.statePersistenceBaseLocation;
    }
    
    /**
     * Sets the overwrite or append file operation status.
     * <p>This value is for outbound data transactions only. You can only
     * overwrite or append a remote file if it exists already. If a file with the same name
     * does not exist, both the append and overwrite operations create
     * a new file on the remote FTP system. The values are either Overwrite or Append.
     * The default value is Overwrite.
     * @param       newOverwriteOrAppend  The overwrite or append status.
     */
    public void setAppend(boolean newAppend) {
        boolean oldAppend = this.append;
        this.append = newAppend;
        if (oldAppend != this.append) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Sets the connection establishment mode.
     * This value specifies how the connection with the external
     * system is established and closed. "Automatic" indicates that
     * the connection is automatically established when the
     * Collaboration is started, and the Collaboration keeps the connection open
     * as needed. "OnDemand" indicates that the connection is
     * established on demand, while Business Rules requiring a
     * connection to the external system are performed. The
     * connection is closed after the methods are completed.
     * "Manual" indicates that you must explicitly call the
     * connection-related connect() and disconnect() methods in your
     * Collaboration as Business Rules.
     *
     * NOTE: If you are using e*Gate version 4.5.1 or earlier, this option is ignored.
     * @param       newConnectionEstablishmentMode  The connection establishment mode.
     *
     * @exception   FtpFileException   If some error occurs.
     */
    void setConnectionEstablishmentMode(String newConnectionEstablishmentMode)
    throws FtpFileException {
        this.connectionEstablishmentMode = newConnectionEstablishmentMode;
        if (this.connectionEstablishmentMode == null
                || !this.isConnectionEstablishmentModeAutomatic()
                && !this.isConnectionEstablishmentModeManual() ) {
            this.msg = mMessages.getString("FTPBC-E006028.ERR_EXT_FTP_INVALID_CONN_MODE", 
                    new Object[] {
                connectionEstablishmentMode,
                "Automatic", "Manual"
            });
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg);
            }
            throw new FtpFileException(this.msg);
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"ConnectionEstablishmentMode", connectionEstablishmentMode}));
            }
        }
        this.connectionEstablishmentMode = this.connectionEstablishmentMode.trim();
    }
    
    /**
     * Sets the post transfer command.
     * <p>This command is executed after a file has been successfully retrieved from or sent to
     * a remote FTP host. The following actions can be performed
     * on the remote file copy:
     * <p>Delete: Deletes the file from the remote host.
     * <p>Rename: Renames the file.
     * <p>None: Does nothing, that is, leaves the file on the remote host intact.
     * @param       newPostTransferCommand  The post transfer command.
     *
     * @exception   FtpFileException   If some error occurs.
     */
    public void setPostTransferCommand(String newPostTransferCommand)
    throws FtpFileException {
        String oldPostTransferCommand = this.postTransferCommand;
        this.postTransferCommand = newPostTransferCommand;
        if (this.postTransferCommand == null
                || !this.isPostTransferCommandDelete()
                && !this.isPostTransferCommandNone()
                && !this.isPostTransferCommandRename()) {
            this.msg = mMessages.getString("FTPBC-E006029.ERR_EXT_FTP_SET_INVALID_POST_CMD", new Object[] {
                "Delete", "Rename", "None"
            });
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, this.msg);
            }
            throw new FtpFileException(this.msg);
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"PostTransferCommand", postTransferCommand}));
            }
        }
        this.postTransferCommand = this.postTransferCommand.trim();
        if (oldPostTransferCommand == null ||
                !oldPostTransferCommand.equals(this.postTransferCommand)) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Sets the post directory name
     * and only applies to the "Rename" option of the post transfer command.
     * This value represents the directory on the external system where file is renamed
     * to. The absolute directory name is expected.
     * <p>Special characters are allowed. The expansion of any
     * special characters is carried out each time this parameter
     * is used.
     * @param       newPostDirectoryName  The post directory name.
     */
    public void setPostDirectoryName(String newPostDirectoryName) {
        String oldPostDirectoryName = this.postDirectoryName;
        this.postDirectoryName = newPostDirectoryName;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"PostDirectoryName", postDirectoryName}));
        }
        if (this.postDirectoryName == null) {
            this.postDirectoryName = "";
        }
        this.postDirectoryName = this.postDirectoryName.trim();
        if (oldPostDirectoryName == null ||
                !oldPostDirectoryName.equals(this.postDirectoryName)) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Sets Pattern/Literal option for Post Directory Name.
     * @param   newPostDirectoryNameIsPattern   true or false
     */
    public void setPostDirectoryNameIsPattern(boolean newPostDirectoryNameIsPattern) {
        boolean oldPostDirectoryNameIsPattern = this.postDirectoryNameIsPattern;
        this.postDirectoryNameIsPattern = newPostDirectoryNameIsPattern;
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"PostDirectoryNameIsPattern", postDirectoryNameIsPattern}));
        }
        if (oldPostDirectoryNameIsPattern != this.postDirectoryNameIsPattern) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Sets the pre transfer command.
     * <p>This command is executed before a file has been successfully retrieved from or sent to
     * a remote FTP host. The following actions can be performed
     * on the remote file copy:
     * <p>Rename: Renames the file on the remote host.
     * <p>Copy: Copies the file.
     * <p>None: Does nothing, that is, leaves the file on the remote host intact.
     * @param       newPreTransferCommand  The pre transfer command.
     * @exception   FtpFileException   If some error occurs.
     */
    public void setPreTransferCommand(String newPreTransferCommand)
    throws FtpFileException {
        String oldPreTransferCommand = this.preTransferCommand;
        this.preTransferCommand = newPreTransferCommand;
        if (this.preTransferCommand == null
                || !this.isPreTransferCommandCopy()
                && !this.isPreTransferCommandNone()
                && !this.isPreTransferCommandRename()) {
            this.msg = mMessages.getString("FTPBC-E006030.ERR_EXT_FTP_SET_INVALID_PRE_CMD",
                    new Object[] {
                "Copy", "Rename", "None"
            });
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, this.msg);
            }
            throw new FtpFileException(this.msg);
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"PreTransferCommand", preTransferCommand}));
            }
        }
        this.preTransferCommand = this.preTransferCommand.trim();
        if (oldPreTransferCommand == null ||
                !oldPreTransferCommand.equals(this.preTransferCommand)) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Sets the pre directory name and is only used for the
     * "Rename" or "Copy" options of the pre transfer commands.
     * This value represents the directory on the remote FTP system where the file is renamed
     * or copied to. The absolute directory name is expected.
     * <p>Special characters are allowed. The expansion of any
     * special characters is carried out each time this parameter
     * is used.
     * @param       newPreDirectoryName  The pre directory name.
     */
    public void setPreDirectoryName(String newPreDirectoryName) {
        String oldPreDirectoryName = this.preDirectoryName;
        this.preDirectoryName = newPreDirectoryName;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"PreDirectoryName", preDirectoryName}));
        }
        if (preDirectoryName == null) {
            preDirectoryName = "";
        }
        this.preDirectoryName = this.preDirectoryName.trim();
        if (oldPreDirectoryName == null ||
                !oldPreDirectoryName.equals(this.preDirectoryName)) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Sets Pattern/Literal option for Pre Directory Name.
     * @param   newPreDirectoryNameIsPattern   true or false
     */
    public void setPreDirectoryNameIsPattern(boolean newPreDirectoryNameIsPattern) {
        boolean oldPreDirectoryNameIsPattern = this.preDirectoryNameIsPattern;
        this.preDirectoryNameIsPattern = newPreDirectoryNameIsPattern;
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"PreDirectoryNameIsPattern", preDirectoryNameIsPattern}));
        }
        if (oldPreDirectoryNameIsPattern != this.preDirectoryNameIsPattern) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Sets the provider class name.
     * <p>This value is an advanced setting that allows for
     * user extensibility.
     * <p>You can provide your own implementation class
     * name. This class must implement the interface
     * com.sun.jbi.ftpbc.ftp.FtpFileProvider or extend the class,
     * com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl. You can overwrite
     * or inherit any method you want.
     * <p>This value is optional but, if supplied, the full class
     * name must be given, for example:
     * com.mycompany.MyFtpFileProviderImpl
     * @param    newProviderClassName   The provider class name.
     * @exception   FtpFileException   If some error occurs.
     */
    //void setProviderClassName(String newProviderClassName) throws FtpFileException {
    public void setProviderClassName(String newProviderClassName) throws FtpFileException {
        String oldProviderClassName = this.providerClassName;
        this.providerClassName = newProviderClassName;
        if (this.providerClassName == null || this.providerClassName.trim().equals("")) {
            mLogger.log(Level.WARNING, mMessages.getString("FTPBC-E006021.ERR_EXT_FTP_MISS_OR_BAD_PARAM", new Object[] {"ProviderClassName", providerClassName}));
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, this.msg);
            }
            this.providerClassName = "com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl";
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"ProviderClassName", providerClassName}));
            }
        }
        
        this.providerClassName = this.providerClassName.trim();
        
        if (oldProviderClassName == null ||
                !oldProviderClassName.equals(this.providerClassName)) {
            this.connectionChanged = true;
            this.providerChanged = true;
            this.createProvider();
            if (oldProviderClassName != null) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006006.WRN_EXT_FTP_PROVIDER_CHANGED"));
                }
            }
        }
    }
    
    /**
     * Sets Pattern/Literal option for target file name.
     * @param    newTargetFileNameIsPattern   true or false
     */
    public void setTargetFileNameIsPattern(boolean newTargetFileNameIsPattern) {
        boolean oldTargetFileNameIsPattern = this.targetFileNameIsPattern;
        this.targetFileNameIsPattern = newTargetFileNameIsPattern;
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"TargetFileNameIsPattern", targetFileNameIsPattern}));
        }
        if (oldTargetFileNameIsPattern != this.targetFileNameIsPattern) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Checks whether the stage transfer is enabled.
     * @return    Whether the stage transfer is enabled.
     */
    public boolean getStageEnabled() {
        return this.stageEnabled;
    }
    
    /**
     * Checks whether the stage transfer is enabled.
     * @return    Whether the stage transfer is enabled.
     */
    public boolean isStageEnabled() {
        return this.stageEnabled;
    }
    
    /**
     * Gets the stage directory name.
     * The absolute directory name is expected.
     * <p>Special characters are allowed. The expansion of any
     * special characters is carried out each time this parameter
     * is used.
     * @return    The stage directory name.
     */
    public String getStageDirectoryName() {
        return this.stageDirectoryName;
    }
    
    /**
     * Gets Stage File Name.
     * It represents the base file name instead of
     * full file name.
     * <p>Special characters are allowed. The expansion of any
     * special characters are carried out each time this parameter
     * is used.
     * @return    The Stage File Name.
     */
    public String getStageFileName() {
        return this.stageFileName;
    }
    
    /**
     * Sets the stage directory name.
     * The absolute directory name is expected.
     * <p>Special characters are allowed. The expansion of any
     * special characters is carried out each time this parameter
     * is used.
     * @param       newStageDirectoryName  The stage directory name.
     */
    public void setStageDirectoryName(String newStageDirectoryName) {
        String oldStageDirectoryName = this.stageDirectoryName;
        this.stageDirectoryName = newStageDirectoryName;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"StageDirectoryName", stageDirectoryName}));
        }
        if (this.stageDirectoryName == null) {
            this.stageDirectoryName = "";
        }
        this.stageDirectoryName = this.stageDirectoryName.trim();
        if (oldStageDirectoryName == null ||
                !oldStageDirectoryName.equals(this.stageDirectoryName)) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Sets Stage File Name.
     * It represents the base file name instead of
     * full file name.
     * <p>Special characters are allowed. The expansion of any
     * special characters are carried out each time this parameter
     * is used.
     * @param       newStageFileName  The Stage File Name.
     */
    public void setStageFileName(String newStageFileName) {
        String oldStageFileName = this.stageFileName;
        this.stageFileName = newStageFileName;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"StageFileName", stageFileName}));
        }
        if (this.stageFileName == null) {
            this.stageFileName = "";
        }
        this.stageFileName = this.stageFileName.trim();
        if (oldStageFileName == null ||
                !oldStageFileName.equals(this.stageFileName)) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Turns on/off stage transfer.
     * @param   newStageEnabled   true or false
     */
    public void setStageEnabled(boolean newStageEnabled) {
        boolean oldStageEnabled = this.stageEnabled;
        this.stageEnabled = newStageEnabled;
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006024.DBG_EXT_FTP_SET_PARAM", new Object[] {"StageEnabled", stageEnabled}));
        }
        if (oldStageEnabled != this.stageEnabled) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Gets the connection establishment mode.
     * This value specifies how the connection with the external
     * system is established and closed. "Automatic" indicates that
     * the connection is automatically established when the
     * Collaboration is started, and the Collaboration keeps the connection open
     * as needed. "OnDemand" indicates that the connection is
     * established on demand, while Business Rules requiring a
     * connection to the external system are performed. The
     * connection is closed after the methods are completed.
     * "Manual" indicates that you must explicitly call the
     * connection-related connect() and disconnect() methods in your
     * Collaboration as Business Rules.
     *
     * NOTE: If you are using e*Gate version 4.5.1 or earlier, this option is ignored.
     * @return       The connection establishment mode.
     */
    public String getConnectionEstablishmentMode() {
        return this.connectionEstablishmentMode;
    }
    
    /**
     * Initializes the configuration from specified properties.
     * @param       props   The properties.
     *
     * @exception   FtpFileException  If some error occurs.
     */
    // called by connector and intf
    public void initialConfigValues(Properties props) throws FtpFileException {
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"initialConfigValues(Properties props)"}));
        
        this.mConfigProps = props;
        
        try {
            this.setMaxRetry(getNumberValue(props, FtpFileConfigConstants.CONNRETRY_MAXRETRIES, 0));
            this.setRetryInterval(getNumberValue(props, FtpFileConfigConstants.CONNRETRY_INTERVAL, 1000));
            
            // section "General Settings"
            this.statePersistenceBaseLocation = props.getProperty(FtpFileConfigConstants.P_GEN_BASE_LOC, System.getProperty("user.home", "/temp"));
            
            this.bSynchronized = props.getProperty(FtpFileConfigConstants.C_P_GEN_SYNC, "Yes").equalsIgnoreCase("Yes");
            
            // section "FTP"
            this.setDirectoryListingStyle(
                    props.getProperty(FtpFileConfigConstants.C_P_FTP_LST_STYLE, "UNIX"));
            this.setHostName(props.getProperty(FtpFileConfigConstants.C_P_FTP_HOST, "localhost"));
            this.setUserName(props.getProperty(FtpFileConfigConstants.C_P_FTP_USR, "anonymous"));
            this.setPassword(props.getProperty(FtpFileConfigConstants.C_P_FTP_PASSWD, ""));
            this.setServerPort(getNumberValue(props, FtpFileConfigConstants.C_P_FTP_PORT, 21));
            
            // added for FTP/TLS support
            this.setSecureFTPType(props.getProperty(FtpFileConfigConstants.C_P_SECURE_FTP_TYPE, "None"));
            String secFTPType = this.getSecureFTPType();

            if ( secFTPType.equals(FtpFileConfigConstants.FTP_SECURE_EXPLICITSSL) || secFTPType.equals(FtpFileConfigConstants.FTP_SECURE_IMPLICITSSL) ) {
                this.setKeyStoreLoc(props.getProperty(FtpFileConfigConstants.C_P_KEY_STORE_LOC, ""));
                this.setKeyStorePassword(props.getProperty(FtpFileConfigConstants.C_P_KEY_STORE_PASSWORD, ""));
                this.setTrustStoreLoc(props.getProperty(FtpFileConfigConstants.C_P_TRUST_STORE_LOC, ""));
                this.setTrustStorePassword(props.getProperty(FtpFileConfigConstants.C_P_TRUST_STORE_PASSWORD, ""));
                this.setKeyAlias(props.getProperty(FtpFileConfigConstants.C_P_KEY_ALIAS, ""));
                this.setKeyPassword(props.getProperty(FtpFileConfigConstants.C_P_KEY_PASSWORD, ""));
                if ( secFTPType.equals(FtpFileConfigConstants.FTP_SECURE_EXPLICITSSL) ) {
                    this.setEnableCCC(props.getProperty(FtpFileConfigConstants.C_P_ENABLE_CCC, "false").equals("true") ? true : false);
                }
            }

            this.setMode(props.getProperty(FtpFileConfigConstants.P_FTP_MODE, "Binary"));
            this.setUsePASV(
                    props.getProperty(FtpFileConfigConstants.C_P_FTP_PASSIVE_ON, "Yes").equalsIgnoreCase("Yes"));
            this.setCommandConnectionTimeout(getNumberValue(props, FtpFileConfigConstants.P_FTP_CMD_TIMEOUT, 45000));
            this.setDataConnectionTimeout(getNumberValue(props, FtpFileConfigConstants.P_FTP_DAT_TIMEOUT, 45000));
            
            this.setUserHeuristicsLocation(props.getProperty(FtpFileConfigConstants.C_P_FTP_UDH_CFG, ""));
            this.setUserDefinedDirectoryListingStyle(props.getProperty(FtpFileConfigConstants.C_P_FTP_UDH_LST_STYLE, ""));
            
            // section "Target Location"
            this.setTargetDirectoryName(
                    props.getProperty(FtpFileConfigConstants.P_TGT_DIR, ""));
            this.setTargetDirectoryNameIsPattern(
                    props.getProperty(FtpFileConfigConstants.P_TGT_DIR_PATT, "No").equalsIgnoreCase("Yes"));
            this.setTargetFileName(props.getProperty(FtpFileConfigConstants.P_TGT_FILE, ""));
            this.setTargetFileNameIsPattern(
                    props.getProperty(FtpFileConfigConstants.P_TGT_FILE_PATT, "Yes").equalsIgnoreCase("Yes"));
            this.setAppend(
                    props.getProperty(FtpFileConfigConstants.P_TGT_APPND, "No").equalsIgnoreCase("Yes"));
            
            // section "Pre Transfer"
            this.setPreTransferCommand(
                    props.getProperty(FtpFileConfigConstants.P_PRE_CMD, "None"));
            this.setPreDirectoryName(
                    props.getProperty(FtpFileConfigConstants.P_PRE_DIR, ""));
            this.setPreDirectoryNameIsPattern(
                    props.getProperty(FtpFileConfigConstants.P_PRE_DIR_PATT, "No").equalsIgnoreCase("Yes"));
            this.setPreFileName(
                    props.getProperty(FtpFileConfigConstants.P_PRE_FILE, ""));
            this.setPreFileNameIsPattern(
                    props.getProperty(FtpFileConfigConstants.P_PRE_FILE_PATT, "Yes").equalsIgnoreCase("Yes"));
            
            // section "Post Transfer"
            this.setPostTransferCommand(
                    props.getProperty(FtpFileConfigConstants.P_POST_CMD, "None"));
            this.setPostDirectoryName(
                    props.getProperty(FtpFileConfigConstants.P_POST_DIR, ""));
            this.setPostDirectoryNameIsPattern(
                    props.getProperty(FtpFileConfigConstants.P_POST_DIR_PATT, "No").equalsIgnoreCase("Yes"));
            this.setPostFileName(
                    props.getProperty(FtpFileConfigConstants.P_POST_FILE, ""));
            this.setPostFileNameIsPattern(
                    props.getProperty(FtpFileConfigConstants.P_POST_FILE_PATT, "Yes").equalsIgnoreCase("Yes"));
            
            // section "Stage Transfer" for "put" only and it is exclusive with "Post Transfer"
            this.setStageEnabled(
                    props.getProperty(FtpFileConfigConstants.P_STAGE_ENABLED, "No").equalsIgnoreCase("Yes"));
            this.setStageDirectoryName(
                    props.getProperty(FtpFileConfigConstants.P_STAGE_DIR, ""));
            this.setStageFileName(
                    props.getProperty(FtpFileConfigConstants.P_STAGE_FILE, ""));
            
            // section "FTP Raw Commands"
            this.setPreTransferRawCommands(
                    props.getProperty(FtpFileConfigConstants.P_RAW_PRE_CMD, ""));
            this.setPostTransferRawCommands(
                    props.getProperty(FtpFileConfigConstants.P_RAW_POST_CMD, ""));
            
            // section "Sequence Numbering"
            this.setStartingSequenceNumber(getNumberValue(props, FtpFileConfigConstants.P_SEQ_START, 1));
            this.setMaxSequenceNumber(getNumberValue(props, FtpFileConfigConstants.P_SEQ_MAX, 999999));
            this.setSequenceNumberPersistenceMedia(
                    props.getProperty("Sequence Numbering/Sequence Number Persistence Media", MEDIA_FLAT_FILE)); // FlatFile, or BinaryFile, or DB
            
            // section "SOCKS"
            this.setSocksEnabled(
                    props.getProperty(FtpFileConfigConstants.C_P_SOC_ON, "No").equalsIgnoreCase("Yes"));
            this.setSocksHostName(props.getProperty(FtpFileConfigConstants.C_P_SOC_HOST, ""));
            this.setSocksServerPort(getNumberValue(props, FtpFileConfigConstants.C_P_SOC_PORT, 1080));
            this.setSocksUserName(props.getProperty(FtpFileConfigConstants.C_P_SOC_USR, ""));
            this.setSocksPassword(props.getProperty(FtpFileConfigConstants.C_P_SOC_PASSWD, ""));
            
            String version = props.getProperty(FtpFileConfigConstants.C_P_SOC_VER, "Unknown").trim();
            if (version.equalsIgnoreCase("Unknown")) {
                this.setSocksVersion(Socks.VERSION_UNKNOWN);
            } else {
                this.setSocksVersion(Integer.parseInt(version));
            }
            
            // section "Extensions"
            this.setProviderClassName(
                    props.getProperty(FtpFileConfigConstants.P_EXTENSION_PROVIDER_CLAZZ, "com.sun.jbi.ftpbc.ftp.FtpFileProviderImpl"));
            this.setClientClassName(
                    props.getProperty(FtpFileConfigConstants.P_EXTENSION_CLIENT_CLAZZ, "com.sun.jbi.ftpbc.ftp.FtpFileClientImpl"));
            
            this.mConnMode =
                    props.getProperty(FtpFileConfigConstants.C_P_GEN_CONN_MODE, "Automatic");
            
            this.setConnectionEstablishmentMode(this.mConnMode);
        } catch (Exception e) {
            this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"initialConfigValues(Properties props)", e});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg, e);
            }
            throw new FtpFileException(this.msg, e);
        }
        
    }
    
    private int getNumberValue(Properties p, String key, int defValue) {
        Object obj = p.get(key);
        int result = defValue;
        if ( obj != null ) {
            result = Integer.parseInt(obj.toString());
        }
        return result;
    }
    
    protected void reset() throws FtpFileException {
        if ( this.mConfigProps != null ) {
            this.initialConfigValues(this.mConfigProps);
        }
    }
    
    /**
     * Checks whether file transfer mode is append or overwrite.
     * <p>This value is for outbound data transfer only. You can only
     * overwrite or append a remote file if it exists already. If a file with the same name
     * does not exist, both the append and overwrite operations create
     * a new file on the remote FTP system. The values are either Overwrite or Append.
     * The default value is Overwrite.
     * @return    The overwrite or append status.
     */
    public boolean isAppend() {
        return this.append;
    }
    
    /**
     * Check whether the post directory name is a pattern or a literal name.
     * @return    Whether the post directory name is a pattern.
     */
    public boolean isPostDirectoryNameIsPattern() {
        return this.postDirectoryNameIsPattern;
    }
    
    /**
     * Check whether the pre directory name is a pattern or a literal name.
     * @return    Whether the pre directory name is a pattern.
     */
    public boolean isPreDirectoryNameIsPattern() {
        return this.preDirectoryNameIsPattern;
    }
    
    /**
     * Check whether the target directory name is a pattern or a literal name.
     * @return    Whether the target directory name is a pattern.
     */
    public boolean isTargetDirectoryNameIsPattern() {
        return this.targetDirectoryNameIsPattern;
    }
    
    /**
     * Checks whether the target file name is a pattern or a literal name.
     * @return    Whether the target file name is a pattern.
     */
    public boolean getTargetFileNameIsPattern() {
        return this.targetFileNameIsPattern;
    }
    
    /**
     * Creates or updates the FtpFileClient object.
     * @exception FtpFileException    If some error occurs.
     */
    private void createClient() throws FtpFileException {
        if (mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"createClient()"}));
        
        Class cls = null;
        try {
            cls = Class.forName(this.clientClassName);
            if (com.sun.jbi.ftpbc.ftp.FtpFileClient.class.isAssignableFrom(cls)) {
                FtpFileClient client = (FtpFileClient) cls.newInstance();
                if (this.intf != null) {
                    client.initialize(this.intf);
                    // update intf
                    this.intf.setClient(client);
                }
            } else {
                this.msg = mMessages.getString("FTPBC-E006031.ERR_EXT_FTP_INVALID_CLIENTTYPE",
                        new Object[] {"createClient()", clientClassName, "com.sun.jbi.ftpbc.ftp.FtpFileClient"});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg);
                }
                throw new FtpFileException(this.msg);
            }
        } catch (Exception e) {
            this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                    new Object[] {"createClient()", e});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg, e);
            }
            throw new FtpFileException(this.msg, e);
        }
    }
    
    /**
     * Creates or updates FtpFileProvider object.
     * @exception FtpFileException    If some error occurs.
     */
    private void createProvider() throws FtpFileException {
        if (mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"createProvider()"}));
        
        Class cls = null;
        try {
            cls = Class.forName(this.providerClassName);
            if (com.sun.jbi.ftpbc.ftp.FtpFileProvider.class.isAssignableFrom(cls)) {
                FtpFileProvider provider = (FtpFileProvider) cls.newInstance();
                if (this.intf != null) {
                    provider.initialize(this.intf);
                    this.intf.setProvider(provider);
                }
            } else {
                this.msg = mMessages.getString("FTPBC-E006032.ERR_EXT_FTP_INVALID_PROVIDERTYPE", 
                        new Object[] {"createProvider()", providerClassName, "com.sun.jbi.ftpbc.ftp.FtpFileProvider"});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, this.msg);
                }
                throw new FtpFileException(this.msg);
            }
        } catch (Exception e) {
            this.msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                    new Object[] {"createProvider()", e});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg, e);
            }
            throw new FtpFileException(this.msg, e);
        }
    }
    
    /**
     * A helper method.
     * @return       true or false
     */
    public boolean isConnectionEstablishmentModeAutomatic() {
        return this.connectionEstablishmentMode.trim().equalsIgnoreCase("Automatic");
    }
    
    /**
     * A helper method.
     * @return       true or false
     */
    public boolean isConnectionEstablishmentModeManual() {
        return this.connectionEstablishmentMode.trim().equalsIgnoreCase("Manual");
    }
    
    /**
     * A helper method.
     * @return       true or false
     */
    public boolean isModeAscii() {
        return this.mode.trim().equalsIgnoreCase("Ascii");
    }
    
    /**
     * A helper method.
     * @return       true or false
     */
    public boolean isModeBinary() {
        return this.mode.trim().equalsIgnoreCase("Binary");
    }
    
    /**
     * A helper method.
     * @return       true or false
     */
    public boolean isModeEbcdic() {
        return this.mode.trim().equalsIgnoreCase("Ebcdic");
    }
    
    /**
     * A helper method.
     * @return       true or false
     */
    public boolean isPostTransferCommandDelete() {
        return this.postTransferCommand.trim().equalsIgnoreCase("Delete");
    }
    
    
    
    /**
     * A helper method.
     * @return       true or false
     */
    public boolean isPostTransferCommandNone() {
        return this.postTransferCommand.trim().equalsIgnoreCase("None");
    }
    
    /**
     * A helper method.
     * @return       true or false
     */
    public boolean isPostTransferCommandRename() {
        return this.postTransferCommand.trim().equalsIgnoreCase("Rename");
    }
    
    
    
    /**
     * A helper method.
     * @return       true or false
     */
    public boolean isPreTransferCommandCopy() {
        return this.preTransferCommand.trim().equalsIgnoreCase("Copy");
    }
    
    /**
     * A helper method.
     * @return       true or false
     */
    public boolean isPreTransferCommandNone() {
        return this.preTransferCommand.trim().equalsIgnoreCase("None");
    }
    
    /**
     * A helper method.
     * @return       true or false
     */
    public boolean isPreTransferCommandRename() {
        return this.preTransferCommand.trim().equalsIgnoreCase("Rename");
    }
    
    /**
     * A helper method.
     * @return       true or false
     */
    public boolean isSocksVersion4() {
        return this.socksVersion == Socks.VERSION_4;
    }
    
    /**
     * A helper method.
     * @return       true or false
     */
    public boolean isSocksVersion5() {
        return this.socksVersion == Socks.VERSION_5;
    }
    
    /**
     * A helper method.
     * @return       true or false
     */
    public boolean isSocksVersionUnknown() {
        return this.socksVersion == Socks.VERSION_UNKNOWN;
    }
    
    /**
     * Gets Post File Name.
     * It is only for 'Rename' of 'Post Transfer Command'.
     * The name on the external system where file will be renamed
     * or copied to. It represents the base file name instead of
     * full file name.
     * <p>Special characters are allowed. The expansion of any
     * special characters are carried out each time this parameter
     * is used.
     * @return    The Post File Name.
     */
    public String getPostFileName() {
        return this.postFileName;
    }
    
    /**
     * Checks whether the post file name is a pattern or a literal name.
     * <p>This value indicates whether the post file name is a pattern or a literal name.
     * @return    Whether the post file name is a pattern.
     */
    public boolean getPostFileNameIsPattern() {
        return this.postFileNameIsPattern;
    }
    
    /**
     * Gets Pre File Name.
     * It is only for 'Rename' or 'Copy' of 'Pre Transfer Command'.
     * The name on the external system where file will be renamed
     * or copied to. It represents the base file name instead of
     * full file name.
     * <p>Special characters are allowed. The expansion of any
     * special characters are carried out each time this parameter
     * is used.
     * @return    The Pre File Name.
     */
    public String getPreFileName() {
        return this.preFileName;
    }
    
    /**
     * Checks whether the pre file name is a pattern or a literal name.
     * @return    Whether the pre file name is a pattern.
     */
    public boolean getPreFileNameIsPattern() {
        return this.preFileNameIsPattern;
    }
    
    /**
     * Checks whether the post file name is a pattern or a literal name.
     * @return    Whether the post file name is a pattern.
     */
    public boolean isPostFileNameIsPattern() {
        return this.postFileNameIsPattern;
    }
    
    /**
     * Checks whether the pre file name is a pattern or a literal name.
     * @return    Whether the pre file name is a pattern.
     */
    public boolean isPreFileNameIsPattern() {
        return this.preFileNameIsPattern;
    }
    
    /**
     * Checks whether the target file name is a pattern or a literal name.
     * @return    Whether the target file name is a pattern.
     */
    public boolean isTargetFileNameIsPattern() {
        return this.targetFileNameIsPattern;
    }
    
    /**
     * Sets Post File Name.
     * It is only for 'Rename' of 'Post Transfer Command'.
     * The name on the external system where file will be renamed
     * or copied to. It represents the base file name instead of
     * full file name.
     * <p>Special characters are allowed. The expansion of any
     * special characters are carried out each time this parameter
     * is used.
     * @param       newPostFileName  The Post File Name.
     */
    public void setPostFileName(String newPostFileName) {
        String oldPostFileName = this.postFileName;
        this.postFileName = newPostFileName;
        if (this.postFileName == null) {
            this.postFileName = "";
        }
        this.postFileName = this.postFileName.trim();
        if (oldPostFileName == null ||
                !oldPostFileName.equals(this.postFileName)) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Sets Pattern/Literal option for Post File Name.
     * @param   newPostFileNameIsPattern   true or false
     */
    public void setPostFileNameIsPattern(boolean newPostFileNameIsPattern) {
        boolean oldPostFileNameIsPattern = this.postFileNameIsPattern;
        this.postFileNameIsPattern = newPostFileNameIsPattern;
        if (oldPostFileNameIsPattern != this.postFileNameIsPattern) {
            this.relationChanged = true;
        }
    }
    
    /**
     * Sets Pre File Name.
     * It is only for 'Rename' or 'Copy' of 'Pre Transfer Command'.
     * The name on the external system where file will be renamed
     * or copied to. It represents the base file name instead of
     * full file name.
     * <p>Special characters are allowed. The expansion of any
     * special characters are carried out each time this parameter
     * is used.
     * @param       newPreFileName  The Pre File Name.
     */
    public void setPreFileName(String newPreFileName) {
        String oldPreFileName = this.preFileName;
        this.preFileName = newPreFileName;
        if (this.preFileName == null) {
            this.preFileName = "";
        }
        this.preFileName = this.preFileName.trim();
        if (oldPreFileName == null ||
                !oldPreFileName.equals(this.preFileName)) {
            // really changed
            this.relationChanged = true;
        }
    }
    
    /**
     * Sets Pattern/Literal option for Pre File Name.
     * @param   newPreFileNameIsPattern   true or false
     */
    public void setPreFileNameIsPattern(boolean newPreFileNameIsPattern) {
        boolean oldPreFileNameIsPattern = this.preFileNameIsPattern;
        this.preFileNameIsPattern = newPreFileNameIsPattern;
        if (oldPreFileNameIsPattern != this.preFileNameIsPattern) {
            this.relationChanged = true;
        }
    }
    
    /**
     * A helper method used for ETD configuration sub-node.
     * @return    An instance of FtpFileConfiguration.
     */
    public FtpFileConfiguration getConnector() {
        return this;
    }
    
    /**
     * Gets the encrypted password.
     * <p>This value is the encrypted password that corresponds to the current user name.
     * There is no default value.
     * @return    The encrypted password.
     */
    public String getEncryptedPassword() throws FtpFileException {
        try {
            return NonEmptyScEncrypt.encrypt(this.userName, this.password);
        } catch (Exception e) {
            this.msg = mMessages.getString("FTPBC-E006033.ERR_EXT_FTP_ENCRYPT_EXCEPTION",
                    new Object[] {
                "getEncryptedPassword()",
                e
            });
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg);
            }
            throw new FtpFileException(this.msg);
        }
    }
    
    /**
     * A helper method used for ETD configuration sub-node.
     * @return    An instance of FtpFileConfiguration.
     */
    public FtpFileConfiguration getExtensions() {
        return this;
    }
    
    /**
     * A helper method used for ETD configuration sub-node.
     * @return    An instance of FtpFileConfiguration.
     */
    public FtpFileConfiguration getFtp() {
        return this;
    }
    
    /**
     * A helper method used for ETD configuration sub-node.
     * @return    An instance of FtpFileConfiguration.
     */
    public FtpFileConfiguration getFTPRawCommands() {
        return this;
    }
    
    /**
     * A helper method used for ETD configuration sub-node.
     * @return    An instance of FtpFileConfiguration.
     */
    public FtpFileConfiguration getGeneralSettings() {
        return this;
    }
    
    /**
     * A helper method used for ETD configuration sub-node.
     * @return    An instance of FtpFileConfiguration.
     */
    public FtpFileConfiguration getPostTransfer() {
        return this;
    }
    
    /**
     * A helper method used for ETD configuration sub-node.
     * @return    An instance of FtpFileConfiguration.
     */
    public FtpFileConfiguration getPreTransfer() {
        return this;
    }
    
    /**
     * A helper method used for ETD configuration sub-node.
     * @return    An instance of FtpFileConfiguration.
     */
    public FtpFileConfiguration getSequenceNumbering() {
        return this;
    }
    
    /**
     * A helper method used for ETD configuration sub-node.
     * @return    An instance of FtpFileConfiguration.
     */
    public FtpFileConfiguration getSOCKS() {
        return this;
    }
    
    /**
     * Gets the encrypted SOCKS password.
     * <p>This value is the encrypted SOCKS password that corresponds to the SOCKS user name.
     * There is no default value.
     * @return    The encrypted SOCKS Password.
     */
    public String getSocksEncryptedPassword() throws FtpFileException {
        try {
            return NonEmptyScEncrypt.encrypt(this.socksUserName, this.socksPassword);
        } catch (Exception e) {
            this.msg = mMessages.getString("FTPBC-E006033.ERR_EXT_FTP_ENCRYPT_EXCEPTION",
                    new Object[] {
                "getSocksEncryptedPassword()",
                e
            });
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, this.msg);
            }
            throw new FtpFileException(this.msg);
        }
    }
    
    /**
     * A helper method used for ETD configuration sub-node.
     * @return    An instance of FtpFileConfiguration.
     */
    public FtpFileConfiguration getTargetLocation() {
        return this;
    }
    
    /**
     * A helper method used for ETD configuration sub-node.
     * @return    An instance of FtpFileConfiguration.
     */
    public FtpFileConfiguration getDynamicConfiguration() {
        return this;
    }
    
    public void setUserHeuristicsLocation(String userHeuristicLoc) {
        this.mUserHeuristicsLoc = userHeuristicLoc;
    }
    
    public String getUserHeuristicsLocation() {
        return this.mUserHeuristicsLoc;
    }
    
    private long mMaxRetry;
    private long mRetryInterval;
    
    public void setMaxRetry(long aRetry) {
        this.mMaxRetry = aRetry;
    }
    
    public long getMaxRetry() {
        return this.mMaxRetry;
    }
    
    public void setRetryInterval(long aInterval) {
        this.mRetryInterval = aInterval;
    }
    
    public long getRetryInterval() {
        return this.mRetryInterval;
    }
    
    protected void setSynchronized(boolean b) {
        this.bSynchronized=b;
    }
    
    public boolean getSynchronized() {
        return this.bSynchronized;
    }
    
    protected String getOID() {
        return this.getProperty("conn-props.collaboration.oid");
    }
    
    protected String getExternalName() {
        return this.getProperty("conn-props.connection.name");
    }
    
    private String getProperty(String key) {
        return this.mConfigProps.getProperty(key);
    }

    // begin --- added setters and getters for FTP/TLS parameters
    public String getSecureFTPType() {
        return mSecureFTPType;
    }

    public void setSecureFTPType(String s) {
        mSecureFTPType = s;
    }
    public String getKeyStoreLoc() {
        return mKeyStoreLoc;
    }

    public void setKeyStoreLoc(String s) {
        mKeyStoreLoc = s;
    }
    
    public String getKeyStorePassword() {
        return mKeyStorePassword;
    }

    public void setKeyStorePassword(String s) {
        mKeyStorePassword = s;
    }
    
    public String getKeyAlias() {
        return mKeyAlias;
    }

    public void setKeyAlias(String s) {
        mKeyAlias = s;
    }
    public String getKeyPassword() {
        return mKeyPassword;
    }

    public void setKeyPassword(String s) {
        mKeyPassword = s;
    }
    
    public String getTrustStoreLoc() {
        return mTrustStoreLoc;
    }

    public void setTrustStoreLoc(String s) {
        mTrustStoreLoc = s;
    }
    
    public String getTrustStorePassword() {
        return mTrustStorePassword;
    }

    public void setTrustStorePassword(String s) {
        mTrustStorePassword = s;
    }
    
    public boolean getEnableCCC() {
        return bEnableCCC;
    }

    public void setEnableCCC(boolean b) {
        bEnableCCC = b;
    }
    // end --- added setters and getters for FTP/TLS parameters

    // for accessing infos
    // regarding current endpoint processed
    public Endpoint getCurrentEndpoint() {
        return mCurrEP;
    }

    public void setCurrentEndpoint(Endpoint ep) {
        mCurrEP = ep;
    }
    
    public QName getCurrentOperationName() {
        return mOpName;
    }

    public void setCurrentOperationName(QName opName) {
        mOpName = opName;
    }

    // calculate a key which identify the 
    // connection relevant characteristics of the
    // configuration 
    // key = connection parameters concatenated with "|" as the delimiter
    public String getKey() throws Exception {
        StringBuffer sb = new StringBuffer();
        sb.append(this.getHostName()).append("|");
        sb.append(this.getServerPort()).append("|");
        sb.append(this.getUserName()).append("|");
        sb.append(this.getPassword()).append("|");
        sb.append(this.getUsePASV()).append("|");
        sb.append(this.isSocksEnabled()).append("|");
        
        if ( this.isSocksEnabled() ) {
            sb.append(this.getSocksHostName()).append("|");
            sb.append(this.getSocksServerPort()).append("|");
            sb.append(this.getSocksUserName()).append("|");
            sb.append(this.getSocksPassword()).append("|");
            sb.append(this.getSocksVersion()).append("|");
        }

        // normalize the charset name to upper case
        String p = this.getControlChannelEncoding() != null ? this.getControlChannelEncoding() : FtpFileConfigConstants.DEFAULT_CNTRL_ENCODING;
        sb.append(p.toUpperCase()).append("|");

        String secFTPType = this.getSecureFTPType();
        
        if ( secFTPType != null && secFTPType.trim().length() > 0 ) {
            if ( secFTPType.equals(FtpFileConfigConstants.FTP_SECURE_EXPLICITSSL) || secFTPType.equals(FtpFileConfigConstants.FTP_SECURE_IMPLICITSSL)) {
                sb.append(this.getKeyStoreLoc()).append("|");
                sb.append(this.getKeyStorePassword()).append("|");
                sb.append(this.getTrustStoreLoc()).append("|");
                sb.append(this.getTrustStorePassword()).append("|");
                sb.append(this.getKeyAlias()).append("|");
                sb.append(this.getKeyPassword()).append("|");
                if ( secFTPType.equals(FtpFileConfigConstants.FTP_SECURE_EXPLICITSSL) ) {
                    sb.append(this.getEnableCCC()).append("|");
                }
            }
            else {
                // must be "None"
            }
        }
        else {
            secFTPType = "None";
        }
        
        sb.append(secFTPType).append("|");
        
        return sb.toString();
    }
    
    // a util for calculating connection key out of connection parameters
    // given in <code>cfg</code>
    public static final String getKey(Properties cfg) {
        StringBuffer sb = new StringBuffer();
        sb.append(cfg.getProperty(FtpFileConfigConstants.C_P_FTP_HOST, "localhost")).append("|");
        sb.append(cfg.get(FtpFileConfigConstants.C_P_FTP_PORT) != null ? cfg.get(FtpFileConfigConstants.C_P_FTP_PORT).toString() : "").append("|");
        sb.append(cfg.getProperty(FtpFileConfigConstants.C_P_FTP_USR)).append("|");
        sb.append(cfg.getProperty(FtpFileConfigConstants.C_P_FTP_PASSWD)).append("|");
        String p = cfg.getProperty(FtpFileConfigConstants.C_P_FTP_PASSIVE_ON);
        
        p = (p != null && p.equalsIgnoreCase("Yes")) ? "true" : "false";
        sb.append(p).append("|");
        p = cfg.getProperty(FtpFileConfigConstants.C_P_SOC_ON);
        
        if ( p != null && p.equalsIgnoreCase("Yes") ) {
            sb.append("true").append("|");
            sb.append(cfg.getProperty(FtpFileConfigConstants.C_P_SOC_HOST)).append("|");
            sb.append(cfg.get(FtpFileConfigConstants.C_P_SOC_PORT) != null ? cfg.get(FtpFileConfigConstants.C_P_SOC_PORT).toString() : "").append("|");
            sb.append(cfg.getProperty(FtpFileConfigConstants.C_P_SOC_USR)).append("|");
            sb.append(cfg.getProperty(FtpFileConfigConstants.C_P_SOC_PASSWD)).append("|");
            sb.append(cfg.getProperty(FtpFileConfigConstants.C_P_SOC_VER)).append("|");
        }
        else {
            sb.append("false").append("|");
        }

        p = cfg.getProperty(FtpFileConfigConstants.C_P_FTP_CNTRL_ENCODING);
        
        if ( p == null || p.trim().length() == 0 )
            p = FtpFileConfigConstants.DEFAULT_CNTRL_ENCODING;
        
        sb.append(p.toUpperCase()).append("|");

        String secFTPType = cfg.getProperty(FtpFileConfigConstants.C_P_SECURE_FTP_TYPE);
        
        if ( secFTPType != null && secFTPType.trim().length() > 0 ) {
            if ( secFTPType.equals(FtpFileConfigConstants.FTP_SECURE_EXPLICITSSL) || secFTPType.equals(FtpFileConfigConstants.FTP_SECURE_IMPLICITSSL)) {
                sb.append(cfg.getProperty(FtpFileConfigConstants.C_P_KEY_STORE_LOC)).append("|");
                sb.append(cfg.getProperty(FtpFileConfigConstants.C_P_KEY_STORE_PASSWORD)).append("|");
                sb.append(cfg.getProperty(FtpFileConfigConstants.C_P_TRUST_STORE_LOC)).append("|");
                sb.append(cfg.getProperty(FtpFileConfigConstants.C_P_TRUST_STORE_PASSWORD)).append("|");
                sb.append(cfg.getProperty(FtpFileConfigConstants.C_P_KEY_ALIAS)).append("|");
                sb.append(cfg.getProperty(FtpFileConfigConstants.C_P_KEY_PASSWORD)).append("|");
                if ( secFTPType.equals(FtpFileConfigConstants.FTP_SECURE_EXPLICITSSL) ) {
                    sb.append(cfg.getProperty(FtpFileConfigConstants.C_P_ENABLE_CCC)).append("|");
                }
            }
            else {
                // must be "None"
            }
        }
        else {
            secFTPType = "None";
        }
        
        sb.append(secFTPType).append("|");

        return sb.toString();
    }
}
