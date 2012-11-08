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
 * @(#)Socks.java 
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

import java.net.InetAddress;
import java.net.UnknownHostException;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

/**
 * This implementation is for the SOCKS (V4, V4A & V5) protocol.
 *
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */
public class Socks implements SocksProxy {
    private static final Messages mMessages =
            Messages.getMessages(Socks.class);
    private static final Logger mLogger =
            Messages.getLogger(Socks.class);
    
    private String socksHost = null;
    private int socksPort = 0;
    private String socksPassword = null;
    private String socksUser = null;
    private int socksVersion = 0;
    
    /**
     * Creates the SOCKS proxy server if the SOCKS version is unknown.
     * The e*Way tries to find the version according to the response of the SOCKS server.
     * @param socksHost The host name of the SOCKS server.
     * @param socksPort The port number of the SOCKS server
     * @param socksUser The user name.
     * @param socksPassword The encrypted password for that user.
     */
    public Socks(String socksHost,
            int socksPort,
            String socksUser,
            String socksPassword) {
        this(socksHost, socksPort, socksUser, socksPassword, VERSION_UNKNOWN);
    }
    
    /**
     * Creates the SOCKS proxy server.
     * @param socksHost The host name of the SOCKS server.
     * @param socksPort The port number of the SOCKS server
     * @param socksUser The user name.
     * @param socksPassword The encrypted password for that user.
     * @param socksVersion The SOCKS version: 4, 5 or -1 (-1 means an unknown version, and
     *        the e*Way tries to find it according to the response of the SOCKS server).
     */
    public Socks(String socksHost,
            int socksPort,
            String socksUser,
            String socksPassword,
            int socksVersion) {
        String msg = null;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"Socks.Socks()"}));
        }
        this.socksHost = Socks.resolveHostName(socksHost);
        this.socksPort = socksPort;
        if (socksUser != null) {
            this.socksUser = socksUser;
        } else {
            try {
                this.socksUser = System.getProperty("user.name", "");
            } catch (SecurityException e) {
                this.socksUser = "";
            }
        }
        
        this.socksPassword = socksPassword;
        this.socksVersion = socksVersion;
    }
    
    /**
     * Creates the SOCKS proxy server using the default SOCKS port (1080).
     * If the SOCKS version is unknown, the e*Way tries to find it according to
     * the response from the SOCKS server.
     * @param socksHost The host name of the SOCKS server.
     * @param socksUser The user name.
     * @param socksPassword The encrypted password for that user.
     */
    public Socks(String socksHost,
            String socksUser,
            String socksPassword) {
        this(socksHost, DEFAULT_PORT, socksUser, socksPassword, VERSION_UNKNOWN);
    }
    
    /**
     * Creates the SOCKS proxy server using the default SOCKS port (1080).
     * @param socksHost The host name of the SOCKS server.
     * @param socksUser The user name.
     * @param socksPassword The encrypted password for that user.
     * @param socksVersion The SOCKS version: 4, 5 or -1 (-1 means an unknown version, and
     *        the e*Way tries to find it according to the response of the SOCKS server).
     */
    public Socks(String socksHost,
            String socksUser,
            String socksPassword,
            int socksVersion) {
        this(socksHost, DEFAULT_PORT, socksUser, socksPassword, socksVersion);
    }
    
    /**
     * Gets the host name on which the SOCKS server is running.
     * @return The SOCKS host name.
     */
    public String getSocksHost() {
        return this.socksHost;
    }
    
    /**
     * Gets the encrypted password for the SOCKS user.
     * @returnThe SOCKS password.
     */
    public String getSocksPassword() {
        return this.socksPassword;
    }
    
    /**
     * Gets the port on which SOCKS server is running.
     * @return The SOCKS host port number.
     */
    public int getSocksPort() {
        return this.socksPort;
    }
    
    /**
     * Gets the SOCKS user name.
     * @return The user name.
     */
    public String getSocksUser() {
        return this.socksUser;
    }
    
    /**
     * Gets the SOCKS version; 4, 5 or -1 is returned.
     * A -1 means the SOCKS version is unknown, and the e*Way tries to find it
     * according to the response from the SOCKS server.
     * @return The SOCKS version.
     */
    public int getSocksVersion() {
        return this.socksVersion;
    }
    
    /**
     * Sets the SOCKS version; 4, 5 or -1 is used.
     * A -1 means the SOCKS version is unknown, and the e*Way tries to find it
     * according to the response from the SOCKS server.
     * @param newSocksVersion The SOCKS version, that is, 4, 5 or -1 (-1 means an unknown version).
     */
    public void setSocksVersion(int newSocksVersion) throws SocksException {
        String msg = null;
        this.socksVersion = newSocksVersion;
        if (this.socksVersion != VERSION_4 &&
                this.socksVersion != VERSION_5 &&
                this.socksVersion != VERSION_UNKNOWN) {
            msg = mMessages.getString("FTPBC-E006027.ERR_EXT_FTP_INVALID_SOCKS_VER", new Object[] {
                new Integer(Socks.VERSION_4), new Integer(Socks.VERSION_5), new Integer(Socks.VERSION_UNKNOWN)
                
            });
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new SocksException(msg);
        }
    }
    
    /**
     * Indicates whether some other SOCKS object is "equal to" the current one.
     * A return of true means the two SOCKS objects have the same host names and port numbers.
     * This method is used for the class SocksChain (it is invoked when SocksChain.add() is called).
     * @param obj A Object object.
     * @return true or false.
     */
    public boolean equals(Object obj) {
        if (!(obj instanceof Socks)) {
            return super.equals(obj);
        }
        
        Socks socks = (Socks) obj;
        boolean ret = false;
        
        try {
            ret = (this.socksHost.equalsIgnoreCase(socks.getSocksHost())) &&
                    (this.socksPort == socks.getSocksPort());
        } catch (Exception e) {
            // do nothing
        }
        
        return ret;
    }
    
    
    
    /**
     * Resolves the host name.
     * Sometimes, "localhost" or "127.0.0.1" cannot be resolved properly.
     * In fact, they mean the host that is local to your module. In these cases, the e*Way
     * tries to replace an unresolvable name with the real machine name,
     * which is resolvable.
     * If any name does not need replacement, this method returns the original input name.
     * @param       hostName  The original host name, for example, "localhost" or "127.0.1.1".
     * @return      The machine name, if needed; otherwise, the original input is returned.
     */
    static String resolveHostName(String hostName) {
        String msg = null;
        if (hostName == null) {
            return hostName;
        }
        
        String newName = hostName;
        if (hostName.equalsIgnoreCase("localhost") ||
                hostName.equalsIgnoreCase("127.0.0.1")) {
            try {
                // get the machine name as the new host name
                newName = InetAddress.getLocalHost().getHostName();
                if (mLogger.isLoggable(Level.FINE))
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006056.DBG_EXT_FTP_RESOLVE_HOST", new Object[] {"Socks.resolveHostName(String hostName)", hostName, newName}));
            } catch (UnknownHostException e) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"Socks.resolveHostName(String hostName)", e}));
                }
                // don't throw exception, just try the original one and might get exception later.
            }
        }
        
        return newName;
    }
    
}
