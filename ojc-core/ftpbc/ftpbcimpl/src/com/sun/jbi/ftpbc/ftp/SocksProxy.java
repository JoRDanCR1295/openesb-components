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
 * @(#)SocksProxy.java 
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

/**
 * This interface is provided for the SOCKS server and client. 
 * Basically, it provides the constants that are used in the 
 * SOCKS protocol.
 *
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */

public interface SocksProxy {
    // The SOCKS conventional well-known TCP port
    public static final int DEFAULT_PORT = 1080;

    // SOCKS Protocol Version:
    // SOCKS 4 - the basic SOCKS support.
    // SOCKS 4A - a simple extension to SOCKS 4.
    // SOCKS 5 - the latest SOCKS version.
    public static final int VERSION_4 = 4;
    public static final int VERSION_4A = 4;
    public static final int VERSION_5 = 5;
    public static final int VERSION_UNKNOWN = -1;

    // NULL value (used to separate USERID)
    public static final int NULL_IND = 0;
    
    // SOCKS request commands
    public static final int COMMAND_CONNECT = 1;
    public static final int COMMAND_BIND = 2;
    public static final int COMMAND_UDP_ASSOCIATE = 3;

    // SOCKS4 reply code
    public static final int REPLY_V4_GRANTED = 90;
    public static final int REPLY_V4_REJECTED = 91;
    public static final int REPLY_V4_REJECTED_NO_IDENTD = 92;
    public static final int REPLY_V4_REJECTED_DIFF_IDENTS = 93;

    // Socks5 reply code
    public static final int REPLY_V5_SUCCEEDED = 0;
    public static final int REPLY_V5_FAILURE = 1;
    public static final int REPLY_V5_NOT_ALLOWED = 2;
    public static final int REPLY_V5_NETWORK_UNREACHABLE = 3;
    public static final int REPLY_V5_HOST_UNREACHABLE = 4;
    public static final int REPLY_V5_REFUSED = 5;
    public static final int REPLY_V5_TTL_EXPIRED = 6;
    public static final int REPLY_V5_COMMAND_NOT_SUPPORTED = 7;
    public static final int REPLY_V5_ADDRESS_TYPE_NOT_SUPPORTED = 8;
    public static final int REPLY_V5_INVALID_ADDRESS = 9;

    // SOCKS5 IP ATYP (address type)
    public static final int ADDR_TYPE_IP_V4 = 1;
    public static final int ADDR_TYPE_DOMAINNAME = 3;
    public static final int ADDR_TYPE_IP_V6 = 4;

    // SOCKS5 METHODS (method selections)
    public static final int METHOD_NO_AUTHENTICATION_REQUIRED = 0;
    public static final int METHOD_GSSAPI = 1;
    public static final int METHOD_USERNAME_PASSWORD = 2;
    public static final int METHOD_CHAP = 3;
    public static final int METHOD_NO_ACCEPTABLE_METHODS = 0xFF;

    // SOCKS5 command dependent flag (default to 0)
    public static final int CMD_FLAG = 0;
    
    // UDP ASSOCIATE request flags
    public static final int UDP_FLAG_INTERFACE_REQUEST = 1;
    public static final int UDP_FLAG_USECLIENTSPORT = 4;
    
    // UDP CONTROL CHANNEL Subcommand
    public static final int UDP_INTERFACE_DATA = 1;
    
}
