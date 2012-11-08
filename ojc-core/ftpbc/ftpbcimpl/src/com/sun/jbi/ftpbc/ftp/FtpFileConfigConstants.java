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
 * @(#)FtpFileConfigConstants.java 
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
 * @author jim.fu@sun.com
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public interface FtpFileConfigConstants {
    public static final String DEFAULT_CNTRL_ENCODING = "ISO-8859-1";
    public static final String ENCODING_SJIS = "pck";
    public static final String ENCODING_EUC_JP = "euc-jp";
    public static final String ENCODING_JIS = "iso-2022-jp";

    public static final String FTP_SECURE_NONE = "None";
    public static final String FTP_SECURE_EXPLICITSSL = "ExplicitSSL";
    public static final String FTP_SECURE_IMPLICITSSL = "ImplicitSSL";
    
    public static final String P_COLLAB_OID = "conn-props.collaboration.oid";
    public static final String P_CONN_NAME = "conn-props.connection.name";
    
    public static final String CONNRETRY_MAXRETRIES     = "connection-retry-settings/ConnectionRetries";
    public static final String CONNRETRY_INTERVAL       = "connection-retry-settings/ConnectionRetryInterval";
    
    public static final String S_FTP = "FTP";
    public static final String S_PRE_TRNS = "Pre Transfer";
    public static final String S_POST_TRNS = "Post Transfer";
    public static final String S_GEN_SETTINGS = "General Settings";
    public static final String S_SOCKS = "SOCKS";
    public static final String S_FTP_RAW = "FTP Raw Commands";
    public static final String S_TARGET = "Target Location";
    public static final String S_SEQ = "Sequence Numbering";
    public static final String S_EXTENSION = "Extensions";
    
    public static final String P_TRNS_TYPE = "Transaction Type";
    public static final String P_BASE_LOC = "State Persistence Base Location";
    public static final String P_SYNC = "Synchronized";
    public static final String P_CONN_MODE = "Connection Mode";

    public static final String P_GEN_TRNS_TYPE = S_GEN_SETTINGS.concat("/").concat(P_TRNS_TYPE) ;
    public static final String P_GEN_BASE_LOC = S_GEN_SETTINGS.concat("/").concat(P_BASE_LOC);
    public static final String C_P_GEN_SYNC = S_GEN_SETTINGS.concat("/").concat(P_SYNC);
    public static final String C_P_GEN_CONN_MODE = S_GEN_SETTINGS.concat("/").concat(P_CONN_MODE);
    
    // section "FTP"
    public static final String P_LST_STYLE = "Directory Listing Style";
    public static final String P_HOST = "Host Name";
    public static final String P_USR = "User Name";
    public static final String P_PASSWD = "Password";
    public static final String P_UDH_CFG = "User Defined Heuristics Configuration File";
    public static final String P_UDH_LST_STYLE = "User Defined Directory Listing Style";
    public static final String P_PORT = "Server Port";
    public static final String P_MODE = "Mode";
    public static final String P_PASSIVE_ON = "Use PASV";
    public static final String P_CMD_TIMEOUT = "Command Connection Timeout";
    public static final String P_DAT_TIMEOUT = "Data Connection Timeout";
    
    public static final String P_CNTRL_ENCODING = "Control Channel Encoding";

    // FTP section:
    // ADDED SUPPORT FOR FTP/TLS explicit and implicit
    public static final String P_SECURE_FTP_TYPE = "Secure FTP Type";
    public static final String P_KEY_STORE_LOC = "Key Store Location";
    public static final String P_KEY_STORE_PASSWORD = "Key Store Password";
    public static final String P_KEY_ALIAS = "Key Alias";
    public static final String P_KEY_PASSWORD = "Key Password";
    public static final String P_TRUST_STORE_LOC = "Trust Store Location";
    public static final String P_TRUST_STORE_PASSWORD = "Trust Store Password";
    public static final String P_ENABLE_CCC = "Enable CCC";
    
    public static final String C_P_FTP_LST_STYLE = S_FTP.concat("/").concat(P_LST_STYLE);
    public static final String C_P_FTP_HOST = S_FTP.concat("/").concat(P_HOST);
    public static final String C_P_FTP_USR = S_FTP.concat("/").concat(P_USR);
    public static final String C_P_FTP_PASSWD = S_FTP.concat("/").concat(P_PASSWD);
    public static final String C_P_FTP_UDH_CFG = S_FTP.concat("/").concat(P_UDH_CFG);
    public static final String C_P_FTP_UDH_LST_STYLE = S_FTP.concat("/").concat(P_UDH_LST_STYLE);
    public static final String C_P_FTP_PORT = S_FTP.concat("/").concat(P_PORT);
    public static final String P_FTP_MODE = S_FTP.concat("/").concat(P_MODE);
    public static final String C_P_FTP_PASSIVE_ON = S_FTP.concat("/").concat(P_PASSIVE_ON);
    public static final String P_FTP_CMD_TIMEOUT = S_FTP.concat("/").concat(P_CMD_TIMEOUT);
    public static final String P_FTP_DAT_TIMEOUT = S_FTP.concat("/").concat(P_DAT_TIMEOUT);

    public static final String C_P_FTP_CNTRL_ENCODING = S_FTP.concat("/").concat(P_CNTRL_ENCODING);

    // Added for FTP/TLS connection params
    // connection parameters are used when looking up a matching connection 
    // from the connection pool
    public static final String C_P_SECURE_FTP_TYPE = S_FTP.concat("/").concat(P_SECURE_FTP_TYPE);
    public static final String C_P_KEY_STORE_LOC = S_FTP.concat("/").concat(P_KEY_STORE_LOC);
    public static final String C_P_KEY_STORE_PASSWORD = S_FTP.concat("/").concat(P_KEY_STORE_PASSWORD);
    public static final String C_P_KEY_ALIAS = S_FTP.concat("/").concat(P_KEY_ALIAS);
    public static final String C_P_KEY_PASSWORD = S_FTP.concat("/").concat(P_KEY_PASSWORD);
    public static final String C_P_TRUST_STORE_LOC = S_FTP.concat("/").concat(P_TRUST_STORE_LOC);
    public static final String C_P_TRUST_STORE_PASSWORD = S_FTP.concat("/").concat(P_TRUST_STORE_PASSWORD);
    public static final String C_P_ENABLE_CCC = S_FTP.concat("/").concat(P_ENABLE_CCC);
    
    // section "Target Location"
    public static final String P_DIR = "Target Directory Name";
    public static final String P_DIR_PATT = "Target Directory Name Is Pattern";
    public static final String P_FILE = "Target File Name";
    public static final String P_FILE_PATT = "Target File Name Is Pattern";
    public static final String P_APPND = "Append";

    public static final String P_TGT_DIR = S_TARGET.concat("/").concat(P_DIR);
    public static final String P_TGT_DIR_PATT = S_TARGET.concat("/").concat(P_DIR_PATT);
    public static final String P_TGT_FILE = S_TARGET.concat("/").concat(P_FILE);
    public static final String P_TGT_FILE_PATT = S_TARGET.concat("/").concat(P_FILE_PATT);
    public static final String P_TGT_APPND = S_TARGET.concat("/").concat(P_APPND);
    
    // section "Pre Transfer"
    public static final String P_PR_CMD = "Pre Transfer Command";
    public static final String P_PR_DIR = "Pre Directory Name";
    public static final String P_PR_DIR_PATT = "Pre Directory Name Is Pattern";
    public static final String P_PR_FILE = "Pre File Name";
    public static final String P_PR_FILE_PATT = "Pre File Name Is Pattern";

    public static final String P_PRE_CMD = S_PRE_TRNS.concat("/").concat(P_PR_CMD);
    public static final String P_PRE_DIR = S_PRE_TRNS.concat("/").concat(P_PR_DIR);
    public static final String P_PRE_DIR_PATT = S_PRE_TRNS.concat("/").concat(P_PR_DIR_PATT);
    public static final String P_PRE_FILE = S_PRE_TRNS.concat("/").concat(P_PR_FILE);
    public static final String P_PRE_FILE_PATT = S_PRE_TRNS.concat("/").concat(P_PR_FILE_PATT);
    
    // section "Post Transfer"
    public static final String P_PO_CMD = "Post Transfer Command";
    public static final String P_PO_DIR = "Post Directory Name";
    public static final String P_PO_DIR_PATT = "Post Directory Name Is Pattern";
    public static final String P_PO_FILE = "Post File Name";
    public static final String P_PO_FILE_PATT = "Post File Name Is Pattern";

    public static final String P_POST_CMD = S_POST_TRNS.concat("/").concat(P_PO_CMD);
    public static final String P_POST_DIR = S_POST_TRNS.concat("/").concat(P_PO_DIR);
    public static final String P_POST_DIR_PATT = S_POST_TRNS.concat("/").concat(P_PO_DIR_PATT);
    public static final String P_POST_FILE = S_POST_TRNS.concat("/").concat(P_PO_FILE);
    public static final String P_POST_FILE_PATT = S_POST_TRNS.concat("/").concat(P_PO_FILE_PATT);
    
    // section "Staging"
    public static final String P_STAGE_ENABLED = "Staging Enabled";
    public static final String P_STAGE_DIR = "Staging Dir";
    public static final String P_STAGE_DIR_IS_PATT = "Staging Dir Is Pattern";
    public static final String P_STAGE_FILE = "Staging File";
    public static final String P_STAGE_FILE_IS_PATT = "Staging File Is Pattern";

    // section "FTP Raw Commands"
    public static final String P_RAW_PRE_CMD = S_FTP_RAW + "/Pre Transfer Raw Commands";
    public static final String P_RAW_POST_CMD = S_FTP_RAW + "/Post Transfer Raw Commands";
    
    // section "Sequence Numbering"
    public static final String P_S_START = "Starting Sequence Number";
    public static final String P_S_MAX = "Max Sequence Number";

    public static final String P_SEQ_START = S_SEQ.concat("/").concat(P_S_START);
    public static final String P_SEQ_MAX = S_SEQ.concat("/").concat(P_S_MAX);
    
    // section "SOCKS"
    public static final String P_SOCKS_ON = "Socks Enabled";
    public static final String P_SOCKS_HOST = "Socks Host Name";
    public static final String P_SOCKS_PORT = "Socks Server Port";
    public static final String P_SOCKS_USR = "Socks User Name";
    public static final String P_SOCKS_PASSWD = "Socks Password";
    public static final String P_SOCKS_VER = "Socks Version";

    public static final String C_P_SOC_ON = S_SOCKS.concat("/").concat(P_SOCKS_ON);
    public static final String C_P_SOC_HOST = S_SOCKS.concat("/").concat(P_SOCKS_HOST);
    public static final String C_P_SOC_PORT = S_SOCKS.concat("/").concat(P_SOCKS_PORT);
    public static final String C_P_SOC_USR = S_SOCKS.concat("/").concat(P_SOCKS_USR);
    public static final String C_P_SOC_PASSWD = S_SOCKS.concat("/").concat(P_SOCKS_PASSWD);
    public static final String C_P_SOC_VER = S_SOCKS.concat("/").concat(P_SOCKS_VER);

    // extension section
    public static final String P_EXT_PROVIDER_CLAZZ = "Provider Class Name";
    public static final String P_EXT_CLIENT_CLAZZ = "Client Class Name";

    public static final String P_EXTENSION_PROVIDER_CLAZZ = S_EXTENSION.concat("/").concat(P_EXT_PROVIDER_CLAZZ);
    public static final String P_EXTENSION_CLIENT_CLAZZ = S_EXTENSION.concat("/").concat(P_EXT_CLIENT_CLAZZ);
}
