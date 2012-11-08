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
 * @(#)FTPConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc.extensions;

import javax.xml.namespace.QName;
import com.ibm.wsdl.Constants;

/**
 *
 * @author jim.fu@sun.com
 */
public class FTPConstants {

    public FTPConstants() {
    }
    // Namespaces
    public static final String NS_URI_FTP = "http://schemas.sun.com/jbi/wsdl-extensions/ftp/";
    // Local element names
    public static final String ELEM_ADDRESS = "address";
    public static final String ELEM_TRANSFER = "transfer";
    public static final String ELEM_MESSAGE = "message";
    //public static final String ELEM_MESSAGE_W_MODE = "messageActivePassive";
    // Qualified element names
    public static final QName QNAME_BINDING = new QName(NS_URI_FTP, Constants.ELEM_BINDING);
    public static final QName QNAME_OPERATION = new QName(NS_URI_FTP, Constants.ELEM_OPERATION);
    public static final QName QNAME_ADDRESS = new QName(NS_URI_FTP, ELEM_ADDRESS);
    public static final QName QNAME_TRANSFER = new QName(NS_URI_FTP, ELEM_TRANSFER);
    public static final QName QNAME_MESSAGE = new QName(NS_URI_FTP, ELEM_MESSAGE);
    //public static final QName QNAME_MESSAGE_W_MODE = new QName(NS_URI_FTP, ELEM_MESSAGE_W_MODE);
    // Literals for message correlation contract
    public static final String MSG_CORRELATE_REQ_PREFIX = "req.";
    public static final String MSG_CORRELATE_RESP_PREFIX = "resp.";
    // Sub-directory names for in-coming message, out-going message
    // , staging message, protected messages, and archived messages
    public static final String MSG_IN_BOX = "inbox";
    public static final String MSG_OUT_BOX = "outbox";
    public static final String MSG_IN_STAGE = "instage";
    public static final String MSG_OUT_STAGE = "outstage";
    public static final String MSG_IN_SELECTED = "inselect";
    public static final String MSG_OUT_SELECTED = "outselect";
    public static final String MSG_IN_ARCHIVE = "inarchive";
    public static final String MSG_OUT_ARCHIVE = "outarchive";
    public static final String MSG_IN_PROTECT = "inprotect";
    public static final String MSG_OUT_PROTECT = "outprotect";
    public static final String MSG_REPO_PATH_SEP = "/";
    public static final String EXT_ELEM_ATTR_USE_ENCODED = "encoded";
    public static final String EXT_ELEM_ATTR_USE_LITERAL = "literal";
    public static final String APP_VAR_TYPE_NUMBER = "NUMBER";
    public static final String APP_VAR_TYPE_BOOLEAN = "BOOLEAN";
    public static final String APP_VAR_TYPE_STRING = "STRING";
    public static final String APP_VAR_TYPE_PASSWORD = "PASSWORD";
}
