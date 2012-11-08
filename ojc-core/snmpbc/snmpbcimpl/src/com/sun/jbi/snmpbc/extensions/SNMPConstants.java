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
 * @(#)SNMPConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc.extensions;

import javax.xml.namespace.QName;
import com.ibm.wsdl.Constants;

public class SNMPConstants {
    
    public SNMPConstants() {
    }
    
    // Namespaces
    public static final String NS_URI_FILE = "http://schemas.sun.com/jbi/wsdl-extensions/snmp/";

    // Local element names
    public static final String ELEM_ADDRESS = "address";
    public static final String ELEM_MESSAGE = "message";
    
    // Qualified element names
    public static final QName QNAME_BINDING = new QName(NS_URI_FILE, Constants.ELEM_BINDING);
    public static final QName QNAME_OPERATION = new QName(NS_URI_FILE, Constants.ELEM_OPERATION);
    public static final QName QNAME_ADDRESS = new QName(NS_URI_FILE, ELEM_ADDRESS);
    public static final QName QNAME_MESSAGE = new QName(NS_URI_FILE, ELEM_MESSAGE);
    
}
