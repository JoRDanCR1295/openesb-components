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
 * @(#)MSMQConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.extensions;

import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;

/**
 * @author Sun Microsystems
 */
public class MSMQConstants {

    public MSMQConstants() {
    }

    // Namespaces
    public static final String NS_URI_MSMQ = "http://schemas.sun.com/jbi/wsdl-extensions/msmq/";

    // Local element names
    public static final String ELEM_ADDRESS = "address";

    public static final String ELEM_MESSAGE = "message";

    // Qualified element names
    public static final QName QNAME_BINDING = new QName(NS_URI_MSMQ, Constants.ELEM_BINDING);

    public static final QName QNAME_OPERATION = new QName(NS_URI_MSMQ, Constants.ELEM_OPERATION);

    public static final QName QNAME_ADDRESS = new QName(NS_URI_MSMQ, ELEM_ADDRESS);

    public static final QName QNAME_MESSAGE = new QName(NS_URI_MSMQ, ELEM_MESSAGE);

    // Transaction Support
    public static final String NO_TRANSACTION = "NoTransaction";

    public static final String SINGLE_TRANSACTION = "SingleTransaction";

    public static final String XA_TRANSACTION = "XATransaction";
}
