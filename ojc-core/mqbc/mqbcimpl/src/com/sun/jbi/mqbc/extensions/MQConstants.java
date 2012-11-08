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
 * @(#)MQConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extensions;

import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;



public class MQConstants {
    
    /** Creates a new instance of MQConstants */
    public MQConstants() {
    }
    
    // Namespaces
    public static final String NS_URI_MQ = "http://schemas.sun.com/jbi/wsdl-extensions/mq/";

    // Local element names
    public static final String ELEM_ADDRESS = "address";
    public static final String ELEM_MQMESSAGE = "body";
    public static final String ELEM_MQHEADER = "header";
    public static final String ELEM_REDELIVERY = "redelivery";
    public static final String ELEM_MQFAULT = "fault";
   
    // Qualified element names
    public static final QName QNAME_BINDING = new QName(NS_URI_MQ, Constants.ELEM_BINDING);
    public static final QName QNAME_OPERATION = new QName(NS_URI_MQ, Constants.ELEM_OPERATION);
    public static final QName QNAME_REDELIVERY = new QName(NS_URI_MQ, ELEM_REDELIVERY);
    public static final QName QNAME_ADDRESS = new QName(NS_URI_MQ, ELEM_ADDRESS);
    public static final QName QNAME_MQMESSAGE = new QName(NS_URI_MQ, ELEM_MQMESSAGE);
    public static final QName QNAME_MQHEADER = new QName(NS_URI_MQ, ELEM_MQHEADER);
    public static final QName QNAME_MQFAULT = new QName(NS_URI_MQ, ELEM_MQFAULT);
}
