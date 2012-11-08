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
 * @(#)JMSConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.extensions;

import javax.xml.namespace.QName;
import com.ibm.wsdl.Constants;

/**
 *
 */
public class JMSConstants {
    
    public JMSConstants() {
    }
    
    // Namespaces
    public static final String NS_URI_JMS = "http://schemas.sun.com/jbi/wsdl-extensions/jms/";

    // Local element names
    public static final String ELEM_ADDRESS = "address";
    public static final String ELEM_MESSAGE = "message";
    public static final String ELEM_MAPMESSAGE = "mapmessage";
    public static final String ELEM_MAPPART = "mappart";
    public static final String ELEM_PROPERTY = "property";
    public static final String ELEM_PROPERTIES = "properties";
    public static final String ELEM_OPTION = "option";
    public static final String ELEM_OPTIONS = "options";
    public static final String ELEM_JNDIENVENTRY = "jndienventry";
    public static final String ELEM_JNDIENV = "jndienv";
    public static final String ELEM_JMSJCAOPTIONS = "jmsjcaOptions";
    public static final String ELEM_CLASSPATH = "classpath";
    
    // Qualified element names
    public static final QName QNAME_BINDING = new QName(NS_URI_JMS, Constants.ELEM_BINDING);
    public static final QName QNAME_OPERATION = new QName(NS_URI_JMS, Constants.ELEM_OPERATION);
    public static final QName QNAME_ADDRESS = new QName(NS_URI_JMS, ELEM_ADDRESS);
    public static final QName QNAME_MESSAGE = new QName(NS_URI_JMS, ELEM_MESSAGE);
    public static final QName QNAME_MAPMESSAGE = new QName(NS_URI_JMS, ELEM_MAPMESSAGE);
    public static final QName QNAME_MAPPART = new QName(NS_URI_JMS, ELEM_MAPPART);
    public static final QName QNAME_PROPERTY = new QName(NS_URI_JMS, ELEM_PROPERTY);
    public static final QName QNAME_PROPERTIES = new QName(NS_URI_JMS, ELEM_PROPERTIES);
    public static final QName QNAME_OPTION = new QName(NS_URI_JMS, ELEM_OPTION);
    public static final QName QNAME_OPTIONS = new QName(NS_URI_JMS, ELEM_OPTIONS);
    public static final QName QNAME_JNDIENVENTRY = new QName(NS_URI_JMS, ELEM_JNDIENVENTRY);
    public static final QName QNAME_JNDIENV = new QName(NS_URI_JMS, ELEM_JNDIENV);
    public static final QName QNAME_JMSJCAOPTIONS = new QName(NS_URI_JMS, ELEM_JMSJCAOPTIONS);
    public static final QName QNAME_CLASSPATH = new QName(NS_URI_JMS, ELEM_CLASSPATH);
    
    // Destination Type (domain)
    public static final String QUEUE = "Queue";
    public static final String TOPIC = "Topic";
    
    // Transaction Support
    public static final String TRANSACTION_NONE  = "NoTransaction";
    public static final String TRANSACTION_LOCAL = "LocalTransaction";
    public static final String TRANSACTION_XA    = "XATransaction";
    
    // Delivery Modes
    public static final String DELIVERYMODE_PERSISTENT = "PERSISTENT";
    public static final String DELIVERYMODE_NON_PERSISTENT = "NON_PERSISTENT";
    
    // Delivery Priority
    public static final int PRIORITY_0 = 0;
    public static final int PRIORITY_1 = 1;
    public static final int PRIORITY_2 = 2;
    public static final int PRIORITY_3 = 3;
    public static final int PRIORITY_4 = 4;
    public static final int PRIORITY_5 = 5;
    public static final int PRIORITY_6 = 6;
    public static final int PRIORITY_7 = 7;
    public static final int PRIORITY_8 = 8;
    public static final int PRIORITY_9 = 9;
    public static final int PIRORITY_DEFAULT = PRIORITY_4;
    
    // Message Types
    public static final String TEXT_MESSAGE = "TextMessage";
    public static final String STREAM_MESSAGE = "StreamMessage";
    public static final String BYTES_MESSAGE = "BytesMessage";
    public static final String MAP_MESSAGE = "MapMessage";
    public static final String OBJECT_MESSAGE = "ObjectMessage";
    public static final String MESSAGE_MESSAGE = "Message";
    
    // Acknowlegement Modes
    public static final String AUTO_ACKNOWLEDGE   = "AUTO_ACKNOWLEDGE";
    public static final String CLIENT_ACKNOWLEDGE = "CLIENT_ACKNOWLEDGE";
    public static final String DUPS_OK_ACKNOWLEGE = "DUPS_OK_ACKNOWLEGE";    
    
    // Subscription Durability Types
    public static final String DURABLE  = "Durable";
    public static final String NON_DURABLE = "NonDurable";    
    
    // JNDI connection URL
    public static final String JNDI_GENERIC_JMS_SCHEME = "jndi://";
    
    //Lookup URL
    public static final String LOOKUP_JMS_SCHEME = "lookup://";
}
