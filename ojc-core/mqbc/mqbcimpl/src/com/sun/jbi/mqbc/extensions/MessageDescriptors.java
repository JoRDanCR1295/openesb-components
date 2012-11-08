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
 */

/*
 * @(#)$Id: MessageDescriptors.java,v 1.1 2008/11/12 23:00:19 noel_ang Exp $
 *
 * Copyright 2008-2011 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extensions;

import java.util.Calendar;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import javax.xml.namespace.QName;

import static com.sun.jbi.mqbc.extensions.XmlSchemaDataTypes.*;

/**
 * The set of all (known and supported) MQ message descriptors.
 *
 * @author Noel.Ang@sun.com
 */
public enum MessageDescriptors {
    accountingToken       (byte[].class,   BASE64, HEXBINARY),
    applicationId         (String.class,   STRING),
    applicationOrigin     (String.class,   STRING),
    backoutCount          (int.class,      INTEGER, STRING),
    characterSet          (int.class,      INTEGER, STRING),
    correlationId         (byte[].class,   BASE64, HEXBINARY),
    encoding              (int.class,      INTEGER, STRING),
    expiry                (int.class,      INTEGER, STRING),
    feedback              (int.class,      INTEGER, STRING),
    format                (String.class,   STRING),
    groupId               (byte[].class,   BASE64, HEXBINARY),
    messageFlags          (int.class,      INTEGER, STRING),
    messageId             (byte[].class,   BASE64, HEXBINARY),
    messageSequenceNumber (int.class,      INTEGER, STRING),
    messageType           (int.class,      INTEGER, STRING),
    offset                (int.class,      INTEGER, STRING),
    originalLength        (int.class,      INTEGER, STRING),
    persistence           (int.class,      INTEGER, STRING),
    priority              (int.class,      INTEGER, STRING),
    putApplicationName    (String.class,   STRING),
    putApplicationType    (int.class,      INTEGER, STRING),
    putDateTime           (Calendar.class, DATETIME, STRING),
    replyToQueueManager   (String.class,   STRING),
    replyToQueueName      (String.class,   STRING),
    report                (int.class,      INTEGER, STRING),
    userId                (String.class,   STRING),
    ;
        
    public boolean isRepresentableAs(QName type) {
        return types.contains(type);
    }
    
    public Set<QName> getRepresentations() {
        return Collections.unmodifiableSet(types);
    }
    
    public Class getNativeRepresentation() {
        return javaType;
    }
    
    private MessageDescriptors(Class javaType, XmlSchemaDataTypes... dataTypes) {
        this.javaType = javaType;
        types = new HashSet<QName>();
        for (XmlSchemaDataTypes type : dataTypes) {
            types.add(type.qname);
        }
    }
        
    private final Set<QName> types;
    private final Class javaType;
}
