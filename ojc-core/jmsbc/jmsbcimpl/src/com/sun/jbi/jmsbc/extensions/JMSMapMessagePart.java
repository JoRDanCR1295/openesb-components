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
 * @(#)JMSMapMessagePart.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * Represents the jms message part mapping to a jms map message object value
 */
public class JMSMapMessagePart extends JMSNamedPart implements ExtensibilityElement, Serializable {

    private static final long serialVersionUID = 1L;

    QName fieldElementType = JMSConstants.QNAME_MAPPART;

    Boolean fieldRequired = null;

    // JMS MapMessage value types
    public static final String MAPMESSAGE_TYPE_BOOLEAN = "boolean";
    public static final String MAPMESSAGE_TYPE_BYTE = "byte";
    public static final String MAPMESSAGE_TYPE_BYTES = "bytes";
    public static final String MAPMESSAGE_TYPE_CHAR = "char";
    public static final String MAPMESSAGE_TYPE_SHORT = "short";
    public static final String MAPMESSAGE_TYPE_INT = "int";
    public static final String MAPMESSAGE_TYPE_LONG = "long";
    public static final String MAPMESSAGE_TYPE_FLOAT = "float";
    public static final String MAPMESSAGE_TYPE_DOUBLE = "double";
    public static final String MAPMESSAGE_TYPE_STRING = "string";    
    
    public JMSMapMessagePart() {
    }

    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set the extensibility element type
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }

    /**
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }

    /**
     * String format of the object
     * @return The String format of the object
     */
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nPart (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        strBuf.append("\nName=" + name);
        strBuf.append("\nPart=" + part);
        strBuf.append("\nType=" + type);
        strBuf.append("\n");
        return strBuf.toString();
    }
}
