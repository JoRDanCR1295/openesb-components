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
 * @(#)JMSOption.java 
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
 */
public class JMSOption implements ExtensibilityElement, Serializable {

    public static final String ATTR_NAME = "name";
    public static final String ATTR_VALUE = "value";
        
    private static final long serialVersionUID = 1L;

    QName fieldElementType = JMSConstants.QNAME_OPTION;

    Boolean fieldRequired = null;

    String name;
    String value;

    public JMSOption() {
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
     * @param required 
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }

    /**
     * Get value of name attribute
     * @return The String value of name attribute
     */
    public String getName() {
        return name;
    }

    /**
     * Sets value of name attribute
     * @param val The String value of name attribute
     */
    public void setName(String val) {
        name = val;
    }

    /**
     * Get value of value attribute
     * @return The String value of value attribute
     */
    public String getValue() {
        return value;
    }

    /**
     * Sets value of value attribute
     * @param val The String value of value attribute
     */
    public void setValue(String val) {
        value = val;
    }

    /**
     * String format of the object
     * @return The String format of the object
     */    
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nJMSOption (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        strBuf.append("\nName=" + name);
        strBuf.append("\nValue=" + value);
        strBuf.append("\n");

        return strBuf.toString();
    }

}
