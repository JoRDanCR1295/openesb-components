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
 * @(#)JMSProperties.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.extensions;

import java.util.Iterator;
import java.util.Map;
import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 */
public class JMSProperties implements ExtensibilityElement, Serializable {

    private static final long serialVersionUID = 1L;

    QName fieldElementType = JMSConstants.QNAME_PROPERTIES;

    Boolean fieldRequired = null;

    Map properties = null;  // <propertyName, JMSProperty>

    public JMSProperties() {
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
     * Gets the properties parts map
     * @return Map
     */
    public Map getProperties() {
        return properties;
    }

    /**
     * Sets the properties parts map
     * @param val Map
     */
    public void setProperties(Map val) {
        properties = val;
    }

    /**
     * String format of the object
     * @return The String format of the object
     */
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nJMS Properties (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        strBuf.append("\nProperty list =[[" + propertyMapToString() + "]]");
        strBuf.append("\n");

        return strBuf.toString();
    }
    
    private String propertyMapToString() {
        StringBuffer strBuff = new StringBuffer();
        if (properties != null) {
            Iterator propsIter = properties.keySet().iterator();
            while (propsIter.hasNext()) {
                JMSProperty property = (JMSProperty)properties.get(propsIter.next());
                strBuff.append(property.toString());
            }
        }
        return strBuff.toString();
    }
}
