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

package com.sun.jbi.imsbc.extensions;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;

/**
 * @author Sun Microsystems
 */
public class IMSBinding implements ExtensibilityElement, Serializable {

    private static final long serialVersionUID = 1L;

    QName fieldElementType = IMSConstants.QNAME_BINDING;

    private Boolean fieldRequired = null;

    public IMSBinding() {
    }

    /**
     * Get the extensibility element type
     * 
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set the extensibility element type
     * 
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

    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nIMSBinding " + fieldElementType + ":");
        strBuf.append("\nRequired=" + fieldRequired);
        return strBuf.toString();
    }

}
