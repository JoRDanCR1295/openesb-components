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
 * @(#)SNMPAddress.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;


public class SNMPAddress implements ExtensibilityElement, Serializable {

    public static final String ATTR_PORT = "port";

    QName fieldElementType = SNMPConstants.QNAME_ADDRESS;

    Boolean fieldRequired = null;

    Integer port;
    
    public SNMPAddress() {
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
     * @return 
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
    
    public Integer getPort() {
        return port;
    }
    
    public void setPort(Integer val) {
        port = val;
    }
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nSNMP address (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        strBuf.append("\nport=" + port);
        return strBuf.toString();
    }
}
