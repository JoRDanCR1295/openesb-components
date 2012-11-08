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
 * @(#)ExecAddress.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

public class ExecAddress implements ExtensibilityElement, Serializable {
    
    private static final long serialVersionUID = 1L;
    
    QName fieldElementType = ExecConstants.QNAME_ADDRESS;
    Boolean fieldRequired;
    String hostName;
    String userName;
    String password;
    
    public ExecAddress() {}
    
    /**
     * Set the extensibility element type
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }
    
    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }
    
    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }
    
    /**
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return fieldRequired;
    }
    
    public String getHostName() {
        return hostName;
    }
    
    public void setHostName(String val) {
        hostName = val;
    }
    
    public String getUserName() {
        return userName;
    }
    
    public void setUserName(String val) {
        userName = val;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String val) {
        password = val;
    }
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nExec address (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        strBuf.append("\nHost Name=" + hostName);
        strBuf.append("\nUser Name=" + userName);
        
        return strBuf.toString();
    }
}
