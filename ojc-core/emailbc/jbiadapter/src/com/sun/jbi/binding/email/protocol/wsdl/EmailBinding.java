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
 * @(#)EmailBinding.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.binding.email.protocol.wsdl;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public abstract class EmailBinding implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;
    private Boolean isRequired = Boolean.TRUE;

    /**
     * @see javax.wsdl.extensions.ExtensibilityElement#setElementType(javax.xml.namespace.QName)
     */
    public void setElementType(QName arg0) {
        // No Op
    }

    /**
     * @see javax.wsdl.extensions.ExtensibilityElement#getElementType()
     */
    public abstract QName getElementType() ;

    /**
     * @see javax.wsdl.extensions.ExtensibilityElement#setRequired(java.lang.Boolean)
     */
    public void setRequired(Boolean required) {
        this.isRequired = required;
    }
    /**
     * @see javax.wsdl.extensions.ExtensibilityElement#getRequired()
     */
    public Boolean getRequired() {
        return isRequired;
    }
    
    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
        final StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\n EmailBinding " + getElementType() + ":");
        strBuf.append("\n Required = " + isRequired);
        return strBuf.toString();
    }
}
