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
 * @(#)SMTPBinding.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;
import com.ibm.wsdl.Constants;

/**
 *
 * @author aegloff
 */
public class SMTPBinding implements ExtensibilityElement, Serializable {
    
    public static final QName QNAME_BINDING =
        new QName(SMTPConstants.NS_URI_SMTP, Constants.ELEM_BINDING);

    private static final long serialVersionUID = 1L;
    
    private Boolean mFieldRequired = null;
    private SMTPProxy mSMTPProxy = null;

    public SMTPBinding() {
    }
    
    /** 
     * Get the extensibility element type
     * @return the extensibility element's type 
     */
    public QName getElementType() {
        return SMTPBinding.QNAME_BINDING;
    }

    /** 
     * Set the extensibility element type
     * @param elementType the type 
     */
    public void setElementType(final QName elementType) {
        // No op 
    }

    /** 
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return mFieldRequired;
    }
    
    /** 
     * Set whether required (for wsdl:required) 
     */
    public void setRequired(final Boolean required) {
        mFieldRequired = required;
    }

    public SMTPProxy getSMTPProxy() {
        return mSMTPProxy;
    }

    public void setSMTPProxy(final SMTPProxy proxy) {
        mSMTPProxy = proxy;
    }

    @Override
	public String toString() {
        final StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nSMTPBinding " + SMTPBinding.QNAME_BINDING + ":");
        strBuf.append("\nRequired=" + mFieldRequired);
        strBuf.append("\nSMTPProxy=" + mSMTPProxy);
        return strBuf.toString();
    }
}
