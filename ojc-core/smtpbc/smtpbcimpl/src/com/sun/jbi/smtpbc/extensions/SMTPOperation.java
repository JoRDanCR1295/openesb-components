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
 * @(#)SMTPOperation.java 
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
public class SMTPOperation implements ExtensibilityElement, Serializable {
    
    // Qualified element names
    public static final QName QNAME_OPERATION =
        new QName(SMTPConstants.NS_URI_SMTP, Constants.ELEM_OPERATION);

    private static final long serialVersionUID = 1L;
    
    private SMTPOperationInput smtpInput = null;
    
    
    Boolean mFieldRequired = null;
    
    /** 
     * Get the extensibility element type
     * @return the extensibility element's type 
     */
    public QName getElementType() {
        return SMTPOperation.QNAME_OPERATION;
    }

    /** 
     * Set the extensibility element type
     * @param elementType the type 
     */
    public void setElementType(final QName elementType) {
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

    
    @Override
	public String toString() {
        final StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nSMTP operation (" + SMTPOperation.QNAME_OPERATION + "):");
        strBuf.append("\nRequired=" + mFieldRequired);

        return strBuf.toString();
    }

	public SMTPOperationInput getSmtpInput() {
		return smtpInput;
	}

	public void setSmtpInput(final SMTPOperationInput smtpInput) {
		this.smtpInput = smtpInput;
	}
}
