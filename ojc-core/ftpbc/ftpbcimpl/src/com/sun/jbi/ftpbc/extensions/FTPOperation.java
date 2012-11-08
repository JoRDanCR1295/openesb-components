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
 * @(#)FTPOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc.extensions;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import java.io.Serializable;

/**
 *
 * @author jfu jim.fu@sun.com
 */
public class FTPOperation implements ExtensibilityElement, Serializable {

    private static final long serialVersionUID = 1L;
    QName fieldElementType = FTPConstants.QNAME_OPERATION;
    FTPInput mInput = null;
    FTPOutput mOutput = null;
    Boolean fieldRequired = Boolean.FALSE;

    public FTPOperation() {
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
     * Set the Input element properties for current FTP BC Operation
     */
    public void setFTPOperationInput(FTPInput input) {
        mInput = input;
    }

    /**
     * Get the Input element properties for current FTP BC Operation
     */
    public FTPInput getFTPOperationInput() {
        return mInput;
    }

    /**
     * Set the Input element properties for current FTP BC Operation
     */
    public void setFTPOperationOutput(FTPOutput output) {
        mOutput = output;
    }

    /**
     * Get the Input element properties for current FTP BC Operation
     */
    public FTPOutput getFTPOperationOutput() {
        return mOutput;
    }

    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nFTP operation (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);

        return strBuf.toString();
    }
}
