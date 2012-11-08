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
 * @(#)FileOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * @author Sherry Weng
 */
public class FileOperation implements ExtensibilityElement, Serializable {

    private static final long serialVersionUID = 1L;
    public static final String VERB_POLL = "poll";
    public static final String VERB_READ = "read";
    public static final String VERB_WRITE = "write";
    QName fieldElementType = FileConstants.QNAME_OPERATION;
    String verb = null;
    FileInput fileInput = null;
    FileOutput fileOutput = null;
    Boolean fieldRequired = Boolean.FALSE;

    public FileOperation() {
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
     * Set the Input element properties for current File BC Operation
     */
    public void setFileOperationInput(FileInput input) {
        fileInput = input;
    }

    /**
     * Get the Input element properties for current File BC Operation
     */
    public FileInput getFileOperationInput() {
        return fileInput;
    }

    /**
     * Set the Input element properties for current File BC Operation
     */
    public void setFileOperationOutput(FileOutput output) {
        fileOutput = output;
    }

    /**
     * Get the Input element properties for current File BC Operation
     */
    public FileOutput getFileOperationOutput() {
        return fileOutput;
    }

    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nFile operation (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);

        return strBuf.toString();
    }

    public String getVerb() {
        return verb;
    }

    public void setVerb(String verb) {
        this.verb = verb;
    }
}
