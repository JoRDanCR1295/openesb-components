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
package com.sun.jbi.swiftbc.extensions;
import com.sun.jbi.swiftbc.SAGConstants;
import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/*
 * @(#)SwiftOperation.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER -
 * @author Sun Microsystems Inc.
 */

/*
 * @(#)SwiftOperation.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER -
 * @author Sun Microsystems Inc.
 */
public class SwiftOperation implements ExtensibilityElement, Serializable {
    
    private static final long serialVersionUID = 1L;
    
    private QName mFieldElementType = SAGConstants.QNAME_OPERATION;
    private SwiftInput mSwiftMessage = null;
    private SwiftOutput mSwiftOutput = null;
    
    Boolean mFieldRequired = null;
    
    
    
    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return mFieldElementType;
    }
    
    /**
     * Set the extensibility element type
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        mFieldElementType = elementType;
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
    public void setRequired(Boolean required) {
        mFieldRequired = required;
    }
    
    /**
     * Set the Input element properties for current Swift BC Operation
     */
    public void setSwiftOperationInput(SwiftInput input) {
        mSwiftMessage = input;
    }
    
    /**
     * Get the Input element properties for current Swift BC Operation
     */
    public SwiftInput getSwiftOperationInput() {
        return mSwiftMessage;
    }
    
    /**
     * Set the Output element properties for current HL7 BC Operation
     */
    public void setSwiftOperationOutput(SwiftOutput output) {
        mSwiftOutput = output;
    }
    
    /**
     * Get the Output element properties for current Swift BC Operation
     */
    public SwiftOutput getSwiftOperationOutput() {
        return mSwiftOutput;
    }
    
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nSwift operation (" + mFieldElementType + "):");
        strBuf.append("\nRequired=" + mFieldRequired);
        return strBuf.toString();
    }
}
