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
 * @(#)HL7Operation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;
import com.sun.jbi.hl7bc.HL7Constants;

/**
 *
 * @author Raghunadh, S. Nageswara Rao
 */
public class HL7Operation implements ExtensibilityElement, Serializable {

	private static final long serialVersionUID = 1L;

	private QName mFieldElementType = HL7Constants.QNAME_OPERATION;

	private HL7Input mHL7Input = null;

	private HL7Output mHL7Output = null;

	Boolean mFieldRequired = null;

    public static final String ATTR_HL7_MESSAGETYPE = "all_HL7_MessageTypes";

	private String mMessageType = ATTR_HL7_MESSAGETYPE;

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
	 * Set the Input element properties for current HL7 BC Operation
	 */
	public void setHL7OperationInput(HL7Input input) {
		mHL7Input = input;
	}

	/**
	 * Get the Input element properties for current HL7 BC Operation
	 */
	public HL7Input getHL7OperationInput() {
		return mHL7Input;
	}

	/**
	 * Set the Output element properties for current HL7 BC Operation
	 */
	public void setHL7OperationOutput(HL7Output output) {
		mHL7Output = output;
	}

	/**
	 * Get the Output element properties for current HL7 BC Operation
	 */
	public HL7Output getHL7OperationOutput() {
		return mHL7Output;
	}

    /**
     * Get Message Type
     */
    public String getMessageType() {
        return mMessageType;
    }
    
    /**
     * Set Message Type
     */
    public void setMessageType(String val) {
		if(!val.equalsIgnoreCase("all"))
           mMessageType = val;
    }

	public String toString() {
		StringBuffer strBuf = new StringBuffer(super.toString());
		strBuf.append("\nHL7 operation (" + mFieldElementType + "):");
		strBuf.append("\nRequired=" + mFieldRequired);
		return strBuf.toString();
	}
}
