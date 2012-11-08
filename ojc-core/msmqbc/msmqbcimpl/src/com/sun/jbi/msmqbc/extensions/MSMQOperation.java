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
 * @(#)MSMQOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.extensions;

import java.io.Serializable;

import javax.wsdl.BindingOperation;
import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;

/**
 * Represents the msmq:operation extensiblity element of msmq:binding
 * 
 * @author Sun Microsystems
 */
public class MSMQOperation implements ExtensibilityElement, Serializable {

    static final long serialVersionUID = 1L;

    private QName fieldElementType = MSMQConstants.QNAME_OPERATION;

    private MSMQInput msmqInput = null;

    private MSMQOutput msmqOutput = null;

    private BindingOperation bindingOp;

    private String mep;

    private Boolean fieldRequired = null;

    /**
     * Get the ElementType property from current MSMQ BC Operation
     * @return QName
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set the ElementType property for current MSMQ BC Operation
     * @param QName
     */
    public void setElementType(QName elementType) {
        this.fieldElementType = elementType;
    }

    /**
     * Get the required property from current MSMQ BC Operation
     * @return Boolean
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    /**
     * Set the required property for current MSMQ BC Operation
     * @param Boolean
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }

    /**
     * Set the BindingOperation for current MSMQ BC Operation
     * @param BindingOperation
     */
    public void setBindingOperation(BindingOperation bindingOp) {
        this.bindingOp = bindingOp;
    }

    /**
     * Get the BindingOperation for current MSMQ BC Operation
     * @return BindingOperation
     */
    public BindingOperation getBindingOperation() {
        return bindingOp;
    }

    /**
     * Set the MessageExchangePattern for the current MSMQ BC Operation
     * @param String
     */
    public void setMEP(String mep) {
        this.mep = mep;
    }

    /**
     * Get the MessageExchangePattern for the current MSMQ BC Operation
     * @return String
     */
    public String getMEP() {
        return mep;
    }

    /**
     * Set the MSMQInput for the current MSMQ BC Operation
     * @param MSMQInput
     */
    public void setMSMQOperationInput(MSMQInput input) {
        this.msmqInput = input;
    }

    /**
     * Get the MSMQInput for the current MSMQ BC Operation
     * @return MSMQInput
     */
    public MSMQInput getMSMQOperationInput() {
        return msmqInput;
    }

    /**
     * Set the MSMQOutput for the current MSMQ BC Operation
     * @param MSMQOutput
     */
    public void setMSMQOperationOutput(MSMQOutput output) {
        msmqOutput = output;
    }

    /**
     * Get the MSMQOutput for the current MSMQ BC Operation
     * @return MSMQOutput
     */
    public MSMQOutput getMSMQOperationOutput() {
        return msmqOutput;
    }

    /**
     * Represents the class as string value
     * @return String
     */
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nMSMQ operation (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);

        return strBuf.toString();
    }

}
