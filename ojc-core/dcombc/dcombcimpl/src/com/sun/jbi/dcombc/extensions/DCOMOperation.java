/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc.extensions;

import java.io.Serializable;

import javax.xml.namespace.QName;

import javax.wsdl.BindingOperation;
import javax.wsdl.extensions.ExtensibilityElement;

/**
 * Represents the dcom:operation extensiblity element of dcom:binding
 * 
 * @author Chandrakanth Belde
 */
public class DCOMOperation implements ExtensibilityElement, Serializable {
	/**
	 *
	 */

    private static final long serialVersionUID = 1L;
    
    public static String ATTR_METHOD = "methodname";
    
    private QName fieldElementType = DCOMConstants.QNAME_OPERATION;

    private String fieldMethod;
    
    private DCOMInput dcomInput = null;

    private DCOMOutput dcomOutput = null;

    private BindingOperation bindingOp;

    private String fieldMEP;

	private Boolean fieldRequired = null;
	
	/**
     * Get the ElementType property from current DCOM BC Operation
	 * 
     * @return QName
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set the ElementType property for current DCOM BC Operation
	 *
     * @param QName
     */
    public void setElementType(QName elementType) {
        this.fieldElementType = elementType;
    }

    /**
     * Get the required property from current DCOM BC Operation
	 * 
     * @return Boolean
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    /**
     * Set the required property for current DCOM BC Operation
	 * 
     * @param Boolean
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }

    /**
     * Set the BindingOperation for current DCOM BC Operation
	 *
     * @param BindingOperation
     */
    public void setBindingOperation(BindingOperation bindingOp) {
        this.bindingOp = bindingOp;
    }

    /**
     * Get the BindingOperation for current DCOM BC Operation
	 *
     * @return BindingOperation
     */
    public BindingOperation getBindingOperation() {
        return bindingOp;
    }

    /**
     * Set the MessageExchangePattern for the current DCOM BC Operation
	 *
     * @param String representing mep
     */
    public void setMEP(String mep) {
        this.fieldMEP = mep;
    }

    /**
     * Get the MessageExchangePattern for the current DCOM BC Operation
	 *
     * @return String representing mep
     */
    public String getMEP() {
        return fieldMEP;
    }

    /**
     * Set the DCOMInput for the current DCOM BC Operation
	 *
     * @param DCOMInput
     */
    public void setDCOMOperationInput(DCOMInput input) {
        this.dcomInput = input;
    }

    /**
     * Get the DCOMInput for the current DCOM BC Operation
	 *
     * @return DCOMInput
     */
    public DCOMInput getDCOMOperationInput() {
        return dcomInput;
    }

    /**
     * Set the DCOMOutput for the current DCOM BC Operation
	 *
     * @param DCOMOutput
     */
    public void setDCOMOperationOutput(DCOMOutput output) {
        this.dcomOutput = output;
    }

    /**
     * Get the DCOMOutput from the current DCOM BC Operation
     *
	 * @return DCOMOutput
     */
    public DCOMOutput getDCOMOperationOutput() {
        return dcomOutput;
    }
    
    
	/**
	 * @return the fieldMethod
	 */
	public String getMethod() {
		return fieldMethod;
	}

	/**
	 * @param fieldMethod the fieldMethod to set
	 */
	public void setMethod(String method) {
		this.fieldMethod = method;
	}
    
    /**
     * Represents the class as string value
	 *
     * @return String
     */
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nDCOM Operation (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired );
        strBuf.append("\nMethodName=" + fieldMethod );        
        return strBuf.toString();
    }

}
