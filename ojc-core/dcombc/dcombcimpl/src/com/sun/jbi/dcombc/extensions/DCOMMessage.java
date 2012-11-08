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
 * Represents dcom:message extensibility element
 * 
 * @author Chandrakanth Belde
 */
public class DCOMMessage implements ExtensibilityElement, Serializable {
	/**
	 *
	 */
    private static final long serialVersionUID = 1L;

    private QName fieldElementType = DCOMConstants.QNAME_OPERATION;

    private BindingOperation bindingOp;

    private Boolean fieldRequired = null;

    private String fieldMEP ;

    /**
     * Get the elementType attribute value
     * 
     * @return ElementType of dcom:message wsdl extension element
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set the elementType attribute
     * 
     * @param ElementType of dcom:message wsdl extension element
     */
    public void setElementType(QName elementType) {
        this.fieldElementType = elementType;
    }

    /**
     * Get the BindingOperation attribute value
     * 
     * @return BindingOperation of the dcom:extension wsdl element
     */
    public BindingOperation getBindingOp() {
        return bindingOp;
    }

    /**
     * Set the BindingOperation attribute value
     * 
     * @param BindingOperation for the dcom:message wsdl extension element
     */
    public void setBindingOp(BindingOperation bindingOp) {
        this.bindingOp = bindingOp;
    }

    /**
     * Get the FileElementType property
     * 
     * @return QName
     */
    public QName getFieldElementType() {
        return fieldElementType;
    }

    /**
     * Set the FileElementType property
     * 
     * @param QName
     */
    public void setFieldElementType(QName fieldElementType) {
        this.fieldElementType = fieldElementType;
    }

    /**
     * Get the required property
     * 
     * @return Boolean
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    /**
     * Set the required property
     * 
     * @param Boolean
     */
    public void setRequired(Boolean required) {
        this.fieldRequired = required;
    }

    /**
     * Get the MessageExchangePattern value
     * 
     * @return message exchange pattern
     */
    public String getMep() {
        return fieldMEP;
    }

    /**
     * Set the MessageExchangePattern value
     * 
     * @param message extension pattern
     */
    public void setMep(String mep) {
        this.fieldMEP = mep;
    }

    /**
     * Represents the class as string value
     * @return String
     */
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nDCOM message (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        return strBuf.toString();
    }
}
