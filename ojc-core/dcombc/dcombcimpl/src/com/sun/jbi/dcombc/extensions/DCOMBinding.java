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

import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;

import sun.reflect.generics.tree.FieldTypeSignature;

/**
 * 
 * @author Chandrakanth Belde
 */

public class DCOMBinding implements ExtensibilityElement, Serializable {
	/**
	 *
	 */
    public static String ATTR_UUID = "uuid";
    
    private static final long serialVersionUID = 1L;

    private QName fieldElementType = DCOMConstants.QNAME_BINDING;    

    private Boolean fieldRequired = null;

    private String fieldUUID;

    public DCOMBinding() {
		//
    }

    /**
     * Get the extensibility element type
     * 
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return this.fieldElementType;
    }

    /**
     * Set the extensibility element type
     * 
     * @param elementType the type
     */
    public void setElementType(QName fieldElementType) {
        this.fieldElementType = fieldElementType;
    }

    /**
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return this.fieldRequired;
    }

    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(Boolean fieldRequired) {
        this.fieldRequired = fieldRequired;
    }

	/**
	 * @return the UUID
	 */
	public String getUUID() {
		return fieldUUID;
	}

	/**
	 * @param UUID the UUID to set
	 */
	public void setUUID(String UUID) {
		this.fieldUUID = UUID;
	}


    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nDCOM Binding (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired );
        strBuf.append("\nUUID=" + fieldUUID);        
        return strBuf.toString();
    }

}
