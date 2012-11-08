package com.sun.jbi.restbc.jbiadapter.wsdl;

import java.io.Serializable;

import javax.wsdl.Binding;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 * RestBinding.java
 *
 * @author Edward Chou
 */
@SuppressWarnings("serial")
public class RestBinding implements ExtensibilityElement, Serializable {
    
    private QName fieldElementType = RestConstants.QNAME_BINDING;
    private Boolean fieldRequired = null;
    private Binding binding = null;
    
    public RestBinding() {
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
     * @return 
     */
    public Boolean getRequired() {
        return fieldRequired;
    }
    
    /** 
     * Set whether required (for wsdl:required) 
     * @param required 
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }

    public void setBinding(Binding val) {
        binding = val;
    }
    
    public Binding getBinding() {
        return binding;
    }
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nRestBinding " + fieldElementType + ":");
        strBuf.append("\nRequired=" + fieldRequired);
        return strBuf.toString();
    }
}
