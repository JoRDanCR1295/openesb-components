package net.openesb.jbi.restbc.jbiadapter.wsdl;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 * RestAddress.java
 *
 * @author Edward Chou
 */
@SuppressWarnings("serial")
public class RestAddress implements ExtensibilityElement, Serializable {

    QName fieldElementType = RestConstants.QNAME_ADDRESS;

    Boolean fieldRequired = null;

    public RestAddress() {
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
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nRestAddress (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        return strBuf.toString();
    }
}
