package net.openesb.jbi.restbc.jbiadapter.wsdl;

import java.io.Serializable;

import javax.wsdl.BindingOperation;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;

/**
 * RestOperation.java
 *
 * @author Edward Chou
 */
@SuppressWarnings("serial")
public class RestOperation implements ExtensibilityElement, Serializable {

    private QName fieldElementType = RestConstants.QNAME_OPERATION;

    private Boolean fieldRequired = null;
    
    private BindingOperation mBindingOp;
    
    private Element el;
    
    private OperationType mep;
    
    private Operation operation;
    
    public RestOperation(Element el) {
        this.el = el;
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
    
    public void setBindingOperation (BindingOperation bindingOp) {
        this.mBindingOp = bindingOp;
    }
    
    public BindingOperation getBindingOperation() {
        return mBindingOp;
    }
    
    public Element getElement() {
        return el;
    }
    
    /**
     * @return the mep
     */
    public OperationType getMep() {
        return mep;
    }

    /**
     * @param mep the mep to set
     */
    public void setMep(OperationType mep) {
        this.mep = mep;
    }

    /**
     * @return the operation
     */
    public Operation getOperation() {
        return operation;
    }

    /**
     * @param operation the operation to set
     */
    public void setOperation(Operation operation) {
        this.operation = operation;
    }

    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nRestOperation (" + fieldElementType + "):");
        strBuf.append("\nBindingOperation=" + (mBindingOp==null?"not known yet" : mBindingOp.toString()));
        return strBuf.toString();
    }
}
