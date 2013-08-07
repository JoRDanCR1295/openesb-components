/*
 * AbstractExtensibilityElement.java
 */

package net.openesb.component.BindingComponent-archetype.common.wsdl;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 * This is an abstract class which can be extended to create jwsdl (wsdl4j's) extension elements model to
 * read/write the wsdl extension elements in wsdl 1.1 xml.
 *
 * @author chikkala
 */
public abstract class AbstractExtensibilityElement implements ExtensibilityElement, java.io.Serializable {
    public static final long serialVersionUID = 1;
    private QName mElementType;
    private Boolean mRequired;
    
    /** Creates a new instance of AbstractExtensibilityElement */
    protected AbstractExtensibilityElement() {
    }
    
    public void setElementType(QName elementType) {
        this.mElementType = elementType;
    }
    
    public QName getElementType() {
        return this.mElementType;
    }
    
    public void setRequired(Boolean required) {
        this.mRequired = required;
    }
    
    public Boolean getRequired() {
        return this.mRequired;
    }
    
}
