/*
 * AbstractExtensionSerializer.java
 */

package com.sun.jbi.sample.component.common.wsdl;

import java.io.PrintWriter;
import java.io.Serializable;
import java.util.Set;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;

/**
 * This class is an abstract class that should be extended to implements extension serializer and 
 * deserializer. This class implements the code required for registering the serializer and deserializer
 * implemented by this class. THe AbstractExtensionRegistry class calls the #registerSerializer method
 * to register the concrete implementation of this class with extension registry.
 * @see AbstractExtensionRegistry
 * @author chikkala
 */
public abstract class AbstractExtensionSerializer
    implements ExtensionSerializer, ExtensionDeserializer,  Serializable {
    
    public static final long serialVersionUID = 1;
    
    private Class mParentType;
    private Class mExtensionType;
    private QName mElementType;
    
    /** Creates a new instance of AbstractSerializer */
    public AbstractExtensionSerializer(Class parentType, QName elementType, Class extensionType) {
        this.mParentType = parentType;
        this.mElementType = elementType;
        this.mExtensionType = extensionType;
    }
    
    public Class getParentType() {
        return this.mParentType;
    }
    public QName getElementType() {
        return this.mElementType;
    }
    public Class getExtensionType() {
        return this.mExtensionType;
    }
    
    public void registerSerializer(ExtensionRegistry extReg) {
        extReg.registerSerializer(this.mParentType, this.mElementType, this);
        extReg.registerDeserializer(this.mParentType, this.mElementType, this);
        extReg.mapExtensionTypes(this.mParentType, this.mElementType, this.mExtensionType);
    }
    
    public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
        PrintWriter pw, Definition def, ExtensionRegistry extReg ) throws WSDLException {
        // DO NOTHING. Binding component runtime does not need to serialize the wsdl extensions.
    }
    
    public ExtensibilityElement unmarshall(Class parentType, QName elementType, Element el,
        Definition def, ExtensionRegistry extReg) throws WSDLException {
        return null;
    }
    
    public static String getAttribute(Element el, String attrName) {
        String attrValue = null;
        Attr   attr = el.getAttributeNode(attrName);
        if ( attr != null ) {
            attrValue = attr.getValue();
        }
        return attrValue;
    }
    
    protected String getNamespacePrefix(Definition def, String namespaceURI, String defPrefix) {
        String prefix = null;
        prefix = def.getPrefix(namespaceURI);
        if ( prefix == null ) {
            Set keySet = def.getNamespaces().keySet();
            String newPrefix = "ns";
            if ( defPrefix != null && defPrefix.trim().length() > 0 ){
                newPrefix = defPrefix;
            }
            prefix = newPrefix;
            for ( int i=0; i < Integer.MAX_VALUE; ++i) {
                if (!keySet.contains(prefix)) {
                    break;
                } else {
                    prefix = newPrefix + i;
                }
            }
        }
        return prefix;
    }
    /**
     * @return the name with the prefix defined for the namespaceURI in the wsdl definition. 
     * @throws WSDLException if the prefix not found in the wsdl definition. note that the 
     * default prefix is an empty string.
     */
    protected String getQualifiedName(Definition def, 
        String namespaceURI, String localName) throws WSDLException {
        String prefix = null;
        if (namespaceURI != null && !namespaceURI.equals("")) {
            prefix = def.getPrefix(namespaceURI);
            if ( prefix == null ) {
                throw new WSDLException(WSDLException.OTHER_ERROR,
                    "Can not find prefix in WSDL Definition for " + namespaceURI);
            }
        }
        if ( prefix != null && !prefix.equals("")) {
            return prefix + ":" + localName;
        } else {
            return localName;
        }
    }
}
