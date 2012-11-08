/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: SEBindingExt.java,v 1.2 2008/01/27 20:59:41 tiago_cury Exp $
 */

package com.sun.jbi.sample.component.common.wsdl;

import java.io.PrintWriter;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;

public interface SEBindingExt  extends  ExtensibilityElement, java.io.Serializable {
    
    public static final String NS_URI = "http://java.sun.com/xml/ns/jbi/binding/service+engine";
    public static final String NS_DEF_PREFIX = "jbise";
    /** Element names. */
    public static final String EL_BINDING_EXT = "binding";
    /**Qualified element names.*/
    public static final QName QN_BINDING_EXT = new QName(NS_URI, EL_BINDING_EXT);
    
    public static class SEBindingExtImpl extends AbstractExtensibilityElement implements SEBindingExt {
        
        public SEBindingExtImpl() {
            setElementType(QN_BINDING_EXT);
        }
        @Override
        public String toString() {
            StringBuffer buff = new StringBuffer();
            buff.append("<"+NS_DEF_PREFIX+":"+EL_BINDING_EXT);
            buff.append("/>");
            return buff.toString();
        }
        /**
         * creates and adds the jbi service engine binding extensibility element to the wsdl definition
         * under specified binding definition.
         */
        public static SEBindingExtImpl addExtensibilityElement(Definition wsdlDef, Binding binding) {
            SEBindingExtImpl  bindingExt = new SEBindingExt.SEBindingExtImpl();
            WSDLProcessor.addExtensibilityElement(wsdlDef, binding, bindingExt, SEBindingExt.NS_DEF_PREFIX);
            return bindingExt;
        }
    }
    /**
     * serializer and descrializer implementation for the binding extension element.
     */
    public static class SEBindingExtSerializer extends AbstractExtensionSerializer {
        
        public SEBindingExtSerializer() {
            super(Binding.class, QN_BINDING_EXT, SEBindingExtImpl.class);
        }
        
        @Override
        public ExtensibilityElement unmarshall(Class parentType, QName elementType, Element el,
            Definition def, ExtensionRegistry extReg) throws WSDLException {            
            SEBindingExt extObj = (SEBindingExt)extReg.createExtension(parentType, elementType);
            return extObj;
        }
        
        @Override
        public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
            PrintWriter pw, Definition def, ExtensionRegistry extReg ) throws WSDLException {
            
            String elName = getQualifiedName(def, NS_URI, EL_BINDING_EXT);
            
            SEBindingExt extObj = (SEBindingExt)extension;
            
            StringBuffer buff = new StringBuffer();
            buff.append("<" + elName );
            buff.append("/>");
            pw.println(buff.toString());
        }
    }
    
}
