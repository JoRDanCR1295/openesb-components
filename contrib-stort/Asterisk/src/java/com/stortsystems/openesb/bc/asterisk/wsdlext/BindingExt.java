/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: BindingExt.java,v 1.1 2008/01/20 16:38:50 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk.wsdlext;

import com.sun.jbi.sample.component.common.wsdl.AbstractExtensibilityElement;
import com.sun.jbi.sample.component.common.wsdl.AbstractExtensionSerializer;
import com.sun.jbi.sample.component.common.wsdl.WSDLProcessor;
import java.io.PrintWriter;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;

public interface BindingExt  extends  ExtConstants, ExtensibilityElement, java.io.Serializable {
    //TODO: define get/set methods for properties for BindingExt if the extension element has attributes.
    
    /**
     * This class is an implementation of BindingExt interface that provides java model
     * for binding extensibility element.
     */
    public static class BindingExtImpl extends AbstractExtensibilityElement implements BindingExt {
        public BindingExtImpl() {
            setElementType(QN_BINDING_EXT);
        }
        /**
         * creates and adds the binding extensibility element to the wsdl definition
         */
        public static BindingExtImpl addExtensibilityElement(Definition wsdlDef, Binding binding) {
            BindingExtImpl  bindingExt = new BindingExt.BindingExtImpl();
            WSDLProcessor.addExtensibilityElement(wsdlDef, binding, bindingExt, NS_DEF_PREFIX);
            return bindingExt;
        }
    }
    /**
     * This class provides the serializer and deserializer implementation for binding extensibility element.
     */
    public static class BindingExtSerializer extends AbstractExtensionSerializer implements ExtConstants {
        
        public BindingExtSerializer() {
            super(Binding.class, QN_BINDING_EXT, BindingExtImpl.class);
        }
        
        @Override
        public ExtensibilityElement unmarshall(Class parentType, QName elementType, Element el,
                Definition def, ExtensionRegistry extReg) throws WSDLException {
            
            BindingExt extObj = (BindingExt)extReg.createExtension(parentType, elementType);
            //TODO: set any attributes from the el to extension object
            // String myAttr = el.getAttribute(ATTR_MY_ATTR);
            // extObj.setMyAttr(myAttr);
            return extObj;
        }
        
        @Override
        public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
                PrintWriter pw, Definition def, ExtensionRegistry extReg ) throws WSDLException {
            
            BindingExt extObj = (BindingExt)extension;
            StringBuffer buff = new StringBuffer();
            
            String elName = getQualifiedName(def, NS_URI, EL_BINDING_EXT);
            buff.append("<" + elName );
            
            //TODO: append any attributes from extension obj to the element
            // String myAttr = extObj.getMyAttr();
            // if ( myAttr != null && myAttr.trim().length() > 0 ) {
            //     buff.append(" myAttr=\"" + myAttr + "\"");
            // }
            //
            
            buff.append("/>");
            pw.println(buff.toString());
            
        }
        
    }
    
}
