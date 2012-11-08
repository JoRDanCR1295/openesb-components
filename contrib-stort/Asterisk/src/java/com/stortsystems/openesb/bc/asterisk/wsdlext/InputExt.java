/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: InputExt.java,v 1.1 2008/01/20 16:38:50 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk.wsdlext;

import com.sun.jbi.sample.component.common.wsdl.AbstractExtensibilityElement;
import com.sun.jbi.sample.component.common.wsdl.AbstractExtensionSerializer;
import com.sun.jbi.sample.component.common.wsdl.WSDLProcessor;
import java.io.PrintWriter;
import javax.wsdl.BindingInput;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;

public interface InputExt  extends  ExtConstants, ExtensibilityElement, java.io.Serializable {
    
    //TODO: define get/set methods for properties for InputExt if the extension element has attributes.
    
    public String getEventTypes();
    public void setEventTypes(String eventtypes);
    
    /**
     * This class is an implementation of InputExt interface that provides java model
     * for binding operation input extensibility element.
     */
    public static class InputExtImpl extends AbstractExtensibilityElement implements InputExt {
        
        private String mEventTypes;
        
        public InputExtImpl() {
            setElementType(QN_INPUT_EXT);
        }
        
        public String getEventTypes() {
            
            return this.mEventTypes;
        }
        
        public void setEventTypes(String eventtypes) {
            
            this.mEventTypes = eventtypes;
        }
        
        
        /**
         * creates and adds the binding operation fault extensibility element to the wsdl definition
         */
        public static InputExtImpl addExtensibilityElement(Definition wsdlDef, BindingInput input) {
            InputExtImpl  inputExt = new InputExt.InputExtImpl();
            WSDLProcessor.addExtensibilityElement(wsdlDef, input, inputExt, NS_DEF_PREFIX);
            return inputExt;
        }
    }
    /**
     * This class provides the serializer and deserializer implementation for binding operation input
     * extensibility element.
     */
    public static class InputExtSerializer extends AbstractExtensionSerializer implements ExtConstants {
        public InputExtSerializer() {
            super(BindingInput.class, QN_INPUT_EXT, InputExtImpl.class);
        }
        @Override
        public ExtensibilityElement unmarshall(Class parentType, QName elementType, Element el,
                Definition def, ExtensionRegistry extReg) throws WSDLException {
            
            InputExt extObj = (InputExt)extReg.createExtension(parentType, elementType);
            //TODO: set any attributes from the el to extension object
            String eventtypes = el.getAttribute(ATTR_EVENTTYPES);
            extObj.setEventTypes(eventtypes);
            return extObj;
        }
        
        @Override
        public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
                PrintWriter pw, Definition def, ExtensionRegistry extReg ) throws WSDLException {
            
            InputExt extObj = (InputExt)extension;
            StringBuffer buff = new StringBuffer();
            
            String elName = getQualifiedName(def, NS_URI, EL_INPUT_EXT);
            buff.append("<" + elName );
            
            //TODO: append any attributes from extension obj to the element
            String eventtypes = extObj.getEventTypes();
            if ( eventtypes != null && eventtypes.trim().length() > 0 ) {
                buff.append(" eventtypes=\"" + eventtypes + "\"");
            }
            //
            
            buff.append("/>");
            pw.println(buff.toString());
            
        }
        
    }
    
}
