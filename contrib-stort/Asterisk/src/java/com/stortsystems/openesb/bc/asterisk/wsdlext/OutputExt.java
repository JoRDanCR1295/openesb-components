/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: OutputExt.java,v 1.1 2008/01/20 16:38:51 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk.wsdlext;

import com.sun.jbi.sample.component.common.wsdl.AbstractExtensibilityElement;
import com.sun.jbi.sample.component.common.wsdl.AbstractExtensionSerializer;
import com.sun.jbi.sample.component.common.wsdl.WSDLProcessor;
import java.io.PrintWriter;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;

public interface OutputExt  extends  ExtConstants, ExtensibilityElement, java.io.Serializable {
    
    //TODO: define get/set methods for properties for OutputExt if the extension element has attributes.
    
    /**
     * This class is an implementation of OutputExt interface that provides java model
     * for binding operation output extensibility element.
     */
    public static class OutputExtImpl extends AbstractExtensibilityElement implements OutputExt {
        public OutputExtImpl() {
            setElementType(QN_OUTPUT_EXT);
        }
        /**
         * creates and adds the binding operation fault extensibility element to the wsdl definition
         */
        public static OutputExtImpl addExtensibilityElement(Definition wsdlDef, BindingOutput output) {
            OutputExtImpl  OutputExt = new OutputExt.OutputExtImpl();
            WSDLProcessor.addExtensibilityElement(wsdlDef, output, OutputExt, NS_DEF_PREFIX);
            return OutputExt;
        }
    }
    /**
     * This class provides the serializer and deserializer implementation for binding operation output
     * extensibility element.
     */
    public static class OutputExtSerializer extends AbstractExtensionSerializer implements ExtConstants {
        public OutputExtSerializer() {
            super(BindingOutput.class, QN_OUTPUT_EXT, OutputExtImpl.class);
        }
        @Override
        public ExtensibilityElement unmarshall(Class parentType, QName elementType, Element el,
                Definition def, ExtensionRegistry extReg) throws WSDLException {
            
            OutputExt extObj = (OutputExt)extReg.createExtension(parentType, elementType);
            //TODO: set any attributes from the el to extension object
            // String myAttr = el.getAttribute(ATTR_MY_ATTR);
            // extObj.setMyAttr(myAttr);
            return extObj;
        }
        
        @Override
        public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
                PrintWriter pw, Definition def, ExtensionRegistry extReg ) throws WSDLException {
            
            OutputExt extObj = (OutputExt)extension;
            StringBuffer buff = new StringBuffer();
            
            String elName = getQualifiedName(def, NS_URI, EL_OUTPUT_EXT);
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
