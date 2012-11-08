/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: OperationExt.java,v 1.1 2008/01/20 16:38:50 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk.wsdlext;

import com.sun.jbi.sample.component.common.wsdl.AbstractExtensibilityElement;
import com.sun.jbi.sample.component.common.wsdl.AbstractExtensionSerializer;
import com.sun.jbi.sample.component.common.wsdl.WSDLProcessor;
import java.io.PrintWriter;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;

public interface OperationExt extends  ExtConstants, ExtensibilityElement, java.io.Serializable {
    
    public String getAction();
    public void setAction(String action);
    
    /**
     * This class is an implementation of OperationExt interface that provides java model
     * for binding operation extensibility element.
     */
    public static class OperationExtImpl extends AbstractExtensibilityElement implements OperationExt {
        
        private String mAction;
        
        
        public OperationExtImpl() {
            setElementType(QN_OPERATION_EXT);
        }
        
        public String getAction() {
            
            return this.mAction;
        }
        
        public void setAction(String action) {
            
            this.mAction = action;
        }
        

        /**
         * creates and adds the binding operation fault extensibility element to the wsdl definition
         */
        public static OperationExtImpl addExtensibilityElement(Definition wsdlDef, BindingOperation operation) {
            OperationExtImpl  operationExt = new OperationExt.OperationExtImpl();
            WSDLProcessor.addExtensibilityElement(wsdlDef, operation, operationExt, NS_DEF_PREFIX);
            return operationExt;
        }
    }
    /**
     * This class provides the serializer and deserializer implementation for binding operation
     * extensibility element.
     */
    public static class OperationExtSerializer extends AbstractExtensionSerializer implements ExtConstants {
        public OperationExtSerializer() {
            super(BindingOperation.class, QN_OPERATION_EXT, OperationExtImpl.class);
        }
        @Override
        public ExtensibilityElement unmarshall(Class parentType, QName elementType, Element el,
                Definition def, ExtensionRegistry extReg) throws WSDLException {
            
            OperationExt extObj = (OperationExt)extReg.createExtension(parentType, elementType);
            
            String action = el.getAttribute(ATTR_ACTION);
            extObj.setAction(action);
            
            return extObj;
        }
        
        @Override
        public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
                PrintWriter pw, Definition def, ExtensionRegistry extReg ) throws WSDLException {
            
            OperationExt extObj = (OperationExt)extension;
            StringBuffer buff = new StringBuffer();
            
            String elName = getQualifiedName(def, NS_URI, EL_OPERATION_EXT);
            buff.append("<" + elName );
            
            String action = extObj.getAction();
                        
            if ( action != null && action.trim().length() > 0 ) {
                buff.append(" " + ATTR_ACTION + "=\"" + action + "\"");
            }
            
            
            buff.append("/>");
            pw.println(buff.toString());
            
        }
        
    }
    
}
