/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: OperationExt.java,v 1.1 2008/01/20 16:40:08 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk.model;

import org.netbeans.modules.xml.wsdl.model.BindingOperation;
import org.netbeans.modules.xml.wsdl.model.WSDLModel;
import org.netbeans.modules.xml.xam.Component;
import org.w3c.dom.Element;
import org.w3c.dom.Element;

public interface OperationExt extends ExtComponent {
    
    /**
     * Getter for property action.
     * @return Value of property action.
     */
    public String getAction();
    public void setAction(String action);
     
    /**
     * This class is an implementation of OperationExt interface that provides java model
     * for binding operation extensibility element.
     */
    public static class OperationExtImpl extends ExtModelImpl implements OperationExt {
        
        public OperationExtImpl(WSDLModel model, Element e) {
            super(model, e);
        }
        
        public OperationExtImpl(WSDLModel model) {
            this(model, createPrefixedElement(QN_OPERATION_EXT, model));
        }
        
        public void accept(ExtVisitor visitor) {
            visitor.visit(this);
        }
        
        @Override
        public boolean canBeAddedTo(Component target) {
            if (target instanceof BindingOperation) {
                return true;
            }
            return false;
        }
        
        public String getAction() {
            return getAttribute(ExtAttribute.ACTION);
        }
        
        public void setAction(String action) {
            setAttribute(ATTR_ACTION, ExtAttribute.ACTION, action);
        }
        
    }
}
