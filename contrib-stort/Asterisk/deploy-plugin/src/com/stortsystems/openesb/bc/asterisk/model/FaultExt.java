/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: FaultExt.java,v 1.1 2008/01/20 16:40:08 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk.model;

import org.netbeans.modules.xml.wsdl.model.BindingFault;
import org.netbeans.modules.xml.wsdl.model.WSDLModel;
import org.netbeans.modules.xml.xam.Component;
import org.w3c.dom.Element;
import org.w3c.dom.Element;

public interface FaultExt extends ExtComponent {
    
    //TODO: define get/set methods for properties for FaultExt if the extension element has attributes.
    /**
     * This class is an implementation of FaultExt interface that provides java model
     * for binding operation fault extensibility element.
     */
    public static class FaultExtImpl extends ExtModelImpl implements FaultExt {
        
        public FaultExtImpl(WSDLModel model, Element e) {
            super(model, e);
        }
        
        public FaultExtImpl(WSDLModel model) {
            this(model, createPrefixedElement(QN_FAULT_EXT, model));
        }
        
        public void accept(ExtVisitor visitor) {
            visitor.visit(this);
        }
        
        @Override
        public boolean canBeAddedTo(Component target) {
            if (target instanceof BindingFault) {
                return true;
            }
            return false;
        }
    }
}
