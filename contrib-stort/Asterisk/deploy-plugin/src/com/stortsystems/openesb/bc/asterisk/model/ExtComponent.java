/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: ExtComponent.java,v 1.1 2008/01/20 16:40:07 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk.model;

import org.netbeans.modules.xml.wsdl.model.ExtensibilityElement;
import org.netbeans.modules.xml.wsdl.model.WSDLModel;
import org.netbeans.modules.xml.wsdl.model.spi.GenericExtensibilityElement;
import org.w3c.dom.Element;

public interface ExtComponent extends ExtConstants, ExtensibilityElement {
    
    void accept(ExtVisitor visitor);
    /**
     *  this class is the base class for the extension model objects
     */
    public static abstract class ExtModelImpl
            extends GenericExtensibilityElement implements ExtComponent {
        
        public ExtModelImpl(WSDLModel model, Element e) {
            super(model, e);
        }
        
        @Override
        protected String getNamespaceURI() {
            return NS_URI;
        }
    }
}
