/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)ServicePortImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;
 
import java.util.logging.Logger;

import com.sun.wsdl.model.Binding;
import com.sun.wsdl.model.NamespaceUtility;
import com.sun.wsdl.model.PortType;
import com.sun.wsdl.model.ServicePort;
import com.sun.wsdl.model.WSDLDocument;
import javax.xml.namespace.QName;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLAttributeEvent;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLElementAdapter;
import com.sun.wsdl.model.common.model.XMLElementListener;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.extensions.soap.SOAPAddress;
import com.sun.wsdl.model.visitor.WSDLVisitor;
 

 
/**
 * Implements the service &lt;port&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class ServicePortImpl extends NamedWSDLExtensibleElementImpl
    implements ServicePort {
        
    /**
     * The logger.
     */
    private static Logger mLogger =
        Logger.getLogger(ServicePortImpl.class.getName());
    
    
    /**
     * Creates a new instance of ServicePortImpl.
     * @param   d   Owner document.
     */
    public ServicePortImpl(XMLDocument d) {
        super(d);
        
        initServicePort();
    }
    
    /**
     * Initializes this class.
     */
    private void initServicePort() {
        setLocalName(ServicePort.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ServicePort.ATTR.NAME, String.class, false, null),
            new XMLAttributeImpl(ServicePort.ATTR.BINDING, String.class, false, null)
        };
        childrenTags = new String[] {
            SOAPAddress.QTAG
        };
    }
    
    
    /**
     * Getter for property binding.
     * @return Value of property binding.
     */
    public QName getBinding() {
        String bindingQNameStr = xmlAttrs[BINDING].getValue();
        if(bindingQNameStr != null) {
        	return NamespaceUtility.resolveAndGetQName(bindingQNameStr, this);
        }
        
        return null;
    }
    
    /**
     * Setter for property binding.
     * @param binding   New value of property binding.
     */
    public void setBinding(QName binding) {
    	if(binding != null) {
    		/*setAttribute(BINDING, binding.toString());*/
    		setAttribute(BINDING, 
    				NamespaceUtility.getQNameAsString(binding));

    	} else {
    		setAttribute(BINDING, null);
    	}
    }
    
    
    public Binding getWSDLBinding() {
    	Binding binding = null;
    	WSDLDocument doc = (WSDLDocument) this.getOwnerDocument();
    	
    	if(doc != null && this.getBinding() != null) {
    		binding = doc.getDocumentDefinitions().getBinding(this.getBinding());
    	}
    	
    	return binding;
	}

	/**
     * Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor w) {
        WSDLVisitor v = morphVisitor(w);

        if (traverseParentFirst(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }

        if (!super.accept(v)) {
            return false;
        }

        if (traverseParentLast(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        return true;
    }
    
}
