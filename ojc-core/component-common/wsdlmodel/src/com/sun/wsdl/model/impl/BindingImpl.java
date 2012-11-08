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
 * @(#)BindingImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import javax.xml.namespace.QName;

import com.sun.wsdl.model.Binding;
import com.sun.wsdl.model.BindingOperation;
import com.sun.wsdl.model.NamespaceUtility;
import com.sun.wsdl.model.PortType;
import com.sun.wsdl.model.WSDLDocument;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLAttributeEvent;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLElementAdapter;
import com.sun.wsdl.model.common.model.XMLElementListener;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.extensions.soap.SOAPBinding;
import com.sun.wsdl.model.visitor.WSDLVisitor;

/**
 * Implements the &lt;binding&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class BindingImpl extends NamedWSDLExtensibleElementImpl implements Binding {
    
    /** Holds list of binding operations sub-elements. */
    private ArrayList bindingOps = new ArrayList();
    
    
    /** Creates a new instance of BindingImpl */
    public BindingImpl() {
        super();
        initBinding();
    }
    
    /** Constructor for new binding instance.
     * @param   d   Owner document.
     */
    public BindingImpl(XMLDocument d) {
        super(d);
        initBinding();
    }
    
    /** Initializes this class.
     */
    private void initBinding() {
        setLocalName(Binding.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(Binding.ATTR.NAME, String.class, true, null),
            new XMLAttributeImpl(Binding.ATTR.TYPE, String.class, true, null)
        };
        childrenTags = new String[] {SOAPBinding.QTAG, BindingOperation.TAG};
    }
    
    
    /**
     * @see com.sun.wsdl.model.Binding#getType()
     */
    public QName getType() {
        String portTypeQName = xmlAttrs[TYPE].getValue();
        if(portTypeQName != null) {

        	//return QName.getQNameFromString(portTypeQName);
        	return NamespaceUtility.resolveAndGetQName(portTypeQName, this);
        }
        
        return null;
    }
    
    /**
     * @see com.sun.wsdl.model.Binding#setType(java.lang.String)
     */
    public void setType(QName type) {
    	if(type != null) {
    		/*setAttribute(TYPE, type.toString());*/
    		setAttribute(TYPE, 
    				NamespaceUtility.getQNameAsString(type));
    	} else {
    		setAttribute(TYPE, null);
    	}
    }
    
    /**
     * Get the PortType for this binding.
     * @return PortType.
     */
    public PortType getWSDLPortType() {
    	PortType portType = null;
    	WSDLDocument doc = (WSDLDocument) this.getOwnerDocument();
    	
    	if(doc != null && this.getType() != null) {
    		portType = doc.getDocumentDefinitions().getPortType(this.getType());
    	}
    	
    	return portType;
    }
    
    /**
     * @see com.sun.wsdl.model.Binding#setType(java.lang.String, java.lang.String)
     */
    public void setType(String qName, String type) {
        setAttribute(TYPE, qName, type);
    }
    
    /**
	 * add a node at the end of a collection.
	 * This will be called while parsing this XMLNode.
	 * This method should always add this child at the end of 
	 * the collection because we want to preserve the order
	 * in which this node occured in the xml.
	 * This method should also set the sibliing sequence order
	 * before adding to collection if it different than the
	 * default Interger.MAX_VALUE
	 * @param c
	 */
	public void addChildAtTail(XMLNode c) {
		if (c instanceof BindingOperation) {
			addBindingOperationAtTail((BindingOperation) c);
        } else {
            super.addChildAtTail(c);
        }
	}
	
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#addChild(com.sun.wsdl.model.common.model.XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof BindingOperation) {
            addBindingOperation((BindingOperation) c);
        } else {
            super.addChild(c);
        }
    }
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#removeChild(com.sun.wsdl.model.common.model.XMLNode)
     */
    public void removeChild(XMLNode c) {
        if (c instanceof BindingOperation) {
            removeBindingOperation((BindingOperation) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /**
     * get all biniding operations
     * @return collection of biniding operation.
     */
    public Collection getBindingOperations() {
    	return Collections.unmodifiableCollection(bindingOps);
    }
    
    /**
     * @see com.sun.wsdl.model.Binding#getBindingOperation(int)
     */
    public BindingOperation getBindingOperation(int index) {
        return (BindingOperation) bindingOps.get(index);
    }
    
    /**
     * @see com.sun.wsdl.model.Binding#getBindingOperationSize()
     */
    public int getBindingOperationSize() {
        return bindingOps.size();
    }
    
    /**
     * @see com.sun.wsdl.model.Binding#addBindingOperation(
     *  com.sun.wsdl.model.BindingOperation)
     */
    public void addBindingOperation(BindingOperation op) {
    	bindingOps.add(op);
    	super.addChild(2, op);
    }
    
    public void addBindingOperationAtTail(BindingOperation op) {
    	bindingOps.add(op);
    	super.addChildAtTheEnd(op, 2);
    }
    
    /**
     * @see com.sun.wsdl.model.Binding#removeBindingOperation(
     *  com.sun.wsdl.model.BindingOperation)
     */
    public boolean removeBindingOperation(BindingOperation op) {
    	boolean result = bindingOps.remove(op);
    	super.removeChild(op);
        return result;
    }
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#accept(com.sun.wsdl.model.common.model.visitor.Visitor)
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
