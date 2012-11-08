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
 * @(#)WSDLExtensibleElementImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;

import com.sun.wsdl.model.NamespaceUtility;
import com.sun.wsdl.model.WSDLExtensibleElement;
//import com.sun.wsdl.model.common.model.QName;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.extensions.ExtensibilityElement;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import javax.xml.namespace.QName;

/**
 * Implements the base for WSDL Extensibility Elements.
 *
 * @author Sun Microsystems
 * @version 
 */
public class WSDLExtensibleElementImpl 
extends WSDLElementImpl implements WSDLExtensibleElement, PropertyChangeListener {
    
    /** Holds the list of extensibility elements. */
    protected ArrayList extensibilityElems = new ArrayList();
    
    /** Creates a new instance of WSDLExtensibleElementImpl. */
    public WSDLExtensibleElementImpl() {
        super();
        initWSDLExtensibleElement();
    }
    
    /** Creates a new instance of WSDLExtensibleElementImpl.
     * @param   d   Owner document.
     */
    public WSDLExtensibleElementImpl(XMLDocument d) {
        super(d);
        if(d.isEnableEvents()) {
        	d.addPropertyChangeListener(this);
        }
        initWSDLExtensibleElement();
    }
    
    /** Initializes this class. */
    private void initWSDLExtensibleElement() {
        childrenTags = null;
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
		if (c instanceof ExtensibilityElement) {
			addExtensibilityElementAtTail((ExtensibilityElement) c);
        } else {
            super.addChildAtTail(c);
        }
	}
	
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof ExtensibilityElement) {
            addExtensibilityElement(1, (ExtensibilityElement) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(int index, XMLNode c) {
        if (c instanceof ExtensibilityElement) {
            addExtensibilityElement(index, (ExtensibilityElement) c);
        } else {
            super.addChild(index, c);
        }
    }
    
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof ExtensibilityElement) {
            removeExtensibilityElement((ExtensibilityElement) c);
        } else {
            super.removeChild(c);
        }
    }    
        
    /**
     * @see WSDLExtensibleElement#getExtensibilityElement
     */
    public ExtensibilityElement getExtensibilityElement(int i) {
        return (ExtensibilityElement) extensibilityElems.get(i);
    }
    
    
    /**
     * @see WSDLExtensibleElement#addExtensibilityElement(ExtensibilityElement)
     */
    public void addExtensibilityElement(ExtensibilityElement elem) {
    	extensibilityElems.add(elem);
    	super.addChild(elem);
    }
    
    /**
     * @see WSDLExtensibleElement#addExtensibilityElement(ExtensibilityElement)
     */
    public void addExtensibilityElementAtTail(ExtensibilityElement elem) {
    	extensibilityElems.add(elem);
    	super.addChildAtTheEnd(elem, 1);
    }
    
    /**
     * @see WSDLExtensibleElement#addExtensibilityElement(int, ExtensibilityElement)
     */
    private void addExtensibilityElement(int i, ExtensibilityElement elem) {
    	extensibilityElems.add(elem);
    	super.addChild(i, elem);
    }
    
    /**
     * @see WSDLExtensibleElement#removeExtensibilityElement(ExtensibilityElement)
     */
    public boolean removeExtensibilityElement(ExtensibilityElement elem) {
    	boolean result = extensibilityElems.remove(elem);
        super.removeChild(elem);
        return result;
    }
    
    /**
     * @see WSDLExtensibleElement#getExtensibilityElementsSize
     */
    public int getExtensibilityElementsSize() {
        return extensibilityElems.size();
    }
    
    /**
     * @see WSDLExtensibleElement#getExtensibilityElements
     */
    public Collection getExtensibilityElements() {
        return Collections.unmodifiableCollection(extensibilityElems);
    }
    
    
    public void propertyChange(PropertyChangeEvent evt) {
    	if(evt.getPropertyName().equals(PROP_PREFIX_NAME_CHANGED)) {
    		String oldPrefix = (String) evt.getOldValue();
    		String newPrefix = (String) evt.getNewValue();
    		
    		String qNameStr = this.getQualifiedName();
    		QName currentQName = NamespaceUtility.resolveAndGetQName(qNameStr, this);
    		if(oldPrefix != null && oldPrefix.equals(currentQName.getPrefix())) {
    			QName newQName = NamespaceUtility.getQName(currentQName.getNamespaceURI(),
    						currentQName.getLocalPart(), newPrefix);
    			this.setQualifiedName(newQName);
    		}
    	}
    }
	
}
