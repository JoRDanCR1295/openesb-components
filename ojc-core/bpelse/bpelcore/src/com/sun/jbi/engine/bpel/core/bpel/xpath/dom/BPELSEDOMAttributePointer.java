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
 * @(#)BPELSEDOMAttributePointer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.xpath.dom;

import java.util.Locale;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.ri.NamespaceResolver;
import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.compiler.NodeTest;
import org.apache.commons.jxpath.ri.model.NodeIterator;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.apache.commons.jxpath.ri.model.dom.DOMAttributePointer;
import org.apache.xmlbeans.SchemaField;
import org.apache.xmlbeans.SchemaProperty;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.XmlBoolean;

public class BPELSEDOMAttributePointer extends DOMAttributePointer {

	private static final long serialVersionUID = 1L;

	private DOMAttributePointer delegate;
	private NodePointer parent;
	private QName name;
	
	private SchemaType mAttrSchemaType;
	
	public BPELSEDOMAttributePointer(DOMAttributePointer delegate, NodePointer parent, QName name) {
		super(null, null);
		this.delegate = delegate;
		this.parent = parent;
		this.name = name;
	}
	
    public QName getName() {
        return delegate.getName();
    }

    public String getNamespaceURI() {
        return delegate.getNamespaceURI();
    }

    /* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#getParent()
	 */
	@Override
	public NodePointer getParent() {
		return delegate.getParent();
	}
	
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#attributeIterator(org.apache.commons.jxpath.ri.QName)
	 */
	@Override
	public NodeIterator attributeIterator(QName qname) {
		return delegate.attributeIterator(qname);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#childIterator(org.apache.commons.jxpath.ri.compiler.NodeTest, boolean, org.apache.commons.jxpath.ri.model.NodePointer)
	 */
	@Override
	public NodeIterator childIterator(NodeTest test, boolean reverse,
			NodePointer startWith) {
		return delegate.childIterator(test, reverse, startWith);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#clone()
	 */
	@Override
	public Object clone() {
		DOMAttributePointer pointer = (DOMAttributePointer) delegate.clone();
		return new BPELSEDOMAttributePointer(pointer, parent, name);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(Object object) {
		return delegate.compareTo(object);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#createAttribute(org.apache.commons.jxpath.JXPathContext, org.apache.commons.jxpath.ri.QName)
	 */
	@Override
	public NodePointer createAttribute(JXPathContext context, QName name) {
		return delegate.createAttribute(context, name);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#createChild(org.apache.commons.jxpath.JXPathContext, org.apache.commons.jxpath.ri.QName, int, java.lang.Object)
	 */
	@Override
	public NodePointer createChild(JXPathContext context, QName name,
			int index, Object value) {
		return delegate.createChild(context, name, index, value);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#createChild(org.apache.commons.jxpath.JXPathContext, org.apache.commons.jxpath.ri.QName, int)
	 */
	@Override
	public NodePointer createChild(JXPathContext context, QName name, int index) {
		return delegate.createChild(context, name, index);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#createPath(org.apache.commons.jxpath.JXPathContext, java.lang.Object)
	 */
	@Override
	public NodePointer createPath(JXPathContext context, Object value) {
		return delegate.createPath(context, value);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#createPath(org.apache.commons.jxpath.JXPathContext)
	 */
	@Override
	public NodePointer createPath(JXPathContext context) {
		return delegate.createPath(context);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#getImmediateParentPointer()
	 */
	@Override
	public NodePointer getImmediateParentPointer() {
		return delegate.getImmediateParentPointer();
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#getImmediateValuePointer()
	 */
	@Override
	public NodePointer getImmediateValuePointer() {
		return delegate.getImmediateValuePointer();
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#getIndex()
	 */
	@Override
	public int getIndex() {
		return delegate.getIndex();
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#getLocale()
	 */
	@Override
	public Locale getLocale() {
		return delegate.getLocale();
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#getNamespaceResolver()
	 */
	@Override
	public NamespaceResolver getNamespaceResolver() {
		return delegate.getNamespaceResolver();
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#getNamespaceURI(java.lang.String)
	 */
	@Override
	public String getNamespaceURI(String prefix) {
		return delegate.getNamespaceURI(prefix);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#getNode()
	 */
	@Override
	public Object getNode() {
		return delegate.getNode();
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#getNodeValue()
	 */
	@Override
	public Object getNodeValue() {
		return delegate.getNodeValue();
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#getPointerByID(org.apache.commons.jxpath.JXPathContext, java.lang.String)
	 */
	@Override
	public Pointer getPointerByID(JXPathContext context, String id) {
		return delegate.getPointerByID(context, id);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#getPointerByKey(org.apache.commons.jxpath.JXPathContext, java.lang.String, java.lang.String)
	 */
	@Override
	public Pointer getPointerByKey(JXPathContext context, String key,
			String value) {
		return delegate.getPointerByKey(context, key, value);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#getRootNode()
	 */
	@Override
	public Object getRootNode() {
		return delegate.getRootNode();
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#getValuePointer()
	 */
	@Override
	public NodePointer getValuePointer() {
		return delegate.getValuePointer();
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#isAttribute()
	 */
	@Override
	public boolean isAttribute() {
		return delegate.isAttribute();
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#isContainer()
	 */
	@Override
	public boolean isContainer() {
		return delegate.isContainer();
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#isLanguage(java.lang.String)
	 */
	@Override
	public boolean isLanguage(String lang) {
		return delegate.isLanguage(lang);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#isNode()
	 */
	@Override
	public boolean isNode() {
		return delegate.isNode();
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#isRoot()
	 */
	@Override
	public boolean isRoot() {
		return delegate.isRoot();
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#namespaceIterator()
	 */
	@Override
	public NodeIterator namespaceIterator() {
		return delegate.namespaceIterator();
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#namespacePointer(java.lang.String)
	 */
	@Override
	public NodePointer namespacePointer(String namespace) {
		return delegate.namespacePointer(namespace);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#printPointerChain()
	 */
	@Override
	public void printPointerChain() {
		delegate.printPointerChain();
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#setAttribute(boolean)
	 */
	@Override
	public void setAttribute(boolean attribute) {
		delegate.setAttribute(attribute);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#setIndex(int)
	 */
	@Override
	public void setIndex(int index) {
		delegate.setIndex(index);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#setNamespaceResolver(org.apache.commons.jxpath.ri.NamespaceResolver)
	 */
	@Override
	public void setNamespaceResolver(NamespaceResolver namespaceResolver) {
		delegate.setNamespaceResolver(namespaceResolver);
	}
	/* (non-Javadoc)
	 * @see org.apache.commons.jxpath.ri.model.NodePointer#toString()
	 */
	@Override
	public String toString() {
		return delegate.toString();
	}
	public Object getValue() {
        Object retValue = delegate.getValue();
        if ((mAttrSchemaType != null) && XmlBoolean.type.isAssignableFrom(mAttrSchemaType)) {
            if (retValue.equals("true")) {
                return new Boolean(true);
            }
            return new Boolean(false);
        }
        return retValue;
    }
    
    public Object getBaseValue() {
        return delegate.getBaseValue();
    }
    
    public boolean isCollection() {
        return delegate.isCollection();
    }
    
    public int getLength() {
        return delegate.getLength();
    }    

    public Object getImmediateNode() {
        return delegate.getImmediateNode();
    }

    public boolean isActual() {
        return delegate.isActual();
    }

    public boolean isLeaf() {
        return delegate.isLeaf();
    }

    public boolean testNode(NodeTest nodeTest) {
        return delegate.testNode(nodeTest);
    }

    public void setValue(Object value) {
    	if (value instanceof Double &&
    			mAttrSchemaType != null) {
    		boolean isTargetNodeFloat = false;
    		int tCode = mAttrSchemaType.getBuiltinTypeCode();
    			
			 if ((tCode == SchemaType.BTC_DECIMAL)
					 || (tCode == SchemaType.BTC_DOUBLE)
					 || (tCode == SchemaType.BTC_FLOAT)) {
				 isTargetNodeFloat = true;
			 }
			 //remove trailing zero
			 if((!isTargetNodeFloat)&& ((((Double)value).doubleValue()- Math.floor((Double)value))==0)){
				 value  = ((Double)value).longValue();
			 } 
    		
    	}
    	delegate.setValue(value);
    }

    public void remove() {
        delegate.remove();
    }

    public String asPath() {
    	return delegate.asPath();
    }

    public int hashCode() {
        return delegate.hashCode();
    }

    public boolean equals(Object object) {
    	return delegate.equals(object);
    }

    public int compareChildNodePointers(NodePointer pointer1,
			NodePointer pointer2) {
		return delegate.compareChildNodePointers(pointer1, pointer2);
	}
    
    public void setAttrSchemaType(SchemaType type) {
    	mAttrSchemaType = type;
    }
    
    public SchemaType getAttrSchemaType() {
    	return mAttrSchemaType;
    }
}
