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
 * @(#)PropertyAliasImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.bpel.impl;


import com.sun.wsdl.model.NamespaceUtility;
import com.sun.wsdl.model.bpel.PropertyAlias;
import com.sun.wsdl.model.bpel.Query;
import javax.xml.namespace.QName;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.extensions.impl.ExtensibilityElementImpl;
import com.sun.wsdl.model.visitor.WSDLVisitor;



/**
 * Implements the &lt;propertyAlias&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class PropertyAliasImpl extends ExtensibilityElementImpl
    implements PropertyAlias {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 5056597506291733884L;
    
    /** For child element for this PropertyAlias */
    private Query mQuery = null;
    
    /** Creates a new instance of PropertyAliasImpl */
    public PropertyAliasImpl() {
        super();
        initPropertyAlias();
    }
    
    /** Creates a new instance of PropertyAliasImpl.
     * @param   d   Owner document.
     */
    public PropertyAliasImpl(XMLDocument d) {
        super(d);
        initPropertyAlias();
    }
    
    /** Initializes this class.
     */
    private void initPropertyAlias() {
        setLocalName(PropertyAlias.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.PROPERTY_NAME, String.class, false, null),
            new XMLAttributeImpl(ATTR.MESSAGE_TYPE, String.class, false, null),
            new XMLAttributeImpl(ATTR.PART, String.class, false, null),
        };
    }
    
    /** Adds a child of the element.
     * @param   c   Child.
     */
    public void addChild(XMLNode c) {
        if(c instanceof Query) {
        	setQueryObject((Query) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** Removes a child of the element
     * @param   c   Child.
     */
    public void removeChild(XMLNode c) {
        if(c instanceof Query) {
        	setQueryObject(null);
        } else {
            super.removeChild(c);
        }
    }
    
    /** Getter for propertyName attribute.
     * @return  Value of propertyName attribute.
     */
    public QName getPropertyName() {
        String propQName = xmlAttrs[PROPERTY_NAME].getValue();
        if(propQName != null) {
            //return QName.getQNameFromString(propQName);
        	return NamespaceUtility.resolveAndGetQName(propQName, this);
        }
        
        return null;
    }
    
    /** Setter for propertyName attribute.
     * @param   n   Value of propertyName attribute.
     */
    public void setPropertyName(QName n) {
        if(n != null) {
            //setAttribute(PROPERTY_NAME, n.toString());
            setAttribute(PROPERTY_NAME, 
                         com.sun.wsdl.model.NamespaceUtility.getQNameAsString(n));
        } else {
            setAttribute(PROPERTY_NAME, null);
        }
    }
    
    /** Getter for messageType attribute.
     * @return  Value of messageType attribute.
     */
    public QName getMessageType() {
        String mesgQName = xmlAttrs[MESSAGE_TYPE].getValue();
        if(mesgQName != null) {
        	//return QName.getQNameFromString(mesgQName);
        	return NamespaceUtility.resolveAndGetQName(mesgQName, this);
        }
        
        return null;
    }
    
    /** Setter for messageType attribute.
     * @param   t   Value of messageType attribute.
     */
    public void setMessageType(QName t) {
        if(t != null) {
            //setAttribute(MESSAGE_TYPE, t.toString());
            setAttribute(MESSAGE_TYPE, 
                         com.sun.wsdl.model.NamespaceUtility.getQNameAsString(t));
        } else {
            setAttribute(MESSAGE_TYPE, null);
        }
    }
    
    /** Getter for part attribute.
     * @return  Value of part attribute.
     */
    public String getPart() {
        return xmlAttrs[PART].getValue();
    }
    
    /** Setter for part attribute.
     * @param   p   Value of part attribute.
     */
    public void setPart(String p) {
        setAttribute(PART, p);
    }
   
   
    /** Getter for query attribute.
     * @return  Value of query attribute.
     */
    public String getQuery() {
    	if(this.mQuery != null) {
        	return this.mQuery.getValue();
        }
        
        return null;
    }
    
    /** Setter for query attribute.
     * @param   s   Value of query attribute.
     */
    public void setQuery(String q) {
    	Query query = null;
    	if(q != null) {
    		query = new QueryImpl(getOwnerDocument());
    		query.setValue(q);
    	}
    	
        setQueryObject(query);
    }
    
    
    public Query getQueryObject() {
		return this.mQuery;
	}

	public void setQueryObject(Query query) {
		this.replaceChild(0, this.mQuery, query);
        mQuery = query;
	}

	/** @see com.sun.wsdl.model.common.model.XMLNode#accept(com.sun.wsdl.model.common.visitor.Visitor)
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
