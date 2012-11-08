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
 * @(#)WSDLMessageImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;

import com.sun.wsdl.model.Part;
import com.sun.wsdl.model.WSDLMessage;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.visitor.WSDLVisitor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

/**
 * Implements the &lt;wsdl:message&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class WSDLMessageImpl extends NamedWSDLExtensibleElementImpl implements WSDLMessage {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 5351659340882024328L;
    
    /** Holds value of property parts. */
    private ArrayList parts = new ArrayList();
    
    /** Creates a new instance of WSDLMessageImpl */
    public WSDLMessageImpl() {
        super();
        initWSDLMessage();
    }
    
    /** Creates a new instance of WSDLMessageImpl.
     * @param   d   Owner document.
     */
    public WSDLMessageImpl(XMLDocument d) {
        super(d);
        initWSDLMessage();
        childrenTags = new String[] {
            Part.TAG
        };
    }
    
    /** Initializes this class.
     */
    private void initWSDLMessage() {
        setLocalName(WSDLMessage.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.NAME, String.class, false, null)
        };
    }
    
    /** @see com.sun.wsdl.model.impl.WSDLMessage#setName(java.lang.String, java.lang.String)
     */
    public void setName(String qName, String name) {
        setAttribute(NAME, qName, name);
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
		if (c instanceof Part) {
			addPartAtTail((Part) c);
        } else {
            super.addChildAtTail(c);
        }
	}
	
    /** @see com.sun.wsdl.model.common.model.XMLNode#addChild(com.sun.wsdl.model.common.model.XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Part) {
            addPart((Part) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see com.sun.wsdl.model.common.model.XMLNode#removeChild(com.sun.wsdl.model.common.model.XMLNode)
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Part) {
            removePart((Part) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /** @see com.sun.wsdl.model.impl.WSDLMessage#getPart(int)
     */
    public Part getPart(int index) {
        return (Part) parts.get(index);
    }
   
    /** getter for property part.
     * @param localName name of the property.
     * @return Value of the property with <CODE>localName</CODE>.
     *
     */
    public Part getPart(String localName) {
    	if(localName == null) {
    		return null;
    	}
    	
    	Part part = null;
    	Iterator it = parts.iterator();
    	while(it.hasNext()) {
    		Part p = (Part) it.next();
    		if(localName.equals(p.getName())) {
    			part = p;
    			break;
    		}
    	}
    	
    	return part;
    }
    
    
    /** @see com.sun.wsdl.model.impl.WSDLMessage#setPart(int,
     *  com.sun.wsdl.model.Part)
     */
    public void setPart(int index, Part part) {
        if (parts.size() == index) {
            addPart(part);
        } else {
        	Part oldPart = (Part) parts.get(index);
        	parts.set(index, part);
        	replaceChild(1, oldPart, part);
        }
    }
    
    /** @see com.sun.wsdl.model.impl.WSDLMessage#addPart(
     *  com.sun.wsdl.model.Part)
     */
    public void addPart(Part part) {
    	parts.add(part);
    	super.addChild(2, part);
    }
    
    public void addPartAtTail(Part part) {
    	parts.add(part);
    	super.addChildAtTheEnd(part, 2);
    }
    
    /** @see com.sun.wsdl.model.impl.WSDLMessage#removePart(
     *  com.sun.wsdl.model.Part)
     */
    public boolean removePart(Part p) {
        boolean result = parts.remove(p);
        super.removeChild(p);
        return result;
    }
    
    /** @see com.sun.wsdl.model.impl.WSDLMessage#getPartSize()
     */
    public int getPartSize() {
        return parts.size();
    }
    
    public Collection getParts() {
    	return Collections.unmodifiableCollection(this.parts);
    }
   
    
    /** @see com.sun.wsdl.model.common.model.XMLNode#accept(com.sun.wsdl.model.common.model.visitor.Visitor)
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
