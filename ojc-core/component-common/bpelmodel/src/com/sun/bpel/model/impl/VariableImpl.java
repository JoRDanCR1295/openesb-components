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
 * @(#)VariableImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import javax.wsdl.Message;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;container&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class VariableImpl extends BPELElementImpl implements Variable {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -7538096301543646373L;
    
    private Message mWSDLMessageType;
    private SchemaGlobalElement mXSDElement;
    private SchemaType mXSDType;
    
    /** Constructs new instance of container element.
     */
    public VariableImpl() {
        super();
        initContainer();
    }
    
    /** Constructs new instance of container element
     * @param   d   Owner document.
     */
    public VariableImpl(XMLDocument d) {
        super(d);
        initContainer();
    }
    
    /** Initializes this class.
     */
    private void initContainer() {
        setLocalName(Variable.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.NAME, String.class, false, null),
            new XMLAttributeImpl(ATTR.MESSAGE_TYPE, String.class, true, null),
            new XMLAttributeImpl(ATTR.TYPE, String.class, true, null),
            new XMLAttributeImpl(ATTR.ELEMENT, String.class, true, null),
            new XMLAttributeImpl(Activity.ATTR.GENERATE_EVENTS, String.class, true, XMLAttribute.BOOLEAN_ENUM_VALS)
        };
    }
    
    /* (non-Javadoc)
     * @see com.sun.bpel.model.Variable#initMembers()
     */
    public void initMembers() {
        initWSDLMessageType();
        initXSDElement();
        initXSDType();
    }

    private void initWSDLMessageType(){
        if(this.getMessageType() != null) {
            BPELDocument bpelDoc = (BPELDocument) this.getOwnerDocument();
            BPELProcess process = bpelDoc.getDocumentProcess();
            if(process != null) {
                mWSDLMessageType = process.getWSDLMessage(this.getMessageType());
            }
        }
    }
    
    private void initXSDElement(){
        if(this.getElement() != null) {
            BPELDocument bpelDoc = (BPELDocument) this.getOwnerDocument();
            BPELProcess process = bpelDoc.getDocumentProcess();
            if(process != null) {
                mXSDElement = BPELHelper.getXSDElement(this.getElement(), this);
            }
        }
    }
    
    private void initXSDType(){
        if(this.getType() != null) {
            BPELDocument bpelDoc = (BPELDocument) this.getOwnerDocument();
            BPELProcess process = bpelDoc.getDocumentProcess();
            if(process != null) {
                mXSDType = BPELHelper.getXSDType(this.getType(), this);
            }
        }
    }
    
    /** Getter for property name.
     * @return Value of property name.
     *
     */
    public String getName() {
        return xmlAttrs[NAME].getValue();
    }
    
    /** Setter for property name.
     * @param name New value of property name.
     *
     */
    public void setName(String name) {
        setAttribute(NAME, name);
    }
    
    /** Setter for property name.
     * @param qName New qName of property name.
     * @param name  New value of property name.
     *
     */
    public void setName(String qName, String name) {
        setAttribute(NAME, qName, name);
    }
    
    /** Getter for property messageType.
     * @return Value of property messageType.
     *
     */
    public QName getMessageType() {
        String msgType = xmlAttrs[MESSAGE_TYPE].getValue();
        if(msgType != null) {
        	return com.sun.bpel.xml.NamespaceUtility.resolveAndGetQName(
        			msgType, this);
        }
        
        return null;
    }
    
    /** Setter for property messageType.
     * @param messageType New value of property messageType.
     *
     */
    public void setMessageType(QName messageType) {
    	if(messageType != null) {
    		setAttribute(MESSAGE_TYPE, 
    				com.sun.bpel.xml.NamespaceUtility.getQNameAsString(messageType));
    	} else {
    		setAttribute(MESSAGE_TYPE, null);
    	}
    }
    
    /** Setter for property messageType.
     * @param qName         New qName of property messageType.
     * @param messageType   New value of property messageType.
     *
     */
    public void setMessageType(String qName, String messageType) {
        setAttribute(MESSAGE_TYPE, qName, messageType);
    }
    
    /**
     * Get xsd type property for this variable.
     * @return type
     */
    public QName getType() {
    	String typeQNameStr = xmlAttrs[TYPE].getValue();
    	if(typeQNameStr != null) {
    		return com.sun.bpel.xml.NamespaceUtility.resolveAndGetQName(
    				typeQNameStr, this);
    	}
    	
    	return null;
    }
    
    /**
     * Set xsd type property for this variable. 
     * @param type xsd type.
     */
    public void setType(QName type) {
    	if(type != null) {
    		setAttribute(TYPE, 
    				com.sun.bpel.xml.NamespaceUtility.getQNameAsString(type));
    	} else {
    		setAttribute(TYPE, null);
    	}
    }
    
    /** @see com.sun.bpel.model.Variable#getGenerateEvents()
     */
    public String getGenerateEvents() {
        return xmlAttrs[GENERATE_EVENTS].getValue();
    }
    
    /** @see com.sun.bpel.model.Variable#setGenerateEvents(java.lang.String)
     */
    public void setGenerateEvents(String generateEvents) {
        setAttribute(GENERATE_EVENTS, generateEvents);
    }
    
    /**
     * Get xsd element property for this variable.
     * @return xsd element.
     */
    public QName getElement() {
    	String elementQNameStr = xmlAttrs[ELEMENT].getValue();
    	if(elementQNameStr != null) {
    		return com.sun.bpel.xml.NamespaceUtility.resolveAndGetQName(
    				elementQNameStr, this);
    	}
    	
    	return null;
    }
    
    /**
     * set xsd element property for this variable.
     * @param element xsd element.
     */
    public void setElement(QName element) {
    	if(element != null) {
    		setAttribute(ELEMENT, 
    				com.sun.bpel.xml.NamespaceUtility.getQNameAsString(element));
    	} else {
    		setAttribute(ELEMENT, null);
    	}
    }
    
	public Message getWSDLMessageType() {
		return mWSDLMessageType;
	}
    
	public SchemaGlobalElement getXSDElement() {
		return mXSDElement;
	}
    
	public SchemaType getXSDType() {
		return mXSDType;
	}

	/** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean accept(Visitor w) {
        BPELVisitor v = morphVisitor(w);
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

    public void performDeferredAction() {
        initMembers();
    }
}
