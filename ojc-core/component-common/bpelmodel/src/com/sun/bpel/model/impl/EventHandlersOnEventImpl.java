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
 * @(#)EventHandlersOnEventImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.EventHandlersOnEvent;
import com.sun.bpel.model.From;
import com.sun.bpel.model.FromPart;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Correlations;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.NamespaceUtility;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;receive&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class EventHandlersOnEventImpl extends ActivityImpl implements EventHandlersOnEvent {
    
//    /** serialVersionUID for this class */
    static final long serialVersionUID = 2776768684869974562L;
    
    /** Holds value of property correlations. */
    private Correlations correlations;
    
    private PartnerLink mPartnerLink;
    
    private Variable mVariable;
    
    private PortType mPortType;
    
    private Operation mOperation;
    
    private FromPart mFromPart;
    
    private QName mMessageType;
    
    private QName mElement;
    
    private Scope mActivity;
            
    /** Creates a new instance of EventHandlersOnEvent.
     */
    public EventHandlersOnEventImpl() {
        super();
        initEventHandlersOnEvent();
    }
    
    /** Creates a new instance of EventHandlersOnEvent.
     * @param   d   Owner document.
     */
    public EventHandlersOnEventImpl(XMLDocument d) {
        super(d);
        initEventHandlersOnEvent();
    }
    
    /** Initializes this class.
     */
    private void initEventHandlersOnEvent() {
        setLocalName(EventHandlersOnEvent.TAG);
        xmlAttrs = new XMLAttribute[NUM_ATTRS];
        // NUM_STANDARD_ATTRS is equal to standardXmlAttrs.length.
        for (int i = 0; i < NUM_STANDARD_ATTRS; i++) {
        	xmlAttrs[i] = standardXmlAttrs[i];
        }
        xmlAttrs[PARTNERLINK] = new XMLAttributeImpl(EventHandlersOnEvent.ATTR.PARTNERLINK, String.class, false, null);
        xmlAttrs[PORT_TYPE] = new XMLAttributeImpl(EventHandlersOnEvent.ATTR.PORT_TYPE, String.class, false, null);
        xmlAttrs[OPERATION] = new XMLAttributeImpl(EventHandlersOnEvent.ATTR.OPERATION, String.class, false, null);
        xmlAttrs[VARIABLE] = new XMLAttributeImpl(EventHandlersOnEvent.ATTR.VARIABLE, String.class, false, null);
        xmlAttrs[MESSAGE_EXCHANGE] = new XMLAttributeImpl(EventHandlersOnEvent.ATTR.MESSAGE_EXCHANGE, String.class, 
        		true, null);
        xmlAttrs[MESSAGE_TYPE] =  new XMLAttributeImpl(EventHandlersOnEvent.ATTR.MESSAGETYPE, String.class, true, null);
        xmlAttrs[ELEMENT_POSITION] = new XMLAttributeImpl(EventHandlersOnEvent.ATTR.ELEMENT, String.class, true, null);

        childrenTags = new String[] {
            Correlations.TAG,
            From.TAG,
            Scope.TAG
        };
    }
    
    /* (non-Javadoc)
     * @see com.sun.bpel.model.EventHandlersOnEvent#initMembers()
     */
    public void initMembers() {
        initMembersLocally();
        resolve();
    }
    
    public void initMembersLocally() {
        if (!BPELHelper.isValueAbsent(getVariable())) {
            //Create a new variable with this name
            mVariable = ((BPELDocument) getOwnerDocument()).createVariable();
            mVariable.setParent(this);
            mVariable.setName(getVariable());
            mVariable.setMessageType(getMessageType());
            mVariable.setElement(getElement());
        }
    }

    private void resolve() {
        if (!BPELHelper.isValueAbsent(getVariable())) {
            mVariable.initMembers();
        }
    }
    
    /** Getter for property partner.
     * @return Value of property partner.
     *
     */
    public String getPartnerLink() {
    	return xmlAttrs[PARTNERLINK].getValue();
    }
    
    /** Setter for property partner.
     * @param partner New value of property partner.
     *
     */
    public void setPartnerLink(String partner) {
    	String oldPartner = getPartnerLink();
        setAttribute(PARTNERLINK, partner);
        //if partnerLink is changed we need to clear out cached partnerLink object
        //so that next call to getBPELPartnerLink can find new PartnerLink object
        if(partner == null || !partner.equals(oldPartner)) {
        	this.mPartnerLink = null;
        }
    }
   
    /** Getter for property portType.
     * @return Value of property portType.
     *
     */
    public QName getPortType() {
        String portTypeQNameStr = xmlAttrs[PORT_TYPE].getValue();
    	if(portTypeQNameStr != null) {
            return com.sun.bpel.xml.NamespaceUtility.resolveAndGetQName(
            		portTypeQNameStr, this);
    	}
    	
    	return null;
    }
    
    
    /** Setter for property portType.
     * @param portType New value of property portType.
     *
     */
    public void setPortType(QName portType) {
    	QName oldPortType = getPortType();
    	if(portType != null) {
    		setAttribute(PORT_TYPE,
    			com.sun.bpel.xml.NamespaceUtility.getQNameAsString(portType));
    		
    	} else {
    		setAttribute(PORT_TYPE, null);
    	}
    	
        //if portType is changed we need to clear out cached portType object
        //so that next call to getWSDLPortType can find new portType object
        if(portType == null || !portType.equals(oldPortType)) {
        	this.mPortType = null;
        }
    }
    
    
    /** Getter for property operation.
     * @return Value of property operation.
     *
     */
    public String getOperation() {
    	return xmlAttrs[OPERATION].getValue();
    }
    
    /** Setter for property operation.
     * @param operation New value of property operation.
     *
     */
    public void setOperation(String operation) {
        String oldOperation = getOperation();
        setAttribute(OPERATION, operation);
        //if operation is changed we need to clear out cached operation object
        //so that next call to getWSDLOperation can find new Operation object
        if(operation == null || !operation.equals(oldOperation)) {
        	this.mOperation = null;
        }
    }
    
    
    /** Getter for property container.
     * @return Value of property container.
     *
     */
    public String getVariable() {
    	return xmlAttrs[VARIABLE].getValue();
    }
    
    /** Setter for property variable.
     * @param variable New value of property container.
     *
     */
    public void setVariable(String variable) {
        String oldVariable = getVariable();
    	setAttribute(VARIABLE, variable);
        //if variable is changed we need to clear out cached variable object
        //so that next call to getBPELVariable can find new variable object
        if(variable == null || !variable.equals(oldVariable)) {
        	this.mVariable = null;
            initMembersLocally();
            resolve();
        }
    }
    
    
    /** Getter for property messageExchange.
     * @return value of property messageExchange.
     *
     */
    public String getMessageExchange() {
    	return xmlAttrs[MESSAGE_EXCHANGE].getValue();
    }

    /** Setter for property messageExchange.
     * @param value New value of property messageExchange.
     *
     */
    public void setMessageExchange(String value) {
        setAttribute(MESSAGE_EXCHANGE, value);
    }
    
    /** Adds a child of the element.
     * @param   c   Child.
     */
    public void addChild(XMLNode c) {
        if (c instanceof Correlations) {
            setCorrelations((Correlations) c);
        } else if (c instanceof FromPart) {
        	setFromPart((FromPart) c);
        } else if ( c instanceof Scope) {
            setActivity((Scope) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** Removes a child of the element
     * @param   c   Child.
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Correlations) {
            setCorrelations(null);
        } else if (c instanceof FromPart) {
        	setFromPart(null);
        } else if (c instanceof Scope) {
            setActivity(null);
        }else {
            super.removeChild(c);
        }
    }
    
    /** Getter for property correlations.
     * @return property correlations.
     */
    public Correlations getCorrelations() {
        return correlations;
    }
    
    /** Setter for property correlations.
     * @param correlations New value of property correlations.
     *
     */
    public void setCorrelations(Correlations correlations) {
        super.replaceChild(3, this.correlations, correlations);
        this.correlations = correlations;
    }
    
    
    public FromPart getFromPart() {
        return this.mFromPart;
    }
    
    public void setFromPart(FromPart fromPart) {
    	FromPart oldFromPart = this.mFromPart;
    	this.mFromPart = fromPart;
    	super.replaceChild(4, oldFromPart, fromPart);
    }
    
    public Variable getBPELVariable() {
		return this.mVariable;
	}

	public PartnerLink getBPELPartnerLink() {
		if(this.mPartnerLink != null) {
			return this.mPartnerLink;
		}
		
		if (BPELHelper.isValueAbsent(getPartnerLink())) {
			return null;
		}
		 
		this.mPartnerLink = BPELHelper.getMatchingPartnerLink(getPartnerLink(), this);
		return this.mPartnerLink;
		
	}
    
    public void setBPELPartnerLink(PartnerLink partnerLink) {
		//update the partnerLink attribute value
		if(partnerLink != null) {
			setPartnerLink(partnerLink.getName());
		} else {
			setPartnerLink(null);
		}
		
		this.mPartnerLink = partnerLink;
		
	}

	public void setBPELVariable(Variable variable) {
		//update the variable attribute value
		if(variable != null) {
			setVariable(variable.getName());
		} else {
			setVariable(null);
		}
		
		this.mVariable = variable;
		
	}
	
	
	public PortType getWSDLPortType() {
		if(this.mPortType != null) {
			return this.mPortType;
		}
		
		this.mPortType = BPELHelper.getWSDLPortType(getPortType(), this);
		
		return this.mPortType;
	}
	
	public void setWSDLPortType(PortType portType) {
		//update the portType attribute value
		if(portType != null) {
			
			if(portType.getQName().getNamespaceURI() != null) {
                setPortType(portType.getQName());
			} else {
				throw new IllegalArgumentException("can not set portType, portType "+ portType.getQName() + " 's owner WSDLDocument does not have targetNamespace attribute defined");
			}
			
		}
		
		this.mPortType = portType;
	}

	/** Getter for property operation.
     * @return Value of property operation.
     *
     */
    public Operation getWSDLOperation() {
    	if(this.mOperation != null) {
    		return this.mOperation;
    	}
    	
    	PortType portType = getWSDLPortType();
    	if(portType == null) {
    		return null;
    	}
    	
    	this.mOperation = BPELHelper.getFirstMatchingWSDLOperation(portType, getOperation());
    	
    	return this.mOperation;
    }
    
    /** Setter for property operation.
     * @param operation New value of property operation.
     *
     */
    public void setWSDLOperation(Operation operation) {
    	//update the operation attribute value
    	if(operation != null) {
    		setOperation(operation.getName());
    	} else {
    		setOperation(null);
    	}
    	
    	this.mOperation = operation;
    }
	/** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
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

    public void setMessageType(QName messageType) {
        // TODO Auto-generated method stub
        QName oldMessageType = getMessageType();
        if(messageType != null) {
        	setAttribute(MESSAGE_TYPE, 
        			com.sun.bpel.xml.NamespaceUtility.getQNameAsString(messageType));
        } else {
            setAttribute(MESSAGE_TYPE, null);
        }
        
        //if portType is changed we need to clear out cached portType object
        //so that next call to getWSDLPortType can find new portType object
        if(messageType == null || !messageType.equals(oldMessageType)) {
            this.mMessageType = null;
        }
    }

    public QName getMessageType() {
        if (mMessageType != null) {
        // TODO Auto-generated method stub
            return mMessageType;
        }
        
        String messageTypeQNameStr = xmlAttrs[MESSAGE_TYPE].getValue();
        if(messageTypeQNameStr != null) {
            mMessageType = com.sun.bpel.xml.NamespaceUtility.resolveAndGetQName(
            		messageTypeQNameStr, this);
            return mMessageType;
        }
        
        return null;
    }

    public Activity getActivity() {
        // TODO Auto-generated method stub
        return mActivity;
    }

    public void setActivity(Activity activity) {
        // TODO Auto-generated method stub
        super.replaceChild(5, this.mActivity, activity);
        this.mActivity = (Scope) activity;

        
    }

    public void setElement(QName element) {
        QName  oldElement = getElement();
        if(element != null) {
        	setAttribute(ELEMENT_POSITION, 
        			com.sun.bpel.xml.NamespaceUtility.getQNameAsString(element));
        } else {
            setAttribute(ELEMENT_POSITION, null);
        }
        
        //if portType is changed we need to clear out cached portType object
        //so that next call to getWSDLPortType can find new portType object
        if(element == null || !element.equals(oldElement)) {
            this.mElement = null;
        }
        
    }

    public QName getElement() {
        // TODO Auto-generated method stub
        if (mElement != null) {
            return mElement;
        }
        String elementQNameStr = xmlAttrs[ELEMENT_POSITION].getValue();
        if(elementQNameStr != null) {
            mElement = com.sun.bpel.xml.NamespaceUtility.resolveAndGetQName(
            		elementQNameStr, this);
            return mElement;
        }
        
        return null;

    }

    public Variable getBPELVariable(String vName) {
        String varName = getVariable();
        // The assumption here is that the vName passed to us will never be null.
        if (vName.equals(varName)) {
            return getBPELVariable();
        } else {
            return null;
        }
        
    }

    public void performDeferredAction() {
        resolve();
    }
}
