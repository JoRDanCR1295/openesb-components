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
 * @(#)ReplyImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.ToPart;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Correlations;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.Reply;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;reply&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class ReplyImpl extends ActivityImpl implements Reply {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 8210543911122918451L;
    
    /** Holds value of property correlations. */
    private Correlations correlations;
    
    private PartnerLink mPartnerLink;
    
    private Variable mVariable;
    
    private PortType mPortType;
    
    private Operation mOperation;
  
    private ToPart mToPart;
    
    /** Creates a new instance of Receive.
     */
    public ReplyImpl() {
        super();
        initReply();
    }
    
    /** Creates a new instance of Receive.
     * @param   d   Owner document.
     */
    public ReplyImpl(XMLDocument d) {
        super(d);
        initReply();
    }
    
    /** Initializes this class.
     */
    private void initReply() {
        setLocalName(Reply.TAG);
        xmlAttrs = new XMLAttribute[NUM_ATTRS];
        // NUM_STANDARD_ATTRS is equal to standardXmlAttrs.length.
        for (int i = 0; i < NUM_STANDARD_ATTRS; i++) {
        	xmlAttrs[i] = standardXmlAttrs[i];
        }
        xmlAttrs[PARTNERLINK] = new XMLAttributeImpl(Reply.ATTR.PARTNERLINK, String.class, false, null);
        xmlAttrs[PORT_TYPE] = new XMLAttributeImpl(Reply.ATTR.PORT_TYPE, String.class, false, null);
        xmlAttrs[OPERATION] = new XMLAttributeImpl(Reply.ATTR.OPERATION, String.class, false, null);
        xmlAttrs[VARIABLE] = new XMLAttributeImpl(Reply.ATTR.VARIABLE, String.class, false, null);
        xmlAttrs[FAULT_NAME] = new XMLAttributeImpl(Reply.ATTR.FAULT_NAME, String.class, true, null);
        xmlAttrs[MESSAGE_EXCHANGE] = new XMLAttributeImpl(Reply.ATTR.MESSAGE_EXCHANGE, String.class, true, null);

        childrenTags = new String[] {
            Correlations.TAG
        };
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
        XMLAttribute value = getAttribute(Reply.ATTR.PORT_TYPE);
    	if(value != null) {
            return com.sun.bpel.xml.NamespaceUtility.resolveAndGetQName(
            		value.getValue(), this);
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
    
    /** Setter for property container.
     * @param container New value of property container.
     *
     */
    public void setVariable(String variable) {
        String oldVariable = getVariable();
    	setAttribute(VARIABLE, variable);
        //if variable is changed we need to clear out cached variable object
        //so that next call to getBPELVariable can find new variable object
        if(variable == null || !variable.equals(oldVariable)) {
        	this.mVariable = null;
        }
    }
    
    /** Getter for property faultName.
     * @return Value of property faultName.
     *
     */
    public QName getFaultName() {
        String faultName = xmlAttrs[FAULT_NAME].getValue();
        if(faultName != null) {
        	return com.sun.bpel.xml.NamespaceUtility.resolveAndGetQName(
        			faultName, this);
        }
        
        return null;
    }
    
    /** Setter for property faultName.
     * @param faultName New value of property faultName.
     *
     */
    public void setFaultName(QName faultName) {
    	if(faultName != null) {
    		setAttribute(FAULT_NAME, 
    				com.sun.bpel.xml.NamespaceUtility.getQNameAsString(faultName));
    		
    	} else {
    		setAttribute(FAULT_NAME, null);
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
        } else if (c instanceof ToPart) { 
        	setToPart((ToPart) c);
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
        } else if(c instanceof ToPart) { 
        	setToPart(null);
        } else {
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
    
      public ToPart getToPart() {
        return this.mToPart;
    }
    
    public void setToPart(ToPart toPart) {
    	ToPart oldToPart = this.mToPart;
    	this.mToPart = toPart;
    	super.replaceChild(4, oldToPart, toPart);
        
    }
    
    public Variable getBPELVariable() {
    	if(this.mVariable != null) {
			return this.mVariable;
		}
		
		if (BPELHelper.isValueAbsent(getVariable())) {
			return null;
		}
		
		this.mVariable = BPELHelper.getMatchingVariable(getVariable(), this);
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

	

	/** Getter for property operation.
     * @return Value of property operation.
     *
     */
    public Operation getWSDLOperation() {
    	PortType portType = getWSDLPortType();
    	if(portType == null) {
    		return null;
    	}
    	
    	return BPELHelper.getFirstMatchingWSDLOperation(portType, getOperation());
    	
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
    
}
