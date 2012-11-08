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
 * @(#)InvokeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.FromPart;
import com.sun.bpel.model.ToPart;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.Catch;
import com.sun.bpel.model.CatchAll;
import com.sun.bpel.model.CompensationHandler;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Correlations;
import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;invoke&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class InvokeImpl extends ActivityImpl implements Invoke {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -2509777934806254290L;
    
    /** Holds value of sub-element correlations. */
    private Correlations correlations;
    
    private PartnerLink mPartnerLink;
    
    private Variable mInputVariable;
    
    private Variable mOutputVariable;
    
    private PortType mPortType;
    
    private Operation mOperation;
   
    private FromPart mFromPart;
    
    private ToPart mToPart;
    
    /** Holds list of catch sub-elements. */
    private ArrayList catches = new ArrayList();
    
    /** Holds value of sub-element catchAll. */
    private CatchAll catchAll;
    
    /** Holds value of sub-element compensationHandler. */
    private CompensationHandler compensationHandler;
    
    /** Creates a new instance of Invoke.
     */
    public InvokeImpl() {
        super();
        initInvoke();
    }
    
    /** Creates a new instance of Invoke.
     * @param   d   Owner document.
     */
    public InvokeImpl(XMLDocument d) {
        super(d);
        initInvoke();
    }
    
    /** Initializes this class.
     */
    private void initInvoke() {
        setLocalName(Invoke.TAG);
        xmlAttrs = new XMLAttribute[NUM_ATTRS];
        // NUM_STANDARD_ATTRS is equal to standardXmlAttrs.length.
        for (int i = 0; i < NUM_STANDARD_ATTRS; i++) {
        	xmlAttrs[i] = standardXmlAttrs[i];
        }
        xmlAttrs[PARTNERLINK] = new XMLAttributeImpl(Invoke.ATTR.PARTNERLINK, String.class, false, null);
        xmlAttrs[PORT_TYPE] = new XMLAttributeImpl(Invoke.ATTR.PORT_TYPE, String.class, false, null);
        xmlAttrs[OPERATION] = new XMLAttributeImpl(Invoke.ATTR.OPERATION, String.class, false, null);
        xmlAttrs[INPUT_VARIABLE] = new XMLAttributeImpl(Invoke.ATTR.INPUT_VARIABLE, String.class, false, null);
        xmlAttrs[OUTPUT_VARIABLE] = new XMLAttributeImpl(Invoke.ATTR.OUTPUT_VARIABLE, String.class, true, null);

        childrenTags = new String[] {
            Correlations.TAG,
            Catch.TAG,
            CatchAll.TAG,
            CompensationHandler.TAG
        };
    }
    
    /** @see Invoke#getPartner
     */
    public String getPartnerLink() {
        return xmlAttrs[PARTNERLINK].getValue();
    }
    
    /** @see Invoke#setPartnerLink(String)
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
   
   
    /** @see Invoke#getPortType
     */
    public QName getPortType() {
        String pt = xmlAttrs[PORT_TYPE].getValue();
        if(pt != null) {
        	return com.sun.bpel.xml.NamespaceUtility.resolveAndGetQName(pt, this);
        }
        
        return null;
    }
    
    /** @see Invoke#setPortType(QName)
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
    
    
    /** @see Invoke#getOperation
     */
    public String getOperation() {
        return xmlAttrs[OPERATION].getValue();
    }
    
    /** @see Invoke#setOperation(String)
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
    
    
    /** @see Invoke#getInputContainer
     */
    public String getInputVariable() {
        return xmlAttrs[INPUT_VARIABLE].getValue();
    }
    
    /** @see Invoke#setInputContainer
     */
    public void setInputVariable(String variable) {
        String oldInputVariable = getInputVariable();
    	setAttribute(INPUT_VARIABLE, variable);
        //if variable is changed we need to clear out cached variable object
        //so that next call to getInputBPELVariable can find new variable object
        if(variable == null || !variable.equals(oldInputVariable)) {
        	this.mInputVariable = null;
        }
    }
    
    /** @see Invoke#getOutputContainer
     */
    public String getOutputVariable() {
        return xmlAttrs[OUTPUT_VARIABLE].getValue();
    }
    
    /** @see Invoke#setOutputContainer
     */
    public void setOutputVariable(String variable) {
        String oldOutputVariable = getOutputVariable();
    	setAttribute(OUTPUT_VARIABLE, variable);
        //if variable is changed we need to clear out cached variable object
        //so that next call to getOutputBPELVariable can find new variable object
        if(variable == null || !variable.equals(oldOutputVariable)) {
        	this.mOutputVariable = null;
        }
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Correlations) {
            setCorrelations((Correlations) c);
        } else if (c instanceof Catch) {
            addCatch((Catch) c);
        } else if (c instanceof CatchAll) {
            setCatchAll((CatchAll) c);
        } else if (c instanceof CompensationHandler) {
            setCompensationHandler((CompensationHandler) c);
        } else if(c instanceof FromPart) { 
        	setFromPart((FromPart)c);
        } else if(c instanceof ToPart) {
        	setToPart((ToPart) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Correlations) {
            setCorrelations(null);
        } else if (c instanceof Catch) {
            removeCatch((Catch) c);
        } else if (c instanceof CatchAll) {
            setCatchAll(null);
        } else if (c instanceof CompensationHandler) {
            setCompensationHandler(null);
        } else if(c instanceof FromPart) {
        	setFromPart(null);
        } else if(c instanceof ToPart) {
        	setToPart(null);
        }
        else {
            super.removeChild(c);
        }
    }
    
    /** @see Invoke#getCorrelations
     */
    public Correlations getCorrelations() {
        return this.correlations;
    }
    
    /** @see Invoke#setCorrelations
     */
    public void setCorrelations(Correlations correlations) {
        super.replaceChild(3, this.correlations, correlations);
        this.correlations = correlations;
    }
    
    /** @see Invoke#getCatch
     */
    public Catch getCatch(int index) {
        return (Catch) catches.get(index);
    }
    
    /** @see Invoke#setCatch
     */
    public synchronized void setCatch(int index, Catch c) {
        if (catches.size() == index) {
            addCatch(c);
        } else {
            replaceChild(4, (Catch) catches.get(index), c);
            catches.set(index, c);
        }
    }
    
    /** @see Invoke#getCatchSize
     */
    public int getCatchSize() {
        return catches.size();
    }
    
    /** @see Invoke#addCatch(Catch)
     */
    public synchronized void addCatch(Catch c) {
        super.addChild(4, c);
        catches.add(c);
    }
    
    /** @see Invoke#addCatch(int, Catch)
     */
    public synchronized void addCatch(int index, Catch c) {
        if ((index < 0) || (index > catches.size())) {
            throw new ArrayIndexOutOfBoundsException("Expected: 0 <= index <= " + catches.size());
        } else if (index == catches.size()) {
            addCatch(c);
        } else {
            super.addChild(4, getCatch(index), c);
            catches.add(index, c);
        }
    }
    
    /** @see Invoke#clearCatches
     */
    public synchronized void clearCatches() {
        while (catches.size() > 0) {
            removeCatch(0);  // stays at 0 because array elements keep shifting to the left
        }
    }
    
    /** @see Invoke#removeCatch(Catch)
     */
    public synchronized boolean removeCatch(Catch c) {
        super.removeChild(c);
        return catches.remove(c);
    }
    
    /** @see Invoke#removeCatch(int)
     */
    public synchronized void removeCatch(int i) {
        removeCatch(getCatch(i));
    }
    
    /** @see Invoke#indexOfCatch
     */
    public int indexOfCatch(XMLNode c) {
        return catches.indexOf(c);
    }
    
    /** @see Invoke#getCatches
     */
    public synchronized Collection getCatches() {
        return Collections.unmodifiableCollection((ArrayList) catches.clone());
    }
    
    /** @see Invoke#getCatchAll
     */
    public CatchAll getCatchAll() {
        return catchAll;
    }
    
    /** @see Invoke#setCatchAll
     */
    public void setCatchAll(CatchAll catchAll) {
        super.replaceChild(5, this.catchAll, catchAll);
        this.catchAll = catchAll;
    }
    
    /** @see Invoke#getCompensationHandler
     */
    public CompensationHandler getCompensationHandler() {
        return compensationHandler;
    }
    
    /** @see Invoke#setCompensationHandler
     */
    public void setCompensationHandler(
                                       CompensationHandler compensationHandler) {
        super.replaceChild(6, this.compensationHandler, compensationHandler);
        this.compensationHandler = compensationHandler;
    }
    
    
    public ToPart getToPart() {
        return this.mToPart;
    }
    
    public void setToPart(ToPart toPart) {
    	ToPart oldToPart = this.mToPart;
    	this.mToPart = toPart;
    	super.replaceChild(7, oldToPart, toPart);
        
    }
    
    public FromPart getFromPart() {
        return this.mFromPart;
    }
    
    public void setFromPart(FromPart fromPart) {
    	FromPart oldFromPart = this.mFromPart;
    	this.mFromPart = fromPart;
    	super.replaceChild(8, oldFromPart, fromPart);
        
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

	public Variable getInputBPELVariable() {
		if(this.mInputVariable != null) {
			return this.mInputVariable;
		}
		
		if (BPELHelper.isValueAbsent(getInputVariable())) {
			return null;
		}
		
		this.mInputVariable = BPELHelper.getMatchingVariable(getInputVariable(), this);
		return this.mInputVariable;
	}

	public Variable getOutputBPELVariable() {
		if(this.mOutputVariable != null) {
			return this.mOutputVariable;
		}
		
		if (BPELHelper.isValueAbsent(getOutputVariable())) {
			return null;
		}
		
		this.mOutputVariable = BPELHelper.getMatchingVariable(getOutputVariable(), this);
		return this.mOutputVariable;
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

	public void setInputBPELVariable(Variable variable) {
		//update the inputVariable attribute value
		if(variable != null) {
			setInputVariable(variable.getName());
		} else {
			setInputVariable(null);
		}
		
		this.mInputVariable = variable;
	}

	public void setOutputBPELVariable(Variable variable) {
		//update the outputVariable attribute value
		if(variable != null) {
			setOutputVariable(variable.getName());
		} else {
			setOutputVariable(null);
		}
		
		this.mOutputVariable = variable;
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

	/** @see XMLNode#accept
     */
    public boolean accept(Visitor w) {
        BPELVisitor v = morphVisitor(w);
        if (traverseParentFirst(v)) {
            XMLNode parent = getParent();
            if (!v.visit(this)) {
                return false;
            }
            
            // Invoke has been expanded and now belongs to a scope so don't continue traversal
            // here; traversal already occurred fully from the new scope
            if (parent != getParent()) {
                return true;
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
