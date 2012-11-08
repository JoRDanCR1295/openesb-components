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
 * @(#)CatchImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import javax.wsdl.Message;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.Catch;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Catch.ATTR;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;


/**
 * Implements the &lt;catch&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class CatchImpl extends BPELElementImpl implements Catch {
     
    /** serialVersionUID for this class */
    static final long serialVersionUID = -1053005665439270637L;
    
    /** Holds value of element activity. */
    private Activity activity;
    
    private Variable mFaultVariable;
    
    private Message mFaultMessageType;
    
    private SchemaGlobalElement mFaultElement;
    
    /** Constructs new instance of catch element.
     */
    public CatchImpl() {
        super();
        initCatch();
    }
    
    /** Constructs new instance of catch element.
     * @param   d   Owner document.
     */
    public CatchImpl(XMLDocument d) {
        super(d);
        initCatch();
    }
    
    /** Initializes this class.
     */
    private void initCatch() {
        setLocalName(Catch.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.FAULT_NAME, String.class, true, null),
            new XMLAttributeImpl(ATTR.FAULT_CONTAINER, String.class, true, null),
            new XMLAttributeImpl(ATTR.FAULT_MESSAGE_TYPE, String.class, true, null),
            new XMLAttributeImpl(ATTR.FAULT_ELEMENT, String.class, true, null)
        };
        childrenTags = new String[] {
            Activity.TAG
        };
    }
    
    
    /* (non-Javadoc)
     * @see com.sun.bpel.model.Catch#initMembers()
     */
    public void initMembers() {
        initMembersLocally();
        resolveFaultVariable();
        initXSDFaultElement();
    }
    
    public void initMembersLocally() {
        initBPELFaultVariable();
        initWSDLFaultMessageType();
    }
    
    private void resolveFaultVariable() {
        if (!BPELHelper.isValueAbsent(getFaultVariable())) {
            mFaultVariable.initMembers();
        }
    }

    private void initWSDLFaultMessageType(){
        QName faultMessageType = getFaultMessageType();
        if (faultMessageType != null) {
            this.mFaultMessageType = BPELHelper.getWSDLMessage(faultMessageType, this);
        }        
    }
    
    private void initXSDFaultElement(){
        QName faultElement = getFaultElement();
        if (faultElement != null) {
            this.mFaultElement = BPELHelper.getXSDElement(faultElement, this);
        }        
    }
    
    private void initBPELFaultVariable() {
        if (!BPELHelper.isValueAbsent(getFaultVariable())) {
            mFaultVariable = ((BPELDocument) getOwnerDocument()).createVariable();
            mFaultVariable.setName(getFaultVariable());
            mFaultVariable.setMessageType(getFaultMessageType());
            mFaultVariable.setElement(getFaultElement());
        }
    }

    /** Getter for property faultName.
     * @return Value of property faultName.
     *
     */
    public QName getFaultName() {
        String faultName = xmlAttrs[FAULT_NAME].getValue();
        if(faultName != null) {
        	return com.sun.bpel.xml.NamespaceUtility.resolveAndGetQName(faultName, this);
        }
        
        return null;
    }
    
    /** Setter for property faultName.
     * @param faultName New value of property faultName.
     *
     */
    public void setFaultName(QName faultQName) {
    	if(faultQName != null) {
    		setAttribute(FAULT_NAME, 
    					 com.sun.bpel.xml.NamespaceUtility.getQNameAsString(faultQName));
    	} else {
    		setAttribute(FAULT_NAME, null);
    	}
    }
    
    /** Getter for property faultContainer.
     * @return Value of property faultContainer.
     *
     */
    public String getFaultVariable() {
        return xmlAttrs[FAULT_CONTAINER].getValue();
    }
    
    /** Setter for property faultContainer.
     * @param faultContainer New value of property faultContainer.
     *
     */
    public void setFaultVariable(String faultVariable) {
        String oldFaultVariable = getFaultVariable();
        setAttribute(FAULT_CONTAINER, faultVariable);
        //if variable is changed we need to clear out cached variable object
        //so that next call to getBPELFaultVariable can find new variable object
        if(faultVariable == null || !faultVariable.equals(oldFaultVariable)) {
        	this.mFaultVariable = null;
            initBPELFaultVariable();
            resolveFaultVariable();
        }
    }
    
	public QName getFaultMessageType() {
		String faultMessageTypeQNameStr = xmlAttrs[FAULT_MESSAGE_TYPE].getValue();
        if(faultMessageTypeQNameStr != null) {
        	return com.sun.bpel.xml.NamespaceUtility.resolveAndGetQName(
        				faultMessageTypeQNameStr, this);
        }
        
        return null;
	}
	
	public void setFaultMessageType(QName faultMessageType) {
		if(faultMessageType != null) {
    		setAttribute(FAULT_MESSAGE_TYPE, 
					 	 com.sun.bpel.xml.NamespaceUtility.getQNameAsString(faultMessageType));
    		
    	} else {
    		setAttribute(FAULT_MESSAGE_TYPE, null);
    	}
		
		this.mFaultMessageType = null;
        initWSDLFaultMessageType();
	}

    public QName getFaultElement() {
    	String faultElementQNameStr = xmlAttrs[FAULT_ELEMENT].getValue();
        if(faultElementQNameStr != null) {
        	return com.sun.bpel.xml.NamespaceUtility.resolveAndGetQName(
        			faultElementQNameStr, this);
        }
        
        return null;
	}

	public void setFaultElement(QName faultElement) {
		if(faultElement != null) {
    		setAttribute(FAULT_ELEMENT, 
    			com.sun.bpel.xml.NamespaceUtility.getQNameAsString(faultElement));
    		
    	} else {
    		setAttribute(FAULT_ELEMENT, null);
    	}
		
		this.mFaultElement = null;
        initXSDFaultElement();        
	}

    /** Adds a child of the element.
     * @param   c   Child.
     */
    public void addChild(XMLNode c) {
        if (c instanceof Activity) {
            setActivity((Activity) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** Removes a child of the element
     * @param   c   Child.
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Activity) {
            setActivity(null);
        } else {
            super.removeChild(c);
        }
    }
   
    /** @see SingleActivityHolder#getActivity
     */
    public Activity getActivity() {
        return activity;
    }
    
    /** @see SingleActivityHolder#setActivity
     */
    public void setActivity(Activity activity) {
        super.replaceChild(1, this.activity, activity);
        this.activity = activity;
    }

    /**
     * Get Variable Given its name.
     * @param vName the name of the variable
     * @return the variable or null if not found
     */
    public Variable getBPELVariable(String vName) {
        
        String varName = getFaultVariable();
        if (varName != null && varName.equals(vName)) {
            return getBPELFaultVariable();
        } else {
            return null;
        }        
    }    
    
    public Variable getBPELFaultVariable() {
        return this.mFaultVariable;
    }

    public void setBPELFaultVariable(Variable variable) {
		//update the faultVariable attribute value
		if(variable != null) {
			setFaultVariable(variable.getName());
		} else {
			setFaultVariable(null);
		}
		
		this.mFaultVariable = null;
        initBPELFaultVariable();
        resolveFaultVariable();
	}
	
	public Message getWSDLFaultMessageType() {
		return this.mFaultMessageType;
	}
    
	public SchemaGlobalElement getXSDFaultElement() {
		return this.mFaultElement;
	}
    
	/** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if auto-traversal is to continue.
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
        resolveFaultVariable();
        initXSDFaultElement();
    }
}
