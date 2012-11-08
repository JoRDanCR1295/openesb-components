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
 * @(#)ThrowImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import javax.xml.namespace.QName;

import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Throw;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;throw&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class ThrowImpl extends ActivityImpl implements Throw {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -1002701284647662364L;
    
    private Variable mFaultVariable;
    
    /** Constructs new instance of throw element.
     */
    public ThrowImpl() {
        super();
        initThrow();
    }
    
    /** Constructs new instance of throw element.
     * @param   d   Owner document.
     */
    public ThrowImpl(XMLDocument d) {
        super(d);
        initThrow();
    }
    
    /** Initializes this class.
     */
    private void initThrow() {
        setLocalName(Throw.TAG);
        xmlAttrs = new XMLAttribute[NUM_ATTRS];
        // NUM_STANDARD_ATTRS is equal to standardXmlAttrs.length.
        for (int i = 0; i < NUM_STANDARD_ATTRS; i++) {
        	xmlAttrs[i] = standardXmlAttrs[i];
        }
        xmlAttrs[FAULT_NAME] = new XMLAttributeImpl(Throw.ATTR.FAULT_NAME, String.class, false, null);
        xmlAttrs[FAULT_VARIABLE] = new XMLAttributeImpl(Throw.ATTR.FAULT_VARIABLE, String.class, true, null);
    }
    
    /** Getter for attribute faultName.
     * @return Value of attribute faultName.
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
    
    /** Setter for attribute faultName.
     * @param faultName New value of attribute faultName.
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
    
    
    /** Getter for attribute faultContainer.
     * @return Value of attribute faultContainer.
     *
     */
    public String getFaultVariable() {
        return xmlAttrs[FAULT_VARIABLE].getValue();
    }
    
    /** Setter for attribute faultContainer.
     * @param faultContainer New value of attribute faultContainer.
     *
     */
    public void setFaultVariable(String faultVariable) {
        String oldFaultVariable = getFaultVariable();
        setAttribute(FAULT_VARIABLE, faultVariable);
        //if variable is changed we need to clear out cached variable object
        //so that next call to getBPELFaultVariable can find new variable object
        if(faultVariable == null || !faultVariable.equals(oldFaultVariable)) {
        	this.mFaultVariable = null;
        }
    }
    
    
    public Variable getBPELFaultVariable() {
    	if(this.mFaultVariable != null) {
			return this.mFaultVariable;
		}
		
		if (BPELHelper.isValueAbsent(getFaultVariable())) {
			return null;
		}
		
		this.mFaultVariable = BPELHelper.getMatchingVariable(getFaultVariable(), this);
		return this.mFaultVariable;
	}

	public void setBPELFaultVariable(Variable variable) {
		//update the faultVariable attribute value
		if(variable != null) {
			setFaultVariable(variable.getName());
		} else {
			setFaultVariable(null);
		}
		
		this.mFaultVariable = variable;
		
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
