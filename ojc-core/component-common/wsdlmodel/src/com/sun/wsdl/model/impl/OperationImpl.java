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
 * @(#)OperationImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;
 


import com.sun.wsdl.model.Operation;
import com.sun.wsdl.model.OperationFault;
import com.sun.wsdl.model.OperationInput;
import com.sun.wsdl.model.OperationOutput;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.visitor.WSDLVisitor;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import java.util.logging.Logger;
 

 
/**
 * Implements the portType &lt;operation&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class OperationImpl
    extends WSDLExtensibleElementImpl
    implements Operation {
        
    /**
     * The logger.
     */
    private static Logger mLogger =
        Logger.getLogger(OperationImpl.class.getName());
        
    /**
     * The operation type.
     */
    private int mOperationType = Operation.UNKNOWN_OPERATION;
        
    /**
     * The input element.
     */
    private OperationInput mInput = null;
    
    /**
     * The output element.
     */
    private OperationOutput mOutput = null;
    
    /**
     * The list of faults.
     */
    private List mFaults;
    
    /**
     * Creates a new instance of OperationImpl.
     * @param   d   Owner document.
     */
    public OperationImpl(XMLDocument d) {
        super(d);
        mFaults = new ArrayList();
        initOperation();
    }
    
    /**
     * Initializes this class.
     */
    private void initOperation() {
        setLocalName(Operation.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.NAME, String.class, false, null),
            new XMLAttributeImpl(ATTR.PARAMETER_ORDER, String.class, true,
                                                                        null)
        };
        childrenTags = new String[] {
            OperationInput.TAG,
            OperationOutput.TAG,
            OperationFault.TAG
        };
    }
    
    /**
     * Gets the operation type.
     * @return the operation type
     */
    public int getOperationType() {
        return mOperationType;
    }
    
    /**
     * Sets the operation type.
     * @param operationType the operation type; it must be one of
     * REQUEST_RESPONSE_ONE_WAY_OPERATION or
     * SOLICIT_RESPONSE_NOTIFICATION_OPERATION.
     */
    public void setOperationType(int operationType) {
        mOperationType = operationType;
    }
    
    /**
     * Getter for property name.
     * @return Value of property name.
     */
    public String getName() {
        return xmlAttrs[NAME].getValue();
    }
    
    /**
     * Setter for property name.
     * @param name   New value of property name.
     */
    public void setName(String name) {
        setAttribute(NAME, name);
    }
    
    /**
     * Getter for property parameterOrder.
     * @return Value of property parameterOrder.
     */
    public String getParameterOrder() {
        return xmlAttrs[PARAMETER_ORDER].getValue();
    }
    
    /**
     * Setter for property parameterOrder.
     * @param parameterOrder   New value of property parameterOrder.
     */
    public void setParameterOrder(String parameterOrder) {
        setAttribute(PARAMETER_ORDER, parameterOrder);
    }
    
    /**
     * Gets the &lt;input&gt; element.
     * @return the input element
     */
    public OperationInput getInput() {
        return mInput;
    }
    
    /**
     * Sets the &lt;input&gt; element.
     * @param input the input element
     */
    public void setInput(OperationInput input) {
    	OperationInput oldInput = mInput;
    	mInput = input;
    	super.replaceChild(2, oldInput, input);
    }
    
    public void setInputAtTail(OperationInput input) {
    	OperationInput oldInput = mInput;
    	mInput = input;
    	super.addChildAtTheEnd(input, 2);
    }
    
    /**
     * Gets the &lt;output&gt; element.
     * @return the output element
     */
    public OperationOutput getOutput() {
        return mOutput;
    }
    
    /**
     * Sets the &lt;output&gt; element.
     * @param output the output element
     */
    public void setOutput(OperationOutput output) {
    	OperationOutput oldOutput = mOutput;
    	mOutput = output;
    	super.replaceChild(2, oldOutput, output);
    }
    
    public void setOutputAtTail(OperationOutput output) {
    	OperationOutput oldOutput = mOutput;
    	mOutput = output;
    	super.addChildAtTheEnd(output, 2);
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
		if (c instanceof OperationInput) {
            setInputAtTail((OperationInput) c);
        } else if (c instanceof OperationOutput) {
            setOutputAtTail((OperationOutput) c);
        } else if (c instanceof OperationFault) {
        	addFaultAtTail((OperationFault) c);
        } else {
            super.addChildAtTail(c);
        }
	}
	
		
    /**
     * Adds a child of the element.
     * @param   c   Child.
     */
    public void addChild(XMLNode c) {
        if (c instanceof OperationInput) {
            setInput((OperationInput) c);
        } else if (c instanceof OperationOutput) {
            setOutput((OperationOutput) c);
        } else if (c instanceof OperationFault) {
        	addFault((OperationFault) c);
        } else {
            super.addChild(c);
        }
    }
    
    /**
     * Removes a child of the element
     * @param   c   Child.
     */
    public void removeChild(XMLNode c) {
    	if (c instanceof OperationInput) {
            setInput(null);
        } else if (c instanceof OperationOutput) {
            setOutput(null);
        } else if (c instanceof OperationFault) {
        	removeFault((OperationFault) c);
        } else {
        	super.removeChild(c);
        }
    	
    }
    
    /**
     * Getter for property fault.
     * @param index index of the property to get
     * @return Value of property fault.
     *
     */
    public OperationFault getFault(int index) {
        return (OperationFault) mFaults.get(index);
    }
    
    /**
     * Adds a new fault.
     * @param fault the fault to add
     */
    public void addFault(OperationFault fault) {
    	mFaults.add(fault);
    	super.addChild(3, fault);
    }
    
    public void addFaultAtTail(OperationFault fault) {
    	mFaults.add(fault);
    	super.addChildAtTheEnd(fault, 3);
    }
    
    /**
     * Removes the fault at the specified position.
     * @param index the position of the fault to remove
     */
    public void removeFault(OperationFault fault) {
    	mFaults.remove(fault);
        super.removeChild(fault);
    }
    
    /**
     * Gets the number of faults.
     * @return the number of faults
     */
    public int countFaults() {
        return mFaults.size();
    }
    

    /**
     * Gets the list of all faults.
     * @return a read-only collection of OperationFaults.
     */
    public Collection getFaults() {
        return Collections.unmodifiableCollection(mFaults);
    }
    
    /**
     * Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
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
