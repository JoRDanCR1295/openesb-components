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
 * @(#)BindingOperationImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;

import com.sun.wsdl.model.BindingFault;
import com.sun.wsdl.model.BindingInput;
import com.sun.wsdl.model.BindingOperation;
import com.sun.wsdl.model.BindingOutput;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.extensions.soap.SOAPOperation;
import com.sun.wsdl.model.visitor.WSDLVisitor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

/**
 * Implements the binding &lt;operation&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class BindingOperationImpl extends WSDLExtensibleElementImpl implements BindingOperation {
    
    /** Holds the binding input element */
    protected BindingInput bindingInput;
    
    /** Holds the binding output element */
    protected BindingOutput bindingOutput;
    
    /** Holds the binding fault list */
    protected ArrayList bindingFaults = new ArrayList();
    
    /** Creates a new instance of BindingOperationImpl */
    public BindingOperationImpl() {
        super();
        initBinding();
    }
    
    /** Constructor for new binding operation instance.
     * @param   d   Owner document.
     */
    public BindingOperationImpl(XMLDocument d) {
        super(d);
        initBinding();
    }
    
    /** Initializes this class.
     */
    private void initBinding() {
        setLocalName(BindingOperation.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(BindingOperation.ATTR.NAME, String.class, true, null)
        };
        childrenTags = new String[] {
            SOAPOperation.QTAG,
            BindingInput.TAG,
            BindingOutput.TAG,
            BindingFault.TAG
        };
    }
   
    /**
     * @see com.sun.wsdl.model.BindingOperation#getName()
     */
    public String getName() {
        return xmlAttrs[NAME].getValue();
    }
    
    /**
     * @see com.sun.wsdl.model.BindingOperation#setName(java.lang.String)
     */
    public void setName(String name) {
        setAttribute(NAME, name);
    }
    
    /**
     * @see com.sun.wsdl.model.BindingOperation#setName(java.lang.String, java.lang.String)
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
		if (c instanceof BindingInput) {
            setBindingInputAtTail((BindingInput) c);
        } else if (c instanceof BindingOutput) {
            setBindingOutputAtTail((BindingOutput) c);
        } else if (c instanceof BindingFault) {
            addBindingFaultAtTail((BindingFault) c);
        } else {
            super.addChildAtTail(c);
        }
	}
	
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#addChild(com.sun.wsdl.model.common.model.XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof BindingInput) {
            setBindingInput((BindingInput) c);
        } else if (c instanceof BindingOutput) {
            setBindingOutput((BindingOutput) c);
        } else if (c instanceof BindingFault) {
            addBindingFault((BindingFault) c);
        } else {
            super.addChild(c);
        }
    }
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#addChild(com.sun.wsdl.model.common.model.XMLNode)
     */
    public void addChild(int index, XMLNode c) {
        if (c instanceof BindingInput) {
            setBindingInput(index, (BindingInput) c);
        } else if (c instanceof BindingOutput) {
            setBindingOutput(index, (BindingOutput) c);
        } else if (c instanceof BindingFault) {
            addBindingFault(index, (BindingFault) c);
        } else {
            super.addChild(index, c);
        }
    }
    
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#removeChild(com.sun.wsdl.model.common.model.XMLNode)
     */
    public void removeChild(XMLNode c) {
        if (c instanceof BindingInput) {
            setBindingInput(null);
        } else if (c instanceof BindingOutput) {
            setBindingOutput(null);
        } else if (c instanceof BindingFault) {
            removeBindingFault((BindingFault) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /**
     * @see com.sun.wsdl.model.BindingOperation#getBindingInput()
     */
    public BindingInput getBindingInput() {
        return bindingInput;
    }
    
    /**
     * @see com.sun.wsdl.model.BindingOperation#setBindingInput(
     *  com.sun.wsdl.model.BindingInput)
     */
    public void setBindingInput(BindingInput input) {
    	BindingInput oldInput = bindingInput;
    	bindingInput = input;
    	super.replaceChild(2, oldInput, input);
        
    }
     
    /**
     * @see com.sun.wsdl.model.BindingOperation#setBindingInput(
     *  com.sun.wsdl.model.BindingInput)
     */
    public void setBindingInput(int index, BindingInput input) {
    	BindingInput oldInput = bindingInput;
    	bindingInput = input;
    	super.replaceChild(index, oldInput, input);
        
    }
    
    
    /**
     * @see com.sun.wsdl.model.BindingOperation#getBindingOutput()
     */
    public BindingOutput getBindingOutput() {
        return bindingOutput;
    }
    
    /**
     * @see com.sun.wsdl.model.BindingOperation#setBindingOutput(
     *  com.sun.wsdl.model.BindingOutput)
     */
    public void setBindingOutput(BindingOutput output) {
    	BindingOutput oldOutput = bindingOutput;
    	bindingOutput = output;
    	super.replaceChild(3, oldOutput, output);
        
    }
    
    
    /**
     * @see com.sun.wsdl.model.BindingOperation#setBindingOutput(
     *  com.sun.wsdl.model.BindingOutput)
     */
    public void setBindingOutput(int index, BindingOutput output) {
    	BindingOutput oldOutput = bindingOutput;
    	bindingOutput = output;
    	super.replaceChild(index, oldOutput, output);
        
    }
    
    /**
     * get all binding faults
     * @return collection of binding faults
     */
    public Collection getBindingFaults() {
    	return Collections.unmodifiableCollection(bindingFaults);
    }
    
    /**
     * @see com.sun.wsdl.model.BindingOperation#getBindingFault(int)
     */
    public BindingFault getBindingFault(int index) {
        return (BindingFault) bindingFaults.get(index);
    }
    
    
    /**
     * @see com.sun.wsdl.model.BindingOperation#getBindingFaultSize()
     */
    public int getBindingFaultSize() {
        return bindingFaults.size();
    }
    
    /**
     * @see com.sun.wsdl.model.BindingOperation#addBindingFault(
     *  com.sun.wsdl.model.BindingFault)
     */
    public void addBindingFault(BindingFault fault) {
    	bindingFaults.add(fault);
    	super.addChild(4, fault);
    }
    
    /**
     * @see com.sun.wsdl.model.BindingOperation#addBindingFault(
     *  com.sun.wsdl.model.BindingFault)
     */
    public void addBindingFault(int index, BindingFault fault) {
    	bindingFaults.add(fault);
    	super.addChild(index, fault);
    }
    
    
    /**
     * @see com.sun.wsdl.model.BindingOperation#removeBindingFault(
     *  com.sun.wsdl.model.BindingFault)
     */
    public boolean removeBindingFault(BindingFault fault) {
    	boolean result = bindingFaults.remove(fault);
        super.removeChild(fault);
        return result;
    }
   
   
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#accept(
     *  com.sun.wsdl.model.common.model.visitor.Visitor)
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
    
    /**
     * @see com.sun.wsdl.model.BindingOperation#setBindingInput(
     *  com.sun.wsdl.model.BindingInput)
     */
    public void setBindingInputAtTail(BindingInput input) {
    	BindingInput oldInput = bindingInput;
    	bindingInput = input;
    	super.addChildAtTheEnd(input, 2);
        
    }
    
    public void setBindingOutputAtTail(BindingOutput output) {
    	BindingOutput oldOutput = bindingOutput;
    	bindingOutput = output;
    	super.addChildAtTheEnd(output, 3);
        
    }
    
    public void addBindingFaultAtTail(BindingFault fault) {
    	bindingFaults.add(fault);
    	super.addChildAtTheEnd(fault, 4);
    }
    
}
