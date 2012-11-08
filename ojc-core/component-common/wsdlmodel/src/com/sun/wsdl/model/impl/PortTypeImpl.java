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
 * @(#)PortTypeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;
 
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import com.sun.wsdl.model.Operation;
import com.sun.wsdl.model.PortType;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.visitor.WSDLVisitor;
 

 
/**
 * Implements the WSDL &lt;portType&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class PortTypeImpl
    extends NamedWSDLElementImpl
    implements PortType {
        
    /**
     * The logger.
     */
    private static Logger mLogger =
        Logger.getLogger(PortTypeImpl.class.getName());
        
    /**
     * The list of operations.
     */
    List mOperations = null;
    
    /**
     * Creates a new instance of PortTypeImpl.
     * @param   d   Owner document.
     */
    public PortTypeImpl(XMLDocument d) {
        super(d);
        mOperations = new ArrayList();
        initPortType();
    }
    
    /**
     * Initializes this class.
     */
    private void initPortType() {
        setLocalName(PortType.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.NAME, String.class, false, null)
        };
        childrenTags = new String[] {
            Operation.TAG
        };
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
		if (c instanceof Operation) {
			addOperationAtTail((Operation) c);
        } else {
            super.addChildAtTail(c);
        }
	}
	
    /**
     * Adds a child of the element.
     * @param   c   Child.
     */
    public void addChild(XMLNode c) {
        if (c instanceof Operation) {
            addOperation((Operation) c);
        } else {
            super.addChild(c);
        }
    }
    
    /**
     * Removes a child of the element
     * @param   c   Child.
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Operation) {
            removeOperation((Operation) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /**
     * Gets the &lt;operation&gt; element at the specified position.
     * @param index the position
     * @return the operation element
     */
    public Operation getOperation(int index) {
        return (Operation) mOperations.get(index);
    }
    
    
    public Collection getOperations(String operationName) {
    	if(operationName == null) {
    		return null;
    	}
    	
		ArrayList matchingOperations = new ArrayList();
		Iterator it = this.mOperations.iterator();
		while(it.hasNext()) {
			Operation operation = (Operation) it.next();
			if(operationName.equals(operation.getName())) {
				matchingOperations.add(operation);
			}
		}
		
		return matchingOperations;
	}

	/**
     * Adds a new operation.
     * @param operation the operation to add
     */
    public void addOperation(Operation operation) {
    	mOperations.add(operation);
    	super.addChild(2, operation);
    }
    
    public void addOperationAtTail(Operation operation) {
    	mOperations.add(operation);
    	super.addChildAtTheEnd(operation, 2);
    }
    
    /**
     * Removes the given operation.
     * @param operation to be removed
     */
    public void removeOperation(Operation operation) {
    	mOperations.remove(operation);
    	super.removeChild(operation);
    }
    
    /**
     * Gets the number of operations.
     * @return the number of operations
     */
    public int countOperations() {
        return mOperations.size();
    }
    
    /**
     * Gets the list of all operations.
     * @return a read-only collection of Operations.
     */
    public Collection getOperations() {
        return Collections.unmodifiableCollection(mOperations);
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
