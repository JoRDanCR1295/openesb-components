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
 * @(#)VariablesImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;containers&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class VariablesImpl extends BPELElementImpl implements Variables {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -760293955762859782L;
    
    /** Holds list of container sub-elements. */
    private ArrayList containers = new ArrayList();

    /** Creates a new instance of VariablesImpl.
     */
    public VariablesImpl() {
        super();
        initContainers();
    }
    
    /** Creates a new instance of VariablesImpl.
     * @param   d   Owner document.
     */
    public VariablesImpl(XMLDocument d) {
        super(d);
        initContainers();
    }
    
    /** Initializes this class.
     */
    private void initContainers() {
        setLocalName(Variables.TAG);
        childrenTags = new String[] {
            Variable.TAG
        };
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Variable) {
            addVariable((Variable) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Variable) {
            removeVariable((Variable) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /** @see Variables#getContainer
     */
    public Variable getVariable(int index) {
        return (Variable) containers.get(index);
    }
    
    
    /** get Variable given its name.
     * @param name name of the Variable.
     * @return Variable with the given name.
     *
     */
    public Variable getVariable(String name) {
    	Variable container = null;
    	if(name == null) {
    		return null;
    	}
    	
    	Iterator it = containers.iterator();
    	while(it.hasNext()) {
    		Variable mContainer = (Variable) it.next();
    		if(name.equals(mContainer.getName())) {
    			container = mContainer;
    			break;
    		}
    	}
    	
    	return container;
    }
    
    
    /** @see Variables#getContainerSize
     */
    public int getVariableSize() {
        return this.containers.size();
    }
    
    /** @see Variables#addVariable(Variable)
     */
    public synchronized void addVariable(Variable c) {
        containers.add(c);
        super.addChild(1, c);
    }
    
     
       
    /** @see Variables#removeVariable(Variable)
     */
    public synchronized boolean removeVariable(Variable c) {
        boolean result = containers.remove(c);
        super.removeChild(c);
        return result;
    }
    
   
    /** @see Variables#getContainers
     */
    public synchronized Collection getVariables() {
        return Collections.unmodifiableCollection((ArrayList) containers.clone());
    }
    
    /** @see XMLNode#accept
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
