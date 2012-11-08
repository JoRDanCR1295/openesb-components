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
 * @(#)CorrelationSetsImpl.java 
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

import com.sun.bpel.model.CorrelationSet;
import com.sun.bpel.model.CorrelationSets;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;correlationSets&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class CorrelationSetsImpl extends BPELElementImpl
    implements CorrelationSets {    
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 1686468235097217026L;
    
    /** Holds the correlationSet sub-elements. */
    private ArrayList correlationSets = new ArrayList();

    /** Creates a new instance of CorrelationSetsImpl.
     */
    public CorrelationSetsImpl() {
        super();
        initCorrelationSets();
    }
    
    /** Creates a new instance of CorrelationSetsImpl.
     * @param   d   Owner document.
     */
    public CorrelationSetsImpl(XMLDocument d) {
        super(d);
        initCorrelationSets();
    }
    
    /** Initializes this class.
     */
    private void initCorrelationSets() {
        setLocalName(CorrelationSets.TAG);
        childrenTags = new String[] {
            CorrelationSet.TAG
        };
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof CorrelationSet) {
            addCorrelationSet((CorrelationSet) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof CorrelationSet) {
            removeCorrelationSet((CorrelationSet) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /** @see CorrelationSet#getCorrelationSet
     */
    public CorrelationSet getCorrelationSet(int index) {
        return (CorrelationSet) correlationSets.get(index);
    }
    
    /** get CorrelationSet given a name
     * @param name name of the correlaton set
     * @return CorrelationSet
     *
     */
    public CorrelationSet getCorrelationSet(String name) {
    	if(name == null) {
    		return null;
    	}
    	
    	CorrelationSet cSet = null;
    	Iterator it = correlationSets.iterator();
    	while(it.hasNext()) {
    		CorrelationSet set = (CorrelationSet) it.next();
    		if(name.equals(set.getName())) {
    			cSet = set;
    			break;
    		}
    	}
    	return cSet;
		
    }
 
    /** @see CorrelationSet#setCorrelationSet
     */
    public synchronized void setCorrelationSet(int index, CorrelationSet correlationSet) {
        if (correlationSets.size() == index) {
            addCorrelationSet(correlationSet);
        } else {
        	CorrelationSet oldCorrelationSet = (CorrelationSet) correlationSets.get(index); 
        	correlationSets.set(index, correlationSet);
        	replaceChild(1, oldCorrelationSet, correlationSet);
        }
    }
    
    /** @see CorrelationSet#getCorrelationSetSize
     */
    public int getCorrelationSetSize() {
        return this.correlationSets.size();
    }
    
    /** @see CorrelationSet#addCorrelationSet(CorrelationSet)
     */
    public synchronized void addCorrelationSet(CorrelationSet c) {
    	correlationSets.add(c);
    	super.addChild(1, c);
    }
    
    /** @see CorrelationSets#addCorrelationSet(int, CorrelationSet)
     */
    public synchronized void addCorrelationSet(int index, CorrelationSet c) {
        if ((index < 0) || (index > correlationSets.size())) {
            throw new ArrayIndexOutOfBoundsException("Expected: 0 <= index <= " + correlationSets.size());
        } else if (index == correlationSets.size()) {
            addCorrelationSet(c);
        } else {
        	CorrelationSet oldCorrelationSet = getCorrelationSet(index); 
        	correlationSets.add(index, c);
        	super.addChild(1, oldCorrelationSet, c);
        }
    }
    
    /** @see CorrelationSets#clearCorrelationSets
     */
    public synchronized void clearCorrelationSets() {
        while (correlationSets.size() > 0) {
            removeCorrelationSet(0);  // stays at 0 because array elements keep shifting to the left
        }
    }
    
    /** @see CorrelationSets#removeCorrelationSet(int)
     */
    public synchronized void removeCorrelationSet(int i) {
        removeCorrelationSet(getCorrelationSet(i));
    }
    
    /** @see CorrelationSets#removeCorrelationSet(CorrelationSet)
     */
    public synchronized boolean removeCorrelationSet(CorrelationSet c) {
    	boolean result = correlationSets.remove(c);
    	super.removeChild(c);
        return result;
    }
    
    /** @see CorrelationSets#indexOfCorrelationSet
     */
    public int indexOfCorrelationSet(XMLNode cs) {
        return correlationSets.indexOf(cs);
    }
    
    /** @see CorrelationSets#getCorrelationSets
     */
    public synchronized Collection getCorrelationSets() {
        return Collections.unmodifiableCollection((ArrayList) correlationSets.clone());
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
