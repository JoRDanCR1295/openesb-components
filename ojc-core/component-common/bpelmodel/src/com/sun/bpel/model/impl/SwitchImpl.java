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
 * @(#)SwitchImpl.java 
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

import com.sun.bpel.model.Case;
import com.sun.bpel.model.Otherwise;
import com.sun.bpel.model.Switch;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;switch&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SwitchImpl extends ActivityImpl implements Switch {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -3119053063920321378L;
    
    /** Holds case elements */
    private ArrayList cases = new ArrayList();
    
    /** Hold otherwise element */
    private Otherwise otherwise;
    
    /** Creates a new instance of SwitchImpl */
    public SwitchImpl() {
        super();
        initSwitch();
    }
    
    /** Creates a new instance of SwitchImpl.
     * @param   d   Owner document.
     */
    public SwitchImpl(XMLDocument d) {
        super(d);
        initSwitch();
    }
    
    /** Initializes this class.
     */
    private void initSwitch() {
        setLocalName(Switch.TAG);
        childrenTags = new String[] {
            Case.TAG,
            Otherwise.TAG
        };
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Case) {
            addCase((Case) c);
        } else if (c instanceof Otherwise) {
            setOtherwise((Otherwise) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Case) {
            removeCase((Case) c);
        } else if (c instanceof Otherwise) {
            setOtherwise(null);
        } else {
            super.removeChild(c);
        }
    }
    
    /** @see Switch#getCase
     */
    public Case getCase(int i) {
        return (Case) cases.get(i);
    }
    
     /**
     * get case with a given line label attribute
     * @param lineLabel line label attribute of the case
     * @return Case
     */
    public Case getCase(String name) {
    	if(name == null) {
    		return null;
    	}
    	
    	Case c = null;
    	Iterator it = cases.iterator();
    	
    	while(it.hasNext()) {
    		Case cs = (Case) it.next();
    		if(name.equals(cs.getLineLabel())) {
    			c = cs;
    			break;
    		}
    	}
    	
    	return c;
    }
    
    /** @see Switch#setCase
     */
    public synchronized void setCase(int i, Case c) {
        if (cases.size() == i) {
            addCase(c);
        } else {
            replaceChild(3, (Case) cases.get(i), c);
            cases.set(i, c);
        }
    }
    
    /** @see Switch#addCase(Case)
     */
    public synchronized void addCase(Case c) {
        super.addChild(3, c);
        cases.add(c);
    }
    
    /** @see Switch#addCase(int, Case)
     */
    public synchronized void addCase(int index, Case c) {
        if ((index < 0) || (index > cases.size())) {
            throw new ArrayIndexOutOfBoundsException("Expected: 0 <= index <= " + cases.size());
        } else if (index == cases.size()) {
            addCase(c);
        } else {
            super.addChild(3, getCase(index), c);
            cases.add(index, c);
        }
    }
    
    /** @see Switch#clearCases
     */
    public synchronized void clearCases() {
        while (cases.size() > 0) {
            removeCase(0);  // stays at 0 because array elements keep shifting to the left
        }
    }
    
    /** @see Switch#removeCase(int)
     */
    public synchronized void removeCase(int i) {
        removeCase(getCase(i));
    }
    
    /** @see Switch#removeCase(Case)
     */
    public synchronized boolean removeCase(Case c) {
        super.removeChild(c);
        return cases.remove(c);
    }
    
    /** @see Switch#getCaseSize
     */
    public int getCaseSize() {
        return cases.size();
    }
    
    /** @see Switch#indexOfCase
     */
    public int indexOfCase(XMLNode c) {
        return cases.indexOf(c);
    }
    
    /** @see Switch#getCases
     */
    public synchronized Collection getCases() {
        return Collections.unmodifiableCollection((ArrayList) cases.clone());
    }
    
    /** @see Switch#getOtherwise
     */
    public Otherwise getOtherwise() {
        return otherwise;
    }
    
    /** @see Switch#setOtherwise
     */
    public void setOtherwise(Otherwise o) {
        super.replaceChild(4, otherwise, o);
        otherwise = o;
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
