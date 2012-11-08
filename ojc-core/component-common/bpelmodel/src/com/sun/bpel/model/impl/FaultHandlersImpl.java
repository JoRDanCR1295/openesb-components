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
 * @(#)FaultHandlersImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import com.sun.bpel.model.Catch;
import com.sun.bpel.model.CatchAll;
import com.sun.bpel.model.FaultHandlers;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;faultHandlers&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class FaultHandlersImpl extends BPELElementImpl
    implements FaultHandlers {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -3098674192363747631L;
    
    /** Holds the catch sub-elements. */
    private ArrayList catches = new ArrayList();
    
    /** Holds the catchAll sub-element. */
    private CatchAll catchAll;
    
    /** Creates a new instance of FaultHandlersImpl.
     */
    public FaultHandlersImpl() {
        super();
        initFaultHandlers();
    }
    
    /** Creates a new instance of FaultHandlersImpl.
     * @param   d   Owner document.
     */
    public FaultHandlersImpl(XMLDocument d) {
        super(d);
        initFaultHandlers();
    }
    
    /** Initializes this class.
     */
    private void initFaultHandlers() {
        setLocalName(FaultHandlers.TAG);
        childrenTags = new String[] {
            Catch.TAG,
            CatchAll.TAG
        };
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Catch) {
            addCatch((Catch) c);
        } else if (c instanceof CatchAll) {
            setCatchAll((CatchAll) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Catch) {
            removeCatch((Catch) c);
        } else if (c instanceof CatchAll) {
            setCatchAll(null);
        } else {
            super.removeChild(c);
        }
    }
    
    /** @see FaultHandlers#getCatch
     */
    public Catch getCatch(int index) {
        return (Catch) catches.get(index);
    }
    
    /** @see FaultHandlers#setCatch
     */
    public synchronized void setCatch(int index, Catch c) {
        if (catches.size() == index) {
            addCatch(c);
        } else {
            replaceChild(1, (Catch) catches.get(index), c);
            catches.set(index, c);
        }
    }
    
    /** @see FaultHandlers#getCatchSize
     */
    public int getCatchSize() {
        return catches.size();
    }
    
    /** @see FaultHandlers#addCatch(Catch)
     */
    public synchronized void addCatch(Catch c) {
        super.addChild(1, c);
        catches.add(c);
    }
    
    /** @see FaultHandlers#addCatch(int, Catch)
     */
    public synchronized void addCatch(int index, Catch c) {
        if ((index < 0) || (index > catches.size())) {
            throw new ArrayIndexOutOfBoundsException("Expected: 0 <= index <= " + catches.size());
        } else if (index == catches.size()) {
            addCatch(c);
        } else {
            super.addChild(1, getCatch(index), c);
            catches.add(index, c);
        }
    }
    
    /** @see FaultHandlers#clearCatches
     */
    public synchronized void clearCatches() {
        while (catches.size() > 0) {
            removeCatch(0);  // stays at 0 because array elements keep shifting to the left
        }
    }
    
    /** @see FaultHandlers#removeCatch(Catch)
     */
    public synchronized boolean removeCatch(Catch c) {
        super.removeChild(c);
        return catches.remove(c);
    }
    
    /** @see FaultHandlers#removeCatch(int)
     */
    public synchronized void removeCatch(int i) {
        removeCatch(getCatch(i));
    }
    
    /** @see FaultHandlers#indexOfCatch
     */
    public int indexOfCatch(XMLNode c) {
        return catches.indexOf(c);
    }
    
    /** @see FaultHandlers#getCatches
     */
    public synchronized Collection getCatches() {
        return Collections.unmodifiableCollection((ArrayList) catches.clone());
    }
    
    /** @see FaultHandlers#getCatchAll
     */
    public CatchAll getCatchAll() {
        return catchAll;
    }
    
    /** @see FaultHandlers#setCatchAll
     */
    public void setCatchAll(CatchAll catchAll) {
        super.replaceChild(2, this.catchAll, catchAll);
        this.catchAll = catchAll;
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
