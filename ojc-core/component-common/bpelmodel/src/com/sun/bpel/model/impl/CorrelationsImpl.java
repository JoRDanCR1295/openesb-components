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
 * @(#)CorrelationsImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import com.sun.bpel.model.Correlation;
import com.sun.bpel.model.Correlations;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;correlations&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class CorrelationsImpl extends BPELElementImpl implements Correlations {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 8272305140523122706L;
    
    /** Holds the correlation sub-elements. */
    private ArrayList correlations = new ArrayList();
    
    /** Creates a new instance of CorrelationsImpl.
     */
    public CorrelationsImpl() {
        super();
        initCorrelations();
    }
    
    /** Creates a new instance of CorrelationsImpl.
     * @param   d   Owner document.
     */
    public CorrelationsImpl(XMLDocument d) {
        super(d);
        initCorrelations();
    }
    
    /** Initializes this class.
     */
    private void initCorrelations() {
        setLocalName(Correlations.TAG);
        childrenTags = new String[] {
            Correlation.TAG
        };
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Correlation) {
            addCorrelation((Correlation) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Correlation) {
            removeCorrelation((Correlation) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /** @see Correlations#getCorrelation
     */
    public Correlation getCorrelation(int index) {
        return (Correlation) correlations.get(index);
    }
    
    /** @see Correlations#setCorrelation
     */
    public synchronized void setCorrelation(int index, Correlation correlation) {
        if (correlations.size() == index) {
            addCorrelation(correlation);
        } else {
            replaceChild(1, (Correlation) correlations.get(index), correlation);
            correlations.set(index, correlation);
        }
    }
    
    /** @see Correlations#getCorrelationSize
     */
    public int getCorrelationSize() {
        return this.correlations.size();
    }
    
    /** @see Correlations#addCorrelation(Correlation)
     */
    public synchronized void addCorrelation(Correlation c) {
        super.addChild(1, c);
        correlations.add(c);
    }
    
    /** @see Correlations#addCorrelation(int, Correlation)
     */
    public synchronized void addCorrelation(int index, Correlation c) {
        if ((index < 0) || (index > correlations.size())) {
            throw new ArrayIndexOutOfBoundsException("Expected: 0 <= index <= " + correlations.size());
        } else if (index == correlations.size()) {
            addCorrelation(c);
        } else {
            super.addChild(1, getCorrelation(index), c);
            correlations.add(index, c);
        }
    }
    
    /** @see Correlations#clearCorrelations
     */
    public synchronized void clearCorrelations() {
        while (correlations.size() > 0) {
            removeCorrelation(0);  // stays at 0 because array elements keep shifting to the left
        }
    }
    
    /** @see Correlations#removeCorrelation(int)
     */
    public synchronized void removeCorrelation(int i) {
        removeCorrelation(getCorrelation(i));
    }
    
    /** @see Correlations#removeCorrelation(Correlation)
     */
    public synchronized boolean removeCorrelation(Correlation c) {
        super.removeChild(c);
        return correlations.remove(c);
    }
    
    /** @see Correlations#indexOfCorrelation
     */
    public int indexOfCorrelation(XMLNode correlation) {
        return correlations.indexOf(correlation);
    }
    
    /** @see Correlations#getCorrelations
     */
    public synchronized Collection getCorrelations() {
        return Collections.unmodifiableCollection((ArrayList) correlations.clone());
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
