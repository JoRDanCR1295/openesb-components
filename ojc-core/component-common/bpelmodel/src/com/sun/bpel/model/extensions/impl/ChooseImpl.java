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
 * @(#)ChooseImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.extensions.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import javax.xml.namespace.QName;

import com.sun.bpel.model.Case;
import com.sun.bpel.model.extensions.Choose;
import com.sun.bpel.model.extensions.Default;
import com.sun.bpel.model.extensions.When;
import com.sun.bpel.model.impl.ActivityImpl;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;choose&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class ChooseImpl extends ActivityImpl implements Choose {

    /** QName object for SeeBeyond Private extension line label */
    public static final QName CHOOSE_QNAME =
        com.sun.bpel.xml.NamespaceUtility.getQName(
        			XMLElement.SBYNBPEL_RUNTIME_EXTN_NAMESPACE,
        			Choose.TAG,
        			XMLElement.SBYNBPEL_RUNTIME_EXTN_PREFIX);

    /** Holds when elements */
    private ArrayList mWhens = new ArrayList();
    
    /** Hold Default element */
    private Default mDefault;
    
    /** Creates a new instance of ChooseImpl */
    public ChooseImpl() {
        super();
        initChoose();
    }
    
    /** Creates a new instance of ChooseImpl.
     * @param   d   Owner document.
     */
    public ChooseImpl(XMLDocument d) {
        super(d);
        initChoose();
    }
    
    /** Initializes this class.
     */
    private void initChoose() {
        setLocalName(Choose.TAG);
        setQualifiedName(CHOOSE_QNAME);
        childrenTags = new String[] {
            When.TAG,
            Default.TAG
        };
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Case) {
            addWhen((When) c);
        } else if (c instanceof Default) {
            setDefault((Default) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof When) {
            removeWhen((When) c);
        } else if (c instanceof Default) {
            setDefault(null);
        } else {
            super.removeChild(c);
        }
    }
    
    /** @see Choose#getWhen
     */
    public When getWhen(int i) {
        return (When) mWhens.get(i);
    }
    
    /** @see Choose#setWhen
     */
    public synchronized void setWhen(int i, When c) {
        if (mWhens.size() == i) {
            addWhen(c);
        } else {
            replaceChild(3, (When) mWhens.get(i), c);
            mWhens.set(i, c);
        }
    }
    
    /** @see Choose#addWhen(When)
     */
    public synchronized void addWhen(When c) {
        super.addChild(3, c);
        mWhens.add(c);
    }
    
    /** @see Choose#addWhen(int, When)
     */
    public synchronized void addWhen(int index, When c) {
        if ((index < 0) || (index > mWhens.size())) {
            throw new ArrayIndexOutOfBoundsException("Expected: 0 <= index <= " + mWhens.size());
        } else if (index == mWhens.size()) {
            addWhen(c);
        } else {
            super.addChild(3, getWhen(index), c);
            mWhens.add(index, c);
        }
    }
    
    /** @see Choose#clearWhens
     */
    public synchronized void clearWhens() {
        while (mWhens.size() > 0) {
            removeWhen(0);  // stays at 0 because array elements keep shifting to the left
        }
    }
    
    /** @see Choose#removeWhen(int)
     */
    public synchronized void removeWhen(int i) {
        removeWhen(getWhen(i));
    }
    
    /** @see Choose#removeWhen(When)
     */
    public synchronized boolean removeWhen(When c) {
        super.removeChild(c);
        return mWhens.remove(c);
    }
    
    /** @see Choose#getWhenSize
     */
    public int getWhenSize() {
        return mWhens.size();
    }
    
    /** @see Choose#indexOfWhen
     */
    public int indexOfWhen(XMLNode c) {
        return mWhens.indexOf(c);
    }
    
    /** @see Choose#getWhens
     */
    public synchronized Collection getWhens() {
        return Collections.unmodifiableCollection((ArrayList) mWhens.clone());
    }
    
    /** @see Choose#getDefault
     */
    public Default getDefault() {
        return mDefault;
    }
    
    /** @see Choose#setDefault
     */
    public void setDefault(Default o) {
        super.replaceChild(4, mDefault, o);
        mDefault = o;
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
