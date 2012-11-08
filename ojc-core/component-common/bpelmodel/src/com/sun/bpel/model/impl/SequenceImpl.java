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
 * @(#)SequenceImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.MultipleActivityHolder;
import com.sun.bpel.model.Sequence;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;sequence&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SequenceImpl extends ActivityImpl implements Sequence {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -3897329536014069456L;
    
    /** Holds activity elements. */
    private ArrayList activities = new ArrayList();
    
    /** Creates a new instance of SequenceImpl */
    public SequenceImpl() {
        super();
        initSequence();
    }
    
    /** Creates a new instance of SequenceImpl.
     * @param   d   Owner document.
     */
    public SequenceImpl(XMLDocument d) {
        super(d);
        initSequence();
    }
    
    /** Initializes this class.
     */
    private void initSequence() {
        setLocalName(Sequence.TAG);
        childrenTags = new String[] {
            Activity.TAG
        };
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Activity) {
            addActivity((Activity) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Activity) {
            removeActivity((Activity) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /** @see MultipleActivityHolder#getActivity
     */
    public Activity getActivity(int index) {
        return (Activity) activities.get(index);
    }
    
    /** @see MultipleActivityHolder#setActivity
     */
    public synchronized void setActivity(int index, Activity activity) {
        if (activities.size() == index) {
            addActivity(activity);
        } else {
            replaceChild(3, (Activity) activities.get(index), activity);
            activities.set(index, activity);
        }
    }
        
    /** @see MultipleActivityHolder#getActivitySize
     */
    public int getActivitySize() {
        return activities.size();
    }
    
    /** @see MultipleActivityHolder#addActivity(Activity)
     */
    public synchronized void addActivity(Activity a) {
        super.addChild(3, a);
        activities.add(a);
    }
    
    /** @see MultipleActivityHolder#addActivity(int, Activity)
     */
    public synchronized void addActivity(int index, Activity a) {
        if ((index < 0) || (index > activities.size())) {
            throw new ArrayIndexOutOfBoundsException("Expected: 0 <= index <= " + activities.size());
        } else if (index == activities.size()) {
            addActivity(a);
        } else {
            super.addChild(3, getActivity(index), a);
            activities.add(index, a);
        }
    }
    
    /** @see MultipleActivityHolder#clearActivities
     */
    public synchronized void clearActivities() {
        while (activities.size() > 0) {
            removeActivity(0);  // stays at 0 because array elements keep shifting to the left
        }
    }
   
    /** @see MultipleActivityHolder#removeActivity(int)
     */
    public synchronized void removeActivity(int i) {
        removeActivity(getActivity(i));
    }
    
    /** @see MultipleActivityHolder#remove(Activity)
     */
    public synchronized boolean removeActivity(Activity a) {
        super.removeChild(a);
        return activities.remove(a);
    }
    
    /** @see MultipleActivityHolder#indexOfActivity
     */
    public int indexOfActivity(XMLNode activity) {
        return activities.indexOf(activity);
    }
    
    /** @see MultipleActivityHolder#getActivities
     */
    public synchronized Collection getActivities() {
        return Collections.unmodifiableCollection((ArrayList) activities.clone());
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
