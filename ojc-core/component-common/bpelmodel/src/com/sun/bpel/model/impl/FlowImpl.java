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
 * @(#)FlowImpl.java 
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
import com.sun.bpel.model.Flow;
import com.sun.bpel.model.Link;
import com.sun.bpel.model.Links;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;flow&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class FlowImpl extends ActivityImpl implements Flow {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -4349314832107569779L;
    
    /** Holds the links element. */
    private Links links;
    
    /** Holds the activity list */
    private ArrayList activities = new ArrayList();
    
    /** Creates a new instance of FlowImpl */
    public FlowImpl() {
        super();
        initFlow();
    }
    
    /** Creates a new instance of FlowImpl.
     * @param   d   Owner document.
     */
    public FlowImpl(XMLDocument d) {
        super(d);
        initFlow();
    }
    
    /** Initializes this class.
     */
    private void initFlow() {
        setLocalName(Flow.TAG);
        childrenTags = new String[] {
            Links.TAG,
            Activity.TAG
        };
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Links) {
            setLinks((Links) c);
        } else if (c instanceof Activity) {
            addActivity((Activity) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Links) {
            setLinks(null);
        } else if (c instanceof Activity) {
            removeActivity((Activity) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /** @see Flow#getLinks
     */
    public Links getLinks() {
        return links;
    }
    
    /** @see Flow#setLinks
     */
    public void setLinks(Links l) {
        super.replaceChild(3, links, l);
        links = l;
    }
    
    /** @see MultipleActivityHolder#getActivity
     */
    public Activity getActivity(int i) {
        return (Activity) activities.get(i);
    }
    
    /** @see MultipleActivityHolder#setActivity
     */
    public synchronized void setActivity(int i, Activity a) {
        if (activities.size() == i) {
            addActivity(a);
        } else {
            replaceChild(4, (Activity) activities.get(i), a);
            activities.set(i, a);
        }
    }
    
    /** @see MultipleActivityHolder#addActivity
     */
    public synchronized void addActivity(Activity a) {
        super.addChild(4, a);
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
            super.addChild(4, getActivity(index), a);
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
    
    /** @see MultipleActivityHolder#removeActivity(Activity)
     */
    public synchronized boolean removeActivity(Activity a) {
        super.removeChild(a);
        return activities.remove(a);
    }
    
    /** @see MultipleActivityHolder#getActivitySize
     */
    public int getActivitySize() {
        return activities.size();
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
    
    
    public Link getLink(String linkName) {
		if(this.links == null) {
			return null;
		}
		
		return this.links.getLink(linkName);
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
