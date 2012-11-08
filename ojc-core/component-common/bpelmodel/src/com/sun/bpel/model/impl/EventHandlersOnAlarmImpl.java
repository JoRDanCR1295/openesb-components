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
 * @(#)EventHandlersOnAlarmImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.EventHandlersOnAlarm;
import com.sun.bpel.model.For;
import com.sun.bpel.model.OnAlarm;
import com.sun.bpel.model.RepeatEvery;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.Until;
import com.sun.bpel.model.OnAlarm.ATTR;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;


/**
 * Implements the &lt;onAlarm&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class EventHandlersOnAlarmImpl extends ActivityImpl implements EventHandlersOnAlarm {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID =  -8138936364859154161L;
    
    /** Holds activity sub-element. */
    private Scope activity;
    
    /** For child element for this wait */
    private For mFor = null;
    
    /** Untill child element for this wait */
    private Until mUntil = null;
    
    /** RepeatEvery child element for this wait */
    private RepeatEvery mRepeatEvery = null;
    
    /** Creates a new instance of EventHandlersOnAlarmImpl */
    public EventHandlersOnAlarmImpl() {
        super();
        initEventHandlersOnAlarm();
    }
    
    /** Creates a new instance of EventHandlersOnAlarmImpl.
     * @param   d   Owner document.
     */
    public EventHandlersOnAlarmImpl(XMLDocument d) {
        super(d);
        initEventHandlersOnAlarm();
    }
    
    /** Initializes this class. 
     */
    private void initEventHandlersOnAlarm() {
        setLocalName(EventHandlersOnAlarm.TAG);
        childrenTags = new String[] {
            For.TAG,
            Until.TAG,
            RepeatEvery.TAG,
            Activity.TAG
        };
    }
    
    /** Getter for the for attribute.
     * @return  Value of the for attribute.
     */
    public String getFor() {
        if(this.mFor != null) {
        	return this.mFor.getValue();
        }
        
        return null;
    }
    
    /** Setter for the for attribute.
     * @param   f   Value of the for attribute.
     */
    public void setFor(String f) {
    	For fr = null;
    	if(f != null) {
    		fr = new ForImpl(getOwnerDocument());
    		fr.setValue(f);
    	}
    	
        setBPELFor(fr);
    }
    
    /** Getter for the until attribute.
     * @return  Value of the until attribute.
     */
    public String getUntil() {
        if(this.mUntil != null) {
        	return this.mUntil.getValue();
        }
        
        return null;
    }
    
    /** Setter for the until attribute.
     * @param   u   Value of the until attribute.
     */
    public void setUntil(String u) {
    	Until ul = null;
    	if(u != null) {
    		ul = new UntilImpl(getOwnerDocument());
    		ul.setValue(u);
    	}
    	
        setBPELUntil(ul);
    }
    
    
    /**
     *  set the For child element for this EventHandlersOnAlarm.
     * @param f for child element.
     */
    public void setBPELFor(For f) {
    	this.replaceChild(1, this.mFor, f);
    	this.mFor = f;
    }
    
    /**
     * get For child element of this EventHandlersOnAlarm.
     * @return For
     */
    public For getBPELFor() {
    	return this.mFor;
    }
    
    /**
     * set Until child element for this EventHandlersOnAlarm.
     * @param u Until child element
     */
    public void setBPELUntil(Until u) {
    	this.replaceChild(1, this.mUntil, u);
    	this.mUntil = u;
    }
    
    /**
     * get Untill child elememt for this EventHandlersOnAlarm.
     * @return Until child element.
     */
    public Until getBPELUntil() {
    	return this.mUntil;
    }
    
	/** Adds a child of the element.
     * @param   c   Child.
     */
    public void addChild(XMLNode c) {
        if (c instanceof Activity) {
            setActivity((Activity) c);
        } else if(c instanceof For) {
        	setBPELFor((For) c);
        } else if(c instanceof Until) {
        	setBPELUntil((Until) c);
        } else if(c instanceof RepeatEvery) {
            setBPELRepeatEvery((RepeatEvery) c);
        } 
        else {
            super.addChild(c);
        }
    }
    
    /** Removes a child of the element
     * @param   c   Child.
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Activity) {
            setActivity(null);
        } else if(c instanceof For) {
        	setBPELFor(null);
        } else if(c instanceof Until) {
        	setBPELUntil(null);
        } else if(c instanceof RepeatEvery) {
            setBPELRepeatEvery(null);
        } else {
            super.removeChild(c);
        }
    }
    
    /** @see SingleActivityHolder#getActivity
     */
    public Activity getActivity() {
        return activity;
    }
    
    /** @see SingleActivityHolder#setActivity
     */
    public void setActivity(Activity a) {
        super.replaceChild(2, activity, a);
        activity = (Scope) a;
    }
    
    /** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
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
    /** Getter for the for attribute.
     * @return  Value of the for attribute.
     */
    public String getRepeatEvery() {
        if(this.mRepeatEvery != null) {
            return this.mRepeatEvery.getValue();
        }
        
        return null;
    }
    
    /** Setter for the for attribute.
     * @param   f   Value of the for attribute.
     */
    public void setRepeatEvery(String f) {
        RepeatEvery fr = null;
        if(f != null) {
            fr = new RepeatEveryImpl(getOwnerDocument());
            fr.setValue(f);
        }
        
        setBPELRepeatEvery(fr);
    }
    
    
    /**
     *  set the RepeatEvery child element for this EventHandlersOnAlarm.
     * @param f for child element.
     */
    public void setBPELRepeatEvery(RepeatEvery f) {
        this.replaceChild(1, this.mRepeatEvery, f);
        this.mRepeatEvery = f;
    }
    
    /**
     * get RepeatEvery child element of this EventHandlersOnAlarm.
     * @return For
     */
    public RepeatEvery RepeatEvery() {
        return this.mRepeatEvery;
    }
    
    
}
