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
 * @(#)RepeatUntilImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.Condition;
import com.sun.bpel.model.RepeatUntil;
import com.sun.bpel.model.SingleActivityHolder;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class RepeatUntilImpl extends ActivityImpl implements RepeatUntil {

	
    private Activity mActivity;
    
    private Condition mCondition;
    
    //private List xpathVariablesList;
    
    /** Creates a new instance of RepeatUntilImpl.
     */
    public RepeatUntilImpl() {
        super();
        initRepeatUntil();
    }
    
    /** Creates a new instance of RepeatUntilImpl.
     * @param   d   Owner document.
     */
    public RepeatUntilImpl(XMLDocument d) {
        super(d);
        initRepeatUntil();
    }
    
    /** Initializes this class.
     */
    private void initRepeatUntil() {
        setLocalName(RepeatUntil.TAG);
        childrenTags = new String[] {
        	Activity.TAG,
        	Condition.TAG
        };
    }
    
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Activity) {
        	setActivity((Activity) c);
        } else if(c instanceof Condition) {
        	setBPELCondition((Condition) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if(c instanceof Activity) {
        	setActivity(null);
        } else if(c instanceof Condition) {
        	setBPELCondition(null);
        } else {
            super.removeChild(c);
        }
    }

	public String getCondition() {
		if(this.mCondition != null) {
			return this.mCondition.getValue();
		}
		
		return null;
	}
	
	public void setCondition(String condition) {
		Condition c = null;
		if(condition != null) {
			c = new ConditionImpl(getOwnerDocument());
			c.setValue(condition);
			
		} 
		
		setBPELCondition(c);
		
	}

	public void setBPELCondition(Condition condition) {
		Condition oldCondition = this.mCondition;
		this.mCondition = condition;
		super.replaceChild(4, oldCondition, condition);
		
	}

	public Condition getBPELCondition() {
		return this.mCondition;
	}


	/** @see SingleActivityHolder#getActivity
     */
    public Activity getActivity() {
        return this.mActivity;
    }
    
    /** @see SingleActivityHolder#setActivity
     */
    public void setActivity(Activity activity) {
    	Activity oldActivity = this.mActivity;
    	this.mActivity = activity;
    	super.replaceChild(3, oldActivity, activity);
    	
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
}
