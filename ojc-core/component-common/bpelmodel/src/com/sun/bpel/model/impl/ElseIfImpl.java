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
 * @(#)ElseIfImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.ArrayList;
import java.util.List;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.Condition;
import com.sun.bpel.model.ElseIf;
import com.sun.bpel.model.RepeatUntil;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;

public class ElseIfImpl extends BPELElementImpl implements ElseIf {

    /** Holds activity element */
    private Activity activity;
    
    private Condition mCondition;
    
    //private List xpathVariablesList;
    
    public ElseIfImpl(XMLDocument d) {
        super(d);
        initElseIf();
    }
    
    public Activity getActivity() {
        return activity;
    }

    private void initElseIf() {
        setLocalName(ElseIfImpl.TAG);
        childrenTags = new String[] {
                Condition.TAG,
                Activity.TAG
        };
    }

    public void setActivity(Activity a) {
        super.replaceChild(2, activity, a);
        activity = a;
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

    public Condition getBPELCondition() {
        return this.mCondition;
    }

    public void setBPELCondition(Condition condition) {
        Condition oldCondition = this.mCondition;
        this.mCondition = condition;
        super.replaceChild(1, oldCondition, condition);
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


    /* (non-Javadoc)
     * @see com.sun.bpel.model.ConditionReference#getXPathVariablesList()
     */
    /*public List getXPathVariablesList() {
        return xpathVariablesList;
    }

     (non-Javadoc)
     * @see com.sun.bpel.model.ConditionReference#setXPathVariablesList(java.util.List)
     
    public void setXPathVariablesList(List list) {
        if(null == xpathVariablesList) {
            xpathVariablesList = new ArrayList();
        }
        xpathVariablesList = list;
    }*/
}
