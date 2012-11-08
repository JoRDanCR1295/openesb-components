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
 * @(#)IfImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.Condition;
import com.sun.bpel.model.Else;
import com.sun.bpel.model.ElseIf;
import com.sun.bpel.model.If;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 *
 * @author Sun Microsystems
 */
public class IfImpl extends ActivityImpl implements If {
    
    private Activity mActivity;
    
    private Condition mCondition;


    private ArrayList elseIfs = new ArrayList();

    private Else elsee = null;
    
    private ElseIf elseIf = null;
    
    //private List xpathVariablesList;
    
    /** Creates a new instance of IfImpl */
    public IfImpl() {
        super();
        initIf();
    }
    
    /** Creates a new instance of RepeatUntilImpl.
     * @param   d   Owner document.
     */
    public IfImpl(XMLDocument d) {
        super(d);
        initIf();
    }
    
    private void initIf() {
          setLocalName(If.TAG);
          childrenTags = new String[] {
              Condition.TAG,
              Activity.TAG,
              ElseIf.TAG,
              Else.TAG
          };
    }
      
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Activity) {
        	//setActivity((Activity) c);
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
        	//setActivity(null);
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
    
    public Condition getBPELCondition() {
        return this.mCondition;
    }
    
    public void setBPELCondition(Condition condition) {
        Condition oldCondition = this.mCondition;
        this.mCondition = condition;
        super.replaceChild(3, oldCondition, condition);
    }
    
    public Activity getActivity() {
        return mActivity;
    }

    public void setActivity(Activity a) {
        super.replaceChild(2, mActivity, a);
        mActivity = a;
    }
    


    /* (non-Javadoc)
     * @see com.sun.bpel.model.If#addElseIf(com.sun.bpel.model.ElseIf)
     */
    public void addElseIf(ElseIf elseIf) {
        super.replaceChild(5, this.elseIf, elseIf);
        this.elseIfs.add(elseIf);
    }

    /* (non-Javadoc)
     * @see com.sun.bpel.model.If#setElse(com.sun.bpel.model.Else)
     */
    public void setElse(Else elsee) {
        super.replaceChild(6, this.elsee, elsee);
        this.elsee = elsee;
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
     * @see com.sun.bpel.model.If#getElseIfs()
     */
    public Collection getElseIfs() {
        return elseIfs;
    }

    /* (non-Javadoc)
     * @see com.sun.bpel.model.If#getElse()
     */
    public Else getElse() {
        return elsee;
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
