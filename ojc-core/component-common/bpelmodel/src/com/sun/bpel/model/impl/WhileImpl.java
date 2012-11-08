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
 * @(#)WhileImpl.java 
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
import com.sun.bpel.model.While;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;while&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class WhileImpl extends ActivityImpl implements While {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -3753767784175439547L;
    
    /** Holds value of activity sub-element. */
    private Activity activity;
    
    /** Holds value of condition sub-element */
    private Condition condition;
    
    //private List xpathVariablesList;
    
    /** Creates a new instance of WhileImpl */
    public WhileImpl() {
        super();
        initWhile();
    }
    
    /** Creates a new instance of WhileImpl.
     * @param   d   Owner document.
     */
    public WhileImpl(XMLDocument d) {
        super(d);
        initWhile();
    }
    
    /** Initializes this class.
     */
    private void initWhile() {
        setLocalName(While.TAG);
        childrenTags = new String[] {
        	Condition.TAG,	
            Activity.TAG
        };
    }
    
    /** Getter for the condition attribute.
     * @return  Value of condition attribute.
     */
    public String getCondition() {
        if(this.condition != null) {
    		return this.condition.getValue();
    	}
    	
    	return null;
    }
    
    /** Setter for the condition attribute.
     * @param   c   Value of condition attribute.
     */
    public void setCondition(String c) {
    	Condition cond = null;
    	
    	if(c != null) {
    		cond = new ConditionImpl(getOwnerDocument());
    		cond.setValue(c);
    	}
    	
    	setBPELCondition(cond);
    	
    }
    
    /** Adds a child of the element.
     * @param   c   Child.
     */
    public void addChild(XMLNode c) {
        if (c instanceof Activity) {
            setActivity((Activity) c);
        } else if (c instanceof Condition) {
        	setBPELCondition((Condition) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** Removes a child of the element
     * @param   c   Child.
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Activity) {
            setActivity(null);
        } else if(c instanceof Condition) {
        	setBPELCondition(null);
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
        super.replaceChild(4, activity, a);
        activity = a;
    }
    
    /**
     * Getter for while Condition.
     * @return Condition
     */
    public Condition getBPELCondition() {
    	return this.condition;
    }
    
    /**
     * Setter for while Condition.
     * @param condition Condition
     */
    public void setBPELCondition(Condition c) {
    	super.replaceChild(3, condition, c);
    	this.condition = c;
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
