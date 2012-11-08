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
 * @(#)ForEachImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import javax.xml.namespace.QName;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.CompletionCondition;
import com.sun.bpel.model.ForEach;
import com.sun.bpel.model.Iterator;
import com.sun.bpel.model.SingleActivityHolder;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class ForEachImpl extends ActivityImpl implements ForEach {

	private Iterator mIterator;
    
    private CompletionCondition mCompletionCondition;
    
    private Activity mActivity;

    private Variable mVariable;
            
    /** Creates a new instance of ForEachImpl.
     */
    public ForEachImpl() {
        super();
        initForEach();
    }
    
    /** Creates a new instance of ForEachImpl.
     * @param   d   Owner document.
     */
    public ForEachImpl(XMLDocument d) {
        super(d);
        initForEach();
    }
    
    /** Initializes this class.
     */
    private void initForEach() {
        setLocalName(ForEach.TAG);
        xmlAttrs = new XMLAttribute[NUM_ATTRS];
        // NUM_STANDARD_ATTRS is equal to standardXmlAttrs.length.
        for (int i = 0; i < NUM_STANDARD_ATTRS; i++) {
        	xmlAttrs[i] = standardXmlAttrs[i];
        }
        xmlAttrs[COUNTERNAME] = new XMLAttributeImpl(ForEach.ATTR.COUNTERNAME, String.class, false, null);
        xmlAttrs[PARALLEL] = new XMLAttributeImpl(ForEach.ATTR.PARALLEL, String.class, false, 
        		new String[] {"yes", "no"});
        
        childrenTags = new String[] {
            Iterator.TAG,
            CompletionCondition.TAG,
            Activity.TAG
        };
    }
    
    /* (non-Javadoc)
     * @see com.sun.bpel.model.ForEach#initMembers()
     */
    public void initMembers() {
        initCounterVariable();
        resolveCounterVariable();
    }
    
    public void initMembersLocally() {
        initCounterVariable();
    }

    private void initCounterVariable(){
        if (!BPELHelper.isValueAbsent(getCounterName())) {
            
            //Create a new variable with this name
            mVariable = ((BPELDocument) getOwnerDocument()).createVariable();
            BPELProcess bpelProcess = ((BPELDocument) getOwnerDocument()).getDocumentProcess ();
            mVariable.setName(getCounterName());
            QName qName = com.sun.bpel.xml.NamespaceUtility.getQName(
            				BPELProcess.XSD_NAME_SPACE_URI,
            				"int",
            				bpelProcess.getNamespacePrefix(BPELProcess.XSD_NAME_SPACE_URI));
            mVariable.setType(qName);
        }
    }
    
    private void resolveCounterVariable() {
        if (!BPELHelper.isValueAbsent(getCounterName())) {
            mVariable.initMembers();
        }
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Iterator) {
            setIterator((Iterator) c);
        } else if (c instanceof CompletionCondition) {
            setCompletionCondition((CompletionCondition) c);
        } else if (c instanceof Activity) {
        	setActivity((Activity) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Iterator) {
            setIterator(null);
        } else if (c instanceof CompletionCondition) {
            setCompletionCondition(null);
        } else if(c instanceof Activity) {
        	setActivity(null);
        } else {
            super.removeChild(c);
        }
    }

	public CompletionCondition getCompletionCondition() {
		return this.mCompletionCondition;
	}

	public String getCounterName() {
		return xmlAttrs[COUNTERNAME].getValue();
	}

	public String getParallel() {
		return xmlAttrs[PARALLEL].getValue();
	}
	
	public Iterator getIterator() {
		return this.mIterator;
	}

	

	public void setCounterName(String counterName) {
		setAttribute(COUNTERNAME, counterName);
	}
	
	public void setIterator(Iterator it) {
		Iterator oldIterator = this.mIterator;
		this.mIterator = it;
		super.replaceChild(3, oldIterator, it);
		
		
	}
	
	public void setCompletionCondition(CompletionCondition cCondition) {
		CompletionCondition oldCCondition = this.mCompletionCondition;
		this.mCompletionCondition = cCondition;
		super.replaceChild(4, oldCCondition, cCondition);
		
	}

	

	public void setParallel(String parallel) {
		setAttribute(PARALLEL, parallel);
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
    	super.replaceChild(5, oldActivity, activity);
    	
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

    /**
     * Return the BPEL variable for counterName
     * @return
     */
    public Variable getCounterVariable() {
        return this.mVariable;
    }    
    
    /*
     *  (non-Javadoc)
     * @see com.sun.bpel.model.VariableScope#getBPELVariable(java.lang.String)
     */public Variable getBPELVariable(String vName) {
        // TODO Auto-generated method stub
         // TODO Auto-generated method stub
         String varName = getCounterName();
         if (varName.equals(vName)) {
             return getCounterVariable();
         } else {
             return null;
         }
    }

    public void performDeferredAction() {
        resolveCounterVariable();;
    }
}
