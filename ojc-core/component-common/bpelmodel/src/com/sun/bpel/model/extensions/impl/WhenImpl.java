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
 * @(#)WhenImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.extensions.impl;

import java.util.ArrayList;
import java.util.List;
import javax.xml.namespace.QName;

import com.sun.bpel.model.Condition;
import com.sun.bpel.model.Copy;
import com.sun.bpel.model.extensions.Choose;
import com.sun.bpel.model.extensions.ForEach;
import com.sun.bpel.model.extensions.When;
import com.sun.bpel.model.impl.ConditionImpl;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.visitor.Visitor;


/**
 * Implements the &lt;When&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class WhenImpl extends ExtnChildrenImpl implements When {

    //private List xpathVariablesList;
    
    /** QName object for SeeBeyond Private extension line label */
    public static final QName WHEN_QNAME =
    	com.sun.bpel.xml.NamespaceUtility.getQName(
    				XMLElement.SBYNBPEL_RUNTIME_EXTN_NAMESPACE,
    				When.TAG,
    				XMLElement.SBYNBPEL_RUNTIME_EXTN_PREFIX);
	
    /** Creates a new instance of WhenImpl */
    public WhenImpl() {
        super();
        initWhen();
    }
    
    /** Creates a new instance of WhenImpl.
     * @param   d   Owner document.
     */
    public WhenImpl(XMLDocument d) {
        super(d);
        initWhen();
    }
    
    /** Initializes this class.
     */
    private void initWhen() {
        setLocalName(When.TAG);
        setQualifiedName(WHEN_QNAME);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.EXPRESSION, String.class, false, null)
        };
        childrenTags = new String[] {
            Copy.TAG,
            ForEach.TAG,
            Choose.TAG
        };

    }
    
    /** Getter for property condition.
     * @return  Value of property condition.
     */
    public String getCondition() {
        return xmlAttrs[EXPRESSION].getValue();
    }
    
    /** Setter for property condition.
     * @param   c   Value of property condition.
     */
    public void setCondition(String c) {
        setAttribute(EXPRESSION, c);
    }
    
    

    public Condition getBPELCondition() {
		Condition condition = new ConditionImpl(getOwnerDocument());
		condition.setValue(getCondition());
		
		return condition;
	}

	public void setBPELCondition(Condition condition) {
		String conditionString = null;
		
		if(condition != null) {
			conditionString = condition.getValue();
			
		} 
		
		setCondition(conditionString);
	}

	/**
     * Check if two ForEaches have the same rules
     * @param other - another <code>ForEach</code> instance
     * @return boolean - true if same
     */
    public boolean hasSameRules(When other) {
        if (other == null) {
            return false;
        }
        if (!strEqual(this.getCondition(), other.getCondition())) {
            return false;
        }
        return true;
    }

    private boolean strEqual(String str1, String str2) {
        if (str1 == null) {
            if (str2 != null) {
                return false;
            }
        } else {
            if (str2 == null) {
                return false;
            } else if (!str1.equals(str2)) {
                return false;
            }
        }
        return true;
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
