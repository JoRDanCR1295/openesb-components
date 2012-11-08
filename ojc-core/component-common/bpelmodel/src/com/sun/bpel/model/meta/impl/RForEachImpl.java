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
 * @(#)RForEachImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta.impl;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.sun.bpel.model.impl.ForEachImpl;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RExpressionElement;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.ScopingElement;
import com.sun.bpel.model.parser.impl.RBPELDocumentImpl;
import com.sun.bpel.model.util.Utility;
import com.sun.bpel.model.util.Utility.XpathVariableInfo;
import com.sun.bpel.model.util.Utility.XpathVariablePropertyInfo;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;

public class RForEachImpl extends ForEachImpl implements RActivity, RExpressionElement, RActivityHolder, ScopingElement {
    
    private long mUniqueID;
    private RActivity mNextAct = null;
    private Set<XpathVariableInfo> varSet;
    private Map<String, MessagePropertyAlias> propAliasMap;
    
    
    /** Creates a new instance of RIfImpl */
    public RForEachImpl(RBPELDocumentImpl bpeldoc, long id) {
        super(bpeldoc);
        mUniqueID = id;
    }
    
    public RActivity getNextActivity() {
        return mNextAct;
    }
    
    public void setNextActivity(RActivity act) {
        mNextAct = act;
    }
    
    public long getUniqueId() {
        return mUniqueID;
    }

     
    public Iterator getVariables() {
        return varSet.iterator();
    }
     
    /** @see com.sun.bpel.model.meta.RExpressionElement#setXPathExpression(java.lang.String)
     */
    public void setXPathExpression(String expression) throws Exception {
        XpathVariablePropertyInfo varPropInfo = Utility.parseExprForVariables(expression, this);
        varSet = varPropInfo.varInfoSet;
        propAliasMap = varPropInfo.propAliasMap;
    }
    
    /**
     * gets child activity
     *
     * @return RActivity child activity
     */
    public RActivity getChildActivity() {
        return (RActivity) super.getActivity();
    }

    /**
     * sets child activity
     *
     * @param act child activity
     *
     * @throws UnsupportedOperationException DOCUMENT ME!
     */
    public void setChildActivity(RActivity act) {
        throw new UnsupportedOperationException();
    }

    public long getScopeId() {
        // TODO Auto-generated method stub
        return mUniqueID;
    }

	/* (non-Javadoc)
	 * @see com.sun.bpel.model.impl.ForEachImpl#initMembers()
	 */
	@Override
	public void initMembers() {
		// TODO Auto-generated method stub
		super.initMembers();
		RVariable var = (RVariable) getCounterVariable();
		if (var.getScopeId() == 0) {
			var.setScopeId(getScopeId());
		}
		
	}

	/** @see com.sun.bpel.model.meta.RExpressionElement#getPropertyAliasForVariableProperty(java.lang.String)
	 */
	public MessagePropertyAlias getPropertyAliasForVariableProperty(String key) {
        MessagePropertyAlias alias = null;
		if (propAliasMap != null && !propAliasMap.isEmpty()) {
			alias = propAliasMap.get(key);
		}
		return alias;
	}
}
