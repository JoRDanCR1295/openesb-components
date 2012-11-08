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
 * @(#)AssignmentImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.jxpath.JXPathContext;
import org.w3c.dom.Node;

import com.sun.jbi.workflow.model.Assignment;
import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.ModelException;
import com.sun.jbi.workflow.model.utl.ModelUtil;
import com.sun.jbi.workflow.model.xmlbeans.TAssignment;
import com.sun.jbi.workflow.model.xmlbeans.TExcluded;
import com.sun.jbi.workflow.model.xmlbeans.TExpression;

/**
 * 
 * 
 */
public class AssignmentImpl extends ModelElementImpl implements Assignment {

    TAssignment mAssign;

    public AssignmentImpl(TAssignment assignment, ModelElement parent) {
        super(assignment, parent);
        mAssign = assignment;
    }

    public synchronized List<String> getGroups(JXPathContext context)
            throws ModelException {
        // TODO Auto-generated method stub
        List<String> mGroups = new ArrayList<String>();
        List<TExpression> groups = mAssign.getGroupList();
        if (groups != null && groups.size() > 0) {
            for (int i = 0; i < groups.size(); i++) {
                TExpression groupExp = groups.get(i);
                String strVal = null;
                if (groupExp != null) {
                    strVal = ModelUtil.getExpressionContent(context, groupExp,
                            getTask(), "|");
                    // String text = getContent(groupExp.getDomNode());
                    // if (text != null && text.trim().length() > 0) {
                    // try {
                    // strVal = ModelUtil.getStringValue(context, text,
                    // "|");
                    // } catch (Exception e) {
                    // // TODO Auto-generated catch block
                    // throw new ModelException(e);
                    // }
                    // }
                }
                if (strVal != null && strVal.trim().length() > 0) {
                    String[] allVals = strVal.split("\\|");
                    for (int j = 0; j < allVals.length; j++) {
                        mGroups.add(allVals[j]);
                    }
                }
            }
        }
        return mGroups;
    }

    public synchronized List<String> getUsers(JXPathContext context)
            throws ModelException {
        List<String> mUsers;
        mUsers = new ArrayList<String>();
        List<TExpression> users = mAssign.getUserList();
        if (users != null && users.size() > 0) {
            for (int i = 0; i < users.size(); i++) {
                TExpression userExp = users.get(i);
                String strVal = null;
                if (userExp != null) {
                    strVal = ModelUtil.getExpressionContent(context, userExp,
                            getTask(), "|");
                    // String text = getContent(userExp.getDomNode());
                    // if (text != null && text.trim().length() > 0) {
                    // try {
                    // strVal = ModelUtil.getStringValue(context, text,
                    // "|");
                    // } catch (Exception e) {
                    // // TODO Auto-generated catch block
                    // throw new ModelException(e);
                    // }
                }
                if (strVal != null && strVal.trim().length() > 0) {
                    String[] allVals = strVal.split("\\|");
                    for (int j = 0; j < allVals.length; j++) {
                        mUsers.add(allVals[j]);
                    }
                }
            }
        }
        return mUsers;
    }

    private static String getContent(Node domNode) {
        if (domNode != null) {
            String strVal = ModelUtil.getText(domNode);
            return strVal;
        }
        return null;
    }

    public List<String> getExcludedGroups(JXPathContext context)
            throws ModelException {
        // TODO Auto-generated method stub
        List<String> mGroups = new ArrayList<String>();
        TExcluded excluded = mAssign.getExcluded();
        if (excluded != null) {
            List<TExpression> groups = excluded.getGroupList();
            if (groups != null && groups.size() > 0) {
                for (int i = 0; i < groups.size(); i++) {
                    TExpression groupExp = groups.get(i);
                    String strVal = null;
                    if (groupExp != null) {
                        strVal = ModelUtil.getExpressionContent(context, groupExp, getTask(), "|");                        
//                        String text = getContent(groupExp.getDomNode());
//                        if (text != null && text.trim().length() > 0) {
//                            try {
//                                strVal = ModelUtil.getStringValue(context,
//                                        text, "|");
//                            } catch (Exception e) {
//                                // TODO Auto-generated catch block
//                                throw new ModelException(e);
//                            }
//                        }
                        if (strVal != null && strVal.trim().length() > 0) {
                            String[] allVals = strVal.split("\\|");
                            for (int j = 0; j < allVals.length; j++) {
                                mGroups.add(allVals[j]);
                            }
                        }
                    }
                }
            }
        }
        return mGroups;
    }

    public List<String> getExcludedUsers(JXPathContext context)
            throws ModelException {
        List<String> mUsers;
        mUsers = new ArrayList<String>();
        TExcluded excluded = mAssign.getExcluded();
        if (excluded != null) {
            List<TExpression> users = excluded.getUserList();
            if (users != null && users.size() > 0) {
                for (int i = 0; i < users.size(); i++) {
                    TExpression userExp = users.get(i);
                    String strVal = null;
                    if (userExp != null) {
                        strVal = ModelUtil.getExpressionContent(context, userExp, getTask(), "|");                       
//                        strVal = ModelUtil.getExpressionContent(context,
//                                userExp, getTask(), "|");
                        // String text = getContent(userExp.getDomNode());
                        // if (text != null && text.trim().length() > 0) {
                        // try {
                        // strVal = ModelUtil.getStringValue(context,
                        // text, "|");
                        // } catch (Exception e) {
                        // // TODO Auto-generated catch block
                        // throw new ModelException(e);
                        // }
                    }
                    if (strVal != null && strVal.trim().length() > 0) {
                        String[] allVals = strVal.split("\\|");
                        for (int j = 0; j < allVals.length; j++) {
                            mUsers.add(allVals[j]);
                        }
                    }
                }
            }
        }
        return mUsers;
    }
}
