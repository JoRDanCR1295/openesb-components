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
 * @(#)StreamProjectionAndFilter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import java.sql.Connection;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.PreparedStatement;

/**
 * StreamProjectionAndFilter.java
 * 
 * Created on September 8, 2005, 12:47 AM
 * 
 * 
 * @author Bing Lu
 */
public class StreamProjectionAndFilter extends AbstractOperator {
    private List<String> mFromColumnList;
    private List<String> mToColumnList; // exp -> asName
    private String mFromClause;
    private String mWhereClause;

    public StreamProjectionAndFilter(Map prop) {
        initialize(prop);
        mFromColumnList = getStrList((String)prop.get(PROP_FROM_COLUMN_LIST));
        mToColumnList = getStrList((String)prop.get(PROP_TO_COLUMN_LIST));
        mFromClause = " " + (String)prop.get(PROP_FROM_CLAUSE) + " ";
        mWhereClause = (String)prop.get(PROP_WHERE_CLAUSE);
    }
    
    public String getOutputType() {
        return IO_TYPE_STREAM;
    }

    @Override
    protected void createSynopsis(Connection con) throws Exception {
        mDbSpecial.createSequence(con, this);
    }
    
    @Override
    protected void dropSynopsis(Connection con) throws Exception {
        mDbSpecial.dropSequence(con, this);
    }

    @Override
    protected void registerInputTableUsage(Connection con) throws Exception {
        String planId = mQueryPlan.getId();
        String fullId = planId + "." + mId;
        for (Operator inputOp: mInputOperatorList) {
            String inputTableName = inputOp.getQueueName();
            String inputPlanId = inputOp.getPlan().getId();
            if (inputPlanId.equals(planId)) {
                Util.initializeTableUsage(con, inputPlanId, inputTableName, mId);
            } else {
                Util.initializeTableUsage(con, inputPlanId, inputTableName, fullId);
            }
        }
        for (Operator inputOp: mStaticInputOperatorList) {
            if (!inputOp.getOutputType().equals(IO_TYPE_RELATION)) {
                continue;
            }
            String inputTableName = inputOp.getQueueName();
            String inputPlanId = inputOp.getPlan().getId();
            if (inputPlanId.equals(planId)) {
                Util.initializeTableUsage(con, inputPlanId, inputTableName, mId);
            } else {
                Util.initializeTableUsage(con, inputPlanId, inputTableName, fullId);
            }
        }
    }

    @Override
    protected void unregisterInputTableUsage(Connection con) throws Exception {
        String planId = mQueryPlan.getId();
        String fullId = mQueryPlan.getId() + "." + mId;
        for (Operator inputOp: mInputOperatorList) {
            String inputTableName = inputOp.getQueueName();
            String inputPlanId = inputOp.getPlan().getId();
            if (inputPlanId.equals(planId)) {
                Util.deleteTableUsage(con, inputPlanId, inputTableName, mId);
            } else {
                Util.deleteTableUsage(con, inputPlanId, inputTableName, fullId);
            }
        }
        for (Operator inputOp: mStaticInputOperatorList) {
            if (!inputOp.getOutputType().equals(IO_TYPE_RELATION)) {
                continue;
            }
            String inputTableName = inputOp.getQueueName();
            String inputPlanId = inputOp.getPlan().getId();
            if (inputPlanId.equals(planId)) {
                Util.deleteTableUsage(con, inputPlanId, inputTableName, mId);
            } else {
                Util.deleteTableUsage(con, inputPlanId, inputTableName, fullId);
            }
        }
    }

    @Override
    protected void createUpdateInputUsageStmt(Connection con) throws Exception {
        String planId = mQueryPlan.getId();
        String fullId = mQueryPlan.getId() + "." + mId;
        List<PreparedStatement> stmtList = new ArrayList<PreparedStatement>();
        for (Operator inputOp: mInputOperatorList) {
            String inputTableName = inputOp.getQueueName();
            String inputPlanId = inputOp.getPlan().getId();
            if (inputPlanId.equals(planId)) {
                stmtList.add(Util.createUpdateTableUsageStmt(con, inputPlanId, inputTableName, mId));
            } else {
                stmtList.add(Util.createUpdateTableUsageStmt(con, inputPlanId, inputTableName, fullId));
            }
        }
        for (Operator inputOp : mStaticInputOperatorList) {
            if (!inputOp.getOutputType().equals(IO_TYPE_RELATION)) {
                continue;
            }
            String inputTableName = inputOp.getQueueName();
            String inputPlanId = inputOp.getPlan().getId();
            if (inputPlanId.equals(planId)) {
                stmtList.add(Util.createUpdateTableUsageStmt(con, inputPlanId, inputTableName, mId));
            } else {
                stmtList.add(Util.createUpdateTableUsageStmt(con, inputPlanId, inputTableName, fullId));
            }
        }
        mUpdateInputUsageStmt = (PreparedStatement[])stmtList.toArray(new PreparedStatement[0]);
    }

    @Override
    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }
        mOperateStmt = mDbSpecial.getStreamProjectionAndFilterDb().createOperateStatements(con, this);
    }
    
    public List<String> getFromColumnList() {
        return mFromColumnList;
    }

    public List<String> getToColumnList() {
        return mToColumnList;
    }
    
    public String getFromClause() {
        return mFromClause;
    }
    
    public String getWhereClause() {
        return mWhereClause;
    }
    
    @Override
    public Map<String, Object> getAdministrableProperties() {
        HashMap<String,Object> map = (HashMap<String,Object>)super.getAdministrableProperties();
        map.put(PROP_WHERE_CLAUSE, getWhereClause());
        map.put(PROP_FROM_CLAUSE, getFromClause());
        
        return map;
    }
     
    @Override
    public void setAdministrableProperty(String propName, Object propValue) {
        if(propName.equals(PROP_WHERE_CLAUSE)) {
            mWhereClause = (String)propValue;
            isReinitNeeded = true;
        }
    }
    
    private static final Pattern SelectPattern = Pattern.compile("\\((\\s)*SELECT(\\s)+", Pattern.CASE_INSENSITIVE);
    private static final Pattern FromPattern = Pattern.compile("(\\s)+FROM(\\s)+", Pattern.CASE_INSENSITIVE);
    private static final Pattern WherePattern = Pattern.compile("(\\s)+WHERE(\\s)+", Pattern.CASE_INSENSITIVE);
    
    // see-saw recursion to expandSubquery
    public static String expandWhereClause(String whereClause, String streamInputQueueName, List<String> relationInputQueueNameList) {
        StringBuffer sb = new StringBuffer();
        int preEnd = -1;
        while (true) {
            if (preEnd + 1 >= whereClause.length()) { 
                break;
            }
            Matcher selectMatcher = SelectPattern.matcher(whereClause);
            boolean foundSelect = selectMatcher.find(preEnd + 1);
            if (!foundSelect) { 
                sb.append(whereClause.substring(preEnd + 1));
                break;
            }
            int subqueryStart = selectMatcher.start();
            sb.append(whereClause.substring(preEnd + 1, subqueryStart)); 
            
            boolean parenthesisMathed = false;
            int leftParenthesisCnt = 0;
            int subqueryEnd = subqueryStart;
            for (int i = subqueryStart; i < whereClause.length(); i++) {
                char c = whereClause.charAt(i);
                if (c == '(') {
                    leftParenthesisCnt++;
                } else if (c == ')') {
                    leftParenthesisCnt--;
                    if (leftParenthesisCnt == 0) {
                        parenthesisMathed = true;
                        subqueryEnd = i;
                        break;
                    }
                }
            }
            if (!parenthesisMathed) { 
                return whereClause;
            }
            sb.append(expandSubquery(whereClause.substring(subqueryStart, subqueryEnd+1), streamInputQueueName, relationInputQueueNameList));
            
            preEnd = subqueryEnd;
        }
        return sb.toString();
    }    

    
    // see-saw recursion to expandWhereClause
    private static String expandSubquery(String subquery, String streamInputQueueName, List<String> relationInputQueueNameList) {
        StringBuffer sb = new StringBuffer();
        Matcher fromMatcher = FromPattern.matcher(subquery);
        boolean foundFrom = fromMatcher.find(0);
        if (!foundFrom) { 
            return subquery;
        }
        int inputListStart = fromMatcher.end();

        List<String> relationInputQueueNameInFromClauseList = new ArrayList<String>();
        Matcher whereMatcher = WherePattern.matcher(subquery);
        boolean foundWhere = whereMatcher.find(inputListStart);
        int inputListEnd = foundWhere? whereMatcher.start() : subquery.length()-1;
        int whereClauseBegin = foundWhere? whereMatcher.end() : subquery.length()-1;
        int whereClauseEnd = subquery.length()-1; 
        
        String inputListStr = subquery.substring(inputListStart, inputListEnd);
        StringTokenizer st = new StringTokenizer(inputListStr, ",");
        while (st.hasMoreTokens()) {
            String token = st.nextToken().trim();
            int spaceBegin = token.indexOf(" ");
            if (spaceBegin >= 0) {
                continue;
            } else {
                if (relationInputQueueNameList.contains(token)) {
                    relationInputQueueNameInFromClauseList.add(token);
                }
            }
        }
        
        sb.append(subquery.substring(0, whereClauseBegin)); 
        if (!foundWhere) { 
            sb.append(" WHERE ");
        }     
        
        for (int i = 0; i < relationInputQueueNameInFromClauseList.size(); i++) {
            String queueName = relationInputQueueNameInFromClauseList.get(i);
            if (i > 0) {
                sb.append(" AND ");
            }
            sb.append(queueName + "." + COL_TIMESTAMP + " <= " + streamInputQueueName + "." + COL_TIMESTAMP);
            sb.append(" AND " + queueName + "." + COL_TAG + " = '+'");
            sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + queueName + " r WHERE");
            sb.append(" r." + COL_TIMESTAMP + " <= " + streamInputQueueName + "." + COL_TIMESTAMP);
            sb.append(" AND r." + COL_SEQID + " = " + queueName + "." + COL_SEQID);
            sb.append(" AND r." + COL_TAG + " = '-')");
        }        

        if (foundWhere) {
            sb.append(" AND "); 
            sb.append(expandWhereClause(subquery.substring(whereClauseBegin, whereClauseEnd), streamInputQueueName, relationInputQueueNameList));
        }
        
        sb.append(")");

        return sb.toString();
    }
}

        
