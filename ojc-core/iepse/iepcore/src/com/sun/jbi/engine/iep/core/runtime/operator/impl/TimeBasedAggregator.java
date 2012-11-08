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
 * @(#)TimeBasedAggregator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.Map;
import java.util.List;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import com.sun.jbi.engine.iep.core.runtime.util.StringUtil;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Level;


/**
 * TimeBasedAggregator.java
 * 
 * Created on August 1, 2005, 2:31 PM
 * 
 * @author: Bing Lu
 */
public class TimeBasedAggregator extends AbstractOperator {
    private static final Messages mMessages = Messages.getMessages(TimeBasedAggregator.class);

    private List<String> mFromColumnList;
    private List<String> mToColumnList; // exp -> asName
    private String mFromClause;
    private String mWhereClause;
    private String mRWhereClause;
    private List<String> mGroupByColumnList; // hence not necessorily map mSize tuples to 1 tuple
    protected long mStart; // exclusive boundary
    protected long mIncrement;
    protected long mSize;
    protected Timestamp mPrevTimeMarker;
    
    public TimeBasedAggregator(Map prop) {
        initialize(prop);
        mFromColumnList = getStrList((String)prop.get(PROP_FROM_COLUMN_LIST));
        mToColumnList = getStrList((String)prop.get(PROP_TO_COLUMN_LIST));
        mFromClause = " " + (String)prop.get(PROP_FROM_CLAUSE) + " ";
        mWhereClause = (String)prop.get(PROP_WHERE_CLAUSE);
        if (mWhereClause != null) {
            mWhereClause = mWhereClause.trim();
        }
        if (mWhereClause.equals("")) {
            mRWhereClause = "";
        } else {
            Map<String, String> tableNameMap = new HashMap<String, String>();
            Operator inputOp = mInputOperatorList.get(0);
            String inputName = inputOp.getName();
            String inputTableName = inputOp.getQueueName();
            tableNameMap.put(inputName, inputTableName);
            for (Operator o : mStaticInputOperatorList) {
                String staticInputName = o.getName();
                String staticInputTableName = o.getQueueName();
                tableNameMap.put(staticInputName, staticInputTableName);
            }
            mRWhereClause = StringUtil.replacePrefixNotStartWithDot(mWhereClause, tableNameMap);
        }
        mGroupByColumnList = getStrList((String)prop.get(PROP_GROUP_BY_COLUMN_LIST));
        mStart = PropertyUtil.getDateStringAsLong(prop, PROP_START, 0);
        
        double increment = PropertyUtil.getdouble(prop, PROP_INCREMENT, 0.001);
        String incrementUnit = PropertyUtil.getString(prop, PROP_INCREMENT_UNIT, TIME_UNIT_SECOND);
        mIncrement = PropertyUtil.getMiliseconds(increment, incrementUnit);

        double size = PropertyUtil.getdouble(prop, PROP_SIZE, 0.001);
        String unit = PropertyUtil.getString(prop, PROP_UNIT, TIME_UNIT_SECOND);
        mSize = PropertyUtil.getMiliseconds(size, unit);
        mPrevTimeMarker = new Timestamp(mStart);
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
    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }
        mOperateStmt = mDbSpecial.getTimeBasedAggregatorDb().createOperateStatements(con, this);
    }
        
    // template method: used by operate(..)
    @Override
    protected void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        mDbSpecial.getTimeBasedAggregatorDb().executeOperateStatements(this, prevT, curT);
    }

    // template method: used by setConnection(..)
    @Override
    protected void createUpdateInputUsageStmt(Connection con) throws Exception {
        String planId = mQueryPlan.getId();
        String inputTableName = mInputOperatorList.get(0).getQueueName();

        // UPDATE usageTableName SET COL_TIMESTAMP  = ? 
        // WHERE COL_TABLE_NAME = 'S1' AND COL_USER_ID = 'mId';
        String usageTableName = Util.getTableUsageTableName(planId);
        StringBuffer sb = new StringBuffer();
        sb.append("UPDATE ");
        sb.append(usageTableName); 
        sb.append(" SET " + COL_TIMESTAMP + " = ?");
        sb.append(" WHERE " + COL_TABLE_NAME + " = '");
        sb.append(inputTableName);
        sb.append("' AND " + COL_USER_ID + " = '");
        sb.append(mId);
        sb.append("'");
        String stmt0 = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.UpdateTableUsageStmt", new Object[]{getName(), stmt0});
        }
        mUpdateInputUsageStmt = new PreparedStatement[]{
            con.prepareStatement(stmt0),
        }; 
    }

    // template method: used by operate(..)
    @Override
    protected void executeUpdateInputUsageStmt(Timestamp curT) throws Exception {
        ResultSet rs = null;
        try {
            // executeUpdateInputUsageStmt is called after executeOperateStmt in operate()
            long t = mPrevTimeMarker.getTime() - mSize;
            if (t > 0) {
                Timestamp tm = new Timestamp(t);
                mUpdateInputUsageStmt[0].setTimestamp(1, tm);
                mUpdateInputUsageStmt[0].executeUpdate();
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Operator_fail_to_update_input_usage", getName(), e);
            throw e;
        } finally {
            Util.close(rs);
        }
    }
    
    @Override
    public void initProcessingState(Properties processingState) {
        mProcessingState = processingState;
        
        long lv = PropertyUtil.getlong(mProcessingState, PS_PREV_TIMESTAMP_TO_PROCESS, 0);
        // remove prevTimetampToProcess so that it won't be persisted
        // prevTimetampToProcess is persisted as plan level state
        mProcessingState.remove(PS_PREV_TIMESTAMP_TO_PROCESS);
        mPrevTimestampToProcess = new Timestamp(lv);
        
        long prevTM = PropertyUtil.getlong(mProcessingState, PS_PREV_TIME_MARKER, 0L);
        mPrevTimeMarker = new Timestamp(prevTM);
    }
    
    @Override
    public Properties getProcessingState() {
        // update mProcessingState with operator level state
        mProcessingState.setProperty(PS_PREV_TIME_MARKER, "" + mPrevTimeMarker.getTime());
        return mProcessingState;
    }
    
    @Override
    public boolean hasWorkToDo(Timestamp timestampToProcess) {
        if (mInputOperatorList == null || mInputOperatorList.size() == 0) {
            return true;
        }
        if (mCheckInputStmt == null) {
            return false;
        }
        ResultSet rs = null;
        try {
            Timestamp ts = new Timestamp(mPrevTimeMarker.getTime() + mIncrement - mSize);
            for (int i = 0, I = mCheckInputStmt.length; i < I; i++) {
                mCheckInputStmt[i].setTimestamp(1, ts);
                mCheckInputStmt[i].setTimestamp(2, timestampToProcess);
                rs = mCheckInputStmt[i].executeQuery();
                if (rs.next()) {
                    // If one input queue is not empty, that's the work to do
                    // This is the case after enumerating all relation-stream, 
                    // relation-relation, stream-relation operators 
                    Util.close(rs);
                    return true;
                }
                Util.close(rs);
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.failed_to_check_input_status", getName(), e);
        } finally {
            Util.close(rs);
        }
        return false;
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
    
    public String getRWhereClause() {
        return mRWhereClause;
    }

    public List<String> getGroupByColumnList() {
        return mGroupByColumnList;
    }
    
    public long getStart() {
        return mStart;
    }
    
    public long getIncrement() {
        return mIncrement;
    }
    
    public long getSize() {
        return mSize;
    }
    

    @Override
    public Map<String, Object> getAdministrableProperties() {
        HashMap<String,Object> map = (HashMap<String,Object>)super.getAdministrableProperties();
        map.put(PROP_WHERE_CLAUSE, getWhereClause());
        map.put(PROP_FROM_CLAUSE, getFromClause());
        return map;
    }
    

    public void setPrevTimeMarker(Timestamp prevTimeMarker) {
        mPrevTimeMarker = prevTimeMarker;
    }
    
    public Timestamp getPrevTimeMarker() {
        return mPrevTimeMarker;
    }
}

        
